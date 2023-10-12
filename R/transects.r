#' Generate Line Transects
#'
#' Creates line transects covering a given sampling frame made up of one or more
#'   polygons, spaced evenly at an interval that results in a total length as
#'   close as possible to the target total provided.
#'
#' @param sampling_frame \code{sf} object  with the polygon(s) to be surveyed.
#'   \code{sampling_frame} should use a projected CRS with meters as the unit.
#' @param total_length_km Total length of transects to generate in kilometers
#' @param frame_buffer_km Distance from polygon edges in kilometers at which
#'   transects start or stop. The actual area allowed for placing transects is
#'   \code{sf::st_buffer(sampling_frame, frame_buffer_km * -1000)}.
#' @param angle_deg Orientation of the output transects in degrees.
#' @param min_length_km Minimum length in kilometers for any individual
#'   transect. Transects shorter than \code{min_length_km} are discarded from
#'   the survey.
#' @param max_length_km Maximum length in kilometers for any individual
#'   transect. Transects longer than \code{max_length_km} are split into
#'   multiple transects with space in between.
#' @param break_buffer_km Determines the amount of space separating the new
#'   transects when a transect longer than \code{max_length_km} is split. This
#'   argument determines the distance from the original transect centroid to the
#'   endpoints of the new transects, so the resulting transect endpoints will be
#'   \code{break_buffer_km * 2} kilometers apart.
#' @param min_space_km Minimal interval in kilometers to consider for transect
#'   spacing. Should be larger than the width of a single transect at a minimum.
#'
#' @return Returns an \code{sf} object with features of type \code{LINESTRING}
#'   with a special attribute \code{spacing_km} that contains the spacing used
#'   to generate the transects.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Create transects across entire herd unit polygon
#' hu <- wgfd_agol_data("AntelopeHerdUnits") |>
#'   dplyr::slice_sample(n = 1) |>
#'   sf::st_transform("ESRI:102212")
#' hu_transects <- generate_transects(hu, 1500)
#'
#' # Create transects at a 45 degree angle
#' hu_angled_transects <- generate_transects(hu, 1500, angle_deg = 45)
#'
#' # Check total length
#' sum(sf::st_length(hu_angled_transects))
#'
#' # Check the spacing selected
#' attr(hu_angled_transects, "spacing_km")
#'
#' # Plot transects
#' hu_angled_transects |>
#'   sf::st_geometry() |>
#'   plot()
#'
#' # Create transects broken up by hunt area
#' ha <- wgfd_agol_data("AntelopeHuntAreas") |>
#'   dplyr::filter(HERDNAME == hu$HERDNAME[1]) |>
#'   sf::st_transform("ESRI:102212")
#' ha_angled_transects <- generate_transects(hu, 1500, angle_deg = 45)
#'
#' # plot those to demostrate the effect of multiple polygons in the input
#' ha_angled_transects |>
#'   sf::st_geometry() |>
#'   plot()
#'
#' }
#'
generate_transects <- function(
  sampling_frame,
  total_length_km,
  frame_buffer_km = 0.5,
  angle_deg = 0,
  min_length_km = 5,
  max_length_km = 1000,
  break_buffer_km = 0.5,
  min_space_km = 1
) {

  # convert all distances to meters
  total_length <- total_length_km * 1000
  frame_buffer <- frame_buffer_km * 1000
  min_length <- min_length_km * 1000
  max_length <- max_length_km * 1000
  break_buffer <- break_buffer_km * 1000
  min_space <- min_space_km * 1000

  tgt_poly <- sampling_frame

  # apply internal buffer to sampling frame polygons
  if (!is.null(frame_buffer) && frame_buffer > 0) {
    tgt_poly <- tgt_poly |>
      sf::st_buffer(frame_buffer * -1)
  }

  # rotate sampling frame counter-clockwise if needed
    # (simpler to rotate sampling frame and generate vertical transects)
  if (!is.null(angle_deg) && angle_deg != 0) {
    tgt_poly <- tgt_poly |>
      rotate_geometry(angle_deg * -1)
  }

  # combine sampling frame polygons
  tgt_poly <- tgt_poly |>
    sf::st_union()

  # determine optimal transect spacing
  opt_sp <- optimal_transect_spacing(
    tgt_poly,
    min_length,
    max_length,
    break_buffer,
    min_space,
    total_length
  )

  # create transects using optimal spacing
  tr <- create_transects(opt_sp)

  # rotate transects clockwise if needed to align with original sampling frame
  if (!is.null(angle_deg) && angle_deg != 0) {
    tr <- tr |>
      rotate_geometry(angle_deg)
  }

  # apply crs
  sf::st_crs(tr) <- sf::st_crs(sampling_frame)

  # number transects by source polygon and calculate length
  jn <- sampling_frame |>
    dplyr::mutate(polygon_id = dplyr::row_number()) |>
    sf::st_set_agr("constant")

  tr <- tr |>
    sf::st_set_agr("constant") |>
    sf::st_join(jn, largest = TRUE) |>
    suppressWarnings() |>
    dplyr::group_by(.data$polygon_id) |>
    dplyr::mutate(transect_number = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      length = sf::st_length(tr),
      transect_length_km = units::set_units(length, "km"),
      transect_length_mi = units::set_units(length, "mi")
    ) |>
    dplyr::select(-length)

  # return transects with spacing in attributes
  attr(tr, "spacing_km") <- opt_sp$spacing / 1000

  tr

}

#' break_transects
#'
#' @description Recursively cut transect lines at centroids until all are below
#'   the specified maximum length
#'
#' @param lines \code{sf} object with features of type \code{LINESTRING}
#' @param max_length Maximum length in meters
#' @param buffer Buffer in meters added to either side of centroid when
#'   separating line segments
#'
#' @return Returns an \code{sf} object with features of type \code{LINESTRING}
#'
#' @keywords internal
#'
break_transects <- function(lines, max_length, buffer) {

  # remove lines shorter than threshold
  out <- lines |>
    dplyr::filter(as.numeric(sf::st_length(lines)) <= max_length)

  if (nrow(out) < nrow(lines)) {

    # extract lines longer than threshold
    spl_lines <- lines |>
      dplyr::filter(as.numeric(sf::st_length(lines)) > max_length)

    # calculate centroids for splitting
    brk_pts <- spl_lines |>
      sf::st_centroid() |>
      sf::st_coordinates() |>
      suppressWarnings()

    # extend centroid by buffer distance in either direction
    brk_lines <- purrr::map2(
      brk_pts[, 1],
      brk_pts[, 2],
      function(x, y) {
        m <- matrix(c(x, y - buffer, x, y + buffer), nrow = 2, byrow = TRUE)
        sf::st_linestring(m)
      }
    ) |>
      sf::st_as_sfc(crs = sf::st_crs(lines)) |>
      sf::st_as_sf() |>
      # buffer to ensure intersection
      sf::st_buffer(1) |>
      # merge
      dplyr::summarize()

    # split input lines with buffered centroids
    spl_lines <- spl_lines |>
      sf::st_difference(brk_lines) |>
      sf::st_cast("MULTILINESTRING") |>
      sf::st_cast("LINESTRING") |>
      # repeat until all lines are shorter than threshold
      break_transects(max_length, buffer) |>
      suppressWarnings()

    # add shortened lines to result
    out <- out |>
      dplyr::bind_rows(spl_lines)

  }

  out

}

#' test_spacing
#'
#' @description Generate transects within the specified polygon(s) at a certain
#'   spacing for optimization
#'
#' @param spacing Spacing between transects in meters
#' @param poly \code{sf} object with the polygon to be surveyed after any
#'   modifications (region exclusion, rotation)
#' @param min_length Minimum length in meters for resulting transects
#' @param max_length Maximum length in meters for resulting transects - any
#'   lines longer than \code{max_length} are split at their centroids via
#'   \code{break_lines} until none remain
#' @param buffer Buffer added to either side of centroid when separating line
#'   segments
#' @param target_length Target total length of resulting transects
#'
#' @return Returns the absolute value of the difference in meters between the
#'   target length and the total length of the generated transects
#'
#' @keywords internal
#'
test_spacing <- function(
  spacing,
  poly,
  min_length,
  max_length,
  buffer,
  target_length
) {

  # get extent
  bbox <- sf::st_bbox(poly)
  x_ctr <- (bbox$xmax + bbox$xmin) / 2

  # generate x coordinates
  x <- c(
    rev(seq(x_ctr, bbox$xmin, spacing * -1)),
    seq(x_ctr + spacing, bbox$xmax, spacing)
  )

  # create vertical lines covering extent
  lines <- purrr::map(
    x,
    function(x, y1, y2) {
      m <- matrix(c(x, y1, x, y2), nrow = 2, byrow = TRUE)
      sf::st_linestring(m)
    },
    y1 = floor(bbox$ymin),
    y2 = ceiling(bbox$ymax)
  ) |>
    sf::st_as_sfc(crs = sf::st_crs(poly)) |>
    sf::st_as_sf() |>
    # remove segments outside polygon
    sf::st_intersection(poly) |>
    sf::st_cast("MULTILINESTRING") |>
    sf::st_cast("LINESTRING") |>
    suppressWarnings()

  # split up long transects
  break_transects(lines, max_length, buffer) |>
    # calculate total length
    sf::st_length() |>
    as.numeric() |>
    (\(x) x[x >= min_length])() |>
    sum() |>
    # compare to target length
    (\(x) x - target_length)() |>
    abs()

}

#' create_transects
#'
#' @description Generate transect lines within the specified polygon at a
#'   certain spacing for final output
#'
#' @param opt_spacing A list produced by \code{optimal_transect_spacing}
#'   containing the spacing in meters and the arguments used to call the
#'   function
#'
#' @return Returns an \code{sf} object with features of type \code{LINESTRING}
#'
#' @keywords internal
#'
create_transects <- function(opt_spacing) {

  # unpack args passed in list
  spacing <- opt_spacing$spacing
  poly <- opt_spacing$poly
  min_length <- opt_spacing$min_length
  max_length <- opt_spacing$max_length
  buffer <- opt_spacing$buffer

  # get extent
  bbox <- sf::st_bbox(poly)
  x_ctr <- (bbox$xmax + bbox$xmin) / 2

  # generate x coordinates
  x <- c(
    rev(seq(x_ctr, bbox$xmin, spacing * -1)),
    seq(x_ctr + spacing, bbox$xmax, spacing)
  )

  # create vertical lines covering extent
  lines <- purrr::map(
    x,
    function(x, y1, y2) {
      m <- matrix(c(x, y1, x, y2), nrow = 2, byrow = TRUE)
      sf::st_linestring(m)
    },
    y1 = floor(bbox$ymin),
    y2 = ceiling(bbox$ymax)
  ) |>
    sf::st_as_sfc(crs = sf::st_crs(poly)) |>
    sf::st_as_sf() |>
    # remove segments outside polygon
    sf::st_intersection(poly) |>
    sf::st_cast("MULTILINESTRING") |>
    sf::st_cast("LINESTRING") |>
    suppressWarnings()

  # split up long transects
  bk <- break_transects(lines, max_length, buffer)

  # remove shrapnel
  bk <- bk |>
    dplyr::filter(as.numeric(sf::st_length(bk)) >= min_length)

  # get coordinates for sorting
  xy <- sf::st_coordinates(bk$x) |>
    tibble::as_tibble() |>
    dplyr::group_by(LineID = .data$L1) |>
    dplyr::summarize(
      min_x = min(.data$X),
      max_y = max(.data$Y),
      .groups = "drop"
    )

  # sort left to right, top to bottom (W-E, N-S)
  bk |>
    tibble::rowid_to_column(var = "LineID") |>
    dplyr::inner_join(xy, by = "LineID") |>
    dplyr::arrange(.data$min_x, dplyr::desc(.data$max_y)) |>
    dplyr::select(-dplyr::all_of(c("min_x", "max_y")))

}

#' optimal_transect_spacing
#'
#' @description Run three optimization stages to determine the ideal spacing
#'   for transects within a polygon given a target length
#'
#' @param poly \code{sf} object with the polygon(s) to be surveyed after any
#'   modifications (region exclusion, rotation)
#' @param min_length Minimum length in meters for resulting transects
#' @param max_length Maximum length in meters for resulting transects -
#'   any lines longer than \code{max_length} are split at their centroids via
#'   \code{break_lines} until none remain
#' @param buffer Buffer in meters added to either side of centroid when
#'   separating line segments
#' @param min_space Minimum spacing in meters to consider between transects
#' @param target_length Target total length in meters of resulting transects
#'
#' @return Returns a list with the optimal spacing, the absolute value of the
#'   difference between the length requested and the length resulting from the
#'   optimal spacing, and the arguments passed into the function for reuse in
#'   \code{create_transects}
#'
#' @keywords internal
#'
optimal_transect_spacing <- function(
  poly,
  min_length,
  max_length,
  buffer,
  min_space,
  target_length
) {

  fn_args <- as.list(environment())

  # get maximum possible spacing (avoids errors in seq.default)
  bbox <- sf::st_bbox(poly)
  max_space <- (bbox$xmax - bbox$xmin) / 2

  # really broad
  opt_1 <- stats::optimize(
    test_spacing,
    c(min_space, max_space),
    tol = 1000,
    poly = poly,
    min_length = min_length,
    max_length = max_length,
    buffer = buffer,
    target_length = target_length
  )

  # tighter
  opt_2 <- stats::optimize(
    test_spacing,
    c(
      max(c(opt_1$minimum - 1000, min_space)),
      min(c(opt_1$minimum + 1000, max_space))
    ),
    tol = 100,
    poly = poly,
    min_length = min_length,
    max_length = max_length,
    buffer = buffer,
    target_length = target_length
  )

  # final
  opt_3 <- stats::optimize(
    test_spacing,
    c(
      max(c(opt_2$minimum - 100, min_space)),
      min(c(opt_2$minimum + 100, max_space))
    ),
    tol = 1,
    poly = poly,
    min_length = min_length,
    max_length = max_length,
    buffer = buffer,
    target_length = target_length
  )

  # output
  c(
    list(spacing = opt_3$minimum, abs_difference = opt_3$objective),
    fn_args
  )

}
