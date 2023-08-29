`%>%` <- dplyr::`%>%`

#' Labeled KML Polygons
#'
#' Creates a kml file with labels visible at centroids
#'
#' @param sf \code{sf} object containing polygons
#' @param labels column to use for labels (unquoted a la \code{dplyr})
#' @param folder name of folder in kml file
#' @param out_file file name or path to be written
#'
#' @return character vector of the lines written to the file
#' @export
#'
#' @examples
#'
#' library(dplyr)
#' library(tibble)
#' library(sf)
#'
#' tf <- tempfile(fileext = ".kml")
#'
#' tibble(
#'   Name = c("59: 1", "59: 2", "59: 3"),
#'   wkt = c(
#'     "MULTIPOLYGON (((
#'       -105.0916 41.96413, -105.0916 41.94263, -105.0627 41.94263,
#'       -105.0627 41.96413, -105.0916 41.96413
#'     )))",
#'     "MULTIPOLYGON (((
#'       -105.0627 41.96413, -105.0627 41.94263, -105.0338 41.94263,
#'       -105.0338 41.96413, -105.0627 41.96413
#'     )))",
#'     "MULTIPOLYGON (((
#'       -105.0338 41.96413, -105.0338 41.94263, -105.0048 41.94263,
#'       -105.0048 41.96413, -105.0338 41.96413
#'     )))"
#'   )
#' ) %>%
#'   st_as_sf(wkt = "wkt", crs = 4326) %>%
#'   mutate(label = paste("Unit", Name)) %>%
#'   labeled_kml(label, "Demo", tf)
#'
labeled_kml <- function(sf, labels, folder, out_file) {

  tf <- tempfile(pattern = "poly", fileext = ".kml")
  tfc <- tempfile(pattern = "ctr", fileext = ".kml")
  labels <- rlang::enquo(labels)

  # write temporary kml files (gross)
  x <- sf %>%
    dplyr::transmute(name = !!labels) %>%
    sf::st_transform(4326)

  x %>%
    sf::st_write(tf, quiet = TRUE)

  x %>%
    sf::st_centroid() %>%
    suppressWarnings() %>%
    sf::st_write(tfc, quiet = TRUE)

  # read geometries back in
  out <- readLines(tf)
  ctr <- readLines(tfc)
  try(unlink(tf), silent = TRUE)
  try(unlink(tfc), silent = TRUE)

  # add style definitions to file header
  ins <- c(
    "<Style id=\"redPoly\">",
    "\t<LineStyle>",
    "\t\t<color>ff0000ff</color>",
    "\t</LineStyle>",
    "\t<PolyStyle>",
    "\t\t<fill>0</fill>",
    "\t</PolyStyle>",
    "</Style>",
    "<Style id=\"labelOnly\">",
    "\t<IconStyle>",
    "\t\t<scale>0</scale>",
    "\t</IconStyle>",
    "</Style>"
  )
  out <- append(out, ins, after = 3)

  # change folder name
  out <- gsub(
    "(<Folder><name>).*(</name>)",
    paste0("\\1", folder, "\\2"),
    out
  )

  # insert style url for polygons
  idx <- which(grepl("<Placemark>", out)) + 0.5
  ins <- rep("\t\t<styleUrl>#redPoly</styleUrl>", length(idx))
  out <- c(out, ins)[order(c(seq_along(out), idx))]

  # extract centroid placemarks
  idx <- min(which(grepl("<Placemark>", ctr)))
  ctr <- ctr[idx:length(ctr)]
  idx <- max(which(grepl("</Placemark>", ctr)))
  ctr <- ctr[1:idx]

  # insert style url for centroids
  idx <- which(grepl("<Placemark>", ctr)) + 0.5
  ins <- rep("\t\t<styleUrl>#labelOnly</styleUrl>", length(idx))
  ctr <- c(ctr, ins)[order(c(seq_along(ctr), idx))]

  # now insert centroids after last polygon
  idx <- max(which(grepl("</Placemark>", out)))
  out <- append(out, ctr, after = idx)

  # save
  try(unlink(out_file), silent = FALSE)
  writeLines(out, out_file)

  out

}
