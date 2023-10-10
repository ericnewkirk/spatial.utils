#' Count Spatial Files
#'
#' Counts the number of files in a vector of file names or paths, treating
#'   each shapefile (made up of several component files) as a single file
#'
#' @param files Character vector of file names or paths
#'
#' @return int The number of unique datasets represented in \code{files}
#' @export
#'
#' @examples
#'
#' # a single shapefile is stored in at least 4 files on disk with the same
#'   # basename
#'
#' f <- c("data/locs.shp", "data/locs.shx", "data/locs.dbf", "data/locs.prj")
#' count_spatial_files(f)
#'
#' # [1] 1
#'
#' # add another shapefile to the file list
#'
#' f <- c(
#'   f,
#'   "data/tracks.shp", "data/tracks.shx", "data/tracks.dbf", "data/tracks.prj"
#' )
#' count_spatial_files(f)
#'
#' # [1] 2
#'
#' # only shapefiles are collapsed, so other files each count as 1
#'
#' f <- c(
#'   f,
#'   "data/gps.csv", "data/herd_unit.geojson", "data/hunt_areas.kml"
#' )
#' count_spatial_files(f)
#'
#' # [1] 5
#'
count_spatial_files <- function(files) {

  shp_exts <- c(
    ".shp", ".shx", ".dbf", ".prj", ".sbn", ".fbn", ".ain", ".ixs",
    ".mxs", ".atx", ".shp.xml", ".cpg"
  )

  shp_ext_regex <- stringr::regex(sprintf(
    "(%s)$",
    paste0("\\", shp_exts, collapse = "|")
  ))

  shp_count <- files |>
    stringr::str_subset(shp_ext_regex) |>
    tools::file_path_sans_ext() |>
    unique() |>
    length()

  other_count <- files |>
    stringr::str_subset(shp_ext_regex, negate = TRUE) |>
    length()

  shp_count + other_count

}

#' Condense Spatial Files
#'
#' Removes shapefile helper files from a list of file names or paths so they can
#'   be passed to functions like \code{sf::st_read} that expect a single file
#'   path for shapefiles
#'
#' @param files Character vector of file names or paths
#'
#' @return Character vector of file names or paths suitable for
#'   \code{sf::st_read}
#' @export
#'
#' @examples
#'
#' f <- c("data/locs.shp", "data/locs.shx", "data/locs.dbf", "data/locs.prj")
#'
#' # sf::st_read(f) results in error (the condition has length > 1)
#'
#' condense_spatial_files(f)
#'
#' # sf::st_read(condense_spatial_file(f)) will succeed with only one shapefile
#'
#' # add another shapefile to the file list
#'
#' f <- c(
#'   f,
#'   "data/tracks.shp", "data/tracks.shx", "data/tracks.dbf", "data/tracks.prj"
#' )
#' condense_spatial_files(f)
#'
#' # helper files have been removed, but st_read can't read multiple datasets
#' # purrr::map_dfr(condense_spatial_files(f), sf::st_read) will do it
#'
#' # only shapefiles are collapsed, so other files are unchanged
#'
#' f <- c(
#'   f,
#'   "data/herd_unit.geojson", "data/hunt_areas.kml"
#' )
#' condense_spatial_files(f)
#'
#' # so purrr::map_dfr(condense_spatial_files(f), sf::st_read) will work even
#'   # with a mix of shapefles and other file types
#'
condense_spatial_files <- function(files) {

  excl_exts <- c(
    ".shx", ".dbf", ".prj", ".sbn", ".fbn", ".ain", ".ixs",
    ".mxs", ".atx", ".shp.xml", ".cpg"
  )

  excl_ext_regex <- stringr::regex(sprintf(
    "(%s)$",
    paste0("\\", excl_exts, collapse = "|")
  ))

  files |>
    stringr::str_subset(excl_ext_regex, negate = TRUE)

}
