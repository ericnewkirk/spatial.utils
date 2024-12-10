#' WGFD AGOL Data
#'
#' Download WGFD data from ArcGIS Online
#'
#' @param layer_name Name of the layer to download
#' @param crs The coordinate reference system for the output \code{sf} object,
#'   see \code{\link[sf]{st_crs}}
#'
#' @return \code{sf} object
#' @export
#'
#' @examples
#'
#' sr <- wgfd_agol_data("Antelope_Seasonal_Range")
#'
#' plot(sr["RANGE"])
#'
wgfd_agol_data <- function(layer_name, crs = 4326) {

  # base url
  dl_url <- httr::parse_url("https://services6.arcgis.com/")

  # path for correct dataset
  dl_url$path <- list(
    "cWzdqIyxbijuhPLw", "arcgis", "rest", "services",
    layer_name, "FeatureServer", "0", "query"
  )

  # query options including crs
  dl_url$query <- list(
    where = "1=1",
    outFields = "*",
    outSR = crs,
    f = "geojson"
  )

  # create sf object directly from url
  sf::read_sf(httr::build_url(dl_url))

}
