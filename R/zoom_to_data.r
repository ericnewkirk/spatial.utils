#' Zoom to Data Extent
#'
#' Set the extent of a leaflet map to the extent of an sf object
#'
#' @param lf \code{leaflet} or \code{leafletProxy} object
#' @param sf_data \code{sf} object
#'
#' @return input \code{leaflet} or \code{leafletProxy} object zoomed to fit
#'   the extent of the \code{sf_data} object
#' @export
#'
#' @examples
#'
#' if (interactive()) {
#' 
#'   # zoom to north carolina counties dataset
#'   nc <- sf::read_sf(system.file("shape/nc.shp", package="sf"))
#' 
#'   leaflet::leaflet() |>
#'     leaflet::addProviderTiles("Esri.WorldImagery") |>
#'     spatial.utils::zoom_to_data(nc)
#' 
#' }
#'
zoom_to_data <- function(lf, sf_data) {

  bbox <- sf_data |>
    sf::st_transform(4326) |>
    sf::st_bbox() |>
    as.character()

  lf |>
    leaflet::fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])

}
