#' Zoom to Wyoming
#'
#' Set the extent of a leaflet map to the state of Wyoming
#'
#' @param lf \code{leaflet} or \code{leafletProxy} object
#'
#' @return input \code{leaflet} or \code{leafletProxy} object zoomed to the
#'   boundary of the state of Wyoming
#' @export
#'
#' @examples
#'
#' if (interactive()) {
#' 
#'   leaflet::leaflet() |>
#'     leaflet::addProviderTiles("Esri.WorldImagery") |>
#'     spatial.utils::zoom_wy()
#' 
#' }
#'
zoom_wy <- function(lf) {

  lf |>
    leaflet::fitBounds(-111.05631, 40.99474, -104.05242, 45.00570)

}
