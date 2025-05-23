% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zoom_wy.r
\name{zoom_wy}
\alias{zoom_wy}
\title{Zoom to Wyoming}
\usage{
zoom_wy(lf)
}
\arguments{
\item{lf}{\code{leaflet} or \code{leafletProxy} object}
}
\value{
input \code{leaflet} or \code{leafletProxy} object zoomed to the
  boundary of the state of Wyoming
}
\description{
Set the extent of a leaflet map to the state of Wyoming
}
\examples{

if (interactive()) {

  leaflet::leaflet() |>
    leaflet::addProviderTiles("Esri.WorldImagery") |>
    spatial.utils::zoom_wy()

}

}
