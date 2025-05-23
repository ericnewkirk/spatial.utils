% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zoom_to_data.r
\name{zoom_to_data}
\alias{zoom_to_data}
\title{Zoom to Data Extent}
\usage{
zoom_to_data(lf, sf_data)
}
\arguments{
\item{lf}{\code{leaflet} or \code{leafletProxy} object}

\item{sf_data}{\code{sf} object}
}
\value{
input \code{leaflet} or \code{leafletProxy} object zoomed to fit
  the extent of the \code{sf_data} object
}
\description{
Set the extent of a leaflet map to the extent of an sf object
}
\examples{

if (interactive()) {

  # zoom to north carolina counties dataset
  nc <- sf::read_sf(system.file("shape/nc.shp", package="sf"))

  leaflet::leaflet() |>
    leaflet::addProviderTiles("Esri.WorldImagery") |>
    spatial.utils::zoom_to_data(nc)

}

}
