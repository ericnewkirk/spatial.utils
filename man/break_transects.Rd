% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transects.r
\name{break_transects}
\alias{break_transects}
\title{break_transects}
\usage{
break_transects(lines, max_length, buffer)
}
\arguments{
\item{lines}{\code{sf} object with features of type \code{LINESTRING}}

\item{max_length}{Maximum length in meters}

\item{buffer}{Buffer in meters added to either side of centroid when
separating line segments}
}
\value{
Returns an \code{sf} object with features of type \code{LINESTRING}
}
\description{
Recursively cut transect lines at centroids until all are below
  the specified maximum length
}
\keyword{internal}
