#' Rotate SF Geometry
#'
#' @description Affine transformation modified from sf vignette
#'   (https://r-spatial.github.io/sf/articles/sf3.html)
#'
#' @param sf \code{sf} object to rotate
#' @param deg Numeric - number of degrees to rotate \code{sf} object by
#' @param ctr \code{sf} point to rotate geometry around. The centroid is used if
#'   \code{ctr} is \code{NULL}.
#' @param byrow Logical - if \code{byrow} = \code{TRUE} then geometries are each
#'   rotated independently around their own centroid; if \code{byrow} = FALSE
#'   geometries are rotated together around the common centroid. Ignored if
#'   \code{ctr} is specified.
#'
#' @return Returns an \code{sf} object with rotated geometries
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' ha <- wgfd_agol_data("DeerHuntAreas") |>
#'   sf::st_transform("ESRI:102212")
#'
#' r1 <- ha |>
#'   rotate_geometry(45)
#'
#' plot(sf::st_geometry(r1))
#'
#' r2 <- ha |>
#'   rotate_geometry(45, byrow = TRUE)
#'
#' plot(sf::st_geometry(r2))
#'
#' }
#'
rotate_geometry <- function(sf, deg, ctr = NULL, byrow = FALSE) {

  out <- sf
  rmat <- rotation_matrix(deg)
  g <- sf::st_geometry(out)

  if (is.null(ctr)) {
    if (byrow) {
      ctr <- sf::st_centroid(g)
    } else {
      ctr <- sf::st_union(g) |>
        sf::st_centroid()
    }
  }

  g <- (g - ctr) * rmat + ctr

  out |>
    sf::st_set_geometry(g)

}

#' rotation_matrix
#'
#' @description Affine transformation modified from sf vignette
#'   (https://r-spatial.github.io/sf/articles/sf3.html)
#'
#' @param deg Numeric - number of degrees to rotate \code{sf} object by
#'
#' @return Returns a matrix that can be multiplied by the geometry column of an
#'   \code{sf} object to rotate the geometries
#'
#' @keywords internal
#'
rotation_matrix <- function(deg) {
  rad <- deg * pi / 180
  matrix(c(cos(rad), sin(rad), -sin(rad), cos(rad)), 2, 2)
}
