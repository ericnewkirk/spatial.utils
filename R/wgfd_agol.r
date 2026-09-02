#' WGFD AGOL Data
#'
#' Download WGFD data from ArcGIS Online
#'
#' @param layer_name Name of the layer to download
#' @param crs The coordinate reference system for the output \code{sf} object,
#'   see \code{\link[sf]{st_crs}}
#' @param layer_index Numeric index of the layer within the feature server
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
wgfd_agol_data <- function(layer_name, crs = 4326, layer_index = 0L) {

  # base url
  dl_url <- httr::parse_url("https://services6.arcgis.com/")

  # path for correct dataset
  dl_url$path <- list(
    "cWzdqIyxbijuhPLw", "arcgis", "rest", "services",
    layer_name, "FeatureServer", layer_index, "query"
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

#' WGFD AGOL Layers
#'
#' Enumerate layers available on WGFD ArcGIS Online server
#'
#' @param name_like character String for filtering feature servers, using
#'   \code{grepl(tolower(name_like), name)}
#'
#' @return \code{tibble::tibble} with one row per available layer and the
#'   following columns:
#'   \describe{
#'     \item{featureserver_url}{URL for the parent feature server container}
#'     \item{featureserver_name}{Name of the parent feature server container}
#'     \item{layer_id}{Numeric index of the layer, used in the URL path}
#'     \item{layer_name}{Name of the layer}
#'   }
#' @export
#'
#' @examples
#'
#' lyrs <- wgfd_agol_layers("Seasonal")
#' # View(lyrs)
#'
#' sr <- wgfd_agol_data("Antelope_Seasonal_Range")
#'
#' plot(sr["RANGE"])
#'
wgfd_agol_layers <- function(name_like = "") {

  resp <- httr::GET(
    "https://services6.arcgis.com/",
    path = list("cWzdqIyxbijuhPLw", "arcgis", "rest", "services"),
    query = list(f = "json")
  )

  httr::stop_for_status(resp)

  stopifnot(
    "REST API endpoint not available" = httr::status_code(resp) == 200
  )

  # extract list of services
  fs <- httr::content(resp)$services
  # convert to tibble
  fs <- dplyr::bind_rows(purrr::map(fs, tibble::as_tibble))
  # filter to feature servers and matching names
  fs <- dplyr::filter(
    fs,
    .data$type == "FeatureServer",
    grepl(tolower(name_like), tolower(.data$name))
  )

  # enumerate layers for each feature server
  lyr <- purrr::map2(
    fs$url,
    fs$name,
    \(x, y) {

      resp <- httr::GET(utils::URLencode(x), query = list(f = "json"))

      httr::stop_for_status(resp)

      if (httr::status_code(resp) == 200) {

        l <- httr::content(resp)$layers
        l <- purrr::map(
          l,
          \(z) {
            tibble::tibble(
              featureserver_url = x,
              featureserver_name = y,
              layer_id = z$id,
              layer_name = z$name
            )
          }
        )

        dplyr::bind_rows(l)

      } else {

        warning(sprintf("Retrieving info for feature server %s failed.", y))

        tibble::tibble()

      }

    }
  )

  # combine
  lyr <- dplyr::bind_rows(lyr)

  # return
  lyr

}
