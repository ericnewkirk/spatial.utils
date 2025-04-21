#' Spatial fileInput
#'
#' A wrapper for \code{\link[shiny]{fileInput}} tailored for uploading spatial
#'   data. The \code{multiple} argument is always \code{TRUE} to allow users to
#'   upload shapefiles, and the default for the \code{accept} argument is a
#'   short list of spatial file extensions including the four required for
#'   shapefiles. Restricting uploads to a single file can be done with the
#'   \code{multiple} argument in the paired server function
#'   \code{spatialFileInputHandler}.
#'
#' @inheritParams shiny::fileInput
#' @param ... Additional arguments passed to \code{shiny::fileInput}
#'
#' @return \code{shiny::fileInput} ui element
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' ui <- shiny::fluidPage(
#'   shiny::column(
#'     width = 4,
#'     spatialFileInput("up_poly", "upload polygons")
#'   ),
#'   shiny::column(
#'     width = 8,
#'     leaflet::leafletOutput("map")
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'
#'   output$map <- leaflet::renderLeaflet({
#'     leaflet::leaflet() |>
#'       leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery)
#'   })
#'
#'   prx <- leaflet::leafletProxy("map")
#'
#'   poly <- reactive({
#'     shiny::req(input$up_poly)
#'     spatialFileInputHandler(input$up_poly, multiple = TRUE)
#'   })
#'
#'   shiny::observeEvent(poly(), {
#'     shiny::req(poly())
#'
#'     xy <- sf::st_transform(poly(), 4326)
#'
#'     bbox <- setNames(sf::st_bbox(xy), NULL)
#'
#'     prx |>
#'       leaflet::clearGroup("uploads") |>
#'       leaflet::addPolygons(
#'         data = xy,
#'         group = "uploads"
#'       ) |>
#'       leaflet::fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])
#'
#'   })
#'
#' }
#'
#' shiny::shinyApp(ui, server)
#'
#'}
#'
spatialFileInput <- function(inputId,
                             label,
                             accept = c(".geojson", ".kml",
                                        ".shp", ".shx", ".dbf", ".prj"),
                             ...) {

  shiny::fileInput(inputId, label, multiple = TRUE, accept = accept, ...)

}

#' @param input_value The server value produced by the \code{shiny::fileInput}
#'   ui element
#' @param multiple Whether multiple spatial datasets are allowed. If
#'   \code{multiple} = \code{FALSE} and more than one dataset is uploaded this
#'   function will return \code{NULL} with a warning. A single uploaded
#'   shapefile counts as one dataset even though it consists of more than one
#'   actual file.
#' @param crs The coordinate reference system for the output \code{sf} object,
#'   see \code{\link[sf]{st_crs}}
#'
#' @return \code{sf} object, or \code{NULL} if A) more than one dataset is
#'   uploaded and \code{multiple} = \code{FALSE} or B) attempting to read any
#'   of the uploaded files with \code{sf::st_read} results in an error
#' @export
#'
#' @describeIn spatialFileInput
#'
spatialFileInputHandler <- function(input_value,
                                    multiple = FALSE,
                                    crs = 4326) {

  # enforce single file restriction
  if (!multiple) {
    if (count_spatial_files(input_value$name) > 1L) {
      warning("More than one file uploaded with multiple = FALSE")
      return(NULL)
    }
  }

  # restore original filenames so shapefiles work as expected
  read_files <- input_value |>
    dplyr::select(dplyr::all_of(c("name", "datapath"))) |>
    dplyr::mutate(
      new_path = suppressWarnings(
        normalizePath(file.path(dirname(.data$datapath), .data$name))
      )
    )

  purrr::walk2(read_files$datapath, read_files$new_path, file.rename)

  # now read files into sf object
  out <- try(
    {
      x <- read_files |>
        dplyr::pull("new_path") |>
        condense_spatial_files() |>
        purrr::map(read_sf_wrapper) |>
        dplyr::bind_rows() |>
        sf::st_zm() |>
        sf::st_set_agr("constant")

      if (!all(sf::st_is_valid(x))) {
        x <- sf::st_make_valid(x)
      }

      x |>
        sf::st_transform(crs)
    },
    silent = TRUE
  )

  if (inherits(out, "try-error")) {
    warning("Error converting uploaded file(s) to sf")
    out <- NULL
  }

  out

}

#' read_sf_wrapper
#'
#' A wrapper for \code{\link[sf]{read_sf}} that reads all layers from files that
#'   support multiple layers, such as .kml files
#'
#' @param file_path Path to a spatial file
#'
#' @return \code{sf} object
#' 
#' @keywords internal
#'
read_sf_wrapper <- function(file_path) {
  lyr_names <- sf::st_layers(file_path)$name
  layers <- purrr::map(lyr_names, function(l) sf::read_sf(file_path, layer = l))
  dplyr::bind_rows(layers)
}
