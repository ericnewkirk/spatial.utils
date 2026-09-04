#' Convert leaflet draw features to sf object
#'
#' Process the json returned by the leaflet draw plugin to return an \code{sf}
#'   object
#'
#' @param features Json list returned to the server by the \code{leaflet.extras}
#'   draw plugin
#' @param proj_crs Coordinate reference system used for enforcing valid
#'   geometries; must be a projected coordinate system such as WYLAM
#' @param out_crs Coordinate reference system of the returned \code{sf} object
#'
#' @return \code{sf} object containing the features drawn on the leaflet map
#' @export
#'
#' @examples
#'
#' if (interactive()) {
#'
#'   ui <- shiny::fluidPage(
#'     shiny::fluidRow(leaflet::leafletOutput("map")),
#'     shiny::fluidRow(
#'       shiny::column(
#'         width = 6,
#'         reactable::reactableOutput("feature_tbl")
#'       ),
#'       shiny::column(
#'         width = 6,
#'         shiny::selectInput(
#'           "print_obj",
#'           "Print:",
#'           choices = c("sf object", "input json")
#'         ),
#'         shiny::verbatimTextOutput("feature_print")
#'       )
#'     )
#'   )
#'
#'   server <- function(input, output, session) {
#'
#'     lro <- tibble::tibble(
#'       name = "WGFD Laramie Regional Office",
#'       lng = -105.61578,
#'       lat = 41.30635
#'     ) |>
#'       sf::st_as_sf(coords = c("lng", "lat"), crs = 4326)
#'
#'     output$map <- leaflet::renderLeaflet({
#'
#'       leaflet::leaflet(lro) |>
#'         leaflet::addMarkers() |>
#'         leaflet.extras::addDrawToolbar(
#'           editOptions = leaflet.extras::editToolbarOptions(
#'             selectedPathOptions = leaflet.extras::selectedPathOptions()
#'           )
#'         )
#'
#'     })
#'
#'     drawn_features <- shiny::reactive({
#'       shiny::req(length(input$map_draw_all_features$features) > 0)
#'       lfdraw_to_sf(input$map_draw_all_features$features)
#'     })
#'
#'     output$feature_tbl <- reactable::renderReactable({
#'
#'       drawn_features() |>
#'         dplyr::mutate(
#'           type = sf::st_geometry_type(.data$geometry),
#'           valid = sf::st_is_valid(geometry)
#'         ) |>
#'         sf::st_drop_geometry() |>
#'         reactable::reactable()
#'
#'     })
#'
#'     output$feature_print <- shiny::renderPrint({
#'
#'       if (input$print_obj == "sf object") {
#'         drawn_features()
#'       } else {
#'         input$map_draw_all_features$features
#'       }
#'
#'     })
#'
#'   }
#'
#'   shiny::shinyApp(ui, server)
#'
#' }
#'
lfdraw_to_sf <- function(features, proj_crs = 32159, out_crs = 4326) {

  purrr::map(
    features,
    \(f) {

      out <- f$geometry |>
        jsonlite::toJSON(auto_unbox = TRUE) |>
        sf::read_sf() |>
        sf::st_transform(proj_crs) |>
        sf::st_make_valid() |>
        dplyr::mutate(leaflet_id = f$properties$`_leaflet_id`)

      if (f$properties$feature_type == "circle") {
        buffer <- units::set_units(f$properties$radius, "m")
        sf::st_buffer(out, buffer)
      } else {
        out
      }

    }
  ) |>
    dplyr::bind_rows() |>
    sf::st_as_sf() |>
    sf::st_transform(out_crs)

}
