#' main_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_main_map_ui <- function(id) {
  ns <- NS(id)
  tagList(
    leaflet::leafletOutput(ns("map"))
  )
}

#' main_map Server Functions
#'
#' @noRd
mod_main_map_server <- function(id, main_raster, zonation_val){
  stopifnot(is.reactive(main_raster))
  stopifnot(zonational_val <= 1 & zonation_val >= 0)
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # TODO: add interactive legend, to reveal top % of zonation output
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet() %>%
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron,
                                  options = leaflet::providerTileOptions(noWrap = TRUE)
        ) %>%
        leaflet::addRasterImage(x = main_raster(),
                                colors = "Spectral") %>%
        leaflet::addLegend(values = terra::values(main_raster()),
                           pal = leaflet::colorNumeric(palette = "Spectral",
                                                       domain = terra::values(main_raster())
                                                       )
                           )
    })
  })
}

## To be copied in the UI
# mod_main_map_ui("main_map_1")

## To be copied in the server
# mod_main_map_server("main_map_1")
