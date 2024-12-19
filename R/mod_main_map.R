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
mod_main_map_server <- function(id, main_raster){
  stopifnot(is.reactive(main_raster))
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet() %>%
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron,
                                  options = leaflet::providerTileOptions(noWrap = TRUE)
        ) %>%
        leaflet::addRasterImage(x = main_raster())
    })
  })
}

## To be copied in the UI
# mod_main_map_ui("main_map_1")

## To be copied in the server
# mod_main_map_server("main_map_1")
