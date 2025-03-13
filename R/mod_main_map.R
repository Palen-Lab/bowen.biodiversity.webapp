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
    actionButton(label = "Submit", ns("submit_button")),
    leaflet::leafletOutput(ns("map"))
  )
}

#' main_map Server Functions
#'
#' @noRd
mod_main_map_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    bowen_island_admin <- sf::st_read(here::here("data-raw/bowen_boundary")) %>%
      sf::st_transform(crs = 4326)
    # leaflet::leafletProxy("main_map", data = bowen_island_admin)

    # TODO: add interactive legend, to reveal top % of zonation output
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet() %>%
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron,
                                  options = leaflet::providerTileOptions(noWrap = TRUE)
        )
      # %>% leaflet::addPolygons(data = bowen_island_admin)
    })
    # TODO: pass list of objects in from outside of module
    # TODO: dynamically create UI for each item in list
    # TODO: probably module within a module
    # TODO: https://rstudio.github.io/leaflet/articles/showhide.html
    observeEvent(input$submit_button, {
      leaflet::leafletProxy("map", data = bowen_island_admin) %>%
        leaflet::addPolygons()
    })
  })
}

## To be copied in the UI
# mod_main_map_ui("main_map_1")

## To be copied in the server
# mod_main_map_server("main_map_1")
