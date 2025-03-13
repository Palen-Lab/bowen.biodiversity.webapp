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
    # actionButton(label = "Submit", ns("submit_button")),
    leaflet::leafletOutput(ns("map"))
  )
}

#' main_map Server Functions
#'
#' @noRd
mod_main_map_server <- function(id, layers_df){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # TODO: add interactive legend, to reveal top % of zonation output
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet() %>%
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron,
                                  options = leaflet::providerTileOptions(noWrap = TRUE)
        )
    })
    # Add sf (vectors)
    sf_objects <- layers_df[layers_df$package == "sf",]
    for(i in 1:nrow(sf_objects)) {
      layer_sf <- sf::st_read(here::here(sf_objects[i,"full_path"])) %>%
        sf::st_transform(crs = 4326)
      leaflet::leafletProxy("map") %>%
        leaflet::addPolygons(data = layer_sf,
                             group = sf_objects[i, "group"])
    }
    # Add terra (rasters)
    terra_objects <- layers_df[layers_df$package == "terra",]
    for(i in 1:nrow(terra_objects)) {
      layer_terra <- terra::rast(here::here(terra_objects[i,"full_path"])) %>%
        terra::project("epsg:4326")
      leaflet::leafletProxy("map") %>%
        leaflet::addRasterImage(x = layer_terra,
                                group = terra_objects[i, "group"])
    }
    # Add Legend with Visibility Control
    leaflet::leafletProxy("map") %>%
      leaflet::addLayersControl(
        overlayGroups = unique(layers_df$group),
        options = leaflet::layersControlOptions(collapsed = F)
      )

    # TODO: pass list of objects in from outside of module
    # TODO: dynamically create UI for each item in list
    # TODO: probably module within a module
    # TODO: https://rstudio.github.io/leaflet/articles/showhide.html
    # observeEvent(input$submit_button, {
    # })
  })
}

## To be copied in the UI
# mod_main_map_ui("main_map_1")

## To be copied in the server
# mod_main_map_server("main_map_1")
