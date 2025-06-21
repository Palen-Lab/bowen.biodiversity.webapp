#' values UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_values_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "values_panel",
    h1("Values")
  )
}

#' values Server Functions
#'
#' @noRd
mod_values_server <- function(id, map_id, parent_session){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    #### Add Zonation Output Raster ####
    zonation <- terra::rast(here::here("inst/extdata/rankmap.tif")) %>%
      terra::project("epsg:4326")
    zonation_pal <- leaflet::colorNumeric(c("#FFFFCC", "#41B6C4", "#0C2C84"), c(0, 1),
                                          na.color = "transparent")

    leaflet::leafletProxy(mapId = map_id,
                          session = parent_session) %>%
      leaflet::clearControls() %>%
      leaflet::clearImages() %>%
      leaflet::addRasterImage(
        x = zonation,
        layerId = "zonation_raster",
        colors = zonation_pal
      ) %>%
      leaflet::addLegend(
        pal = zonation_pal,
        layerId = "zonation_legend",
        values =  c(0,1),
        title = "Rel. Conservation Value",
      )
  })
}

## To be copied in the UI
# mod_values_ui("values_1")

## To be copied in the server
# mod_values_server("values_1")
