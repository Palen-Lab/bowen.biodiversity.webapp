#' people UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_people_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "people_panel",
    h1("People"),
    p("Under Construction")
  )
}

#' people Server Functions
#'
#' @noRd
mod_people_server <- function(id, map_id, parent_session){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    #### Add Human Footprint Raster ####
    human_footprint <- terra::rast(here::here("inst/extdata/4_people/bowen_human_footprint.tif")) %>%
      terra::project("epsg:4326")
    human_footprint_domain <- c(0, terra::minmax(human_footprint)[2])
    human_footprint_pal <- leaflet::colorNumeric(c("#1a9641", "#a6d96a", "#ffffbf", "#fdae61", "#d7191c"), human_footprint_domain,
                                                 na.color = "transparent")

    leaflet::leafletProxy(mapId = map_id,
                          session = parent_session) %>%
      leaflet::clearControls() %>%
      leaflet::clearImages() %>%
      leaflet::addRasterImage(
        x = human_footprint,
        layerId = "human_footprint_raster",
        colors = human_footprint_pal
      ) %>%
      leaflet::addLegend(
        pal = human_footprint_pal,
        layerId = "human_footprint_legend",
        values =  human_footprint_domain,
        title = "Human Footprint"
      )
  })
}

## To be copied in the UI
# mod_people_ui("people_1")

## To be copied in the server
# mod_people_server("people_1")
