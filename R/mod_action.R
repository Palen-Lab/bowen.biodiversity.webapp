#' action UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_action_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "action_panel",
    h1("Action"),
    p("Under Construction")
  )
}

#' action Server Functions
#'
#' @noRd
mod_action_server <- function(id, map_id, parent_session){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
#
#     species_richness <- terra::rast(here::here("inst/extdata/bowen_sdm_richness.tif")) %>%
#       terra::project("epsg:4326")
#     species_richness_group <- "Species Richness"
#     species_richness_domain <- c(0, terra::minmax(species_richness)[2])
#     species_richness_pal <- leaflet::colorNumeric(c('#edf8fb','#b2e2e2','#66c2a4','#2ca25f','#006d2c'), species_richness_domain,
#                                                   na.color = "transparent")
#
#     leaflet::leafletProxy(mapId = map_id,
#                           session = parent_session) %>%
#       leaflet::addRasterImage(x = species_richness,
#                               layerId = "species_richness_raster",
#                               colors = species_richness_pal)

  })
}

## To be copied in the UI
# mod_action_ui("action_1")

## To be copied in the server
# mod_action_server("action_1")
