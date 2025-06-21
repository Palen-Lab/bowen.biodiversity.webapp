#' species UI Function
#'
#' @description This should be in the sidebar.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_species_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "species_panel",
    h1("Species")
  )
}

#' species Server Functions
#'
#' @noRd
mod_species_server <- function(id, map_id, parent_session){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    species_richness <- terra::rast(here::here("inst/extdata/bowen_sdm_richness.tif")) %>%
      terra::project("epsg:4326")
    species_richness_group <- "Species Richness"
    species_richness_domain <- c(0, terra::minmax(species_richness)[2])
    species_richness_pal <- leaflet::colorNumeric(c('#edf8fb','#b2e2e2','#66c2a4','#2ca25f','#006d2c'), species_richness_domain,
                                                 na.color = "transparent")

    leaflet::leafletProxy(mapId = map_id,
                          session = parent_session) %>%
      leaflet::clearControls() %>%
      leaflet::clearImages() %>%
      leaflet::addRasterImage(
        x = species_richness,
        layerId = "species_richness_raster",
        colors = species_richness_pal
      ) %>%
      leaflet::addLegend(
        pal = species_richness_pal,
        layerId = "species_richness_legend",
        values =  species_richness_domain,
        title = "Species Richness"
      )
  })
}

## To be copied in the UI
# mod_species_ui("species_1")

## To be copied in the server
# mod_species_server("species_1")
