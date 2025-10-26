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
    bslib::card(
      bslib::card_body(
        tagList(
          h1("Potential Protected Areas: Public Lands"),
          util_ui_simple_legend_element(label = "Relatively Higher Value", colour = "#7a0177"),
          util_ui_simple_legend_element(label = "Relatively Lower Value", colour = "#feebe2"),
          util_ui_simple_legend_element(label = "Existing Protected Areas", colour = "#a1d76a"),
          p("Protected areas on Bowen Island are crucial for safeguarding ecosystems and biodiversity from increasing threats such as development and habitat fragmentation."),
          p("This map was created using Prioritizr software to identify the top 30% of public lands (Crown, municipal, and Metro Vancouver) with the highest conservation value. Areas were limited to those outside existing protected areas and weighted by their connectivity to those protected areas."),
          em(strong("Note: "), "While this map can provide high-level view of biodiversity, this should not replace site-specific environmental assessments and consultation with experts.")
        )
      )
    ),
    docs_link()
  )
}

#' action Server Functions
#'
#' @noRd
mod_action_server <- function(id, map_id, parent_session){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    #### Add Potential Protected Areas Raster ####
    protected_areas_raster <- terra::rast(here::here("inst/extdata/7_action/potential_protected_areas.tif")) %>%
      terra::project("epsg:4326")

    raster_domain <- seq(from = 1, to = 5)
    raster_labels <- c("Low", "Medium-Low", "Medium", "Medium-High", "High")
    raster_colours <- c("#feebe2", "#fbb4b9", "#f768a1", "#c51b8a", "#7a0177")
    raster_pal <- leaflet::colorFactor(
      raster_colours,
      raster_domain,
      na.color = "transparent"
    )

    #### Add Existing Protected Areas Vector ####
    bowen_pa <- here::here("inst/extdata/7_action/existing_protected_areas.gpkg") %>%
      sf::st_read()

    leaflet::leafletProxy(mapId = map_id,
                          session = parent_session) %>%
      leaflet::clearControls() %>%
      leaflet::clearImages() %>%
      leaflet::clearGroup(group = "clear_each_update") %>%
      leaflet::addRasterImage(
        x = protected_areas_raster,
        layerId = "protected_areas_raster",
        colors = raster_pal
      ) %>%
      leaflet::addPolygons(
        data = bowen_pa,
        group = "clear_each_update",
        color = "#a1d76a",
        stroke = FALSE,
        fillOpacity = 1,
        smoothFactor = 0.2
      ) %>%
      leaflet::addLegend(
        colors = raster_pal(raster_domain),
        layerId = "protected_areas_raster_legend",
        labels = raster_labels,
        title = "Poten. Protected Areas",
      ) %>%
      leaflet::addLegend(
        colors = "#a1d76a",
        layerId = "protected_areas_vector_legend",
        labels = "Present",
        title = "Exist. Protected Areas",
      )
  })
}

## To be copied in the UI
# mod_action_ui("action_1")

## To be copied in the server
# mod_action_server("action_1")
