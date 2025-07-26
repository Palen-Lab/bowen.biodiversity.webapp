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
    bslib::card(
      bslib::card_body(
        tagList(
          h1("Conservation Values"),
          util_ui_simple_legend_element(label = "Relatively Higher Value", colour = "#0C2C84"),
          util_ui_simple_legend_element(label = "Relatively Lower Value", colour = "#FFFFCC"),
          p("We used ", strong("Zonation5,"), "a robust conservation prioritization software, to calculate relative conservation values from the input layers visualized under the Species, Habitats, and People sections."),
          p("This can help decide which natural areas are most important for biodiversity, informing how efforts and resources should be used for maximum impact."),
          p("Note that these values are relative within Bowen Island. When compared to the rest of British Columbia, Bowen Island in general has very high biodiversity. Areas that appear low on this map may still have high biodiversity compared to other places outside of Bowen Island."),
          em(strong("Note: "), "While this map can provide high-level view of biodiversity, this should not replace site-specific environmental assessments and consultation with experts.")
        )
      )
    ),
    docs_link
  )
}

#' values Server Functions
#'
#' @noRd
mod_values_server <- function(id, map_id, parent_session){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    #### Add Zonation Output Raster ####
    zonation <- terra::rast(here::here("inst/extdata/5_values/rankmap.tif")) %>%
      terra::project("epsg:4326")
    zonation_pal <- leaflet::colorNumeric(c("#FFFFCC", "#41B6C4", "#0C2C84"), c(0, 1),
                                          na.color = "transparent")

    leaflet::leafletProxy(mapId = map_id,
                          session = parent_session) %>%
      leaflet::clearControls() %>%
      leaflet::clearImages() %>%
      leaflet::clearGroup(group = "clear_each_update") %>%
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
