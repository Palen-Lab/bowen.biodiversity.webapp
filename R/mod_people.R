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
    bslib::card(
      bslib::card_body(
        tagList(
          h1("Human Disturbance"),
          util_ui_simple_legend_element(label = "Higher", colour = "#d7191c"),
          util_ui_simple_legend_element(label = "Medium", colour = "#ffffbf"),
          util_ui_simple_legend_element(label = "Lower", colour = "#1a9641"),
          p("Human disturbance on Bowen Island, including development, recreation, and habitat fragmentation, has impacted native ecosystems by altering land cover, increasing pollution, and putting pressure on sensitive habitats like wetlands, forests, and shorelines."),
          # p("Kristen Hirsh-Pearson, Chris J. Johnson, Richard Schuster, Roger D. Wheate, and Oscar Venter. 2022. Canada’s human footprint reveals large intact areas juxtaposed against areas under immense anthropogenic pressure. FACETS. 7: 398-419.")
        )
      )
    ),
    docs_link
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
