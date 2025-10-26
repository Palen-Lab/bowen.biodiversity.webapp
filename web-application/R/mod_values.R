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
  tabPanel(
    "values_panel",
    bslib::card(
      bslib::card_body(
        tagList(
          h1("Conservation Values"),
          util_ui_simple_legend_element(label = "Relatively Higher Value", colour = viridis::viridis(2)[1]),
          util_ui_simple_legend_element(label = "Relatively Lower Value", colour = viridis::viridis(2)[2]),
          sliderInput(NS(id, "top_pct_slider"), label = "Top % Values", min = 10, max = 100, value = 100, step = 5),
          p("We used ", strong("Zonation5,"), "a robust conservation prioritization software, to calculate relative conservation values from the input layers visualized under the Species, Habitats, and People sections."),
          p("This can help decide which natural areas are most important for biodiversity, informing how efforts and resources should be used for maximum impact."),
          p("Note that these values are relative within Bowen Island. When compared to the rest of British Columbia, Bowen Island in general has very high biodiversity. Areas that appear low on this map may still have high biodiversity compared to other places outside of Bowen Island."),
          em(strong("Note: "), "While this map can provide high-level view of biodiversity, this should not replace site-specific environmental assessments and consultation with experts.")
        )
      )
    ),
    docs_link("https://palen-lab.github.io/bowen.biodiversity.webapp/vignettes/2025_05_02_output_zonation.html")
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

    #### Update map each time slider is updated ####
    observeEvent(input$top_pct_slider, {
      top_pct <- input$top_pct_slider
      zonation_colour_ramp <- viridis::viridis(100)[0:top_pct]
      # zonation_colour_ramp <- viridis::viridis(100)
      zonation_pal <- leaflet::colorNumeric(
        palette = colorRamp(colors = zonation_colour_ramp),
        domain = c((1 - top_pct/100), 1),
        na.color = "transparent",
        reverse = TRUE
      )
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
  })
}

## To be copied in the UI
# mod_values_ui("values_1")

## To be copied in the server
# mod_values_server("values_1")
