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
  tagList(
    div(class = "d-flex align-items-center gap-2",
      checkboxInput(NS(id, "values_show"), "Conservation Values", value = FALSE),
      hover_popover(
        icon("circle-info", style = "cursor:pointer;"),
        title = "Conservation Values",
        p("We used ", strong("Zonation5,"), "a robust conservation prioritization software, to calculate relative conservation values from the input layers visualized under the Species, Habitats, and People sections."),
        p("This can help decide which natural areas are most important for biodiversity, informing how efforts and resources should be used for maximum impact."),
        p("Note that these values are relative within Bowen Island. Areas that appear low on this map may still have high biodiversity compared to other places outside of Bowen Island."),
        em(strong("Note: "), "While this map can provide a high-level view of biodiversity, it should not replace site-specific environmental assessments and consultation with experts.")
      )
    ),
    sliderInput(NS(id, "top_pct_slider"), label = "Top % Values", min = 10, max = 100, value = 100, step = 5)
  )
}

#' values Server Functions
#'
#' @noRd
mod_values_server <- function(id, map_id, parent_session, active_raster = NULL){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    #### Add Zonation Output Raster ####
    zonation <- terra::rast(here::here("inst/extdata/5_values/rankmap.tif")) %>%
      terra::project("epsg:4326")

    #### Cross-module raster exclusivity ####
    observe({
      if (isTRUE(input$values_show)) active_raster("values")
    })
    observeEvent(active_raster(), {
      if (!is.null(active_raster()) && active_raster() != "values") {
        updateCheckboxInput(session, "values_show", value = FALSE)
      }
    }, ignoreInit = TRUE)

    #### Update map each time slider or checkbox is updated ####
    observe({
      map <- leaflet::leafletProxy(mapId = map_id, session = parent_session)

      if (!isTRUE(input$values_show)) {
        map %>%
          leaflet::removeImage(layerId = "values_raster") %>%
          leaflet::removeControl(layerId = "values_legend")
        return()
      }

      top_pct <- input$top_pct_slider
      zonation_colour_ramp <- viridis::viridis(100)[0:top_pct]
      zonation_pal <- leaflet::colorNumeric(
        palette = colorRamp(colors = zonation_colour_ramp),
        domain = c((1 - top_pct/100), 1),
        na.color = "transparent",
        reverse = TRUE
      )
      map %>%
        leaflet::removeImage(layerId = "values_raster") %>%
        leaflet::removeControl(layerId = "values_legend") %>%
        leaflet::addRasterImage(x = zonation, layerId = "values_raster", colors = zonation_pal) %>%
        leaflet::addLegend(pal = zonation_pal, layerId = "values_legend",
                           values = c(0, 1), title = "Rel. Conservation Value")
    })
  })
}

## To be copied in the UI
# mod_values_ui("values_1")

## To be copied in the server
# mod_values_server("values_1")
