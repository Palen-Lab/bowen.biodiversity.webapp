#' threats UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_threats_ui <- function(id) {
  tagList(
    tags$p(tags$strong("Wildfire"), class = "mb-0"),
    div(class = "d-flex align-items-center gap-2",
      checkboxInput(NS(id, "threat_fire_index"), "Fire Vulnerability Index", value = FALSE),
      hover_popover(
        icon("circle-info", style = "cursor:pointer;"),
        title = "Wildfire Vulnerability Index (WVI)",
        p("This map shows how vulnerable different parts of Bowen Island are to biodiversity loss in the event of a wildfire — not a prediction of where fires will occur, but an assessment of where ecological damage could be greatest."),
        p("Vulnerability is based on slope steepness, aspect (south-facing slopes burn hotter and faster), and habitat type. Private lands are masked.")
      )
    ),
    div(class = "d-flex align-items-center gap-2",
      checkboxInput(NS(id, "threat_fire_wui"), "Wildland Urban Interface", value = FALSE),
      hover_popover(
        icon("circle-info", style = "cursor:pointer;"),
        title = "Wildland Urban Interface",
        p("A rasterized version of the Wildland Urban Interface (WUI) risk class maps produced by the BC Wildfire Service."),
        a(icon("up-right-from-square"), "About WUI Risk Class Maps",
          href = "https://www2.gov.bc.ca/gov/content/safety/wildfire-status/prevention/fire-fuel-management/wui-risk-class-maps",
          target = "_blank")
      )
    ),
  )
}

#' threats Server Functions
#'
#' @noRd
mod_threats_server <- function(id, map_id, parent_session, active_raster = NULL){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    threat_raster_boxes <- c("threat_fire_index", "threat_fire_wui")

    #### Enforce mutual exclusivity among raster boxes ####
    lapply(threat_raster_boxes, function(checked_box) {
      observeEvent(input[[checked_box]], {
        if (isTRUE(input[[checked_box]])) {
          for (other in setdiff(threat_raster_boxes, checked_box)) {
            updateCheckboxInput(session, other, value = FALSE)
          }
        }
      }, ignoreInit = TRUE)
    })

    #### Reactive: which raster box is checked ####
    active_raster_group <- reactive({
      for (box in threat_raster_boxes) {
        if (isTRUE(input[[box]])) return(box)
      }
      NULL
    })

    #### Cross-module raster exclusivity ####
    observe({
      if (!is.null(active_raster_group())) active_raster("threats")
    })
    observeEvent(active_raster(), {
      if (!is.null(active_raster()) && active_raster() != "threats") {
        for (box in threat_raster_boxes) updateCheckboxInput(session, box, value = FALSE)
      }
    }, ignoreInit = TRUE)

    #### Update wildfire raster layer ####
    observe({
      group <- active_raster_group()
      map <- leaflet::leafletProxy(mapId = map_id, session = parent_session)

      if (is.null(group)) {
        map %>%
          leaflet::removeImage(layerId = "threats_raster") %>%
          leaflet::removeControl(layerId = "threats_raster_legend")
        return()
      }

      if (group == "threat_fire_index") {
        fire_index_simple <- rast_layer("6_threats/fire_index_40m.tif")
        terra::NAflag(fire_index_simple) <- 4294967296
        raster_domain <- terra::values(fire_index_simple) %>% unique() %>% sort()
        raster_pal <- leaflet::colorNumeric("magma", raster_domain, na.color = "transparent", reverse = TRUE)
        map %>%
          leaflet::removeImage(layerId = "threats_raster") %>%
          leaflet::removeControl(layerId = "threats_raster_legend") %>%
          leaflet::addRasterImage(x = fire_index_simple, layerId = "threats_raster", colors = raster_pal) %>%
          leaflet::addLegend(layerId = "threats_raster_legend",
                             colors = raster_pal(raster_domain), labels = raster_domain,
                             labFormat = labelFormat(), title = "Relative Wildfire Vuln.")
      } else if (group == "threat_fire_wui") {
        fire_wui <- rast_layer("6_threats/fire_wui_40m.tif")
        terra::NAflag(fire_wui) <- 4294967296
        raster_domain <- c(1, 2, 3, 4, 5)
        raster_labels <- c("Private Land / 1-4", "Water / 1-4", "Low / 1-4", "Moderate / 1-4", "Moderate / 5-6")
        raster_colours <- c("#a6cee3", "#abdda4", "#ffffbf", "#fdae61", "#d7191c")
        raster_pal <- leaflet::colorFactor(raster_colours, raster_domain, na.color = "transparent")
        map %>%
          leaflet::removeImage(layerId = "threats_raster") %>%
          leaflet::removeControl(layerId = "threats_raster_legend") %>%
          leaflet::addRasterImage(x = fire_wui, layerId = "threats_raster", colors = raster_pal) %>%
          leaflet::addLegend(layerId = "threats_raster_legend",
                             colors = raster_pal(raster_domain), labels = raster_labels,
                             labFormat = labelFormat(), title = "WUI / PSTA")
      }
    })

  })
}

## To be copied in the UI
# mod_threats_ui("threats_1")

## To be copied in the server
# mod_threats_server("threats_1")
