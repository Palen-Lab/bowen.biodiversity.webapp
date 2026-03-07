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
  tagList(
    div(class = "d-flex align-items-center gap-2",
      checkboxInput(NS(id, "people_inat"), "iNaturalist Observations", value = FALSE),
      hover_popover(
        icon("circle-info", style = "cursor:pointer;"),
        title = "iNaturalist Observations",
        p("iNaturalist is a community science platform where people can record and share observations of plants, animals, and other organisms, with identifications aided by experts and AI."),
        p("While useful, iNaturalist is subject to observer bias — we have the most observations in places that are easy for people to get to, but not necessarily where organisms prefer to live."),
        a(icon("up-right-from-square"), "iNaturalist Observations on Bowen Island",
          href = "https://www.inaturalist.org/observations?place_id=53787",
          target = "_blank")
      )
    ),
    div(class = "d-flex align-items-center gap-2",
      checkboxInput(NS(id, "people_disturb"), "Ecological Intactness", value = FALSE),
      hover_popover(
        icon("circle-info", style = "cursor:pointer;"),
        title = "Ecological Intactness",
        p("Human disturbance on Bowen Island, including development, recreation, and habitat fragmentation, has impacted native ecosystems by altering land cover, increasing pollution, and putting pressure on sensitive habitats like wetlands, forests, and shorelines.")
      )
    )
  )
}

#' people Server Functions
#'
#' @noRd
mod_people_server <- function(id, map_id, parent_session, active_raster = NULL){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    #### Load ####
    human_footprint <- rast_layer("4_people/bowen_human_footprint.tif") %>%
      terra::project("epsg:4326")
    inat_obs <- rast_layer("4_people/bowen_inat.tif") %>%
      terra::project("epsg:4326")

    people_boxes <- c("people_inat", "people_disturb")

    #### Enforce mutual exclusivity ####
    lapply(people_boxes, function(checked_box) {
      observeEvent(input[[checked_box]], {
        if (isTRUE(input[[checked_box]])) {
          for (other in setdiff(people_boxes, checked_box)) {
            updateCheckboxInput(session, other, value = FALSE)
          }
        }
      }, ignoreInit = TRUE)
    })

    #### Reactive: which box is currently checked ####
    active_group <- reactive({
      for (box in people_boxes) {
        if (isTRUE(input[[box]])) return(box)
      }
      NULL
    })

    #### Cross-module raster exclusivity ####
    observe({
      if (!is.null(active_group())) active_raster("people")
    })
    observeEvent(active_raster(), {
      if (!is.null(active_raster()) && active_raster() != "people") {
        for (box in people_boxes) updateCheckboxInput(session, box, value = FALSE)
      }
    }, ignoreInit = TRUE)

    #### Update map when selection changes ####
    observe({
      group <- active_group()
      map <- leaflet::leafletProxy(mapId = map_id, session = parent_session)

      if (is.null(group)) {
        map %>%
          leaflet::removeImage(layerId = "human_footprint_raster") %>%
          leaflet::removeControl(layerId = "human_footprint_legend") %>%
          leaflet::removeImage(layerId = "inat_raster") %>%
          leaflet::removeControl(layerId = "inat_legend")
        return()
      }

      if (group == "people_disturb") {
        human_footprint_domain <- c(0, terra::minmax(human_footprint)[2])
        human_footprint_pal <- leaflet::colorNumeric(
          c("#1a9641", "#a6d96a", "#ffffbf", "#fdae61", "#d7191c"),
          human_footprint_domain, na.color = "transparent"
        )
        map %>%
          leaflet::removeImage(layerId = "inat_raster") %>%
          leaflet::removeControl(layerId = "inat_legend") %>%
          leaflet::addRasterImage(x = human_footprint, layerId = "human_footprint_raster",
                                  colors = human_footprint_pal) %>%
          leaflet::addLegend(pal = human_footprint_pal, layerId = "human_footprint_legend",
                             values = human_footprint_domain, title = "Ecological Intactness")
      } else if (group == "people_inat") {
        domain <- c(0, terra::minmax(inat_obs)[2])
        colours_vect <- c('#feebe2','#fcc5c0','#fa9fb5','#f768a1','#dd3497','#ae017e','#7a0177')
        colour_ramp <- colorRamp(colours_vect)
        pal <- leaflet::colorBin(
          palette = colour_ramp,
          bins = signif(10^seq(0, log10(10000), length.out = 8), 1),
          domain = domain, na.color = "transparent"
        )
        map %>%
          leaflet::removeImage(layerId = "human_footprint_raster") %>%
          leaflet::removeControl(layerId = "human_footprint_legend") %>%
          leaflet::addRasterImage(x = inat_obs, layerId = "inat_raster", colors = pal) %>%
          leaflet::addLegend(pal = pal, layerId = "inat_legend", values = domain, title = "iNaturalist Obs.")
      }
    })
  })
}

## To be copied in the UI
# mod_people_ui("people_1")

## To be copied in the server
# mod_people_server("people_1")
