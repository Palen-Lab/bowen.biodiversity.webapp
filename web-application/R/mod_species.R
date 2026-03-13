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
  tagList(
    div(class = "d-flex align-items-center gap-2",
      checkboxInput(NS(id, "species_all"), "All Species", value = FALSE),
      hover_popover(
        icon("circle-info", style = "cursor:pointer;"),
        title = "All Species",
        p("This map shows vertebrate species richness at 100m resolution across Bowen Island — the first wall-to-wall species richness map of its kind for the island."),
        p("Species Distribution Models (SDMs) from the Palen Lab at Simon Fraser University were used to estimate the distribution of 193 vertebrate species, combined with locally-counted heron and raptor nest data, for 195 data layers in total.")
      )
    ),
    div(class = "d-flex align-items-center gap-2",
      checkboxInput(NS(id, "species_threatened"), "Threatened Species", value = FALSE),
      hover_popover(
        icon("circle-info", style = "cursor:pointer;"),
        title = "Threatened Species",
        p("This map shows the richness of species at risk at 100m resolution across Bowen Island."),
        p("Species listed on the Red or Blue Lists of the British Columbia Conservation Data Centre are included. There are 24 of these species present on Bowen Island with Species Distribution Models available.")
      )
    ),
    div(class = "d-flex align-items-center gap-2",
      checkboxInput(NS(id, "species_birds"), "Bird Species", value = FALSE),
      hover_popover(
        icon("circle-info", style = "cursor:pointer;"),
        title = "Bird Species",
        p("This map shows bird species richness at 100m resolution across Bowen Island."),
        p("There are 176 bird species recorded on Bowen Island, based on citizen science data from eBird.")
      )
    ),
    div(class = "d-flex align-items-center gap-2",
      checkboxInput(NS(id, "species_other"), "Other Species", value = FALSE),
      hover_popover(
        icon("circle-info", style = "cursor:pointer;"),
        title = "Other Species",
        p("This map shows the richness of reptiles, amphibians, and small mammals at 100m resolution across Bowen Island."),
        p("There are 17 of these species recorded on Bowen Island, based on iNaturalist citizen science data and previous environmental reports.")
      )
    )
  )
}

#' species Server Functions
#'
#' @noRd
mod_species_server <- function(id, map_id, parent_session, active_raster = NULL){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Names of the four mutually exclusive checkboxes
    species_boxes <- c("species_all", "species_threatened", "species_birds", "species_other")

    #### Enforce mutual exclusivity ####
    # When a box is checked, uncheck the others
    lapply(species_boxes, function(checked_box) {
      observeEvent(input[[checked_box]], {
        if (isTRUE(input[[checked_box]])) {
          for (other in setdiff(species_boxes, checked_box)) {
            updateCheckboxInput(session, other, value = FALSE)
          }
        }
      }, ignoreInit = TRUE)
    })

    #### Reactive: which box is currently checked ####
    active_group <- reactive({
      for (box in species_boxes) {
        if (isTRUE(input[[box]])) return(box)
      }
      NULL
    })

    #### Cross-module raster exclusivity ####
    observe({
      if (!is.null(active_group())) active_raster("species")
    })
    observeEvent(active_raster(), {
      if (!is.null(active_raster()) && active_raster() != "species") {
        for (box in species_boxes) updateCheckboxInput(session, box, value = FALSE)
      }
    }, ignoreInit = TRUE)

    #### Update map when selection changes ####
    observe({
      group <- active_group()
      map <- leaflet::leafletProxy(mapId = map_id, session = parent_session)

      if (is.null(group)) {
        map %>%
          leaflet::removeImage(layerId = "species_raster") %>%
          leaflet::removeControl(layerId = "species_legend")
        return()
      }

      rast <- switch(group,
        species_all        = rast_layer("2_species/total_richness.tif"),
        species_threatened = rast_layer("2_species/threatened_richness.tif"),
        species_birds      = rast_layer("2_species/birds_richness.tif"),
        species_other = {
          sm   <- rast_layer("2_species/sm_mammals_richness.tif")
          herp <- rast_layer("2_species/herptiles_richness.tif")
          sum(sm, herp, na.rm = TRUE)
        }
      )

      domain <- c(0, terra::minmax(rast)[2])
      pal <- leaflet::colorNumeric(
        c('#edf8fb','#b2e2e2','#66c2a4','#2ca25f','#006d2c'),
        domain, na.color = "transparent"
      )

      legend_title <- switch(group,
        species_all       = "All Species Richness",
        species_threatened = "Threatened Species Richness",
        species_birds     = "Bird Species Richness",
        species_other     = "Other Species Richness"
      )

      map %>%
        leaflet::removeImage(layerId = "species_raster") %>%
        leaflet::removeControl(layerId = "species_legend") %>%
        leaflet::addRasterImage(x = rast, layerId = "species_raster", colors = pal) %>%
        leaflet::addLegend(pal = pal, layerId = "species_legend",
                           values = domain, title = legend_title)
    })

  })
}

## To be copied in the UI
# mod_species_ui("species_1")

## To be copied in the server
# mod_species_server("species_1")
