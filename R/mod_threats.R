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
  ns <- NS(id)
  tabPanel(
    "threats_panel",
    selectInput(
      NS(id, "selectGroup"),
      "Select Threat:",
      choices = c(
        "Development" = "development",
        "Wildfire" = "wildfire"
      ),
      selected = "development"
    ),
    bslib::card(
      bslib::card_body(
        htmlOutput(NS(id, "sidebarInfo")),
      )
    ),
    docs_link
  )
}

#' threats Server Functions
#'
#' @noRd
mod_threats_server <- function(id, map_id, parent_session){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    #### Update raster when input changes ####
    select_raster <- reactiveVal({
      terra::rast(here::here("inst/extdata/3_habitats/total_habitat_richness.tif")) %>%
        terra::project("epsg:4326")
    })
    observeEvent(input$selectGroup, {
      #### DEVELOPMENT ####
      if(input$selectGroup == "development") {
        # Update Leaflet Map Parameters
        raster_group <- "Development Potential"
        terra::rast(here::here("inst/extdata/3_habitats/total_habitat_richness.tif")) %>%
          terra::project("epsg:3857", method = "near") %>%
          select_raster()
        raster_domain <- terra::values(select_raster()) %>%
          unique() %>%
          sort()
        raster_labels <- raster_domain
        raster_pal <- leaflet::colorNumeric(
          c('#edf8fb','#b2e2e2','#66c2a4','#2ca25f','#006d2c'),
          raster_domain,
          na.color = "transparent"
        )
        # Update Specfic Sidebar
        output$sidebarInfo <- renderUI({
          tagList(
            h1("Development"),
            util_ui_simple_legend(low_colour = '#edf8fb', high_colour = '#006d2c', low_label =  "Fewer Habitat Types", high_label =  "More Habitat Types"),
            p("Bowen Island features a rich mosaic of habitats—ranging from mature temperate rainforests and dry coastal bluffs to freshwater wetlands, lakes, streams, and intertidal shores—each supporting distinct communities of plants, animals, and fungi. This diversity of ecosystems underpins high ecological resilience and biodiversity, making the island a vital refuge for both terrestrial and aquatic life.")
          )
        })
      }
      #### WILDFIRE ####
      else if (input$selectGroup == "wildfire") {
        # Update Leaflet Map Parameters
        raster_group <- "Relative Wildfire Vuln."
        fire_index_simple <- terra::rast(here::here("inst/extdata/6_threats/fire_index_40m.tif")) %>%
          terra::project("epsg:3857", method = "near")
        terra::NAflag(fire_index_simple) <- 4294967296
        select_raster(fire_index_simple)

        raster_domain <- terra::values(select_raster()) %>%
          unique() %>%
          sort()
        raster_labels <- raster_domain
        raster_pal <- leaflet::colorNumeric(
          "magma",
          raster_domain,
          na.color = "transparent",
          reverse = T
        )
        # Update Specific Sidebar
        output$sidebarInfo <- renderUI({
          tagList(
            h1("Wildfire Vulnerability"),
            util_ui_simple_legend_element(label = "Higher Vulnerability", colour = "#5B177E"),
            util_ui_simple_legend_element(label = "Lower Vulnerability", colour = "#FEB078"),
            p("This is a simple map to show how vulnerable different parts of Bowen Island are to biodiversity loss if a wildfire were to happen — not predicting fires, just showing where damage could be worst."),
            p("We base this on three things: how steep the land is (fires spread faster uphill), which direction it faces (south-facing slopes tend to burn hotter), and what kind of habitat is there (some forests and sensitive ecosystems are more easily harmed by fire).")
          )
        })
      }
      #### Update Leaflet Map ####
      leaflet::leafletProxy(mapId = map_id,
                            session = parent_session) %>%
        leaflet::clearControls() %>%
        leaflet::clearImages() %>%
        leaflet::addRasterImage(
          x = select_raster(),
          colors = raster_pal
        ) %>%
        leaflet::addLegend(
          colors = raster_pal(raster_domain),
          labels = raster_labels,
          labFormat = labelFormat(),
          title = raster_group
        )
    })
  })
}

## To be copied in the UI
# mod_threats_ui("threats_1")

## To be copied in the server
# mod_threats_server("threats_1")
