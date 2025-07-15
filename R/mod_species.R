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
  tabPanel(
    "species_panel",
    selectInput(
      NS(id, "selectSpeciesGroup"),
      "Select Species Group:",
      choices = c("All Species" = "all",
                  "Threatened Species" = "threatened",
                  "Bird Species" = "birds",
                  "Small Mammals" = "sm_mammals",
                  "Amphibians / Reptile" = "herptiles",
                  "Summed Species Distribution Models" = "sum_sdms"),
      selected = "all"
    ),
    htmlOutput(NS(id, "sidebarInfo"))
  )
}

#' species Server Functions
#'
#' @noRd
mod_species_server <- function(id, map_id, parent_session){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    #### Update raster when input changes ####
    select_raster <- reactiveVal({
      terra::rast(here::here("inst/extdata/2_species/total_richness.tif")) %>%
        terra::project("epsg:4326")
    })
    observeEvent(input$selectSpeciesGroup, {
      # Options for species richness
      # 1. All Species Richness
      # 2. Threatened Species Richness
      # 3. Bird Species Richness
      # 4. Small Mammal Species Richness
      # 5. Amphibian / Reptile Species Richness
      # 6. Summed Species Distribution Models

      if(input$selectSpeciesGroup == "all") {
        terra::rast(here::here("inst/extdata/2_species/total_richness.tif")) %>%
          terra::project("epsg:4326") %>%
          select_raster()
      } else if (input$selectSpeciesGroup == "threatened") {
        terra::rast(here::here("inst/extdata/2_species/threatened_richness.tif")) %>%
          terra::project("epsg:4326") %>%
          select_raster()
      } else if (input$selectSpeciesGroup == "birds") {
        terra::rast(here::here("inst/extdata/2_species/birds_richness.tif")) %>%
          terra::project("epsg:4326") %>%
          select_raster()
      } else if (input$selectSpeciesGroup == "sm_mammals") {
        terra::rast(here::here("inst/extdata/2_species/sm_mammals_richness.tif")) %>%
          terra::project("epsg:4326") %>%
          select_raster()
      } else if (input$selectSpeciesGroup == "herptiles") {
        terra::rast(here::here("inst/extdata/2_species/herptiles_richness.tif")) %>%
          terra::project("epsg:4326") %>%
          select_raster()
      } else if (input$selectSpeciesGroup == "sum_sdms") {
        terra::rast(here::here("inst/extdata/2_species/sum_sdms.tif")) %>%
          terra::project("epsg:4326") %>%
          select_raster()
      }

      species_richness_group <- "Species Richness"
      species_richness_domain <- c(0, terra::minmax(select_raster())[2])
      species_richness_pal <- leaflet::colorNumeric(c('#edf8fb','#b2e2e2','#66c2a4','#2ca25f','#006d2c'), species_richness_domain,
                                                    na.color = "transparent")

      # Update map
      leaflet::leafletProxy(mapId = map_id,
                            session = parent_session) %>%
        leaflet::clearControls() %>%
        leaflet::clearImages() %>%
        leaflet::addRasterImage(
          x = select_raster(),
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

    #### Update sidebar content based on selection ####


    observeEvent(input$selectSpeciesGroup, {
      # Options for species richness
      # 1. All Species Richness
      # 2. Threatened Species Richness
      # 3. Bird Species Richness
      # 4. Small Mammal Species Richness
      # 5. Amphibian / Reptile Species Richness
      # 6. Summed Species Distribution Models

      if(input$selectSpeciesGroup == "all") {
        output$sidebarInfo <- renderUI({
          h1("All Species")
        })
      } else if (input$selectSpeciesGroup == "threatened") {
        output$sidebarInfo <- renderUI({
          h1("Threatened Species")
        })
      } else if (input$selectSpeciesGroup == "birds") {
        output$sidebarInfo <- renderUI({
          h1("Bird Species")
        })
      } else if (input$selectSpeciesGroup == "sm_mammals") {
        output$sidebarInfo <- renderUI({
          h1("Small Mammal Species")
        })
      } else if (input$selectSpeciesGroup == "herptiles") {
        output$sidebarInfo <- renderUI({
          h1("Amphibian and Reptile Species")
        })
      } else if (input$selectSpeciesGroup == "sum_sdms") {
        output$sidebarInfo <- renderUI({
          h1("Summed Species Distribution Models")
        })
      }
    })

  })
}

## To be copied in the UI
# mod_species_ui("species_1")

## To be copied in the server
# mod_species_server("species_1")
