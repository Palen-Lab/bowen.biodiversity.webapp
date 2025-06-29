#' habitats UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_habitats_ui <- function(id) {
  tabPanel(
    "habitats_panel",
    selectInput(
      NS(id, "selectGroup"),
      "Select Habitats:",
      choices = c("All Habitats" = "all",
                  "Freshwater" = "freshwater",
                  "Forests" = "forests",
                  "Other Terrestrial" = "other_terrestrial",
                  "Intertidal" = "intertidal"),
      selected = "all"
    ),
    htmlOutput(NS(id, "sidebarInfo")),
    textOutput(NS(id, "test"))
  )
}

#' habitats Server Functions
#'
#' @noRd
mod_habitats_server <- function(id, map_id, parent_session){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    #### Update raster when input changes ####
    select_raster <- reactiveVal({
      terra::rast(here::here("inst/extdata/bowen_sdm_richness.tif")) %>%
        terra::project("epsg:4326")
    })
    observeEvent(input$selectGroup, {
      # Options for habitats
      if(input$selectGroup == "all") {
        terra::rast(here::here("inst/extdata/3_habitats/total_habitat_richness.tif")) %>%
          terra::project("epsg:4326") %>%
          select_raster()
      } else if (input$selectGroup == "freshwater") {
        terra::rast(here::here("inst/extdata/3_habitats/fw_richness.tif")) %>%
          terra::project("epsg:4326") %>%
          select_raster()
      } else if (input$selectGroup == "forests") {
        # TODO: replace with actual
        terra::rast(here::here("inst/extdata/2_species/birds_richness.tif")) %>%
          terra::project("epsg:4326") %>%
          select_raster()
      } else if (input$selectGroup == "other_terrestrial") {
        # TODO: replace with actual
        terra::rast(here::here("inst/extdata/2_species/sm_mammals_richness.tif")) %>%
          terra::project("epsg:4326") %>%
          select_raster()
      } else if (input$selectGroup == "intertidal") {
        # TODO: replace with actual
        terra::rast(here::here("inst/extdata/2_species/herptiles_richness.tif")) %>%
          terra::project("epsg:4326") %>%
          select_raster()
      }

      raster_group <- "Species Richness"
      raster_domain <- c(0, terra::minmax(select_raster())[2])
      raster_pal <- leaflet::colorNumeric(c('#edf8fb','#b2e2e2','#66c2a4','#2ca25f','#006d2c'), raster_domain,
                                                    na.color = "transparent")

      # Update map
      leaflet::leafletProxy(mapId = map_id,
                            session = parent_session) %>%
        leaflet::clearControls() %>%
        leaflet::clearImages() %>%
        leaflet::addRasterImage(
          x = select_raster(),
          colors = raster_pal
        ) %>%
        leaflet::addLegend(
          pal = raster_pal,
          values =  raster_domain,
          title = raster_group
        )
    })

    #### Update sidebar content based on selectGroup ####
    observeEvent(input$selectGroup, {
      if(input$selectGroup == "all") {
        output$sidebarInfo <- renderUI({
          h1("All Habitats")
        })
      } else if (input$selectGroup == "freshwater") {
        output$sidebarInfo <- renderUI({
          tagList(
            h1("Freshwater Habitats"),
            selectInput(session$ns("subselectGroup"),
                        "Select specific habitat",
                        c("Richness", "Ponds"),
                        selected = "Richness")
          )
        })
      } else if (input$selectGroup == "forests") {
        output$sidebarInfo <- renderUI({
          tagList(
            h1("Forest Habitats"),
            selectInput(session$ns("subselectGroup"),
                        "Select specific habitat",
                        c("Richness", "Ponds"),
                        selected = "Richness")
          )
        })
      } else if (input$selectGroup == "other_terrestrial") {
        output$sidebarInfo <- renderUI({
          tagList(
            h1("Other Terrestrial Habitats"),
            selectInput(session$ns("subselectGroup"),
                        "Select specific habitat",
                        c("Richness", "Ponds"),
                        selected = "Richness")
          )
        })
      } else if (input$selectGroup == "intertidal") {
        output$sidebarInfo <- renderUI({
          tagList(
            h1("Intertidal Habitats"),
            selectInput(session$ns("subselectGroup"),
                        "Select specific habitat",
                        c("Richness", "Ponds"),
                        selected = "Richness")
          )
        })
      }
    })

    #### Freshwater ####
    subselect <- reactive({
      req(input$subselectGroup)
      input$subselectGroup
    })

    observeEvent(input$subselectGroup, {
      if(subselect() == "Richness") {
        print("Richness")
        terra::rast(here::here("inst/extdata/3_habitats/fw_richness.tif")) %>%
          terra::project("epsg:4326") %>%
          select_raster()
      } else if (subselect() == "Ponds") {
        print("Ponds")
        terra::rast(here::here("inst/extdata/3_habitats/fw_ponds.tif")) %>%
          terra::project("epsg:4326") %>%
          select_raster()
      }
      raster_group <- "Species Richness"
      raster_domain <- c(0, terra::minmax(select_raster())[2])
      raster_pal <- leaflet::colorNumeric(c('#edf8fb','#b2e2e2','#66c2a4','#2ca25f','#006d2c'), raster_domain,
                                          na.color = "transparent")

      # Update map
      leaflet::leafletProxy(mapId = map_id,
                            session = parent_session) %>%
        leaflet::clearControls() %>%
        leaflet::clearImages() %>%
        leaflet::addRasterImage(
          x = select_raster(),
          colors = raster_pal
        ) %>%
        leaflet::addLegend(
          pal = raster_pal,
          values =  raster_domain,
          title = raster_group
        )
    })


    # output$test <- renderText({
    #   req(input$select_fw)
    #   input$select_fw
    # })
  })
}

## To be copied in the UI
# mod_habitats_ui("habitats_1")

## To be copied in the server
# mod_habitats_server("habitats_1")
