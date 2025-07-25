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
                  # "Bird Species" = "birds",
                  # "Small Mammals" = "sm_mammals",
                  # "Amphibians / Reptile" = "herptiles",
                  "Summed Species Distribution Models" = "sum_sdms"),
      selected = "all"
    ),
    bslib::card(
      bslib::card_body(
        htmlOutput(NS(id, "sidebarInfo")),
      )
    ),
    docs_link
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
      # } else if (input$selectSpeciesGroup == "birds") {
      #   terra::rast(here::here("inst/extdata/2_species/birds_richness.tif")) %>%
      #     terra::project("epsg:4326") %>%
      #     select_raster()
      # } else if (input$selectSpeciesGroup == "sm_mammals") {
      #   terra::rast(here::here("inst/extdata/2_species/sm_mammals_richness.tif")) %>%
      #     terra::project("epsg:4326") %>%
      #     select_raster()
      # } else if (input$selectSpeciesGroup == "herptiles") {
      #   terra::rast(here::here("inst/extdata/2_species/herptiles_richness.tif")) %>%
      #     terra::project("epsg:4326") %>%
      #     select_raster()
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

    # Simple Legend
    simple_legend <- tagList(
      div(
        style = "display: inline-flex",
        div(
          style = "height: 24px; width: 24px; background-color: #006d2c; border-radius: 50%; border-style: solid; border-color: #555;"
        ),
        strong("= More Species", style = "margin-left: 5px;")
      ),
      div(
        style = "display: inline-flex",
        div(
          style = "height: 24px; width: 24px; background-color: #edf8fb; border-radius: 50%; border-style: solid; border-color: #555"
        ),
        strong("= Less Species", style = "margin-left: 5px;")
      )
    )

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
          tagList(
            h1("All Species"),
            tags$figure(
              img(
                src = "www/pileated-woodpecker-close-up.jpg",
                width = "100%",
                alt = "This photo displays the Pileated Woodpecker as an example of a species present on Bowen Island.",
                title = "The Pileated Woodpecker is an example of a species present on Bowen Island."
              ),
              tags$figcaption(
                em("Pileated Woodpecker"),
                class = "text-center"
              ),
              class = "p-0 m-0"
            ),
            simple_legend,
            p("This map shows the Species Richness by 100m resolution cell or pixel on Bowen Island."),
            p("Based on probability of occurrence in Species Distribution Models. There are 193 of these species present on Bowen Island with species distribution models available.")
          )
        })
      } else if (input$selectSpeciesGroup == "threatened") {
        output$sidebarInfo <- renderUI({
          tagList(
            h1("Threatened Species"),
            simple_legend,
            p("This map shows the Species Richness by 100m resolution cell or pixel on Bowen Island."),
            p("Based on probability of occurrence in Species Distribution Models. Species that are listed under the Red or Blue Lists according to the British Columbia Conservation Data Centre are included in this map. There are 24 of these species present on Bowen Island with species distribution models available.")
          )
        })
      # } else if (input$selectSpeciesGroup == "birds") {
      #   output$sidebarInfo <- renderUI({
      #     h1("Bird Species")
      #   })
      # } else if (input$selectSpeciesGroup == "sm_mammals") {
      #   output$sidebarInfo <- renderUI({
      #     h1("Small Mammal Species")
      #   })
      # } else if (input$selectSpeciesGroup == "herptiles") {
      #   output$sidebarInfo <- renderUI({
      #     h1("Amphibian and Reptile Species")
      #   })
      } else if (input$selectSpeciesGroup == "sum_sdms") {
        output$sidebarInfo <- renderUI({
          tagList(
            h1("Summed Species Distribution Models"),
            simple_legend,
            p("The values from each species distribution model (ranging from 0 to 1 as probability of occurrence in the cell) are summed to provide a relative biodiversity layer."),
            p("Higher values correspond to cells with higher probability of species occurring, and vice versa. 193 species included in this analysis.")
          )
        })
      }
    })

  })
}

## To be copied in the UI
# mod_species_ui("species_1")

## To be copied in the server
# mod_species_server("species_1")
