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
    checkboxInput(
      NS(id, "species_show"),
      "Show layer",
      value = FALSE
    ),
    selectInput(
      NS(id, "selectSpeciesGroup"),
      "Select Species Group:",
      choices = c(
        "All Species" = "all",
        "Threatened Species" = "threatened",
        "Bird Species" = "birds",
        "Other Species" = "other_species"
      ),
      selected = "all"
    ),
    bslib::card(
      bslib::card_body(
        htmlOutput(NS(id, "sidebarInfo")),
      )
    ),
    docs_link("https://palen-lab.github.io/bowen.biodiversity.webapp/vignettes/input_sdm.html")
  )
}

#' species Server Functions
#'
#' @noRd
mod_species_server <- function(id, map_id, parent_session){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    #### Reactive raster based on selection ####
    select_raster <- reactive({
      if(input$selectSpeciesGroup == "all") {
        terra::rast(here::here("inst/extdata/2_species/total_richness.tif")) %>%
          terra::project("epsg:4326")
      } else if (input$selectSpeciesGroup == "threatened") {
        terra::rast(here::here("inst/extdata/2_species/threatened_richness.tif")) %>%
          terra::project("epsg:4326")
      } else if (input$selectSpeciesGroup == "birds") {
        terra::rast(here::here("inst/extdata/2_species/birds_richness.tif")) %>%
          terra::project("epsg:4326")
      } else if (input$selectSpeciesGroup == "other_species") {
        sm_mammals_rast <- terra::rast(here::here("inst/extdata/2_species/sm_mammals_richness.tif")) %>%
          terra::project("epsg:4326")
        herptiles_rast <- terra::rast(here::here("inst/extdata/2_species/herptiles_richness.tif")) %>%
          terra::project("epsg:4326")
        sum(sm_mammals_rast, herptiles_rast, na.rm = TRUE)
      }
    })

    #### Update map when checkbox or selection changes ####
    observeEvent(list(input$species_show, input$selectSpeciesGroup), {
      if (!isTRUE(input$species_show)) {
        leaflet::leafletProxy(mapId = map_id, session = parent_session) %>%
          leaflet::removeImage(layerId = "species_raster") %>%
          leaflet::removeControl(layerId = "species_legend")
        return()
      }

      rast <- select_raster()
      domain <- c(0, terra::minmax(rast)[2])
      pal <- leaflet::colorNumeric(
        c('#edf8fb','#b2e2e2','#66c2a4','#2ca25f','#006d2c'),
        domain,
        na.color = "transparent"
      )

      leaflet::leafletProxy(mapId = map_id, session = parent_session) %>%
        leaflet::removeImage(layerId = "species_raster") %>%
        leaflet::removeControl(layerId = "species_legend") %>%
        leaflet::addRasterImage(
          x = rast,
          layerId = "species_raster",
          colors = pal
        ) %>%
        leaflet::addLegend(
          pal = pal,
          layerId = "species_legend",
          values = domain,
          title = "Species Richness"
        )
    })

    #### Update sidebar content based on selection ####
    simple_legend <- tagList(
      div(
        style = "display: inline-flex",
        div(style = "height: 24px; width: 24px; background-color: #006d2c; border-radius: 50%; border-style: solid; border-color: #555;"),
        strong("= More Species", style = "margin-left: 5px;")
      ),
      div(
        style = "display: inline-flex",
        div(style = "height: 24px; width: 24px; background-color: #edf8fb; border-radius: 50%; border-style: solid; border-color: #555"),
        strong("= Less Species", style = "margin-left: 5px;")
      )
    )

    observeEvent(input$selectSpeciesGroup, {
      if(input$selectSpeciesGroup == "all") {
        output$sidebarInfo <- renderUI({
          tagList(
            h1("All Species"),
            tags$figure(
              a(
                img(
                  src = "https://upload.wikimedia.org/wikipedia/commons/thumb/5/50/Dryocopus_pileatus_%28Pileated_Woodpecker%29_35.jpg/512px-Dryocopus_pileatus_%28Pileated_Woodpecker%29_35.jpg?20210211204234",
                  width = "100%",
                  alt = "Dryocopus pileatus (Pileated Woodpecker) 35"
                ),
                target = "_blank",
                href = "https://commons.wikimedia.org/wiki/File:Dryocopus_pileatus_(Pileated_Woodpecker)_35.jpg"
              ),
              tags$figcaption(p("Pileated Woodpecker (", em("Dryocopus pileatus"), ")"), class = "text-center"),
              class = "p-0 m-0"
            ),
            simple_legend,
            p("This map shows the Species Richness by 100m resolution cell or pixel on Bowen Island."),
            p("Presence in each pixel is based on the species' probability of occurrence in Species Distribution Models (SDM). There are 193 of these species present on Bowen Island with species distribution models available.")
          )
        })
      } else if (input$selectSpeciesGroup == "threatened") {
        output$sidebarInfo <- renderUI({
          tagList(
            h1("Threatened Species"),
            tags$figure(
              a(
                img(
                  src = "https://upload.wikimedia.org/wikipedia/commons/thumb/4/41/Little_brown_bat_face_closeup.jpg/512px-Little_brown_bat_face_closeup.jpg?20181204040912",
                  width = "100%",
                  alt = "Little brown bat face closeup"
                ),
                target = "_blank",
                href = "https://commons.wikimedia.org/wiki/File:Little_brown_bat_face_closeup.jpg"
              ),
              tags$figcaption(p("Little brown bat (", em("Myotis lucifugus"), ")"), class = "text-center"),
              class = "p-0 m-0"
            ),
            simple_legend,
            p("This map shows the Species Richness by 100m resolution cell or pixel on Bowen Island."),
            p("Species listed under the Red or Blue Lists according to the British Columbia Conservation Data Centre are included. There are 24 of these species present on Bowen Island with species distribution models available.")
          )
        })
      } else if (input$selectSpeciesGroup == "birds") {
        output$sidebarInfo <- renderUI({
          tagList(
            h1("Bird Species"),
            tags$figure(
              a(
                img(
                  src = "https://upload.wikimedia.org/wikipedia/commons/thumb/7/79/Trumpeter_swan_%2891399%29.jpg/512px-Trumpeter_swan_%2891399%29.jpg?20231114145551",
                  width = "100%",
                  alt = "Trumpeter swan at Sunnyside Beach, Toronto"
                ),
                target = "_blank",
                href = "https://commons.wikimedia.org/wiki/File:Trumpeter_swan_(91399).jpg"
              ),
              tags$figcaption(p("Trumpeter Swan (", em("Cygnus buccinator"), ")"), class = "text-center"),
              class = "p-0 m-0"
            ),
            simple_legend,
            p("This map shows the Species Richness of Birds by 100m resolution cell or pixel on Bowen Island."),
            p("There are 176 of these species present on Bowen Island based on eBird citizen science databases.")
          )
        })
      } else if (input$selectSpeciesGroup == "other_species") {
        output$sidebarInfo <- renderUI({
          tagList(
            h1("Other Species"),
            tags$figure(
              a(
                img(
                  src = "https://upload.wikimedia.org/wikipedia/commons/thumb/8/8d/Rana_aurora_6227.JPG/512px-Rana_aurora_6227.JPG?20090503182920",
                  width = "100%",
                  alt = "Rana aurora"
                ),
                target = "_blank",
                href = "https://commons.wikimedia.org/wiki/File:Rana_aurora_6227.JPG"
              ),
              tags$figcaption(p("Red-legged Frog (", em("Rana aurora"), ")"), class = "text-center"),
              class = "p-0 m-0"
            ),
            simple_legend,
            p("This map shows the Species Richness of other species, including reptiles, amphibians, and small mammals, by 100m resolution cell or pixel on Bowen Island."),
            p("There are 17 of these species present on Bowen Island based on iNaturalist and previous environmental reports.")
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
