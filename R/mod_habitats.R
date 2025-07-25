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
  # Description and Links, across all Habitat Pages
  docs_link <- tagList(
    h4("More Details / Downloads"),
    p("The full code for producing these maps can be found at our documentation site. Many of these layers can also be downloaded."),
    a("Documentation / Downloads",
      href = "https://palen-lab.github.io/bowen.biodiversity.webapp/",
      target = "_blank",
      class = "btn btn-primary")
  )

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
    bslib::card(
      bslib::card_body(
        htmlOutput(NS(id, "sidebarInfo")),
        htmlOutput(NS(id, "specific_sidebarInfo"))
      )
    ),
    docs_link
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
      terra::rast(here::here("inst/extdata/3_habitats/total_habitat_richness.tif")) %>%
        terra::project("epsg:4326")
    })
    observeEvent(input$selectGroup, {
      # Habitat Category Selection
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
      # TODO: rename raster group
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
      # Select subcategory / specific layer within each category
      if(input$selectGroup == "all") {
        output$sidebarInfo <- renderUI({
          h1("All Habitats")
        })
        # Update Sidebar
        output$specific_sidebarInfo <- renderUI({
          tagList(
            h3("Diversity of Bowen Island Habitats"),
            p("Bowen Island features a rich mosaic of habitats—ranging from mature temperate rainforests and dry coastal bluffs to freshwater wetlands, lakes, streams, and intertidal shores—each supporting distinct communities of plants, animals, and fungi. This diversity of ecosystems underpins high ecological resilience and biodiversity, making the island a vital refuge for both terrestrial and aquatic life.")
          )
        })

      } else if (input$selectGroup == "freshwater") {
        output$sidebarInfo <- renderUI({
          tagList(
            h1("Freshwater Habitats"),
            selectInput(session$ns("subselectGroup"),
                        "Select specific habitat",
                        c("Freshwater Richness", "Lakes", "Ponds", "Riparian", "Streams", "Wetlands"),
                        selected = "Freshwater Richness")
          )
        })
      } else if (input$selectGroup == "forests") {
        output$sidebarInfo <- renderUI({
          tagList(
            h1("Forest Habitats"),
            selectInput(session$ns("subselectGroup"),
                        "Select specific habitat",
                        c("Forest Richness", "Old Forest", "Mature Forest", "Young Forest"),
                        selected = "Forest Richness")
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

    subselect <- reactive({
      req(input$subselectGroup)
      input$subselectGroup
    })
    # Update raster layer on map based on specific layer selection
    observeEvent(input$subselectGroup, {
      #### FRESHWATER ####
      if(subselect() == "Freshwater Richness") {
        # Update Map
        raster_group <- "Freshwater Richness"
        terra::rast(here::here("inst/extdata/3_habitats/fw_richness.tif")) %>%
          # method = "near" or else get mean values from multiple cells
          terra::project("epsg:3857", method = "near") %>%
          select_raster()
        raster_domain <- terra::values(select_raster()) %>%
          unique()
        raster_labels <- raster_domain
        raster_pal <- leaflet::colorNumeric(
          c('#eff3ff','#bdd7e7','#6baed6','#3182bd','#08519c'),
          raster_domain,
          na.color = "transparent"
        )
        # Update Sidebar
        output$specific_sidebarInfo <- renderUI({
          tagList(
            h3("Diversity of Freshwater Habitats"),
            # TODO: getting images
            # tags$figure(
            #   img(
            #     src = "www/pileated-woodpecker-close-up.jpg",
            #     width = "100%",
            #     alt = "This photo displays the Pileated Woodpecker as an example of a species present on Bowen Island.",
            #     title = "The Pileated Woodpecker is an example of a species present on Bowen Island."
            #   ),
            #   tags$figcaption(
            #     em("Pileated Woodpecker"),
            #     class = "text-center"
            #   ),
            #   class = "p-0 m-0"
            # ),
            util_ui_simple_legend(low_colour = '#eff3ff', high_colour = '#08519c', low_label = "= Fewer Habitats", high_label = "= More Habitats"),
            p("Bowen Island hosts a diverse range of freshwater habitats, including lakes, wetlands, and streams that support species such as amphibians, aquatic invertebrates, and native fish. These ecosystems are shaped by the island's forested terrain and seasonal rainfall, contributing to rich biodiversity and ecological resilience.")
          )
        })
      } else if (subselect() == "Lakes") {
        # Update Map
        raster_group <- "Lakes"
        terra::rast(here::here("inst/extdata/3_habitats/fw_lakes.tif")) %>%
          terra::project("epsg:3857", method = "near") %>%
          select_raster()
        raster_domain <- 1
        raster_labels <- "Present"
        raster_pal <- leaflet::colorFactor(
          c('#6baed6'),
          raster_domain,
          na.color = "transparent"
        )
        # Update Sidebar
        output$specific_sidebarInfo <- renderUI({
          tagList(
            h3("Lakes of Bowen Island"),
            util_ui_simple_legend(low_colour = '#00000000', high_colour = '#6baed6', low_label = "= Not Present", high_label = "= Present"),
            p("The lakes on Bowen Island, such as Killarney and Grafton Lakes, are ecologically significant as they provide critical habitat for amphibians, birds, and aquatic invertebrates, help regulate local hydrology, and serve as important sources of clean water and biodiversity within the island’s forested ecosystem.")
          )
        })
      } else if (subselect() == "Ponds") {
        # Update Map
        raster_group <- "Ponds"
        terra::rast(here::here("inst/extdata/3_habitats/fw_ponds.tif")) %>%
          terra::project("epsg:3857", method = "near") %>%
          select_raster()
        raster_domain <- 1
        raster_labels <- "Present"
        raster_pal <- leaflet::colorFactor(
          c('#6baed6'),
          raster_domain,
          na.color = "transparent"
        )
        # Update Sidebar
        output$specific_sidebarInfo <- renderUI({
          tagList(
            h3("Ponds of Bowen Island"),
            util_ui_simple_legend(low_colour = '#00000000', high_colour = '#6baed6', low_label = "= Not Present", high_label = "= Present"),
            p("The ponds on Bowen Island—including natural and beaver-created wetlands—are ecologically vital as they provide habitat for amphibians, invertebrates, and waterfowl, support sediment and nutrient retention, and serve as key micro‑watersheds within the island’s freshwater network.")
          )
        })
      } else if (subselect() == "Riparian") {
        # Update Map
        raster_group <- "Riparian"
        terra::rast(here::here("inst/extdata/3_habitats/fw_riparian.tif")) %>%
          terra::project("epsg:3857", method = "near") %>%
          select_raster()
        raster_domain <- 1
        raster_labels <- "Present"
        raster_pal <- leaflet::colorFactor(
          c('#6baed6'),
          raster_domain,
          na.color = "transparent"
        )
        # Update Sidebar
        output$specific_sidebarInfo <- renderUI({
          tagList(
            h3("Riparian Habitat of Bowen Island"),
            util_ui_simple_legend(low_colour = '#00000000', high_colour = '#6baed6', low_label = "= Not Present", high_label = "= Present"),
            p("Riparian zones on Bowen Island—those lush, water‑adjacent strips of vegetation along creeks, streams, and lakes—are ecologically crucial because they stabilize stream banks, filter sediment and nutrients, moderate water temperatures, recharge groundwater, and provide high‑value habitat and movement corridors for fish, amphibians, birds, and terrestrial wildlife, playing a key role in watershed health and biodiversity.")
          )
        })
      } else if (subselect() == "Streams") {
        # Update Map
        raster_group <- "Streams"
        terra::rast(here::here("inst/extdata/3_habitats/fw_streams.tif")) %>%
          terra::project("epsg:3857", method = "near") %>%
          select_raster()
        raster_domain <- c(1, 2, 3)
        raster_labels <- c("non-fish bearing","tributary to fish-bearing","fish-bearing")
        raster_pal <- leaflet::colorFactor(
          c('#ece7f2', '#a6bddb', '#2b8cbe'),
          raster_domain,
          na.color = "transparent"
        )
        # Update Sidebar
        output$specific_sidebarInfo <- renderUI({
          tagList(
            h3("Streams of Bowen Island"),
            util_ui_simple_legend_element("#ece7f2", "= Non-fish bearing"),
            util_ui_simple_legend_element('#a6bddb', "= Tributary to fish-bearing"),
            util_ui_simple_legend_element('#2b8cbe', "= Fish-bearing"),
            p("Streams on Bowen Island play a vital ecological role by sustaining freshwater habitats, supporting salmonid populations, and connecting upland forests to wetlands and marine ecosystems, thereby maintaining biodiversity and ecosystem health.")
          )
        })
      } else if (subselect() == "Wetlands") {
        # Update Map
        raster_group <- "Wetlands"
        terra::rast(here::here("inst/extdata/3_habitats/fw_wetlands.tif")) %>%
          terra::project("epsg:3857", method = "near") %>%
          select_raster()
        raster_domain <- 1
        raster_labels <- "Present"
        raster_pal <- leaflet::colorFactor(
          c('#6baed6'),
          raster_domain,
          na.color = "transparent"
        )
        # Update Sidebar
        output$specific_sidebarInfo <- renderUI({
          tagList(
            h3("Wetlands of Bowen Island"),
            util_ui_simple_legend(low_colour = '#00000000', high_colour = '#6baed6', low_label = "= Not Present", high_label = "= Present"),
            p("Wetlands on Bowen Island are ecologically significant as they regulate water flow, improve water quality, support high biodiversity, and provide critical breeding and foraging habitat for amphibians, birds, and other wildlife.")
          )
        })
      #### FORESTS ####
      } else if (subselect() == "Forest Richness") {
        # Update Map
        raster_group <- "Forest Richness"
        terra::rast(here::here("inst/extdata/3_habitats/fw_richness.tif")) %>%
          # method = "near" or else get mean values from multiple cells
          terra::project("epsg:3857", method = "near") %>%
          select_raster()
        raster_domain <- terra::values(select_raster()) %>%
          unique()
        raster_labels <- raster_domain
        raster_pal <- leaflet::colorNumeric(
          c('#eff3ff','#bdd7e7','#6baed6','#3182bd','#08519c'),
          raster_domain,
          na.color = "transparent"
        )
        # Update Sidebar
        output$specific_sidebarInfo <- renderUI({
          tagList(
            h3("Forests of Bowen Island"),
            p("Wetlands on Bowen Island are ecologically significant as they regulate water flow, improve water quality, support high biodiversity, and provide critical breeding and foraging habitat for amphibians, birds, and other wildlife.")
          )
        })
      }

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
          colors = raster_pal(raster_domain),
          labels = raster_labels,
          labFormat = labelFormat(),
          title = raster_group
        )
    })
  })
}

## To be copied in the UI
# mod_habitats_ui("habitats_1")

## To be copied in the server
# mod_habitats_server("habitats_1")
