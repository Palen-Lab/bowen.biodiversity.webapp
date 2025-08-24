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
      choices = c(
        "All Habitats" = "all",
        "Freshwater" = "freshwater",
        "Terrestrial" = "terrestrial"
        # "Forests" = "forests",
        # "Other Terrestrial" = "other_terrestrial",
        # "Coastal" = "Coastal"
      ),
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
#' @importFrom foreach %do% foreach
#' @noRd
mod_habitats_server <- function(id, map_id, parent_session){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    #### Update raster when input changes ####
    select_raster <- reactiveVal({
      terra::rast(here::here("inst/extdata/3_habitats/total_habitat_richness.tif")) %>%
        terra::project("epsg:4326")
    })
    #### Define reactive value for specificselectGroup ####
    subselect <- reactive({
      req(input$subselectGroup)
      input$subselectGroup
    })
    #### Update sidebar based on selectGroup ####
    observeEvent(input$selectGroup, {
      # Habitat Category Selection
      if(input$selectGroup == "all") {
        # Update Sidebar
        output$sidebarInfo <- renderUI({
          h1("Diversity of Bowen Island Habitats")
        })
      } else if (input$selectGroup == "freshwater") {
        # Update Sidebar
        output$sidebarInfo <- renderUI({
          tagList(
            h1("Freshwater Habitats"),
            selectInput(session$ns("subselectGroup"),
                        "Select specific habitat",
                        c("Freshwater Richness", "Lakes", "Ponds", "Riparian", "Streams", "Wetlands"),
                        selected = "Freshwater Richness")
          )
        })
      } else if (input$selectGroup == "terrestrial") {
        # Update Sidebar
        output$sidebarInfo <- renderUI({
          tagList(
            h1("Terrestrial Habitats"),
            selectInput(session$ns("subselectGroup"),
                        "Select specific habitat",
                        c("Old Forest", "Mature Forest", "Young Forest", "Young Forest (small)", "Other Terrestrial", "Coastal"),
                        selected = "Freshwater Richness")
          )
        })
      }
      # } else if (input$selectGroup == "forests") {
      #   # Update Sidebar
      #   output$sidebarInfo <- renderUI({
      #     tagList(
      #       h1("Forest Habitats"),
      #       selectInput(session$ns("subselectGroup"),
      #                   "Select specific habitat",
      #                   c("Old Forest", "Mature Forest", "Young Forest", "Young Forest (small)"),
      #                   selected = "Old Forest")
      #     )
      #   })
      # } else if (input$selectGroup == "other_terrestrial") {
      #   # Update Sidebar
      #   output$sidebarInfo <- renderUI({
      #     tagList(
      #       h1("Other Terrestrial Habitats")
      #     )
      #   })
      # } else if (input$selectGroup == "Coastal") {
      #   # Update UI
      #   output$sidebarInfo <- renderUI({
      #     tagList(
      #       h1("Coastal Habitats")
      #     )
      #   })
      # }
    })

    #### Update habitat raster layer and specific_sidebarInfo on Leaflet ####
    # Triggered by changes in both selectGroup and subselectGroup inputs
    # For habitats without subselectGroups, need to put them before in the else if
    observeEvent(list(input$subselectGroup, input$selectGroup), {
      #### ALL HABITATS ####
      if(input$selectGroup == "all") {
        # Update Leaflet Map Parameters
        raster_group <- "Habitat Richness"
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
        output$specific_sidebarInfo <- renderUI({
          tagList(
            util_ui_simple_legend(low_colour = '#edf8fb', high_colour = '#006d2c', low_label =  "Fewer Habitat Types", high_label =  "More Habitat Types"),
            p("Bowen Island features a rich mosaic of habitats—ranging from mature temperate rainforests and dry coastal bluffs to freshwater wetlands, lakes, streams, and Coastal shores—each supporting distinct communities of plants, animals, and fungi. This diversity of ecosystems underpins high ecological resilience and biodiversity, making the island a vital refuge for both terrestrial and aquatic life.")
          )
        })
      }
      #### OTHER TERRESTRIAL ####
      else if (subselect() == "Other Terrestrial") {
        # Update Leaflet Map Parameters
        raster_group <- "Other Terrestrial"
        terra::rast(here::here("inst/extdata/3_habitats/other_tem.tif")) %>%
          terra::project("epsg:3857", method = "near") %>%
          select_raster()
        bowen_TEM_habitat_types <- readRDS("inst/extdata/3_habitats/other_tem_types.rds")

        raster_domain <- seq(from = 0, to = (length(bowen_TEM_habitat_types$SITEMC_S1_)-1))
        raster_labels <- bowen_TEM_habitat_types$SITE_S1_LA
        raster_pal <- leaflet::colorFactor(
          "viridis",
          raster_domain,
          na.color = "transparent"
        )
        # Update Specific Sidebar
        output$specific_sidebarInfo <- renderUI({
          tagList(
            h3("Other Terrestrial Habitats"),
            p("Here, the more detailed habitat types captured by the Terrestrial Ecosystem Mapping project are represented. Specific plant, soil, and moisture characteristics were mapped on Bowen Island.")
          )
        })
      }
      #### Coastal ####
      else if (subselect() == "Coastal") {
        # Update Leaflet Map Parameters
        raster_group <- "Coastal"
        terra::rast(here::here("inst/extdata/3_habitats/intertidal.tif")) %>%
          terra::project("epsg:3857", method = "near") %>%
          select_raster()

        raster_domain <- seq(from = 0, to = 4)
        raster_labels <- c("Beaches", "Coastal Herbaceous", "Eelgrass", "Mudflats", "Vegetated Shoreline")
        raster_colours <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99")
        raster_pal <- leaflet::colorFactor(
          raster_colours,
          raster_domain,
          na.color = "transparent"
        )
        # Update Specific Sidebar
        output$specific_sidebarInfo <- renderUI({
          tagList(
            h3("Coastal Habitats"),
            foreach(i = 1:length(raster_labels)) %do% {
              util_ui_simple_legend_element(
                label = raster_labels[i],
                colour = raster_colours[i]
              )
            },
            p("Coastal ecosystems on Bowen Island play a vital ecological role by supporting high biodiversity, including shellfish, seaweeds, and invertebrates that serve as food for birds and fish; they also contribute to nutrient cycling, filter water, and provide natural shoreline protection against erosion, storm surges, and sea-level rise, making them key components of coastal resilience.")
          )
        })
      }
      #### FRESHWATER ####
      else if(subselect() == "Freshwater Richness") {
        # Update Leaflet Map Parameters
        raster_group <- "Freshwater Richness"
        terra::rast(here::here("inst/extdata/3_habitats/fw_richness.tif")) %>%
          # method = "near" or else get mean values from multiple cells
          terra::project("epsg:3857", method = "near") %>%
          select_raster()
        raster_domain <- terra::values(select_raster()) %>%
          unique()
        raster_labels <- raster_domain
        raster_colour_ramp <- viridis::mako(5, end = 0.8, direction = -1)
        raster_pal <- leaflet::colorNumeric(
          raster_colour_ramp,
          raster_domain,
          na.color = "transparent"
        )
        # Update Specific Sidebar
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
            util_ui_simple_legend(low_colour = raster_colour_ramp[1], high_colour = raster_colour_ramp[5], low_label = "Fewer Habitats", high_label = "More Habitats"),
            p("Bowen Island hosts a diverse range of freshwater habitats, including lakes, wetlands, and streams that support species such as amphibians, aquatic invertebrates, and native fish. These ecosystems are shaped by the island's forested terrain and seasonal rainfall, contributing to rich biodiversity and ecological resilience.")
          )
        })
      } else if (subselect() == "Lakes") {
        # Update Leaflet Map Parameters
        raster_group <- "Lakes"
        terra::rast(here::here("inst/extdata/3_habitats/fw_lakes.tif")) %>%
          terra::project("epsg:3857", method = "near") %>%
          select_raster()
        raster_domain <- 1
        raster_labels <- "Present"
        raster_pal <- leaflet::colorFactor(
          c('#41B6C4'),
          raster_domain,
          na.color = "transparent"
        )
        # Update Specific Sidebar
        output$specific_sidebarInfo <- renderUI({
          tagList(
            h3("Lakes of Bowen Island"),
            util_ui_simple_legend(low_colour = '#00000000', high_colour = '#41B6C4', low_label = "Not Present", high_label = "Present"),
            p("The lakes on Bowen Island, such as Killarney and Grafton Lakes, are ecologically significant as they provide critical habitat for amphibians, birds, and aquatic invertebrates, help regulate local hydrology, and serve as important sources of clean water and biodiversity within the island’s forested ecosystem.")
          )
        })
      } else if (subselect() == "Ponds") {
        # Update Leaflet Map Parameters
        raster_group <- "Ponds"
        terra::rast(here::here("inst/extdata/3_habitats/fw_ponds.tif")) %>%
          terra::project("epsg:3857", method = "near") %>%
          select_raster()
        raster_domain <- 1
        raster_labels <- "Present"
        raster_pal <- leaflet::colorFactor(
          c("#2C7FB8"),
          raster_domain,
          na.color = "transparent"
        )
        # Update Specific Sidebar
        output$specific_sidebarInfo <- renderUI({
          tagList(
            h3("Ponds of Bowen Island"),
            util_ui_simple_legend(low_colour = '#00000000', high_colour = "#2C7FB8", low_label = "Not Present", high_label = "Present"),
            p("The ponds on Bowen Island—including natural and beaver-created wetlands—are ecologically vital as they provide habitat for amphibians, invertebrates, and waterfowl, support sediment and nutrient retention, and serve as key micro‑watersheds within the island’s freshwater network.")
          )
        })
      } else if (subselect() == "Riparian") {
        # Update Leaflet Map Parameters
        raster_group <- "Riparian"
        terra::rast(here::here("inst/extdata/3_habitats/fw_riparian.tif")) %>%
          terra::project("epsg:3857", method = "near") %>%
          select_raster()
        raster_domain <- 1
        raster_labels <- "Present"
        raster_pal <- leaflet::colorFactor(
          c("#A1DAB4"),
          raster_domain,
          na.color = "transparent"
        )
        # Update Specific Sidebar
        output$specific_sidebarInfo <- renderUI({
          tagList(
            h3("Riparian Habitat of Bowen Island"),
            util_ui_simple_legend(low_colour = '#00000000', high_colour = "#A1DAB4", low_label = "Not Present", high_label = "Present"),
            p("Riparian zones on Bowen Island—those lush, water‑adjacent strips of vegetation along creeks, streams, and lakes—are ecologically crucial because they stabilize stream banks, filter sediment and nutrients, moderate water temperatures, recharge groundwater, and provide high‑value habitat and movement corridors for fish, amphibians, birds, and terrestrial wildlife, playing a key role in watershed health and biodiversity.")
          )
        })
      } else if (subselect() == "Streams") {
        # Update Leaflet Map Parameters
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
        # Update Specific Sidebar
        output$specific_sidebarInfo <- renderUI({
          tagList(
            h3("Streams of Bowen Island"),
            util_ui_simple_legend_element("#ece7f2", "Non-fish bearing"),
            util_ui_simple_legend_element('#a6bddb', "Tributary to fish-bearing"),
            util_ui_simple_legend_element('#2b8cbe', "Fish-bearing"),
            p("Streams on Bowen Island play a vital ecological role by sustaining freshwater habitats, supporting salmonid populations, and connecting upland forests to wetlands and marine ecosystems, thereby maintaining biodiversity and ecosystem health.")
          )
        })
      } else if (subselect() == "Wetlands") {
        # Update Leaflet Map Parameters
        raster_group <- "Wetlands"
        terra::rast(here::here("inst/extdata/3_habitats/fw_wetlands.tif")) %>%
          terra::project("epsg:3857", method = "near") %>%
          select_raster()
        raster_domain <- 1
        raster_labels <- "Present"
        raster_pal <- leaflet::colorFactor(
          c("#253494"),
          raster_domain,
          na.color = "transparent"
        )
        # Update Specific Sidebar
        output$specific_sidebarInfo <- renderUI({
          tagList(
            h3("Wetlands of Bowen Island"),
            util_ui_simple_legend(low_colour = '#00000000', high_colour = "#253494", low_label = "Not Present", high_label = "Present"),
            p("Wetlands on Bowen Island are ecologically significant as they regulate water flow, improve water quality, support high biodiversity, and provide critical breeding and foraging habitat for amphibians, birds, and other wildlife.")
          )
        })
      }
      #### FORESTS ####
      else if (subselect() == "Old Forest") {
        # Update Leaflet Map Parameters
        raster_group <- "Old Forest"
        terra::rast(here::here("inst/extdata/3_habitats/forests_OF.tif")) %>%
          terra::project("epsg:3857", method = "near") %>%
          select_raster()
        raster_domain <- 1
        raster_labels <- "Present"
        raster_pal <- leaflet::colorFactor(
          c('#2ca25f'),
          raster_domain,
          na.color = "transparent"
        )
        # Update Specific Sidebar
        output$specific_sidebarInfo <- renderUI({
          tagList(
            h3("Old Forests of Bowen Island"),
            util_ui_simple_legend(low_colour = '#00000000', high_colour = '#2ca25f', low_label = "Not Present", high_label = "Present"),
            p("Old forests are generally conifer-dominated forest with complex vertical structure, where the canopy tree ages are mostly 250 years old or older, but may include older mixed coniferous stands."),
            p("Old forests on Bowen Island are ecologically significant for providing rare habitat for diverse species, storing large amounts of carbon, and maintaining watershed stability and long-term forest resilience.")
          )
        })
      } else if (subselect() == "Mature Forest") {
        # Update Leaflet Map Parameters
        raster_group <- "Mature Forest"
        terra::rast(here::here("inst/extdata/3_habitats/forests_MF.tif")) %>%
          terra::project("epsg:3857", method = "near") %>%
          select_raster()
        raster_domain <- 1
        raster_labels <- "Present"
        raster_pal <- leaflet::colorFactor(
          c('#2ca25f'),
          raster_domain,
          na.color = "transparent"
        )
        # Update Specific Sidebar
        output$specific_sidebarInfo <- renderUI({
          tagList(
            h3("Mature Forests of Bowen Island"),
            util_ui_simple_legend(low_colour = '#00000000', high_colour = '#2ca25f', low_label = "Not Present", high_label = "Present"),
            p("Mature forests are generally greater than 80 years old and less than 250 years old. Mature forests are not as structurally complex as old forests, but can function as essential habitat areas for many wildlife species and as primary connections between ecosystems in a highly fragmented landscape.")
          )
        })
      } else if (subselect() == "Young Forest") {
        # Update Leaflet Map Parameters
        raster_group <- "Mature Forest"
        terra::rast(here::here("inst/extdata/3_habitats/forests_YF.tif")) %>%
          terra::project("epsg:3857", method = "near") %>%
          select_raster()
        raster_domain <- 1
        raster_labels <- "Present"
        raster_pal <- leaflet::colorFactor(
          c('#2ca25f'),
          raster_domain,
          na.color = "transparent"
        )
        # Update Specific Sidebar
        output$specific_sidebarInfo <- renderUI({
          tagList(
            h3("Young Forests of Bowen Island"),
            util_ui_simple_legend(low_colour = '#00000000', high_colour = '#2ca25f', low_label = "Not Present", high_label = "Present"),
            p("Young forests are generally greater than 30 – 40 years old and less than 80 years old. Young forests can be important habitat areas for many wildlife species and serve as primary connections between ecosystems in a highly fragmented landscape.")
          )
        })
      } else if (subselect() == "Young Forest (small)") {
        # Update Leaflet Map Parameters
        raster_group <- "Young Forest (small)"
        terra::rast(here::here("inst/extdata/3_habitats/forests_YS.tif")) %>%
          terra::project("epsg:3857", method = "near") %>%
          select_raster()
        raster_domain <- 1
        raster_labels <- "Present"
        raster_pal <- leaflet::colorFactor(
          c('#2ca25f'),
          raster_domain,
          na.color = "transparent"
        )
        # Update Specific Sidebar
        output$specific_sidebarInfo <- renderUI({
          tagList(
            h3("Young Forests (small)"),
            util_ui_simple_legend(low_colour = '#00000000', high_colour = '#2ca25f', low_label = "Not Present", high_label = "Present"),
            p("These are patches of young forests that are less than 5 ha. Patch size is important in ecosystems because larger patches typically support greater biodiversity, offer more stable habitats, and enhance ecological processes by reducing edge effects and increasing habitat connectivity.")
          )
        })
      }

      #### Update Leaflet Map ####
      leaflet::leafletProxy(mapId = map_id,
                            session = parent_session) %>%
        leaflet::clearControls() %>%
        leaflet::clearImages() %>%
        leaflet::clearGroup(group = "clear_each_update") %>%
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
