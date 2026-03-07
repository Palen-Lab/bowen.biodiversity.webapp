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
  tagList(
    checkboxInput(
      NS(id, "habitats_show"),
      "Show layer",
      value = FALSE
    ),
    selectInput(
      NS(id, "selectGroup"),
      "Select Habitats:",
      choices = c(
        "All Habitats" = "all",
        "Freshwater" = "freshwater",
        "Terrestrial" = "terrestrial"
      ),
      selected = "all"
    ),
    bslib::card(
      bslib::card_body(
        htmlOutput(NS(id, "sidebarInfo")),
        htmlOutput(NS(id, "specific_sidebarInfo"))
      )
    ),
    docs_link("https://palen-lab.github.io/bowen.biodiversity.webapp/vignettes/input_habitats.html")
  )
}

#' habitats Server Functions
#' @importFrom foreach %do% foreach
#' @noRd
mod_habitats_server <- function(id, map_id, parent_session){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    select_raster <- reactiveVal(NULL)

    #### Define reactive value for subselectGroup ####
    subselect <- reactive({
      req(input$subselectGroup)
      input$subselectGroup
    })

    #### Update sidebar based on selectGroup ####
    observeEvent(input$selectGroup, {
      if(input$selectGroup == "all") {
        output$sidebarInfo <- renderUI({ h1("Diversity of Bowen Island Habitats") })
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
      } else if (input$selectGroup == "terrestrial") {
        output$sidebarInfo <- renderUI({
          tagList(
            h1("Terrestrial Habitats"),
            selectInput(session$ns("subselectGroup"),
                        "Select specific habitat",
                        c("Old Forest", "Mature Forest", "Young Forest", "Young Forest (small)", "Other Terrestrial", "Coastal"),
                        selected = "Old Forest")
          )
        })
      }
    })

    #### Update habitat raster + specific_sidebarInfo ####
    observeEvent(list(input$habitats_show, input$subselectGroup, input$selectGroup), {

      if (!isTRUE(input$habitats_show)) {
        leaflet::leafletProxy(mapId = map_id, session = parent_session) %>%
          leaflet::removeImage(layerId = "habitats_raster") %>%
          leaflet::removeControl(layerId = "habitats_legend")
        return()
      }

      #### ALL HABITATS ####
      if(input$selectGroup == "all") {
        raster_group <- "Habitat Richness"
        terra::rast(here::here("inst/extdata/3_habitats/total_habitat_richness.tif")) %>%
          terra::project("epsg:3857", method = "near") %>%
          select_raster()
        raster_domain <- terra::values(select_raster()) %>% unique() %>% sort()
        raster_labels <- raster_domain
        raster_pal <- leaflet::colorNumeric(
          c('#edf8fb','#b2e2e2','#66c2a4','#2ca25f','#006d2c'),
          raster_domain, na.color = "transparent"
        )
        output$specific_sidebarInfo <- renderUI({
          tagList(
            util_ui_simple_legend(low_colour = '#edf8fb', high_colour = '#006d2c',
                                  low_label = "Fewer Habitat Types", high_label = "More Habitat Types"),
            p("Bowen Island features a rich mosaic of habitats—ranging from mature temperate rainforests and dry coastal bluffs to freshwater wetlands, lakes, streams, and Coastal shores—each supporting distinct communities of plants, animals, and fungi.")
          )
        })
      }
      #### OTHER TERRESTRIAL ####
      else if (subselect() == "Other Terrestrial") {
        raster_group <- "Other Terrestrial"
        terra::rast(here::here("inst/extdata/3_habitats/other_tem.tif")) %>%
          terra::project("epsg:3857", method = "near") %>%
          select_raster()
        bowen_TEM_habitat_types <- readRDS("inst/extdata/3_habitats/other_tem_types.rds")
        raster_domain <- seq(from = 0, to = (length(bowen_TEM_habitat_types$SITEMC_S1_)-1))
        raster_labels <- bowen_TEM_habitat_types$SITE_S1_LA
        raster_pal <- leaflet::colorFactor("viridis", raster_domain, na.color = "transparent")
        output$specific_sidebarInfo <- renderUI({
          tagList(
            h3("Other Terrestrial Habitats"),
            p("Here, the more detailed habitat types captured by the Terrestrial Ecosystem Mapping project are represented.")
          )
        })
      }
      #### Coastal ####
      else if (subselect() == "Coastal") {
        raster_group <- "Coastal"
        terra::rast(here::here("inst/extdata/3_habitats/intertidal.tif")) %>%
          terra::project("epsg:3857", method = "near") %>%
          select_raster()
        raster_domain <- seq(from = 0, to = 4)
        raster_labels <- c("Beaches", "Coastal Herbaceous", "Eelgrass", "Mudflats", "Vegetated Shoreline")
        raster_colours <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99")
        raster_pal <- leaflet::colorFactor(raster_colours, raster_domain, na.color = "transparent")
        output$specific_sidebarInfo <- renderUI({
          tagList(
            h3("Coastal Habitats"),
            foreach(i = 1:length(raster_labels)) %do% {
              util_ui_simple_legend_element(label = raster_labels[i], colour = raster_colours[i])
            },
            p("Coastal ecosystems on Bowen Island play a vital ecological role by supporting high biodiversity, including shellfish, seaweeds, and invertebrates.")
          )
        })
      }
      #### FRESHWATER ####
      else if(subselect() == "Freshwater Richness") {
        raster_group <- "Freshwater Richness"
        terra::rast(here::here("inst/extdata/3_habitats/fw_richness.tif")) %>%
          terra::project("epsg:3857", method = "near") %>%
          select_raster()
        raster_domain <- terra::values(select_raster()) %>% unique()
        raster_labels <- raster_domain
        raster_colour_ramp <- viridis::mako(5, end = 0.8, direction = -1)
        raster_pal <- leaflet::colorNumeric(raster_colour_ramp, raster_domain, na.color = "transparent")
        output$specific_sidebarInfo <- renderUI({
          tagList(
            h3("Diversity of Freshwater Habitats"),
            util_ui_simple_legend(low_colour = raster_colour_ramp[1], high_colour = raster_colour_ramp[5],
                                  low_label = "Fewer Habitats", high_label = "More Habitats"),
            p("Bowen Island hosts a diverse range of freshwater habitats, including lakes, wetlands, and streams.")
          )
        })
      } else if (subselect() == "Lakes") {
        raster_group <- "Lakes"
        terra::rast(here::here("inst/extdata/3_habitats/fw_lakes.tif")) %>%
          terra::project("epsg:3857", method = "near") %>%
          select_raster()
        raster_domain <- 1; raster_labels <- "Present"
        raster_pal <- leaflet::colorFactor(c('#41B6C4'), raster_domain, na.color = "transparent")
        output$specific_sidebarInfo <- renderUI({
          tagList(
            h3("Lakes of Bowen Island"),
            util_ui_simple_legend(low_colour = '#00000000', high_colour = '#41B6C4', low_label = "Not Present", high_label = "Present"),
            p("The lakes on Bowen Island, such as Killarney and Grafton Lakes, are ecologically significant.")
          )
        })
      } else if (subselect() == "Ponds") {
        raster_group <- "Ponds"
        terra::rast(here::here("inst/extdata/3_habitats/fw_ponds.tif")) %>%
          terra::project("epsg:3857", method = "near") %>%
          select_raster()
        raster_domain <- 1; raster_labels <- "Present"
        raster_pal <- leaflet::colorFactor(c("#2C7FB8"), raster_domain, na.color = "transparent")
        output$specific_sidebarInfo <- renderUI({
          tagList(
            h3("Ponds of Bowen Island"),
            util_ui_simple_legend(low_colour = '#00000000', high_colour = "#2C7FB8", low_label = "Not Present", high_label = "Present"),
            p("The ponds on Bowen Island—including natural and beaver-created wetlands—are ecologically vital.")
          )
        })
      } else if (subselect() == "Riparian") {
        raster_group <- "Riparian"
        terra::rast(here::here("inst/extdata/3_habitats/fw_riparian.tif")) %>%
          terra::project("epsg:3857", method = "near") %>%
          select_raster()
        raster_domain <- 1; raster_labels <- "Present"
        raster_pal <- leaflet::colorFactor(c("#A1DAB4"), raster_domain, na.color = "transparent")
        output$specific_sidebarInfo <- renderUI({
          tagList(
            h3("Riparian Habitat of Bowen Island"),
            util_ui_simple_legend(low_colour = '#00000000', high_colour = "#A1DAB4", low_label = "Not Present", high_label = "Present"),
            p("Riparian zones on Bowen Island are ecologically crucial for watershed health and biodiversity.")
          )
        })
      } else if (subselect() == "Streams") {
        raster_group <- "Streams"
        terra::rast(here::here("inst/extdata/3_habitats/fw_streams.tif")) %>%
          terra::project("epsg:3857", method = "near") %>%
          select_raster()
        raster_domain <- c(1, 2, 3)
        raster_labels <- c("non-fish bearing","tributary to fish-bearing","fish-bearing")
        raster_pal <- leaflet::colorFactor(c('#ece7f2', '#a6bddb', '#2b8cbe'), raster_domain, na.color = "transparent")
        output$specific_sidebarInfo <- renderUI({
          tagList(
            h3("Streams of Bowen Island"),
            util_ui_simple_legend_element("#ece7f2", "Non-fish bearing"),
            util_ui_simple_legend_element('#a6bddb', "Tributary to fish-bearing"),
            util_ui_simple_legend_element('#2b8cbe', "Fish-bearing"),
            p("Streams on Bowen Island play a vital ecological role supporting salmonid populations.")
          )
        })
      } else if (subselect() == "Wetlands") {
        raster_group <- "Wetlands"
        terra::rast(here::here("inst/extdata/3_habitats/fw_wetlands.tif")) %>%
          terra::project("epsg:3857", method = "near") %>%
          select_raster()
        raster_domain <- 1; raster_labels <- "Present"
        raster_pal <- leaflet::colorFactor(c("#253494"), raster_domain, na.color = "transparent")
        output$specific_sidebarInfo <- renderUI({
          tagList(
            h3("Wetlands of Bowen Island"),
            util_ui_simple_legend(low_colour = '#00000000', high_colour = "#253494", low_label = "Not Present", high_label = "Present"),
            p("Wetlands on Bowen Island regulate water flow, improve water quality, and support high biodiversity.")
          )
        })
      }
      #### FORESTS ####
      else if (subselect() == "Old Forest") {
        raster_group <- "Old Forest"
        terra::rast(here::here("inst/extdata/3_habitats/forests_OF.tif")) %>%
          terra::project("epsg:3857", method = "near") %>%
          select_raster()
        raster_domain <- 1; raster_labels <- "Present"
        raster_pal <- leaflet::colorFactor(c('#2ca25f'), raster_domain, na.color = "transparent")
        output$specific_sidebarInfo <- renderUI({
          tagList(
            h3("Old Forests of Bowen Island"),
            util_ui_simple_legend(low_colour = '#00000000', high_colour = '#2ca25f', low_label = "Not Present", high_label = "Present"),
            p("Old forests are generally conifer-dominated forest with complex vertical structure, where canopy tree ages are mostly 250 years old or older.")
          )
        })
      } else if (subselect() == "Mature Forest") {
        raster_group <- "Mature Forest"
        terra::rast(here::here("inst/extdata/3_habitats/forests_MF.tif")) %>%
          terra::project("epsg:3857", method = "near") %>%
          select_raster()
        raster_domain <- 1; raster_labels <- "Present"
        raster_pal <- leaflet::colorFactor(c('#2ca25f'), raster_domain, na.color = "transparent")
        output$specific_sidebarInfo <- renderUI({
          tagList(
            h3("Mature Forests of Bowen Island"),
            util_ui_simple_legend(low_colour = '#00000000', high_colour = '#2ca25f', low_label = "Not Present", high_label = "Present"),
            p("Mature forests are generally greater than 80 years old and less than 250 years old.")
          )
        })
      } else if (subselect() == "Young Forest") {
        raster_group <- "Young Forest"
        terra::rast(here::here("inst/extdata/3_habitats/forests_YF.tif")) %>%
          terra::project("epsg:3857", method = "near") %>%
          select_raster()
        raster_domain <- 1; raster_labels <- "Present"
        raster_pal <- leaflet::colorFactor(c('#2ca25f'), raster_domain, na.color = "transparent")
        output$specific_sidebarInfo <- renderUI({
          tagList(
            h3("Young Forests of Bowen Island"),
            util_ui_simple_legend(low_colour = '#00000000', high_colour = '#2ca25f', low_label = "Not Present", high_label = "Present"),
            p("Young forests are generally greater than 30 to 40 years old and less than 80 years old.")
          )
        })
      } else if (subselect() == "Young Forest (small)") {
        raster_group <- "Young Forest (small)"
        terra::rast(here::here("inst/extdata/3_habitats/forests_YS.tif")) %>%
          terra::project("epsg:3857", method = "near") %>%
          select_raster()
        raster_domain <- 1; raster_labels <- "Present"
        raster_pal <- leaflet::colorFactor(c('#2ca25f'), raster_domain, na.color = "transparent")
        output$specific_sidebarInfo <- renderUI({
          tagList(
            h3("Young Forests (small)"),
            util_ui_simple_legend(low_colour = '#00000000', high_colour = '#2ca25f', low_label = "Not Present", high_label = "Present"),
            p("These are patches of young forests that are less than 5 ha.")
          )
        })
      } else {
        return()
      }

      #### Update Leaflet Map ####
      leaflet::leafletProxy(mapId = map_id, session = parent_session) %>%
        leaflet::removeImage(layerId = "habitats_raster") %>%
        leaflet::removeControl(layerId = "habitats_legend") %>%
        leaflet::addRasterImage(
          x = select_raster(),
          layerId = "habitats_raster",
          colors = raster_pal
        ) %>%
        leaflet::addLegend(
          layerId = "habitats_legend",
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
