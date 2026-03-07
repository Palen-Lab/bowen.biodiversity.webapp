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
  tagList(
    checkboxInput(
      NS(id, "threats_show"),
      "Show layer",
      value = FALSE
    ),
    bslib::card(
      bslib::card_body(
        selectInput(
          NS(id, "selectGroup"),
          "Select Threat:",
          choices = c(
            "Development" = "development",
            "Wildfire" = "wildfire"
          ),
          selected = "development"
        ),
        htmlOutput(NS(id, "sidebarInfo")),
        htmlOutput(NS(id, "specific_sidebarInfo"))
      )
    )
  )
}

#' threats Server Functions
#'
#' @noRd
mod_threats_server <- function(id, map_id, parent_session){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    #### Load vector layers ####
    dev_layer <- sf::st_read(here::here("inst/extdata/6_threats/development_potential.gpkg"))

    #### Define reactive value for subselectGroup ####
    subselect <- reactive({
      req(input$subselectGroup)
      input$subselectGroup
    })

    #### Clear all threats layers helper ####
    clear_threats_layers <- function(map) {
      map %>%
        leaflet::removeImage(layerId = "threats_raster") %>%
        leaflet::removeControl(layerId = "threats_legend") %>%
        leaflet::clearGroup("threats_vector")
    }

    #### Update sidebar based on selectGroup ####
    observeEvent(input$selectGroup, {
      if(input$selectGroup == "development") {
        output$sidebarInfo <- renderUI({
          tagList(
            h1("Development"),
            selectInput(session$ns("subselectGroup"), "Select layer to view",
                        c("Development", "Development and Biodiversity"), selected = "Development")
          )
        })
      }
      else if (input$selectGroup == "wildfire") {
        output$sidebarInfo <- renderUI({
          tagList(
            h1("Wildfire"),
            selectInput(session$ns("subselectGroup"), "Select wildfire layer",
                        c("Fire Index", "Wildland Urban Interface"), selected = "Fire Index")
          )
        })
      }
    })

    #### Update layer on checkbox / selection changes ####
    observeEvent(list(input$threats_show, input$subselectGroup, input$selectGroup), {
      map <- leaflet::leafletProxy(mapId = map_id, session = parent_session)

      if (!isTRUE(input$threats_show)) {
        clear_threats_layers(map)
        return()
      }

      # Wildfire - Fire Index
      if(subselect() == "Fire Index") {
        fire_index_simple <- terra::rast(here::here("inst/extdata/6_threats/fire_index_40m.tif")) %>%
          terra::project("epsg:3857", method = "near")
        terra::NAflag(fire_index_simple) <- 4294967296
        raster_domain <- terra::values(fire_index_simple) %>% unique() %>% sort()
        raster_labels <- raster_domain
        raster_pal <- leaflet::colorNumeric("magma", raster_domain, na.color = "transparent", reverse = TRUE)

        output$specific_sidebarInfo <- renderUI({
          tagList(
            h1("Wildfire Vulnerability Index (WVI)"),
            util_ui_simple_legend_element(label = "Higher Vulnerability", colour = "#5B177E"),
            util_ui_simple_legend_element(label = "Lower Vulnerability", colour = "#FEB078"),
            p("This is a simple map to show how vulnerable different parts of Bowen Island are to biodiversity loss if a wildfire were to happen — not predicting fires, just showing where damage could be worst."),
            p("We base this on three things: how steep the land is (fires spread faster uphill), which direction it faces (south-facing slopes tend to burn hotter), and what kind of habitat is there (some forests and sensitive ecosystems are more easily harmed by fire)."),
            p("Private lands are masked in this map, so we can only see the WVI for public lands on this web app.")
          )
        })

        clear_threats_layers(map)
        map %>%
          leaflet::addRasterImage(x = fire_index_simple, layerId = "threats_raster", colors = raster_pal) %>%
          leaflet::addLegend(layerId = "threats_legend",
                             colors = raster_pal(raster_domain), labels = raster_labels,
                             labFormat = labelFormat(), title = "Relative Wildfire Vuln.")
      }
      # Wildfire - Wildland Urban Interface
      else if(subselect() == "Wildland Urban Interface") {
        fire_wui <- terra::rast(here::here("inst/extdata/6_threats/fire_wui_40m.tif")) %>%
          terra::project("epsg:3857", method = "near")
        terra::NAflag(fire_wui) <- 4294967296
        raster_domain <- c(1, 2, 3, 4, 5)
        raster_labels <- c("Private Land / 1-4", "Water / 1-4", "Low / 1-4", "Moderate / 1-4", "Moderate / 5-6")
        raster_colours <- c("#a6cee3", "#abdda4", "#ffffbf", "#fdae61", "#d7191c")
        raster_pal <- leaflet::colorFactor(raster_colours, raster_domain, na.color = "transparent")

        output$specific_sidebarInfo <- renderUI({
          tagList(
            h1("Wildfire Vulnerability"),
            foreach(i = 1:length(raster_labels)) %do% {
              util_ui_simple_legend_element(label = raster_labels[i], colour = raster_colours[i])
            },
            p("This is a simple map showing a rasterized version of the Wildland Urban Interface (WUI) risk class maps produced by the BC Wildfire Service."),
            a(icon("up-right-from-square"), "About Wildland Urban Interface Maps",
              href = "https://www2.gov.bc.ca/gov/content/safety/wildfire-status/prevention/fire-fuel-management/wui-risk-class-maps",
              target = "_blank",
              class = c("btn", "btn-primary"))
          )
        })

        clear_threats_layers(map)
        map %>%
          leaflet::addRasterImage(x = fire_wui, layerId = "threats_raster", colors = raster_pal) %>%
          leaflet::addLegend(layerId = "threats_legend",
                             colors = raster_pal(raster_domain), labels = raster_labels,
                             labFormat = labelFormat(), title = "WUI / PSTA")
      }
      # Development
      else if(subselect() == "Development") {
        layer_domain <- c(0, dev_layer$potential_units)
        pal <- leaflet::colorNumeric(palette = "YlOrRd", domain = layer_domain)

        output$specific_sidebarInfo <- renderUI({
          tagList(
            h1("Development Potential"),
            util_ui_simple_legend(low_colour = '#ffeda0', high_colour = '#f03b20',
                                  low_label = "Fewer Potential Units", high_label = "More Potential Units"),
            p("This section outlines how development potential on Bowen Island can be quantified by identifying where land can be subdivided to allow more buildings."),
            em(strong("Note: "), "This map does not show all developments or plots on Bowen Island, only those identified as having potential for future development / densification.")
          )
        })

        clear_threats_layers(map)
        map %>%
          leaflet::addPolygons(data = dev_layer, group = "threats_vector",
                               color = ~pal(potential_units), stroke = FALSE,
                               fillOpacity = 1, smoothFactor = 0.2) %>%
          leaflet::addLegend(layerId = "threats_legend", pal = pal, values = layer_domain,
                             title = "Potential Units", opacity = 0.7)
      }
      # Development and Biodiversity
      else if(subselect() == "Development and Biodiversity") {
        layer_domain <- c(0, dev_layer$bioval_per_unit)
        pal <- leaflet::colorNumeric(palette = "YlOrRd", domain = layer_domain)

        output$specific_sidebarInfo <- renderUI({
          tagList(
            h1("Development Potential and Biodiversity"),
            util_ui_simple_legend(low_colour = '#ffeda0', high_colour = '#f03b20',
                                  low_label = "Lower Biodiversity Per Unit", high_label = "Higher Biodiversity Per Unit"),
            p("This development potential was compared to the ", strong("Conservation Values"),
              " to produce this map, showing the biodiversity per potential unit."),
            em(strong("Note: "), "This map does not show all developments or plots on Bowen Island, only those identified as having potential for future development / densification.")
          )
        })

        clear_threats_layers(map)
        map %>%
          leaflet::addPolygons(data = dev_layer, group = "threats_vector",
                               color = ~pal(bioval_per_unit), stroke = FALSE,
                               fillOpacity = 1, smoothFactor = 0.2) %>%
          leaflet::addLegend(layerId = "threats_legend", pal = pal, values = layer_domain,
                             title = "Rel. Biodiversity per Potential Unit", opacity = 0.7)
      }
    })
  })
}

## To be copied in the UI
# mod_threats_ui("threats_1")

## To be copied in the server
# mod_threats_server("threats_1")
