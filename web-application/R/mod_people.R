#' people UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_people_ui <- function(id) {
  ns <- NS(id)
  tagList(
    checkboxInput(
      NS(id, "people_show"),
      "Show layer",
      value = FALSE
    ),
    selectInput(
      NS(id, "selectGroup"),
      "Select Layer:",
      choices = c(
        "iNaturalist Observations" = "inat",
        "Human Disturbance" = "disturb"
      ),
      selected = "inat"
    ),
    bslib::card(
      bslib::card_body(
        htmlOutput(NS(id, "sidebarInfo")),
        htmlOutput(NS(id, "specific_sidebarInfo"))
      )
    )
  )
}

#' people Server Functions
#'
#' @noRd
mod_people_server <- function(id, map_id, parent_session){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    #### Load ####
    human_footprint <- terra::rast(here::here("inst/extdata/4_people/bowen_human_footprint.tif")) %>%
      terra::project("epsg:4326")
    inat_obs <- terra::rast(here::here("inst/extdata/4_people/bowen_inat.tif")) %>%
      terra::project("epsg:4326")

    #### Clear all people layers helper ####
    clear_people_layers <- function(map) {
      map %>%
        leaflet::removeImage(layerId = "human_footprint_raster") %>%
        leaflet::removeControl(layerId = "human_footprint_legend") %>%
        leaflet::removeImage(layerId = "inat_raster") %>%
        leaflet::removeControl(layerId = "inat_legend")
    }

    #### Update on checkbox or selectGroup ####
    observeEvent(list(input$people_show, input$selectGroup), {
      map <- leaflet::leafletProxy(mapId = map_id, session = parent_session)

      if (!isTRUE(input$people_show)) {
        clear_people_layers(map)
        return()
      }

      if(input$selectGroup == "disturb") {
        clear_people_layers(map)
        human_footprint_domain <- c(0, terra::minmax(human_footprint)[2])
        human_footprint_pal <- leaflet::colorNumeric(
          c("#1a9641", "#a6d96a", "#ffffbf", "#fdae61", "#d7191c"),
          human_footprint_domain, na.color = "transparent"
        )
        map %>%
          leaflet::addRasterImage(x = human_footprint, layerId = "human_footprint_raster",
                                  colors = human_footprint_pal) %>%
          leaflet::addLegend(pal = human_footprint_pal, layerId = "human_footprint_legend",
                             values = human_footprint_domain, title = "Human Footprint")
        output$sidebarInfo <- renderUI({
          tagList(
            h1("Human Disturbance"),
            util_ui_simple_legend_element(label = "Higher", colour = "#d7191c"),
            util_ui_simple_legend_element(label = "Medium", colour = "#ffffbf"),
            util_ui_simple_legend_element(label = "Lower", colour = "#1a9641"),
            p("Human disturbance on Bowen Island, including development, recreation, and habitat fragmentation, has impacted native ecosystems by altering land cover, increasing pollution, and putting pressure on sensitive habitats like wetlands, forests, and shorelines.")
          )
        })
      }
      else if (input$selectGroup == "inat") {
        clear_people_layers(map)
        domain <- c(0, terra::minmax(inat_obs)[2])
        colours_vect <- c('#feebe2','#fcc5c0','#fa9fb5','#f768a1','#dd3497','#ae017e','#7a0177')
        colour_ramp <- colorRamp(colours_vect)
        pal <- leaflet::colorBin(
          palette = colour_ramp,
          bins = signif(10^seq(0, log10(10000), length.out = 8), 1),
          domain = domain, na.color = "transparent"
        )
        map %>%
          leaflet::addRasterImage(x = inat_obs, layerId = "inat_raster", colors = pal) %>%
          leaflet::addLegend(pal = pal, layerId = "inat_legend", values = domain, title = "iNaturalist Obs.")
        output$sidebarInfo <- renderUI({
          tagList(
            h1("iNaturalist Observations"),
            util_ui_simple_legend_element(label = "More Observations per Cell", colour = colours_vect[length(colours_vect)]),
            util_ui_simple_legend_element(label = "Medium", colour = colours_vect[ceiling(length(colours_vect)/2)]),
            util_ui_simple_legend_element(label = "Fewer Observations per Cell", colour = colours_vect[1]),
            p("iNaturalist is a community science platform where people can record and share observations of plants, animals, and other organisms, with identifications aided by experts and AI."),
            p("While useful, iNaturalist is subject to observer bias — we have the most observations in places that are easy for people to get to, but not necessarily where organisms prefer to live."),
            a(icon("up-right-from-square"), "iNaturalist Observations on Bowen Island",
              href = "https://www.inaturalist.org/observations?place_id=53787",
              target = "_blank",
              class = c("btn", "btn-primary"))
          )
        })
      }
    })
  })
}

## To be copied in the UI
# mod_people_ui("people_1")

## To be copied in the server
# mod_people_server("people_1")
