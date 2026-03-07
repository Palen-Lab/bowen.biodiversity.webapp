#' protected_areas UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_protected_areas_ui <- function(id) {
  ns <- NS(id)
  tagList(
    checkboxInput(NS(id, "pa_show"), "Show existing protected areas", value = FALSE),
    checkboxInput(NS(id, "pa_candidates_show"), "Show candidate protected areas", value = FALSE),
    bslib::card(
      bslib::card_body(
        htmlOutput(NS(id, "sidebarInfo")),
        htmlOutput(NS(id, "specific_sidebarInfo"))
      )
    )
  )
}

#' protected_areas Server Functions
#'
#' @noRd
mod_protected_areas_server <- function(id, map_id, parent_session){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    #### Load vector layers ####
    bowen_pa <- here::here("inst/extdata/7_protected_areas/existing_protected_areas.gpkg") %>%
      sf::st_read()
    # bowen_ogma <- ogma %>%
    #   sf::st_transform(sf::st_crs(bowen_pa))
    bowen_new_pa <- here::here("inst/extdata/7_protected_areas/new_protected_areas.gpkg") %>%
      sf::st_read() %>%
      sf::st_transform(sf::st_crs(bowen_pa))

    #### Sidebar info ####
    output$sidebarInfo <- renderUI({
      tagList(
        h1("Protected Areas on Bowen Island"),
        util_ui_simple_legend_element(label = "Existing Protected Areas", colour = "#a1d76a", border_colour = "lightgrey"),
        util_ui_simple_legend_element(label = "Candidate Protected Areas", colour = "orange", border_colour = "lightgrey"),
        p("Expanding and connecting protected areas on Bowen Island would help preserve rare ecosystems, maintain wildlife corridors, and contribute to Canada's 30 by 30 biodiversity targets."),
        p("Canada's 30 by 30 target commits to protecting 30% of the country's lands and waters by 2030, a goal aimed at conserving biodiversity, mitigating climate change, and safeguarding ecosystem services.")
      )
    })

    #### Show / hide existing protected areas ####
    observeEvent(input$pa_show, {
      map <- leaflet::leafletProxy(mapId = map_id, session = parent_session)

      if (!isTRUE(input$pa_show)) {
        map %>%
          leaflet::clearGroup("existing_pa") %>%
          leaflet::removeControl(layerId = "pa_legend")
        return()
      }

      map %>%
        leaflet::addPolygons(
          data = bowen_pa,
          group = "existing_pa",
          fillColor = "#a1d76a",
          color = "lightgrey",
          weight = 3,
          fillOpacity = 1,
          smoothFactor = 0.2,
          label = ~lapply(
            paste0("<div style='color:darkgreen; font-size:14px;'>",
                   "<b>", name, "</b><br/>",
                   "<span style='color:gray;'>Type: ", type, "</span>",
                   "</div>"),
            HTML
          ),
          highlightOptions = leaflet::highlightOptions(color = "orange", weight = 3, bringToFront = FALSE)
        ) %>%
        leaflet::addLegend(layerId = "pa_legend", colors = "#a1d76a",
                           labels = "Existing Protected Areas", title = "Protected Areas")
    })

    #### Show / hide candidate protected areas ####
    observeEvent(input$pa_candidates_show, {
      map <- leaflet::leafletProxy(mapId = map_id, session = parent_session)

      if (!isTRUE(input$pa_candidates_show)) {
        map %>%
          leaflet::clearGroup("candidate_pa") %>%
          leaflet::removeControl(layerId = "candidate_pa_legend")
        return()
      }

      map %>%
        leaflet::addPolygons(
          data = bowen_new_pa,
          group = "candidate_pa",
          fillColor = "orange",
          color = "lightgrey",
          fillOpacity = 0.8,
          smoothFactor = 0.2,
          label = ~lapply(
            paste0("<div style='color:orange; font-size:14px;'>",
                   "<b>", name, "</b><br/>",
                   "<span style='color:gray;'>Type: Proposed</span>",
                   "</div>"),
            HTML
          ),
          highlightOptions = leaflet::highlightOptions(bringToFront = TRUE)
        ) %>%
        leaflet::addLegend(layerId = "candidate_pa_legend", colors = "orange",
                           labels = "Candidate Protected Areas", title = "Candidates")
    })

  })
}

## To be copied in the UI
# mod_protected_areas_ui("protected_areas_1")

## To be copied in the server
# mod_protected_areas_server("protected_areas_1")
