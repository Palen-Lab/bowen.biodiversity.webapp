#' land_management UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_land_management_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$p(tags$strong("Protected Areas"), class = "mb-0 mt-2"),
    div(class = "d-flex align-items-center gap-2",
      checkboxInput(NS(id, "pa_show"), "Existing Protected Areas", value = FALSE),
      hover_popover(
        icon("circle-info", style = "cursor:pointer;"),
        title = "Existing Protected Areas",
        p("Expanding and connecting protected areas on Bowen Island would help preserve rare ecosystems, maintain wildlife corridors, and contribute to Canada's 30 by 30 biodiversity targets."),
        p("Canada's 30 by 30 target commits to protecting 30% of the country's lands and waters by 2030, aimed at conserving biodiversity, mitigating climate change, and safeguarding ecosystem services.")
      )
    ),
    div(class = "d-flex align-items-center gap-2",
      checkboxInput(NS(id, "pa_candidates_show"), "Candidate Protected Areas", value = FALSE),
      hover_popover(
        icon("circle-info", style = "cursor:pointer;"),
        title = "Candidate Protected Areas",
        p("These are areas identified as high-priority candidates for protection based on their biodiversity values and connectivity potential.")
      )
    ),
    tags$p(tags$strong("Development"), class = "mb-0 mt-2"),
    div(class = "d-flex align-items-center gap-2",
      checkboxInput(NS(id, "dev_potential"), "Development Potential", value = FALSE),
      hover_popover(
        icon("circle-info", style = "cursor:pointer;"),
        title = "Development Potential",
        p("Quantifies where land on Bowen Island can be subdivided to allow more buildings."),
        em(strong("Note: "), "Shows only parcels identified as having potential for future development / densification.")
      )
    ),
    div(class = "d-flex align-items-center gap-2",
      checkboxInput(NS(id, "dev_bio"), "Development and Biodiversity", value = FALSE),
      hover_popover(
        icon("circle-info", style = "cursor:pointer;"),
        title = "Development and Biodiversity",
        p("Development potential compared to Conservation Values, showing the biodiversity value at risk per potential unit."),
        em(strong("Note: "), "Shows only parcels identified as having potential for future development / densification.")
      )
    )
  )
}

#' land_management Server Functions
#'
#' @noRd
mod_land_management_server <- function(id, map_id, parent_session){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    #### Load vector layers ####
    dev_layer <- vect_layer("7_land_management/biod_val_parcel.gpkg") %>%
      dplyr::filter(subdividable) %>%
      dplyr::mutate(
        potential_units = `Can Subdivide?`,
        bioval_per_unit = rankmap / potential_units
      ) %>%
      sf::st_transform(4326)
    bowen_pa <- vect_layer("7_land_management/existing_protected_areas.gpkg") %>%
      sf::st_transform(4326)
    bowen_new_pa <- vect_layer("7_land_management/pa_candidates.gpkg") %>%
      sf::st_transform(4326)

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

    #### Development vector layers (independent toggles) ####
    observeEvent(input$dev_potential, {
      map <- leaflet::leafletProxy(mapId = map_id, session = parent_session)
      if (!isTRUE(input$dev_potential)) {
        map %>%
          leaflet::clearGroup("dev_potential") %>%
          leaflet::removeControl(layerId = "dev_potential_legend")
        return()
      }
      layer_domain <- c(0, dev_layer$potential_units)
      pal <- leaflet::colorNumeric(palette = "YlOrRd", domain = layer_domain)
      map %>%
        leaflet::addPolygons(data = dev_layer, group = "dev_potential",
                             color = ~pal(potential_units), stroke = FALSE,
                             fillOpacity = 1, smoothFactor = 0.2) %>%
        leaflet::addLegend(layerId = "dev_potential_legend", pal = pal, values = layer_domain,
                           title = "Potential Units", opacity = 0.7)
    })

    observeEvent(input$dev_bio, {
      map <- leaflet::leafletProxy(mapId = map_id, session = parent_session)
      if (!isTRUE(input$dev_bio)) {
        map %>%
          leaflet::clearGroup("dev_bio") %>%
          leaflet::removeControl(layerId = "dev_bio_legend")
        return()
      }
      layer_domain <- c(0, dev_layer$bioval_per_unit)
      pal <- leaflet::colorNumeric(palette = "YlOrRd", domain = layer_domain)
      map %>%
        leaflet::addPolygons(data = dev_layer, group = "dev_bio",
                             color = ~pal(bioval_per_unit), stroke = FALSE,
                             fillOpacity = 1, smoothFactor = 0.2) %>%
        leaflet::addLegend(layerId = "dev_bio_legend", pal = pal, values = layer_domain,
                           title = "Rel. Biodiversity per Potential Unit", opacity = 0.7)
    })

  })
}

## To be copied in the UI
# mod_land_management_ui("land_management_1")

## To be copied in the server
# mod_land_management_server("land_management_1")
