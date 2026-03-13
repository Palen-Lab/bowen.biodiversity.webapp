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
    tags$p(tags$strong("Land Ownership"), class = "mb-0 mt-2"),
    div(class = "d-flex align-items-center gap-2",
      checkboxInput(NS(id, "ownership_rast_show"), "Land Ownership Raster", value = FALSE),
      hover_popover(
        icon("circle-info", style = "cursor:pointer;"),
        title = "Land Ownership Map",
        p("Shows land ownership categories — Private, Protected, Crown (Public), and Mixed — masked to the top percentage of biodiversity values."),
        p("Use the slider to focus on areas with the highest conservation value.")
      )
    ),
    sliderInput(NS(id, "ownership_top_pct"), label = "Top % Biodiversity Values",
                min = 10, max = 100, value = 100, step = 5),
    div(class = "d-flex align-items-center gap-2",
      checkboxInput(NS(id, "privateland_show"), "Private Land", value = FALSE),
      hover_popover(
        icon("circle-info", style = "cursor:pointer;"),
        title = "Private Land",
        p("Privately owned parcels on Bowen Island, excluding conservancies and protected areas.")
      )
    ),
    div(class = "d-flex align-items-center gap-2",
      checkboxInput(NS(id, "crown_show"), "Crown (Public) Land", value = FALSE),
      hover_popover(
        icon("circle-info", style = "cursor:pointer;"),
        title = "Crown (Public) Land",
        p("Unprotected Crown land on Bowen Island — publicly owned land not yet designated as a protected area.")
      )
    ),
    tags$p(tags$strong("Protected Areas"), class = "mb-0 mt-2"),
    div(class = "d-flex align-items-center gap-2",
      checkboxInput(NS(id, "pa_show"), "Existing Protected Areas", value = FALSE),
      hover_popover(
        icon("circle-info", style = "cursor:pointer;"),
        title = "Existing Protected Areas",
        p("Expanding and connecting protected areas on Bowen Island would help preserve rare ecosystems, maintain wildlife corridors, and advance Canada's 30 by 30 commitment to protect 30% of lands and waters by 2030."),
        p("Biodiversity protection on Bowen Island also aligns with the conservation vision of the Átl'ka7tsem/Howe Sound UNESCO Biosphere Region and the Preserve and Protect Mandate of the Islands Trust.")
      )
    ),
    div(class = "d-flex align-items-center gap-2",
      checkboxInput(NS(id, "pa_candidates_show"), "Candidate Protected Areas", value = FALSE),
      hover_popover(
        icon("circle-info", style = "cursor:pointer;"),
        title = "Candidate Protected Areas",
        p("These are high-priority candidates for protection identified through the Bowen Island Biodiversity Plan, based on their integrated conservation values, connectivity potential, and land ownership.")
      )
    ),
    tags$p(tags$strong("Development"), class = "mb-0 mt-2"),
    div(class = "d-flex align-items-center gap-2",
      checkboxInput(NS(id, "dev_potential"), "Development Potential", value = FALSE),
      hover_popover(
        icon("circle-info", style = "cursor:pointer;"),
        title = "Development Potential",
        p("Shows where land on Bowen Island has potential for subdivision and additional development, quantified as the number of potential new units per parcel."),
        em(strong("Note: "), "Shows only parcels identified as having potential for future development or densification.")
      )
    ),
    div(class = "d-flex align-items-center gap-2",
      checkboxInput(NS(id, "dev_bio"), "Development and Biodiversity", value = FALSE),
      hover_popover(
        icon("circle-info", style = "cursor:pointer;"),
        title = "Development and Biodiversity",
        p("Development pressure overlaid with Integrated Conservation Values, showing the relative biodiversity value at risk per potential new unit. Higher values indicate development potential in areas of greater conservation significance."),
        em(strong("Note: "), "Shows only parcels identified as having potential for future development or densification.")
      )
    )
  )
}

#' land_management Server Functions
#'
#' @noRd
mod_land_management_server <- function(id, map_id, parent_session, active_raster = NULL){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    #### Load raster layers ####
    land_ownership_rast <- rast_layer("7_land_management/land_ownership_rast.tif") %>%
      terra::as.factor() %>%
      terra::project("EPSG:4326", method = "near")
    rankmap_matched     <- rast_layer("5_values/rankmap.tif") %>%
      terra::project("EPSG:4326", method = "bilinear")

    ownership_pal <- leaflet::colorFactor(
      palette   = c("brown", "purple", "lightgreen", "blue"),
      levels    = 1:4,
      na.color  = "transparent"
    )

    #### Cross-module raster exclusivity ####
    observe({
      if (isTRUE(input$ownership_rast_show)) active_raster("land_ownership")
    })
    observeEvent(active_raster(), {
      if (!is.null(active_raster()) && active_raster() != "land_ownership") {
        updateCheckboxInput(session, "ownership_rast_show", value = FALSE)
      }
    }, ignoreInit = TRUE)

    #### Land ownership raster: update on checkbox or slider change ####
    observe({
      map <- leaflet::leafletProxy(mapId = map_id, session = parent_session)

      if (!isTRUE(input$ownership_rast_show)) {
        map %>%
          leaflet::removeImage(layerId = "ownership_raster") %>%
          leaflet::removeControl(layerId = "ownership_raster_legend")
        return()
      }

      top_pct   <- input$ownership_top_pct
      threshold <- 1 - top_pct / 100
      masked    <- terra::mask(land_ownership_rast, rankmap_matched >= threshold, maskvalues = 0)

      map %>%
        leaflet::removeImage(layerId = "ownership_raster") %>%
        leaflet::removeControl(layerId = "ownership_raster_legend") %>%
        leaflet::addRasterImage(x = masked, layerId = "ownership_raster",
                                colors = ownership_pal, opacity = 0.3) %>%
        leaflet::addLegend(
          layerId = "ownership_raster_legend",
          colors  = c("brown", "purple", "lightgreen", "blue"),
          labels  = c("Private", "Protected", "Public", "Mixed"),
          title   = "Land Ownership",
          opacity = 0.3
        )
    })

    #### Load vector layers ####
    privateland_layer <- vect_layer("7_land_management/privateland.gpkg") %>%
      sf::st_transform(4326)
    crown_layer <- vect_layer("7_land_management/unprotected_crown.gpkg") %>%
      sf::st_transform(4326)
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

    #### Show / hide private land ####
    observeEvent(input$privateland_show, {
      map <- leaflet::leafletProxy(mapId = map_id, session = parent_session)

      if (!isTRUE(input$privateland_show)) {
        map %>%
          leaflet::clearGroup("privateland") %>%
          leaflet::removeControl(layerId = "privateland_legend")
        return()
      }

      map %>%
        leaflet::addPolygons(
          data = privateland_layer,
          group = "privateland",
          fillColor = "brown",
          color = "brown",
          weight = 1,
          fillOpacity = 0.4,
          smoothFactor = 0.2,
          label = "Private Land",
          highlightOptions = leaflet::highlightOptions(color = "brown", weight = 2, bringToFront = FALSE)
        ) %>%
        leaflet::addLegend(layerId = "privateland_legend", colors = "brown",
                           labels = "Private Land", title = "Land Ownership")
    })

    #### Show / hide crown land ####
    observeEvent(input$crown_show, {
      map <- leaflet::leafletProxy(mapId = map_id, session = parent_session)

      if (!isTRUE(input$crown_show)) {
        map %>%
          leaflet::clearGroup("crown") %>%
          leaflet::removeControl(layerId = "crown_legend")
        return()
      }

      map %>%
        leaflet::addPolygons(
          data = crown_layer,
          group = "crown",
          fillColor = "green",
          color = "green",
          weight = 1,
          fillOpacity = 0.4,
          smoothFactor = 0.2,
          label = ~lapply(
            paste0("<div style='color:darkgreen; font-size:14px;'>",
                   "<b>", name, "</b><br/>",
                   "<span style='color:gray;'>", type, "</span>",
                   "</div>"),
            HTML
          ),
          highlightOptions = leaflet::highlightOptions(color = "green", weight = 2, bringToFront = FALSE)
        ) %>%
        leaflet::addLegend(layerId = "crown_legend", colors = "green",
                           labels = "Crown (Public) Land", title = "Land Ownership")
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
