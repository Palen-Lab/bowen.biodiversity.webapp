#' overlay UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_overlay_ui <- function(id) {
  tabPanel(
    "overlay_panel",
    bslib::card(
      bslib::card_body(
        tagList(
          h1("Overlay with Conservation Values"),
          util_ui_simple_legend_element(label = "Relatively Higher Value", colour = viridis::viridis(2)[1]),
          util_ui_simple_legend_element(label = "Relatively Lower Value", colour = viridis::viridis(2)[2]),
          sliderInput(NS(id, "top_pct_slider"), label = "Top % Values", min = 10, max = 100, value = 100, step = 5),
          selectInput(
            NS(id, "selectGroup"),
            label = "Select Layer",
            choices = c("Choose Layer", "Land Use", "Habitats", "Threats")
          ),
          htmlOutput(NS(id, "sidebarInfo")),
          htmlOutput(NS(id, "specific_sidebarInfo"))
        )
      )
    ),
    docs_link
  )
}

#' overlay Server Functions
#'
#' @noRd
mod_overlay_server <- function(id, map_id, parent_session){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    #### Load Layers ####
    # Have both project_crs and 4326 objects, since leaflet cannot visualize non-longlat coordinates.
    # For area queries, still need to use the project_crs with metres units.
    # Load Zonation Output Raster
    zonation_og <- terra::rast(here::here("inst/extdata/5_values/rankmap.tif"))
    zonation <- zonation_og %>%
      terra::project("epsg:4326")
    # Load Private Land - vector
    bowen_pm <- privateland %>%
      sf::st_transform(4326)
    # Load Protected Areas - vector
    bowen_pa <- here::here("inst/extdata/7_protected_areas/existing_protected_areas.gpkg") %>%
      sf::st_read() %>%
      sf::st_transform(4326) %>%
      sf::st_make_valid() %>%
      sf::st_union()
    # Load Unprotected Crown Land - vector
    bowen_uc <- unprotected_crown %>%
      sf::st_transform(4326)
    # Load Freshwater Habitats - raster
    # Load Terrestrial Habitats - raster
    # Load Development Potential - vector
    # Load Wildfire - raster

    # Initial Leaflet Map
    zonation_colour_ramp <- viridis::viridis(100)[0:100]
    # zonation_colour_ramp <- viridis::viridis(100)
    zonation_pal <- leaflet::colorNumeric(
      palette = colorRamp(colors = zonation_colour_ramp),
      domain = c(0, 1),
      na.color = "transparent",
      reverse = TRUE
    )
    map <- leaflet::leafletProxy(mapId = map_id,
                          session = parent_session) %>%
      leaflet::clearControls() %>%
      leaflet::clearImages() %>%
      leaflet::clearGroup(group = "clear_each_update") %>%
      leaflet::addRasterImage(
        x = zonation,
        layerId = "zonation_raster",
        colors = zonation_pal
      ) %>%
      leaflet::addLegend(
        pal = zonation_pal,
        layerId = "zonation_legend",
        values =  c(0,1),
        title = "Rel. Conservation Value",
      )
    #### Update map each time slider is updated ####
    observeEvent(list(input$top_pct_slider, input$selectGroup), {
      top_pct <- input$top_pct_slider
      zonation_colour_ramp <- viridis::viridis(100)[0:top_pct]
      # zonation_colour_ramp <- viridis::viridis(100)
      zonation_pal <- leaflet::colorNumeric(
        palette = colorRamp(colors = zonation_colour_ramp),
        domain = c((1 - top_pct/100), 1),
        na.color = "transparent",
        reverse = TRUE
      )
      map %>%
        leaflet::clearImages() %>%
        leaflet::addRasterImage(
          x = zonation,
          layerId = "zonation_raster",
          colors = zonation_pal
        )

      if(input$selectGroup == "Land Use") {
        # Remove pixels below quantile, get mask
        top_pct_zonation <- remove_by_quantile(zonation_og, (1-top_pct/100)) %>%
          terra::not.na(falseNA=T)
        # Number of total cells in top_pct
        top_pct_zonation_count <- top_pct_zonation %>%
          terra::values() %>%
          sum(na.rm = T)
        # Number of cells within protected area vector
        pa_vect <- bowen_pa %>%
          terra::vect() %>%
          terra::project(top_pct_zonation)
        top_pct_zonation_pa_count <- top_pct_zonation %>%
          terra::mask(pa_vect) %>%
          terra::values() %>%
          sum(na.rm = T)

        top_pct_in_pa <-
        output$sidebarInfo <- renderUI(
          tagList(
            p(top_pct),
            p("Top Pixels Total: ", top_pct_zonation_count),
            p("Top Pixels in Protected Areas: ", top_pct_zonation_pa_count)
          )
        )
      }

      # TODO: Update overlap calculation for each selectGroup
    })

    # Create hatch
    # TODO: may need to create these objects ahead of time, time consuming to generate
    bowen_pa_hatch <- hatched.sf(bowen_pa, density = 3)
    bowen_pm_hatch <- hatched.sf(bowen_pm, density = 3)
    bowen_uc_hatch <- hatched.sf(bowen_uc, density = 3)

    #### Update map based on selectGroup ####
    observeEvent(input$selectGroup, {
      if(input$selectGroup == "Choose Layer") {
        map %>%
          leaflet::clearGroup(
            "overlay_polygons"
          )
      }
      else if (input$selectGroup == "Land Use") {
        strokeWeight <- 2
        map %>%
          leaflet::clearGroup(
            "overlay_polygons"
          ) %>%
          leaflet::addPolygons(
            data = bowen_pa,
            group = "overlay_polygons",
            color = "#a1d76a",
            fill = F,
            opacity = 1,
            weight = strokeWeight
          ) %>%
          leaflet::addPolylines(
            data = bowen_pa_hatch,
            group = "overlay_polygons",
            color = "#a1d76a",
            fill = F,
            opacity = 1,
            weight = strokeWeight
          ) %>%
          leaflet:: addPolygons(
            data = bowen_pm,
            group = "overlay_polygons",
            color = "brown",
            fill = F,
            opacity = 1,
            weight = strokeWeight
          ) %>%
          leaflet::addPolylines(
            data = bowen_pm_hatch,
            group = "overlay_polygons",
            color = "brown",
            fill = F,
            opacity = 1,
            weight = strokeWeight
          ) %>%
          leaflet::addPolygons(
            data = bowen_uc,
            group = "overlay_polygons",
            color = "darkgreen",
            fill = F,
            opacity = 1,
            weight = strokeWeight
          ) %>%
          leaflet::addPolylines(
            data = bowen_uc_hatch,
            group = "overlay_polygons",
            color = "darkgreen",
            fill = F,
            opacity = 1,
            weight = strokeWeight
          )
      }
    })
  })
}

## To be copied in the UI
# mod_overlay_ui("values_1")

## To be copied in the server
# mod_overlay_server("values_1")



