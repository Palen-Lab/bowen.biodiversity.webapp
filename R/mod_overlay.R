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
          sliderInput(NS(id, "top_pct_slider"), label = "Top % Values", min = 10, max = 100, value = 100, step = 5),
          selectInput(
            NS(id, "selectGroup"),
            label = "Select Category",
            choices = c("Choose Category", "Land Use", "Habitats", "Threats")
          ),
          htmlOutput(NS(id, "sidebarInfo")),
          DT::DTOutput(NS(id, "sidebarTable")),
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

    # Use just for visualizing, use zonation_og for calculations
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
    bowen_fw <- terra::rast(here::here("inst/extdata/3_habitats/fw_richness.tif"))
    # Load Terrestrial Habitats - raster
    # Load Development Potential - vector
    # Load Wildfire - raster

    # Initial Leaflet Map
    # Generate Zonation palette
    # zonation_colour_vect <- viridis::viridis(100) # Old colour scheme
    # zonation_colour_func <- colorRampPalette(c('#ffffff','#f0f0f0','#d9d9d9','#bdbdbd','#969696','#737373','#525252','#252525','#000000'))
    # zonation_colour_func <- colorRampPalette(c('#fff7fb','#ece2f0','#d0d1e6','#a6bddb','#67a9cf','#3690c0','#02818a','#016c59','#014636'))
    zonation_colour_func <- colorRampPalette(c('#a1d99b'))
    zonation_colour_vect <- zonation_colour_func(100) %>% rev()
    zonation_colour_ramp <- zonation_colour_vect[0:100]
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
      zonation_colour_ramp <- zonation_colour_vect[0:top_pct]
      # zonation_colour_ramp <- viridis::viridis(100)
      zonation_pal <- leaflet::colorNumeric(
        palette = colorRamp(colors = zonation_colour_ramp),
        domain = c((1 - top_pct/100), 1),
        na.color = "transparent",
        reverse = TRUE
      )

      # Removing layers when navigating off the page
      if(input$selectGroup != "Land Use") {
        map %>%
          leaflet::removeShape("bowen_pa") %>%
          leaflet::removeShape("bowen_pa_hatch") %>%
          leaflet::removeShape("bowen_pm") %>%
          leaflet::removeShape("bowen_pm_hatch") %>%
          leaflet::removeShape("bowen_uc") %>%
          leaflet::removeShape("bowen_uc_hatch")
      }
      if(input$selectGroup != "Habitats") {
        # Update Leaflet Map
        map %>%
          leaflet::removeShape("top_pct_zonation_vect")
      }

      if(input$selectGroup == "Choose Category") {
        output$sidebarInfo <- renderUI("Choose category from the above selection.")
        output$sidebarTable <- DT::renderDT(NULL)
        # Update Leaflet Map
        map %>%
          leaflet::clearImages() %>%
          leaflet::addRasterImage(
            x = zonation,
            layerId = "zonation_raster",
            colors = zonation_pal
          )
      }
      else if(input$selectGroup == "Land Use") {
        # Disaggregate to get closer overlap area estimates later
        zonation_d <- zonation_og %>%
          terra::disagg(2)
        # Remove pixels below quantile, get mask
        top_pct_zonation <- remove_by_quantile(zonation_d, (1-top_pct/100)) %>%
          terra::not.na(falseNA=T)
        # Number of total cells with zonation vals
        zonation_count <- zonation_d %>%
          terra::not.na(falseNA=T) %>%
          terra::values() %>%
          sum(na.rm = T)

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
        # Number of cells within unprotected crown land vector
        uc_vect <- bowen_uc %>%
          terra::vect() %>%
          terra::project(top_pct_zonation)
        top_pct_zonation_uc_count <- top_pct_zonation %>%
          terra::mask(uc_vect) %>%
          terra::values() %>%
          sum(na.rm = T)
        # Number of cells within private land vector
        pm_vect <- bowen_pm %>%
          terra::vect() %>%
          terra::project(top_pct_zonation)
        top_pct_zonation_pm_count <- top_pct_zonation %>%
          terra::mask(pm_vect) %>%
          terra::values() %>%
          sum(na.rm = T)

        # Create table to render
        sidebarTable <- data.frame(type = paste0("Top ", top_pct,"%"), ncells = top_pct_zonation_count) %>%
          rbind(data.frame(type = "Protected Area", ncells = top_pct_zonation_pa_count)) %>%
          rbind(data.frame(type = "Unprotected Crown Land", ncells = top_pct_zonation_uc_count)) %>%
          rbind(data.frame(type = "Private Land", ncells = top_pct_zonation_pm_count))
        sidebarTable$area_ha <- sidebarTable$ncells / 100
        sidebarTable$pct_prop <- round(sidebarTable$ncells / top_pct_zonation_count * 100, digits = 1)
        sidebarTable$pct_total <- round(sidebarTable$ncells / zonation_count * 100, digits = 1)
        # Render DT
        output$sidebarInfo <- renderUI({
          "The percentages may not add up exactly to the selected %, due to some top % not falling on lands in these categories."
        })
        output$sidebarTable <- DT::renderDT(
          DT::datatable(sidebarTable[, c("type", "area_ha", "pct_prop", "pct_total")], options = list(
            paging = F,
            searching = F,
            ordering = F,
            lengthChange = F,
            info = F
          ),
          rownames = FALSE,
          colnames = c("Overlap", "Area (ha)", "% of Top Values", "% of Bowen Island")
          )
        )

        # Update Leaflet Map
        map %>%
          leaflet::clearImages() %>%
          leaflet::addRasterImage(
            x = zonation,
            layerId = "zonation_raster",
            colors = zonation_pal
          )
      }
      else if(input$selectGroup == "Habitats") {
        # Remove pixels below quantile, get mask
        top_pct_zonation <- remove_by_quantile(zonation_og, (1-top_pct/100)) %>%
          terra::not.na(falseNA=T)
          # terra::is.na()
        top_pct_zonation_vect <- terra::ifel(is.na(top_pct_zonation), 1, NA) %>%
          terra::extend(c(1000, 1000, 1000, 1000), fill = 1) %>%
          terra::as.polygons(dissolve = T) %>%
          sf::st_as_sf() %>%
          sf::st_transform(4326)
        # Overlap between top % and freshwater habitats
        # Overlap between top % and terrestrial habitats

        # Update Leaflet Map
        map %>%
          leaflet::addPolygons(
            layerId = "top_pct_zonation_vect",
            group = "overlap_polygons",
            data = top_pct_zonation_vect,
            weight = 2,
            color = "grey",
            fillColor = "grey",
            opacity = 0.8,
            fillOpacity = 0.6
          )
      }
      else if(input$selectGroup == "Threats") {

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
      if(input$selectGroup == "Choose Category") {
        map %>%
          leaflet::clearGroup(
            "overlay_polygons"
          )
      }
      else if (input$selectGroup == "Land Use") {
        strokeWeight <- 2
        bowen_pa_col <- "#377eb8"
        bowen_uc_col <- "#984ea3"
        bowen_pm_col <- "#ff7f00"
        map %>%
          leaflet::clearGroup(
            "overlay_polygons"
          ) %>%
          leaflet::addPolygons(
            data = bowen_pa,
            layerId = "bowen_pa",
            group = "overlay_polygons",
            color = bowen_pa_col,
            fill = F,
            opacity = 1,
            weight = strokeWeight
          ) %>%
          leaflet::addPolylines(
            data = bowen_pa_hatch,
            layerId = "bowen_pa_hatch",
            group = "overlay_polygons",
            color = bowen_pa_col,
            fill = F,
            opacity = 1,
            weight = strokeWeight
          ) %>%
          leaflet:: addPolygons(
            data = bowen_pm,
            layerId = "bowen_pm",
            group = "overlay_polygons",
            color = bowen_pm_col,
            fill = F,
            opacity = 1,
            weight = strokeWeight
          ) %>%
          leaflet::addPolylines(
            data = bowen_pm_hatch,
            layerId = "bowen_pm_hatch",
            group = "overlay_polygons",
            color = bowen_pm_col,
            fill = F,
            opacity = 1,
            weight = strokeWeight
          ) %>%
          leaflet::addPolygons(
            data = bowen_uc,
            layerId = "bowen_uc",
            group = "overlay_polygons",
            color = bowen_uc_col,
            fill = F,
            opacity = 1,
            weight = strokeWeight
          ) %>%
          leaflet::addPolylines(
            data = bowen_uc_hatch,
            layerId = "bowen_uc_hatch",
            group = "overlay_polygons",
            color = bowen_uc_col,
            fill = F,
            opacity = 1,
            weight = strokeWeight
          )
      }
      else if(input$selectGroup == "Habitats") {
        # Project raster to use with Leaflet
        bowen_fw_p <- bowen_fw %>% terra::project("epsg:3857", method = "near")
        # Leaflet parameters
        raster_domain <- terra::values(bowen_fw_p) %>%
          unique()
        raster_labels <- raster_domain
        raster_colour_ramp <- viridis::mako(5, end = 0.8, direction = -1)
        raster_pal <- leaflet::colorNumeric(
          raster_colour_ramp,
          raster_domain,
          na.color = "transparent"
        )
        # Update Leaflet
        map %>%
          leaflet::clearGroup(
            "overlay_polygons"
          ) %>%
          leaflet::clearImages() %>%
          leaflet::clearControls() %>%
          leaflet::addRasterImage(
            x = bowen_fw_p,
            colors = raster_pal
          )
      }
    })
  })
}

## To be copied in the UI
# mod_overlay_ui("values_1")

## To be copied in the server
# mod_overlay_server("values_1")



