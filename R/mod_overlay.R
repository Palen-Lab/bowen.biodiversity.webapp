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
          selectInput(
            NS(id, "selectGroup"),
            label = "Select Category",
            choices = c(
              "Choose Category",
              "Land Use",
              "Human Disturbance",
              "Habitats - Freshwater",
              "Habitats - Terrestrial",
              "Threats - Development",
              "Threats - Wildfire")
          ),
          p("This tool is for overlaying various layers together, and getting summaries of what is inside and outside of the overlapping areas."),
          sliderInput(NS(id, "top_pct_slider"), label = "Top % Values", min = 10, max = 100, value = 100, step = 5),
          htmlOutput(NS(id, "sidebarInfo")),
          htmlOutput(NS(id, "specific_sidebarInfo")),
          DT::DTOutput(NS(id, "sidebarTable")),
        )
      )
    ),
    docs_link()
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
    # Number of total cells with zonation vals
    zonation_count <- zonation_og %>%
      terra::not.na(falseNA=T) %>%
      terra::values() %>%
      sum(na.rm = T)

    ## Land Use
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
      sf::st_transform(4326) %>%
      sf::st_cast("MULTIPOLYGON") %>%
      sf::st_union()

    ## Human Disturbance
    bowen_hd <- terra::rast(here::here("inst/extdata/4_people/bowen_human_footprint_recl.tif"))
    bowen_hd_p <- bowen_hd %>%
      terra::project("epsg:3857", method = "near")

    ## Habitats
    # Load Freshwater Habitats - raster
    bowen_fw <- terra::rast(here::here("inst/extdata/3_habitats/fw_richness.tif"))
    bowen_fw_p <- bowen_fw %>% terra::project("epsg:4326", method = "near") # Project raster to use with Leaflet
    # Load Terrestrial Habitats - raster
    # TODO: replace with terrestrial only richness raster
    bowen_te <- terra::rast(here::here("inst/extdata/3_habitats/total_habitat_richness.tif"))
    bowen_te_p <- bowen_te %>% terra::project("epsg:4326", method = "near") # Project raster to use with Leaflet

    ## Threats - Development
    # Load Development Potential - vector
    bowen_dp <- sf::st_read(here::here("inst/extdata/6_threats/development_potential.gpkg")) %>%
      sf::st_transform(4326)
    # Development Potential - raster from vector
    bowen_dp_r <- bowen_dp %>%
      terra::vect() %>%
      terra::rasterize(zonation, cover=T)

    ## Threats - Wildfire
    # Load Wildfire - raster
    bowen_wf <- terra::rast(here::here("inst/extdata/6_threats/fire_index_40m.tif")) %>%
      terra::project("epsg:3857", method = "near")
    terra::NAflag(bowen_wf) <- 4294967296

    #### Initial Leaflet Map ####
    # Generate Zonation palette
    # zonation_colour_vect <- viridis::viridis(100) # Old colour scheme
    zonation_colour_func <- colorRampPalette(c('#a1d99b'))
    zonation_colour_vect <- zonation_colour_func(100) %>% rev()
    zonation_colour_ramp <- zonation_colour_vect[0:100]
    zonation_pal <- leaflet::colorNumeric(
      palette = colorRamp(colors = zonation_colour_ramp),
      domain = c(0, 1),
      na.color = "transparent",
      reverse = TRUE
    )
    # Create Leaflet
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
    #### Update each time slider is changed and selected category changes ####
    observeEvent(list(input$top_pct_slider, input$selectGroup), {
      top_pct <- input$top_pct_slider
      # Remove pixels below quantile, get mask
      top_pct_zonation <- remove_by_quantile(zonation_og, (1-top_pct/100)) %>%
        terra::not.na(falseNA=T)

      # Update Zonation Colour Ramp
      # Not necessary now, just needed this for dealing with gradients before
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
      # if(input$selectGroup != "Habitats - Freshwater" | input$selectGroup != "Habitats - Terrestrial") {
      #   # Update Leaflet Map
      #   map %>%
      #     leaflet::removeShape("top_pct_zonation_vect")
      #   # Update sidebarInfo
      # }

      if(input$selectGroup == "Choose Category") {
        # Update sidebar
        output$sidebarInfo <- renderUI("Choose category from the above selection.")
        output$sidebarTable <- DT::renderDT(NULL)
        # Update Leaflet Map
        map %>%
          leaflet::clearImages() %>%
          leaflet::removeShape("top_pct_zonation_vect") %>%
          leaflet::addRasterImage(
            x = zonation,
            layerId = "zonation_raster",
            colors = zonation_pal
          )
      }
      else if (input$selectGroup == "Land Use") {
        # Disaggregate to get closer overlap area estimates later
        zonation_d <- zonation_og %>%
          terra::disagg(2)
        # # Remove pixels below quantile, get mask
        # top_pct_zonation <- remove_by_quantile(zonation_d, (1-top_pct/100)) %>%
        #   terra::not.na(falseNA=T)
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
        sidebarTable$area_ha <- sidebarTable$ncells / 25
        sidebarTable$pct_prop <- round(sidebarTable$ncells / top_pct_zonation_count * 100, digits = 1)
        sidebarTable$pct_total <- round(sidebarTable$ncells / zonation_count * 100, digits = 1)

        # Update sidebar
        output$sidebarInfo <- renderUI({
          tagList(
            h2("Land Use"),
            p("The percentages may not add up exactly to the selected %, due to some top % not falling on lands in these categories.")
          )
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
          leaflet::removeShape("top_pct_zonation_vect") %>%
          leaflet::addRasterImage(
            x = zonation,
            layerId = "zonation_raster",
            colors = zonation_pal
          )
      }
      else if (input$selectGroup == "Human Disturbance") {
        # Apply grey out effect and update map
        top_pct_zonation_vect(map, top_pct_zonation, top_pct)
        # Overlap values
        bowen_hd_pz <- bowen_hd %>% terra::project(zonation_og)
        res <- values_in_out(zonation_og, top_pct_zonation, bowen_hd_pz)
        inside_mean <- res$inside_vals %>% mean()
        outside_mean <- res$outside_vals %>% mean()

        inside_high_count <- res$inside_vals[res$inside_vals == 3] %>%
          length()
        inside_medium_count <- res$inside_vals[res$inside_vals == 2] %>%
          length()
        inside_low_count <- res$inside_vals[res$inside_vals == 1] %>%
          length()
        outside_high_count <- res$outside_vals[res$outside_vals == 3] %>%
          length()
        outside_medium_count <- res$outside_vals[res$outside_vals == 2] %>%
          length()
        outside_low_count <- res$outside_vals[res$outside_vals == 1] %>%
          length()

        # Update sidebar
        sidebarTable <- data.frame(
          type = c("Inside", "Outside"),
          mean = c(round(inside_mean, 2), round(outside_mean, 2)),
          high = c(inside_high_count, outside_high_count),
          medium = c(inside_medium_count, outside_medium_count),
          low = c(inside_low_count, outside_low_count)
        )
        output$sidebarInfo <- renderUI({
          tagList(
            h2("Human Disturbance")
          )
        })
        output$sidebarTable <- DT::renderDT(
          DT::datatable(sidebarTable[,c("type", "high", "medium", "low")], options = list(
            paging = F,
            searching = F,
            ordering = F,
            lengthChange = F,
            info = F
          ),
          rownames = FALSE,
          colnames = c("High (ha)", "Medium (ha)", "Low (ha)")
          )
        )
      }
      else if (input$selectGroup == "Habitats - Freshwater") {
        # Apply grey out effect and update map
        top_pct_zonation_vect(map, top_pct_zonation, top_pct)

        # Overlap values
        bowen_hd_pz <- bowen_hd %>% terra::project(zonation_og)
        res <- values_in_out(zonation_og, top_pct_zonation, bowen_hd_pz)
        inside_sum <- res$inside_vals %>% sum()
        outside_sum <- res$outside_vals %>% sum()
        total_sum <- inside_sum + outside_sum

        # Update Text and Table
        sidebarTable <- data.frame(
          type = c("Inside", "Outside"),
          ncells = c(inside_sum, (total_sum - inside_sum))
        ) %>%
          dplyr::mutate(area_ha = ncells / 10) %>%
          dplyr::mutate(pct = round(ncells / total_sum * 100))

        output$sidebarInfo <- renderUI({
          tagList(
            h2("Freshwater Habitat Richness")
          )
        })
        output$sidebarTable <- DT::renderDT(
          DT::datatable(sidebarTable[,c("type", "area_ha", "pct")], options = list(
            paging = F,
            searching = F,
            ordering = F,
            lengthChange = F,
            info = F
          ),
          rownames = FALSE,
          colnames = c("Area (ha)", "% of Habitat Covered")
          )
        )
      }
      else if (input$selectGroup == "Habitats - Terrestrial") {
        # Apply grey out effect and update map
        top_pct_zonation_vect(map, top_pct_zonation, top_pct)

        # Update Text and Table
        # Overlap values
        bowen_te_pz <- bowen_te_p %>% terra::project(zonation_og)
        res <- values_in_out(zonation_og, top_pct_zonation, bowen_te_pz)
        inside_mean <- res$inside_vals %>% mean()
        outside_mean <- res$outside_vals %>% mean()

        sidebarTable <- data.frame(
          type = c("Inside", "Outside"),
          mean = c(round(inside_mean, 2), round(outside_mean, 2))
        )
        output$sidebarInfo <- renderUI({
          tagList(
            h2("Habitat Richness")
          )
        })
        output$sidebarTable <- DT::renderDT(
          DT::datatable(sidebarTable[,c("type", "mean")], options = list(
            paging = F,
            searching = F,
            ordering = F,
            lengthChange = F,
            info = F
          ),
          rownames = FALSE,
          colnames = c("Mean Habitat Richness")
          )
        )
      }
      else if (input$selectGroup == "Threats - Development") {
        # Apply grey out effect and update map
        top_pct_zonation_vect(map, top_pct_zonation, top_pct)

        # Overlap values
        bowen_dp_pz <- bowen_dp_r %>% terra::project(zonation_og)
        res <- values_in_out(zonation_og, top_pct_zonation, bowen_dp_pz)
        inside_mean <- res$inside_vals %>% mean()
        outside_mean <- res$outside_vals %>% mean()

        # Update sidebar
        sidebarTable <- data.frame(
          type = c("Inside", "Outside"),
          mean = c(round(inside_mean, 2), round(outside_mean, 2))
        )
        output$sidebarInfo <- renderUI({
          tagList(
            h2("Development Potential")
          )
        })
        output$sidebarTable <- DT::renderDT(
          DT::datatable(sidebarTable[,c("type", "mean")], options = list(
            paging = F,
            searching = F,
            ordering = F,
            lengthChange = F,
            info = F
          ),
          rownames = FALSE,
          colnames = c("Mean Potential Dev. Units / ha")
          )
        )

      }
      else if(input$selectGroup == "Threats - Wildfire") {
        # Apply grey out effect and update map
        top_pct_zonation_vect(map, top_pct_zonation, top_pct)

        # Overlap values
        bowen_wf_pz <- bowen_wf %>%
          terra::project(zonation_og) %>%
          terra::resample(zonation_og, method = "average")
        res <- values_in_out(zonation_og, top_pct_zonation, bowen_wf_pz)
        inside_mean <- res$inside_vals %>% mean()
        outside_mean <- res$outside_vals %>% mean()

        sidebarTable <- data.frame(
          type = c("Inside", "Outside"),
          mean = c(round(inside_mean, 2), round(outside_mean, 2))
        )
        output$sidebarInfo <- renderUI({
          tagList(
            h2("Vulnerability Fire Index")
          )
        })
        output$sidebarTable <- DT::renderDT(
          DT::datatable(sidebarTable[,c("type", "mean")], options = list(
            paging = F,
            searching = F,
            ordering = F,
            lengthChange = F,
            info = F
          ),
          rownames = FALSE,
          colnames = c("Mean Vuln. Fire Index")
          )
        )
      }

      # TODO: Update overlap calculation for each selectGroup
    })

    #### Update when selectGroup changes ####
    observeEvent(input$selectGroup, {
      if(input$selectGroup == "Choose Category") {
        map %>%
          leaflet::clearGroup(
            "overlay_polygons"
          )
      }
      else if (input$selectGroup == "Land Use") {
        strokeWeight <- 3
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
            fillColor = bowen_pa_col,
            opacity = 1,
            fillOpacity = 0.2,
            weight = strokeWeight
          ) %>%
          # leaflet::addPolylines(
          #   data = bowen_pa_hatch,
          #   layerId = "bowen_pa_hatch",
          #   group = "overlay_polygons",
          #   color = bowen_pa_col,
          #   fill = F,
          #   opacity = 1,
          #   weight = strokeWeight
          # ) %>%
          leaflet:: addPolygons(
            data = bowen_pm,
            layerId = "bowen_pm",
            group = "overlay_polygons",
            color = bowen_pm_col,
            fillColor = bowen_pm_col,
            opacity = 1,
            fillOpacity = 0.2,
            weight = strokeWeight
          ) %>%
          # leaflet::addPolylines(
          #   data = bowen_pm_hatch,
          #   layerId = "bowen_pm_hatch",
          #   group = "overlay_polygons",
          #   color = bowen_pm_col,
          #   fill = F,
          #   opacity = 1,
          #   weight = strokeWeight
          # ) %>%
          leaflet::addPolygons(
            data = bowen_uc,
            layerId = "bowen_uc",
            group = "overlay_polygons",
            color = bowen_uc_col,
            fillColor = bowen_uc_col,
            opacity = 1,
            fillOpacity = 0.2,
            weight = strokeWeight
          )
          # leaflet::addPolylines(
          #   data = bowen_uc_hatch,
          #   layerId = "bowen_uc_hatch",
          #   group = "overlay_polygons",
          #   color = bowen_uc_col,
          #   fill = F,
          #   opacity = 1,
          #   weight = strokeWeight
          # )
      }
      else if (input$selectGroup == "Human Disturbance") {
        # Leaflet parameters
        raster_domain <- c(1, 2, 3)
        raster_labels <- c("1","2","3")
        raster_pal <- leaflet::colorFactor(
          c("#ffeda0", "#feb24c", "#f03b20"),
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
            x = bowen_hd_p,
            colors = raster_pal
          )
      }
      else if (input$selectGroup == "Habitats - Freshwater") {
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
      else if (input$selectGroup == "Habitats - Terrestrial") {
        # Leaflet parameters
        raster_domain <- terra::values(bowen_te_p) %>%
          unique()
        raster_labels <- raster_domain
        raster_colours <- c('#ffffcc','#d9f0a3','#addd8e','#78c679','#41ab5d','#238443','#005a32')
        raster_colour_func <- colorRampPalette(raster_colours) # For custom palette
        raster_colour_ramp <- raster_colour_func(length(raster_domain)) %>% rev()

        # raster_colour_ramp <- viridis::mako(5, end = 0.8, direction = -1)
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
            x = bowen_te_p,
            colors = raster_pal
          )
      }
      else if (input$selectGroup == "Threats - Development") {
        # Update Leaflet Map Parameters
        layer_domain <- c(0, bowen_dp$potential_units)
        pal <- leaflet::colorNumeric(
          palette = "YlOrRd",
          domain = layer_domain
        )
        # Update Leaflet
        map %>%
          leaflet::clearGroup(
            "overlay_polygons"
          ) %>%
          leaflet::clearImages() %>%
          leaflet::clearControls() %>%
          leaflet::addPolygons(
            data = bowen_dp,
            # TODO: apply layerId iteratively, since there are multiple polygons
            # layerId = "bowen_dp",
            group = "overlay_polygons",
            color = ~pal(potential_units),
            fillColor = ~pal(potential_units),
            opacity = 1,
            fillOpacity = 1,
            weight = 3
          )
      }
      else if (input$selectGroup == "Threats - Wildfire") {
        # Leaflet parameters
        raster_domain <- terra::values(bowen_wf) %>%
          unique() %>%
          sort()
        raster_labels <- raster_domain
        raster_pal <- leaflet::colorNumeric(
          "magma",
          raster_domain,
          na.color = "transparent",
          reverse = T
        )
        # Update Leaflet
        map %>%
          leaflet::clearGroup(
            "overlay_polygons"
          ) %>%
          leaflet::clearImages() %>%
          leaflet::clearControls() %>%
          leaflet::addRasterImage(
            x = bowen_wf,
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

#' top_pct_zonation_vect
#' @description
#' For providing the greyed out effect on areas not in top %
top_pct_zonation_vect <- function(map, data, top_pct) {
  # Create polygon that is inverse of selected raster cells
  top_pct_zonation_vect <- terra::ifel(is.na(data), 1, NA) %>%
    terra::extend(c(1000, 1000, 1000, 1000), fill = 1) %>%
    terra::as.polygons(dissolve = T) %>%
    sf::st_as_sf() %>%
    sf::st_transform(4326)

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

  # TODO: fix that polygon doesn't line up with raster exactly on leaflet map
}

#' Overlap
#' @param area_mask SpatRaster, has values for study area and NA for
#' outside study area
#' @param selected_mask SpatRaster, has values for selected pixels
#' within study area, and NA for not selected pixels and outside study
#' area
#' @param input_rast SpatRaster, has values that will be extracted for
#' inside and outside study area.
#' @returns named list, for values inside and outside selected pixels
#' from input_rast. Removes NA values.
values_in_out <- function(area_mask, selected_mask, input_rast) {
  # Check if inputs have same CRS
  a <- terra::crs(area_mask)
  b <- terra::crs(selected_mask)
  c <- terra::crs(input_rast)
  if(!all(sapply(list(a,b,c), function(x) x == c))) {
    stop("Inputs do not have the same CRS.")
  }
  # Create raster masked to pixels within area_mask
  input_masked <- terra::mask(input_rast, area_mask)
  # Create raster masked to pixels within selected_mask
  ins_rast <- terra::mask(input_masked, selected_mask)
  # Create raster masked to pixels outside selected_mask
  out_rast <- terra::mask(input_masked, selected_mask, inverse = T)
  # Values inside selected pixels
  ins_vals <- terra::values(ins_rast)
  ins_vals_nn <- ins_vals[!is.na(ins_vals)]
  # Values outside selected pixels
  out_vals <- terra::values(out_rast)
  out_vals_nn <- out_vals[!is.na(out_vals)]
  # hacky, for some reason there is just a single cell with value when 100% is selected
  # should not be any cells, can't figure out why there is one
  # removes the first two unique values (which are NA and one cell)
  if(length(out_vals_nn) <= 1) {
    out_vals_nn <- NULL
  }
  # Return list of lists
  list("inside_vals" = ins_vals_nn, "outside_vals" = out_vals_nn)
}

