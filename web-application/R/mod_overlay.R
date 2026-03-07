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
  tagList(
    checkboxInput(
      NS(id, "overlay_show"),
      "Show overlay",
      value = FALSE
    ),
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
    zonation_og <- terra::rast(here::here("inst/extdata/5_values/rankmap.tif"))
    zonation <- zonation_og %>% terra::project("epsg:4326")
    zonation_count <- zonation_og %>%
      terra::not.na(falseNA = TRUE) %>%
      terra::values() %>%
      sum(na.rm = TRUE)

    ## Land Use (loaded lazily inside observer to avoid startup errors if files missing)
    bowen_pa_path <- here::here("inst/extdata/7_protected_areas/existing_protected_areas.gpkg")

    ## Human Disturbance
    bowen_hd <- terra::rast(here::here("inst/extdata/4_people/bowen_human_footprint_recl.tif"))
    bowen_hd_p <- bowen_hd %>% terra::project("epsg:3857", method = "near")

    ## Habitats
    bowen_fw <- terra::rast(here::here("inst/extdata/3_habitats/fw_richness.tif"))
    bowen_fw_p <- bowen_fw %>% terra::project("epsg:4326", method = "near")
    bowen_te <- terra::rast(here::here("inst/extdata/3_habitats/total_habitat_richness.tif"))
    bowen_te_p <- bowen_te %>% terra::project("epsg:4326", method = "near")

    ## Threats - Development
    bowen_dp <- sf::st_read(here::here("inst/extdata/6_threats/development_potential.gpkg")) %>%
      sf::st_transform(4326)
    bowen_dp_r <- bowen_dp %>% terra::vect() %>% terra::rasterize(zonation, cover = TRUE)

    ## Threats - Wildfire
    bowen_wf <- terra::rast(here::here("inst/extdata/6_threats/fire_index_40m.tif")) %>%
      terra::project("epsg:3857", method = "near")
    terra::NAflag(bowen_wf) <- 4294967296

    #### Zonation palette ####
    zonation_colour_func <- colorRampPalette(c('#a1d99b'))
    zonation_colour_vect <- zonation_colour_func(100) %>% rev()

    #### Clear all overlay layers helper ####
    clear_overlay_layers <- function(map) {
      map %>%
        leaflet::removeImage(layerId = "overlay_zonation_raster") %>%
        leaflet::removeControl(layerId = "overlay_zonation_legend") %>%
        leaflet::clearGroup("overlay_polygons") %>%
        leaflet::removeShape("top_pct_zonation_vect")
    }

    #### Update when checkbox or slider or category changes ####
    observeEvent(list(input$overlay_show, input$top_pct_slider, input$selectGroup), {
      map <- leaflet::leafletProxy(mapId = map_id, session = parent_session)

      if (!isTRUE(input$overlay_show)) {
        clear_overlay_layers(map)
        output$sidebarInfo <- renderUI(NULL)
        output$sidebarTable <- DT::renderDT(NULL)
        return()
      }

      top_pct <- input$top_pct_slider
      top_pct_zonation <- remove_by_quantile(zonation_og, (1 - top_pct/100)) %>%
        terra::not.na(falseNA = TRUE)

      zonation_colour_ramp <- zonation_colour_vect[0:top_pct]
      zonation_pal <- leaflet::colorNumeric(
        palette = colorRamp(colors = zonation_colour_ramp),
        domain = c((1 - top_pct/100), 1),
        na.color = "transparent",
        reverse = TRUE
      )

      # Remove vector overlays when switching category
      if(input$selectGroup != "Land Use") {
        map %>%
          leaflet::removeShape("bowen_pa") %>%
          leaflet::removeShape("bowen_pm") %>%
          leaflet::removeShape("bowen_uc")
      }

      if(input$selectGroup == "Choose Category") {
        output$sidebarInfo <- renderUI("Choose category from the above selection.")
        output$sidebarTable <- DT::renderDT(NULL)
        map %>%
          clear_overlay_layers() %>%
          leaflet::addRasterImage(x = zonation, layerId = "overlay_zonation_raster", colors = zonation_pal) %>%
          leaflet::addLegend(pal = zonation_pal, layerId = "overlay_zonation_legend",
                             values = c(0, 1), title = "Rel. Conservation Value")
      }
      else if (input$selectGroup == "Land Use") {
        bowen_pm <- vect_layer("1_base/privateland.gpkg") %>% sf::st_transform(4326)
        bowen_pa <- sf::st_read(bowen_pa_path) %>% sf::st_transform(4326) %>% sf::st_make_valid() %>% sf::st_union()
        bowen_uc <- vect_layer("1_base/unprotected_crown.gpkg") %>%
          sf::st_transform(4326) %>% sf::st_cast("MULTIPOLYGON") %>% sf::st_union()

        zonation_d <- zonation_og %>% terra::disagg(2)
        zonation_count_d <- zonation_d %>% terra::not.na(falseNA = TRUE) %>% terra::values() %>% sum(na.rm = TRUE)
        top_pct_zonation_count <- top_pct_zonation %>% terra::values() %>% sum(na.rm = TRUE)

        pa_vect <- bowen_pa %>% terra::vect() %>% terra::project(top_pct_zonation)
        top_pct_zonation_pa_count <- top_pct_zonation %>% terra::mask(pa_vect) %>% terra::values() %>% sum(na.rm = TRUE)
        uc_vect <- bowen_uc %>% terra::vect() %>% terra::project(top_pct_zonation)
        top_pct_zonation_uc_count <- top_pct_zonation %>% terra::mask(uc_vect) %>% terra::values() %>% sum(na.rm = TRUE)
        pm_vect <- bowen_pm %>% terra::vect() %>% terra::project(top_pct_zonation)
        top_pct_zonation_pm_count <- top_pct_zonation %>% terra::mask(pm_vect) %>% terra::values() %>% sum(na.rm = TRUE)

        sidebarTable <- data.frame(type = paste0("Top ", top_pct, "%"), ncells = top_pct_zonation_count) %>%
          rbind(data.frame(type = "Protected Area", ncells = top_pct_zonation_pa_count)) %>%
          rbind(data.frame(type = "Unprotected Crown Land", ncells = top_pct_zonation_uc_count)) %>%
          rbind(data.frame(type = "Private Land", ncells = top_pct_zonation_pm_count))
        sidebarTable$area_ha <- sidebarTable$ncells / 25
        sidebarTable$pct_prop <- round(sidebarTable$ncells / top_pct_zonation_count * 100, digits = 1)
        sidebarTable$pct_total <- round(sidebarTable$ncells / zonation_count * 100, digits = 1)

        output$sidebarInfo <- renderUI({
          tagList(h2("Land Use"), p("The percentages may not add up exactly to the selected %, due to some top % not falling on lands in these categories."))
        })
        output$sidebarTable <- DT::renderDT(
          DT::datatable(sidebarTable[, c("type", "area_ha", "pct_prop", "pct_total")],
                        options = list(paging = FALSE, searching = FALSE, ordering = FALSE, lengthChange = FALSE, info = FALSE),
                        rownames = FALSE,
                        colnames = c("Overlap", "Area (ha)", "% of Top Values", "% of Bowen Island"))
        )

        strokeWeight <- 3
        map %>%
          clear_overlay_layers() %>%
          leaflet::addRasterImage(x = zonation, layerId = "overlay_zonation_raster", colors = zonation_pal) %>%
          leaflet::addLegend(pal = zonation_pal, layerId = "overlay_zonation_legend",
                             values = c(0, 1), title = "Rel. Conservation Value") %>%
          leaflet::addPolygons(data = bowen_pa, layerId = "bowen_pa", group = "overlay_polygons",
                               color = "#377eb8", fillColor = "#377eb8", opacity = 1, fillOpacity = 0.2, weight = strokeWeight) %>%
          leaflet::addPolygons(data = bowen_pm, layerId = "bowen_pm", group = "overlay_polygons",
                               color = "#ff7f00", fillColor = "#ff7f00", opacity = 1, fillOpacity = 0.2, weight = strokeWeight) %>%
          leaflet::addPolygons(data = bowen_uc, layerId = "bowen_uc", group = "overlay_polygons",
                               color = "#984ea3", fillColor = "#984ea3", opacity = 1, fillOpacity = 0.2, weight = strokeWeight)
      }
      else if (input$selectGroup == "Human Disturbance") {
        top_pct_zonation_vect(map, top_pct_zonation, top_pct)
        bowen_hd_pz <- bowen_hd %>% terra::project(zonation_og)
        res <- values_in_out(zonation_og, top_pct_zonation, bowen_hd_pz)
        inside_mean <- res$inside_vals %>% mean()
        outside_mean <- res$outside_vals %>% mean()

        inside_high_count  <- length(res$inside_vals[res$inside_vals == 3])
        inside_medium_count <- length(res$inside_vals[res$inside_vals == 2])
        inside_low_count   <- length(res$inside_vals[res$inside_vals == 1])
        outside_high_count  <- length(res$outside_vals[res$outside_vals == 3])
        outside_medium_count <- length(res$outside_vals[res$outside_vals == 2])
        outside_low_count   <- length(res$outside_vals[res$outside_vals == 1])

        sidebarTable <- data.frame(
          type = c("Inside", "Outside"),
          mean = c(round(inside_mean, 2), round(outside_mean, 2)),
          high = c(inside_high_count, outside_high_count),
          medium = c(inside_medium_count, outside_medium_count),
          low = c(inside_low_count, outside_low_count)
        )
        output$sidebarInfo <- renderUI({ tagList(h2("Human Disturbance")) })
        output$sidebarTable <- DT::renderDT(
          DT::datatable(sidebarTable[, c("type", "high", "medium", "low")],
                        options = list(paging = FALSE, searching = FALSE, ordering = FALSE, lengthChange = FALSE, info = FALSE),
                        rownames = FALSE,
                        colnames = c("High (ha)", "Medium (ha)", "Low (ha)"))
        )

        map %>%
          clear_overlay_layers() %>%
          leaflet::clearGroup("overlay_polygons") %>%
          leaflet::addRasterImage(x = bowen_hd_p, layerId = "overlay_zonation_raster",
                                  colors = leaflet::colorFactor(c("#ffeda0", "#feb24c", "#f03b20"), c(1, 2, 3), na.color = "transparent"))
      }
      else if (input$selectGroup == "Habitats - Freshwater") {
        top_pct_zonation_vect(map, top_pct_zonation, top_pct)
        bowen_hd_pz <- bowen_hd %>% terra::project(zonation_og)
        res <- values_in_out(zonation_og, top_pct_zonation, bowen_hd_pz)
        inside_sum <- sum(res$inside_vals)
        outside_sum <- sum(res$outside_vals)
        total_sum <- inside_sum + outside_sum

        sidebarTable <- data.frame(type = c("Inside", "Outside"), ncells = c(inside_sum, total_sum - inside_sum)) %>%
          dplyr::mutate(area_ha = ncells / 10) %>%
          dplyr::mutate(pct = round(ncells / total_sum * 100))
        output$sidebarInfo <- renderUI({ tagList(h2("Freshwater Habitat Richness")) })
        output$sidebarTable <- DT::renderDT(
          DT::datatable(sidebarTable[, c("type", "area_ha", "pct")],
                        options = list(paging = FALSE, searching = FALSE, ordering = FALSE, lengthChange = FALSE, info = FALSE),
                        rownames = FALSE,
                        colnames = c("Area (ha)", "% of Habitat Covered"))
        )

        raster_colour_ramp <- viridis::mako(5, end = 0.8, direction = -1)
        raster_pal <- leaflet::colorNumeric(raster_colour_ramp, terra::values(bowen_fw_p) %>% unique(), na.color = "transparent")
        map %>%
          clear_overlay_layers() %>%
          leaflet::clearGroup("overlay_polygons") %>%
          leaflet::addRasterImage(x = bowen_fw_p, layerId = "overlay_zonation_raster", colors = raster_pal)
      }
      else if (input$selectGroup == "Habitats - Terrestrial") {
        top_pct_zonation_vect(map, top_pct_zonation, top_pct)
        bowen_te_pz <- bowen_te_p %>% terra::project(zonation_og)
        res <- values_in_out(zonation_og, top_pct_zonation, bowen_te_pz)
        inside_mean <- mean(res$inside_vals)
        outside_mean <- mean(res$outside_vals)

        sidebarTable <- data.frame(type = c("Inside", "Outside"), mean = c(round(inside_mean, 2), round(outside_mean, 2)))
        output$sidebarInfo <- renderUI({ tagList(h2("Habitat Richness")) })
        output$sidebarTable <- DT::renderDT(
          DT::datatable(sidebarTable[, c("type", "mean")],
                        options = list(paging = FALSE, searching = FALSE, ordering = FALSE, lengthChange = FALSE, info = FALSE),
                        rownames = FALSE,
                        colnames = c("Mean Habitat Richness"))
        )

        raster_domain <- terra::values(bowen_te_p) %>% unique()
        raster_colours <- c('#ffffcc','#d9f0a3','#addd8e','#78c679','#41ab5d','#238443','#005a32')
        raster_colour_ramp <- colorRampPalette(raster_colours)(length(raster_domain)) %>% rev()
        raster_pal <- leaflet::colorNumeric(raster_colour_ramp, raster_domain, na.color = "transparent")
        map %>%
          clear_overlay_layers() %>%
          leaflet::clearGroup("overlay_polygons") %>%
          leaflet::addRasterImage(x = bowen_te_p, layerId = "overlay_zonation_raster", colors = raster_pal)
      }
      else if (input$selectGroup == "Threats - Development") {
        top_pct_zonation_vect(map, top_pct_zonation, top_pct)
        bowen_dp_pz <- bowen_dp_r %>% terra::project(zonation_og)
        res <- values_in_out(zonation_og, top_pct_zonation, bowen_dp_pz)
        inside_mean <- mean(res$inside_vals)
        outside_mean <- mean(res$outside_vals)

        sidebarTable <- data.frame(type = c("Inside", "Outside"), mean = c(round(inside_mean, 2), round(outside_mean, 2)))
        output$sidebarInfo <- renderUI({ tagList(h2("Development Potential")) })
        output$sidebarTable <- DT::renderDT(
          DT::datatable(sidebarTable[, c("type", "mean")],
                        options = list(paging = FALSE, searching = FALSE, ordering = FALSE, lengthChange = FALSE, info = FALSE),
                        rownames = FALSE,
                        colnames = c("Mean Potential Dev. Units / ha"))
        )

        layer_domain <- c(0, bowen_dp$potential_units)
        pal <- leaflet::colorNumeric(palette = "YlOrRd", domain = layer_domain)
        map %>%
          clear_overlay_layers() %>%
          leaflet::clearGroup("overlay_polygons") %>%
          leaflet::addPolygons(data = bowen_dp, group = "overlay_polygons",
                               color = ~pal(potential_units), fillColor = ~pal(potential_units),
                               opacity = 1, fillOpacity = 1, weight = 3)
      }
      else if(input$selectGroup == "Threats - Wildfire") {
        top_pct_zonation_vect(map, top_pct_zonation, top_pct)
        bowen_wf_pz <- bowen_wf %>% terra::project(zonation_og) %>% terra::resample(zonation_og, method = "average")
        res <- values_in_out(zonation_og, top_pct_zonation, bowen_wf_pz)
        inside_mean <- mean(res$inside_vals)
        outside_mean <- mean(res$outside_vals)

        sidebarTable <- data.frame(type = c("Inside", "Outside"), mean = c(round(inside_mean, 2), round(outside_mean, 2)))
        output$sidebarInfo <- renderUI({ tagList(h2("Vulnerability Fire Index")) })
        output$sidebarTable <- DT::renderDT(
          DT::datatable(sidebarTable[, c("type", "mean")],
                        options = list(paging = FALSE, searching = FALSE, ordering = FALSE, lengthChange = FALSE, info = FALSE),
                        rownames = FALSE,
                        colnames = c("Mean Vuln. Fire Index"))
        )

        raster_domain <- terra::values(bowen_wf) %>% unique() %>% sort()
        raster_pal <- leaflet::colorNumeric("magma", raster_domain, na.color = "transparent", reverse = TRUE)
        map %>%
          clear_overlay_layers() %>%
          leaflet::clearGroup("overlay_polygons") %>%
          leaflet::addRasterImage(x = bowen_wf, layerId = "overlay_zonation_raster", colors = raster_pal)
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
  top_pct_zonation_vect <- terra::ifel(is.na(data), 1, NA) %>%
    terra::extend(c(1000, 1000, 1000, 1000), fill = 1) %>%
    terra::as.polygons(dissolve = TRUE) %>%
    sf::st_as_sf() %>%
    sf::st_transform(4326)

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
  a <- terra::crs(area_mask)
  b <- terra::crs(selected_mask)
  c <- terra::crs(input_rast)
  if(!all(sapply(list(a, b, c), function(x) x == c))) {
    stop("Inputs do not have the same CRS.")
  }
  input_masked <- terra::mask(input_rast, area_mask)
  ins_rast <- terra::mask(input_masked, selected_mask)
  out_rast <- terra::mask(input_masked, selected_mask, inverse = TRUE)
  ins_vals <- terra::values(ins_rast)
  ins_vals_nn <- ins_vals[!is.na(ins_vals)]
  out_vals <- terra::values(out_rast)
  out_vals_nn <- out_vals[!is.na(out_vals)]
  if(length(out_vals_nn) <= 1) {
    out_vals_nn <- NULL
  }
  list("inside_vals" = ins_vals_nn, "outside_vals" = out_vals_nn)
}
