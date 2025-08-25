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
  tabPanel(
    "action_panel",
    selectInput(
      NS(id, "selectGroup"),
      "Select Layer:",
      choices = c(
        "Existing Protection" = "exist",
        "Expansion / Connectivity" = "expand",
        "New Protected Areas" = "new",
        "30 by 30" = "all"
      ),
      selected = "exist"
    ),
    bslib::card(
      bslib::card_body(
        htmlOutput(NS(id, "sidebarInfo")),
        htmlOutput(NS(id, "specific_sidebarInfo"))
      )
    ),
    docs_link
  )
}

#' protected_areas Server Functions
#'
#' @noRd
mod_protected_areas_server <- function(id, map_id, parent_session){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    #### Update raster when input changes ####
    select_raster <- reactiveVal({
      terra::rast(here::here("inst/extdata/bowen_mask.tif")) %>%
        terra::project("epsg:4326")
    })
    #### Load vector layers ####
    bowen_pa <- here::here("inst/extdata/7_action/existing_protected_areas.gpkg") %>%
      sf::st_read()
    bowen_ogma <- ogma %>%
      sf::st_transform(sf::st_crs(bowen_pa))

    #### Define reactive value for specificselectGroup ####
    subselect <- reactive({
      req(input$subselectGroup)
      input$subselectGroup
    })
    #### Update sidebar based on selectGroup ####
    observeEvent(input$selectGroup, {
      # Development Category Selection
      if(input$selectGroup == "exist") {
        # Update Sidebar
        output$sidebarInfo <- renderUI({
          tagList(
            h1("Existing Protected Areas"),
            selectInput(session$ns("subselectGroup"),
                        "Select protected area to view",
                        c(
                          "All Existing",
                          "Fairy Fen Nature Reserve",
                          "Bowen Island Ecological Reserve",
                          "Art Rennison Nature Park",
                          "Crippen Regional Park"
                        ),
                        selected = "All Existing")
          )
        })
      }
      # Wildfire Category Selection
      else if (input$selectGroup == "expand") {
        # Update Sidebar
        output$sidebarInfo <- renderUI({
          tagList(
            h1("Expanding and Connecting Protected Areas"),
            selectInput(session$ns("subselectGroup"),
                        "Select protected area to view",
                        c("expand_1", "expand_2"),
                        selected = "expand_1")
          )
        })
      }
      else if (input$selectGroup == "new") {
        # Update Sidebar
        output$sidebarInfo <- renderUI({
          tagList(
            h1("New Protected Areas"),
            selectInput(session$ns("subselectGroup"),
                        "Select protected area to view",
                        c("new_1", "new_2"),
                        selected = "new_1")
          )
        })
      }
      else if (input$selectGroup == "all") {
        # Update Sidebar
        output$sidebarInfo <- renderUI({
          tagList(
            h1("30 by 30")
          )
        })
      }
    })
    #### Update raster layer and specific_sidebarInfo on Leaflet ####
    # Triggered by changes in both selectGroup and subselectGroup inputs
    # For pages without subselectGroups, need to put them before in the else-if
    observeEvent(list(input$subselectGroup, input$selectGroup), {
      #### SINGLE PAGE SELECT GROUPS ####
      # all - 30 by 30 full picture
      if(input$selectGroup == "all") {
        #### Add Potential Protected Areas Raster ####
        protected_areas_raster <- terra::rast(here::here("inst/extdata/7_action/potential_protected_areas.tif")) %>%
          terra::project("epsg:4326")

        raster_domain <- seq(from = 1, to = 5)
        raster_labels <- c("Low", "Medium-Low", "Medium", "Medium-High", "High")
        raster_colours <- c("#feebe2", "#fbb4b9", "#f768a1", "#c51b8a", "#7a0177")
        raster_pal <- leaflet::colorFactor(
          raster_colours,
          raster_domain,
          na.color = "transparent"
        )
        # Update Specific Sidebar
        output$specific_sidebarInfo <- renderUI({
          tagList(
          )
        })
        # Update Leaflet Map
        leaflet::leafletProxy(mapId = map_id,
                              session = parent_session) %>%
          leaflet::clearControls() %>%
          leaflet::clearImages() %>%
          leaflet::clearGroup(group = "clear_each_update") %>%
          leaflet::addRasterImage(
            x = protected_areas_raster,
            layerId = "protected_areas_raster",
            colors = raster_pal
          ) %>%
          leaflet::addPolygons(
            data = bowen_pa,
            group = "clear_each_update",
            color = "#a1d76a",
            stroke = FALSE,
            fillOpacity = 1,
            smoothFactor = 0.2
          ) %>%
          leaflet::addLegend(
            colors = raster_pal(raster_domain),
            layerId = "protected_areas_raster_legend",
            labels = raster_labels,
            title = "Poten. Protected Areas",
          ) %>%
          leaflet::addLegend(
            colors = "#a1d76a",
            layerId = "protected_areas_vector_legend",
            labels = "Present",
            title = "Exist. Protected Areas",
          )
      }
      #### MULTIPLE PAGE SUBSELECT GROUPS ####
      # Existing
      else if (subselect() == "All Existing") {
        # Update Leaflet Map Parameters
        ## Read / Prepare Map Layers
        ## Symbology
        # Update Specific Sidebar
        output$specific_sidebarInfo <- renderUI({
          tagList(
            util_ui_simple_legend_element(label = "Existing Protected Areas", colour = "#a1d76a", border_colour = "darkgreen"),
            util_ui_simple_legend_element(label = "Old Growth Management Areas", colour = "beige", border_colour = "brown"),
            p("This is a simple map to show all existing protected areas on Bowen Island. This section will walk through the existing major protected areas and their significance."),
          )
        })
        # Update Leaflet Map
        leaflet::leafletProxy(mapId = map_id,
                              session = parent_session) %>%
          leaflet::clearControls() %>%
          leaflet::clearImages() %>%
          leaflet::clearGroup(group = "clear_each_update") %>%
          leaflet::flyTo(-123.370, 49.374, 13) %>%
          # Add Protected Areas
          leaflet::addPolygons(
            data = bowen_pa,
            group = "clear_each_update",
            fillColor = "#a1d76a",
            color = "darkgreen",
            fillOpacity = 1,
            smoothFactor = 0.2,
            label = ~lapply(
              paste0("<div style='color:darkgreen; font-size:14px;'>",
                     "<b>", name, "</b><br/>",
                     "<span style='color:gray;'>Type: ", type, "</span>",
                     "</div>"),
              HTML
            ),
            highlightOptions = leaflet::highlightOptions(
              color = "white",
              weight = 3,
              bringToFront = TRUE
            )
          ) %>%
          leaflet::addLegend(
            colors = "#a1d76a",
            labels = "Present",
            title = "Exist. Protected Areas",
          ) %>%
          # Add OGMAs
          leaflet::addPolygons(
            data = bowen_ogma,
            group = "clear_each_update",
            fillColor = "beige",
            color = "brown",
            fillOpacity = 1,
            smoothFactor = 0.2,
            label = ~lapply(
              paste0("<div style='color:brown; font-size:14px;'>",
                     "<b>", name, "</b><br/>",
                     "<span style='color:gray;'>Type: ", type, "</span>",
                     "</div>"),
              HTML
            ),
            highlightOptions = leaflet::highlightOptions(
              color = "white",
              weight = 3,
              bringToFront = TRUE
            )
          ) %>%
          leaflet::addLegend(
            colors = "beige",
            labels = "Present",
            title = "OGMAs",
          )
      }
      else if (subselect() == "Fairy Fen Nature Reserve") {
        # Update Leaflet Map Parameters
        ## Read / Prepare Map Layers
        ## Symbology
        # Update Specific Sidebar
        output$specific_sidebarInfo <- renderUI({
          tagList(
            h1(subselect()),
            util_ui_simple_legend_element(label = "Existing Protected Areas", colour = "#a1d76a", border_colour = "darkgreen"),
            util_ui_simple_legend_element(label = "Old Growth Management Areas", colour = "beige", border_colour = "brown"),
            p("This is a simple map to show all existing protected areas on Bowen Island. This section will walk through the existing major protected areas and their significance."),
          )
        })
        # Update Leaflet Map
        selected_pa <- (bowen_pa$name == subselect()) %>%
          lapply(., function(i) replace(i, is.na(i), FALSE)) %>%
          unlist() %>%
          bowen_pa[.,]

        ## get coordinates for zoom
        bb <- st_bbox(selected_pa)
        x_cent <- (bb["xmin"] + bb["xmax"]) / 2
        names(x_cent) <- NULL
        y_cent <- (bb["ymin"] + bb["ymax"]) / 2
        names(y_cent) <- NULL

        leaflet::leafletProxy(mapId = map_id,
                              session = parent_session) %>%
          leaflet::flyTo(x_cent, y_cent, 14.5) %>%
          leaflet::removeShape("highlight_pa") %>%
          leaflet::addPolygons(
            data = selected_pa,
            layerId = "highlight_pa",
            group = "clear_each_update",
            fillColor = "yellow",
            color = "white",
            weight = 10,
            fillOpacity = 1,
            smoothFactor = 0.2,
            label = ~lapply(
              paste0("<div style='color:darkgreen; font-size:14px;'>",
                     "<b>", name, "</b><br/>",
                     "<span style='color:gray;'>Type: ", type, "</span>",
                     "</div>"),
              HTML
            ),
            highlightOptions = leaflet::highlightOptions(
              bringToFront = TRUE
            )
          )
      }
      else if (subselect() == "Bowen Island Ecological Reserve") {
        # Update Leaflet Map Parameters
        ## Read / Prepare Map Layers
        ## Symbology
        # Update Specific Sidebar
        output$specific_sidebarInfo <- renderUI({
          tagList(
            h1(subselect()),
            util_ui_simple_legend_element(label = "Existing Protected Areas", colour = "#a1d76a", border_colour = "darkgreen"),
            util_ui_simple_legend_element(label = "Old Growth Management Areas", colour = "beige", border_colour = "brown"),
            p("This is a simple map to show all existing protected areas on Bowen Island. This section will walk through the existing major protected areas and their significance."),
          )
        })
        # Update Leaflet Map
        selected_pa <- (bowen_pa$name == subselect()) %>%
          lapply(., function(i) replace(i, is.na(i), FALSE)) %>%
          unlist() %>%
          bowen_pa[.,]

        ## get coordinates for zoom
        bb <- st_bbox(selected_pa)
        x_cent <- (bb["xmin"] + bb["xmax"]) / 2
        names(x_cent) <- NULL
        y_cent <- (bb["ymin"] + bb["ymax"]) / 2
        names(y_cent) <- NULL

        leaflet::leafletProxy(mapId = map_id,
                              session = parent_session) %>%
          leaflet::flyTo(x_cent, y_cent, 14.5) %>%
          leaflet::removeShape("highlight_pa") %>%
          leaflet::addPolygons(
            data = selected_pa,
            layerId = "highlight_pa",
            group = "clear_each_update",
            fillColor = "yellow",
            color = "white",
            weight = 10,
            fillOpacity = 1,
            smoothFactor = 0.2,
            label = ~lapply(
              paste0("<div style='color:darkgreen; font-size:14px;'>",
                     "<b>", name, "</b><br/>",
                     "<span style='color:gray;'>Type: ", type, "</span>",
                     "</div>"),
              HTML
            ),
            highlightOptions = leaflet::highlightOptions(
              bringToFront = TRUE
            )
          )
      }
      else if (subselect() == "Art Rennison Nature Park") {
        # Update Leaflet Map Parameters
        ## Read / Prepare Map Layers
        ## Symbology
        # Update Specific Sidebar
        output$specific_sidebarInfo <- renderUI({
          tagList(
            h1(subselect()),
            util_ui_simple_legend_element(label = "Existing Protected Areas", colour = "#a1d76a", border_colour = "darkgreen"),
            util_ui_simple_legend_element(label = "Old Growth Management Areas", colour = "beige", border_colour = "brown"),
            p("This is a simple map to show all existing protected areas on Bowen Island. This section will walk through the existing major protected areas and their significance."),
          )
        })
        # Update Leaflet Map
        sf::sf_use_s2(FALSE)
        selected_pa <- (bowen_pa$name == subselect()) %>%
          lapply(., function(i) replace(i, is.na(i), FALSE)) %>%
          unlist() %>%
          bowen_pa[.,]
        ## second row is broken, just get the first row geometry
        selected_pa <- selected_pa[1,] %>%
          sf::st_make_valid()
        ## get coordinates for zoom
        bb <- st_bbox(selected_pa)
        x_cent <- (bb["xmin"] + bb["xmax"]) / 2
        names(x_cent) <- NULL
        y_cent <- (bb["ymin"] + bb["ymax"]) / 2
        names(y_cent) <- NULL

        leaflet::leafletProxy(mapId = map_id,
                              session = parent_session) %>%
          leaflet::flyTo(x_cent, y_cent, 14.5) %>%
          leaflet::removeShape("highlight_pa") %>%
          leaflet::addPolygons(
            data = selected_pa,
            layerId = "highlight_pa",
            group = "clear_each_update",
            fillColor = "yellow",
            color = "white",
            weight = 10,
            fillOpacity = 1,
            smoothFactor = 0.2,
            label = ~lapply(
              paste0("<div style='color:darkgreen; font-size:14px;'>",
                     "<b>", name, "</b><br/>",
                     "<span style='color:gray;'>Type: ", type, "</span>",
                     "</div>"),
              HTML
            ),
            highlightOptions = leaflet::highlightOptions(
              bringToFront = TRUE
            )
          )
      }
      else if (subselect() == "Crippen Regional Park") {
        # Update Leaflet Map Parameters
        ## Read / Prepare Map Layers
        ## Symbology
        # Update Specific Sidebar
        output$specific_sidebarInfo <- renderUI({
          tagList(
            h1(subselect()),
            util_ui_simple_legend_element(label = "Existing Protected Areas", colour = "#a1d76a", border_colour = "darkgreen"),
            util_ui_simple_legend_element(label = "Old Growth Management Areas", colour = "beige", border_colour = "brown"),
            p("This is a simple map to show all existing protected areas on Bowen Island. This section will walk through the existing major protected areas and their significance."),
          )
        })
        # Update Leaflet Map
        selected_pa <- (bowen_pa$name == subselect()) %>%
          lapply(., function(i) replace(i, is.na(i), FALSE)) %>%
          unlist() %>%
          bowen_pa[.,]

        ## get coordinates for zoom
        bb <- st_bbox(selected_pa)
        x_cent <- (bb["xmin"] + bb["xmax"]) / 2
        names(x_cent) <- NULL
        y_cent <- (bb["ymin"] + bb["ymax"]) / 2
        names(y_cent) <- NULL

        leaflet::leafletProxy(mapId = map_id,
                              session = parent_session) %>%
          leaflet::flyTo(x_cent, y_cent, 14.5) %>%
          leaflet::removeShape("highlight_pa") %>%
          leaflet::addPolygons(
            data = selected_pa,
            layerId = "highlight_pa",
            group = "clear_each_update",
            fillColor = "yellow",
            color = "white",
            weight = 10,
            fillOpacity = 1,
            smoothFactor = 0.2,
            label = ~lapply(
              paste0("<div style='color:darkgreen; font-size:14px;'>",
                     "<b>", name, "</b><br/>",
                     "<span style='color:gray;'>Type: ", type, "</span>",
                     "</div>"),
              HTML
            ),
            highlightOptions = leaflet::highlightOptions(
              bringToFront = TRUE
            )
          )
      }
    })
  })
}

## To be copied in the UI
# mod_protected_areas_ui("protected_areas_1")

## To be copied in the server
# mod_protected_areas_server("protected_areas_1")
