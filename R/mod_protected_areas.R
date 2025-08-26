# selectGroup choices
selectGroupChoices <- c(
  "All Existing",
  "Fairy Fen Nature Reserve",
  "Bowen Island Ecological Reserve",
  "Art Rennison Nature Park",
  "Crippen Regional Park",
  "Proposed: Mount Collins Reserve",
  "Full 30 by 30 Scenario"
)

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
      choices = selectGroupChoices,
      selected = selectGroupChoices[1]
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
    bowen_pa <- here::here("inst/extdata/7_protected_areas/existing_protected_areas.gpkg") %>%
      sf::st_read()
    bowen_ogma <- ogma %>%
      sf::st_transform(sf::st_crs(bowen_pa))
    bowen_new_pa <- here::here("inst/extdata/7_protected_areas/new_protected_areas.gpkg") %>%
      sf::st_read() %>%
      sf::st_transform(sf::st_crs(bowen_pa))

    #### Update raster layer and specific_sidebarInfo on Leaflet ####
    # Triggered by changes in both selectGroup and subselectGroup inputs
    # For pages without subselectGroups, need to put them before in the else-if
    observeEvent(input$selectGroup, {

      #### SINGLE PAGE SELECT GROUPS ####
      #### MULTIPLE PAGE SUBSELECT GROUPS ####
      # Existing
      if (input$selectGroup == "All Existing") {
        # Update Specific Sidebar
        output$sidebarInfo <- renderUI({
          tagList(
            util_ui_simple_legend_element(label = "Existing Protected Areas", colour = "#a1d76a", border_colour = "lightgrey"),
            util_ui_simple_legend_element(label = "Old Growth Management Areas", colour = "beige", border_colour = "lightgrey"),
            p("This is a simple map to show all existing protected areas on Bowen Island. This section will walk through the existing major protected areas and their significance."),
          )
        })
        # Update Leaflet Map Parameters
        ## Read / Prepare Map Layers
        ## Symbology

        # Update Leaflet Map
        leaflet::leafletProxy(mapId = map_id,
                              session = parent_session) %>%
          leaflet::clearControls() %>%
          leaflet::clearImages() %>%
          leaflet::clearGroup(group = "highlight_pa") %>%
          leaflet::clearGroup(group = "added_pa") %>%
          leaflet::clearGroup(group = "clear_each_update") %>%
          leaflet::flyTo(-123.370, 49.374, 13) %>%
          # Add Protected Areas
          leaflet::addPolygons(
            data = bowen_pa,
            group = "clear_each_update",
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
            highlightOptions = leaflet::highlightOptions(
              color = "orange",
              weight = 3,
              bringToFront = F
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
            color = "lightgrey",
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
              color = "orange",
              weight = 3,
              bringToFront = F
            )
          ) %>%
          leaflet::addLegend(
            colors = "beige",
            labels = "Present",
            title = "OGMAs",
          )
      }
      else if (input$selectGroup == "Fairy Fen Nature Reserve") {
        # Update Specific Sidebar
        output$sidebarInfo <- renderUI({
          tagList(
            h1(input$selectGroup),
            expandProtectionUI(session),
          )
        })
        # Update Leaflet Map Parameters
        # Read / Prepare Map Layers
        selected_pa <- (bowen_pa$name == input$selectGroup) %>%
          lapply(., function(i) replace(i, is.na(i), FALSE)) %>%
          unlist() %>%
          bowen_pa[.,]
        # Update Leaflet Map
        leaflet::leafletProxy(mapId = map_id,
                              session = parent_session) %>%
          centerViewPolygon(selected_pa) %>%
          highlightProtectedArea(selected_pa)
      }
      else if (input$selectGroup == "Bowen Island Ecological Reserve") {
        # Update Specific Sidebar
        output$sidebarInfo <- renderUI({
          tagList(
            h1(input$selectGroup),
            expandProtectionUI(session),
          )
        })
        # Update Leaflet Map Parameters
        # Read / Prepare Map Layers
        selected_pa <- (bowen_pa$name == input$selectGroup) %>%
          lapply(., function(i) replace(i, is.na(i), FALSE)) %>%
          unlist() %>%
          bowen_pa[.,]
        # Update Leaflet Map
        leaflet::leafletProxy(mapId = map_id,
                              session = parent_session) %>%
          centerViewPolygon(selected_pa) %>%
          highlightProtectedArea(selected_pa)
      }
      else if (input$selectGroup == "Art Rennison Nature Park") {
        # Update Specific Sidebar
        output$sidebarInfo <- renderUI({
          tagList(
            h1(input$selectGroup),
            expandProtectionUI(session),
          )
        })
        # Update Leaflet Map Parameters
        ## Read / Prepare Map Layers
        sf::sf_use_s2(FALSE)
        selected_pa <- (bowen_pa$name == input$selectGroup) %>%
          lapply(., function(i) replace(i, is.na(i), FALSE)) %>%
          unlist() %>%
          bowen_pa[.,]
        ## second row is broken, just get the first row geometry
        selected_pa <- selected_pa[1,] %>%
          sf::st_make_valid()
        # Update Leaflet Map
        leaflet::leafletProxy(mapId = map_id,
                              session = parent_session) %>%
          centerViewPolygon(selected_pa) %>%
          highlightProtectedArea(selected_pa)
      }
      else if (input$selectGroup == "Crippen Regional Park") {
        # Update Leaflet Map Parameters
        ## Read / Prepare Map Layers
        ## Symbology
        # Update Specific Sidebar
        output$sidebarInfo <- renderUI({
          tagList(
            h1(input$selectGroup),
            expandProtectionUI(session),
          )
        })
        # Update Leaflet Map
        selected_pa <- (bowen_pa$name == input$selectGroup) %>%
          lapply(., function(i) replace(i, is.na(i), FALSE)) %>%
          unlist() %>%
          bowen_pa[.,]

        leaflet::leafletProxy(mapId = map_id,
                              session = parent_session) %>%
          centerViewPolygon(selected_pa) %>%
          highlightProtectedArea(selected_pa)
      }
      else if (input$selectGroup == "Proposed: Mount Collins Reserve") {
        # Update Specific Sidebar
        output$sidebarInfo <- renderUI({
          tagList(
            h1(input$selectGroup),
            # TODO: Add explanation to how Mount Collins was chosen
            p()
          )
        })
        # Update Leaflet Map Parameters
        # Read / Prepare Map Layers
        selected_pa <- bowen_new_pa[bowen_new_pa$name == "Mount Collins Proposed Reserve" | bowen_new_pa$name == "Mount Collins Proposed Reserve",]
        # Update Leaflet Map
        leaflet::leafletProxy(mapId = map_id,
                              session = parent_session) %>%
          centerViewPolygon(selected_pa) %>%
          addNewProtectedArea(selected_pa)
      }
      else if (input$selectGroup == "Full 30 by 30 Scenario") {
        # Update Specific Sidebar
        output$sidebarInfo <- renderUI({
          tagList(
            h1(input$selectGroup),
            # TODO: Add explanation
            p()
          )
        })
        # Update Leaflet Map Parameters
        # Read / Prepare Map Layers
        # Update Leaflet Map
        leaflet::leafletProxy(mapId = map_id,
                              session = parent_session) %>%
          # TODO: flow from All Existing to Full 30 by 30
          leaflet::clearGroup("highlight_pa") %>%
          leaflet::flyTo(-123.370, 49.374, 13)
      }
    })
    #### Define reactive value for Expand Protected Areas actionButton ####
    observeEvent(input$new_protected_areas, {
      # for each existing protected area listed
      if (input$selectGroup == "Fairy Fen Nature Reserve") {
        selected_pa <- bowen_new_pa[bowen_new_pa$name == "Fairy Fen Nature Reserve Expansion",]
        leaflet::leafletProxy(mapId = map_id,
                              session = parent_session) %>%
          addNewProtectedArea(selected_pa, "fairy_fen_expansion") %>%
          highlightProtectedArea(selected_pa)
      }
      else if (input$selectGroup == "Art Rennison Nature Park") {
        selected_pa <- bowen_new_pa[bowen_new_pa$name == "Art Rennison Nature Park Expansion",]
        leaflet::leafletProxy(mapId = map_id,
                              session = parent_session) %>%
          addNewProtectedArea(selected_pa, "art_rennison_expansion") %>%
          highlightProtectedArea(selected_pa)
      }
      else if (input$selectGroup == "Bowen Island Ecological Reserve") {
        selected_pa <- bowen_new_pa[bowen_new_pa$name == "Bowen Island Ecological Reserve Expansion",]
        leaflet::leafletProxy(mapId = map_id,
                              session = parent_session) %>%
          addNewProtectedArea(selected_pa, "bowen_ecological_reserve_expansion") %>%
          highlightProtectedArea(selected_pa)
      }
      else if (input$selectGroup == "Crippen Regional Park") {
        selected_pa <- bowen_new_pa[bowen_new_pa$name == "Crippen Regional Park North Expansion" | bowen_new_pa$name == "Crippen Regional Park West Expansion",]
        leaflet::leafletProxy(mapId = map_id,
                              session = parent_session) %>%
          addNewProtectedArea(selected_pa, "crippen_expansion") %>%
          highlightProtectedArea(selected_pa)
      }
    })
  })
}

## To be copied in the UI
# mod_protected_areas_ui("protected_areas_1")

## To be copied in the server
# mod_protected_areas_server("protected_areas_1")

## Utils
#' Fly to center of data
centerViewPolygon <- function(map, data, zoom = 14.5) {
  ## get coordinates for zoom
  bb <- sf::st_bbox(data)
  x_cent <- (bb["xmin"] + bb["xmax"]) / 2
  names(x_cent) <- NULL
  y_cent <- (bb["ymin"] + bb["ymax"]) / 2
  names(y_cent) <- NULL

  map %>%
    leaflet::flyTo(x_cent, y_cent, 14.5)
}

#' Update Leaflet Map for each Existing Protected Area ####
highlightProtectedArea <- function(map, data) {
  map %>%
    # leaflet::flyTo(x_cent, y_cent, 14.5) %>%
    leaflet::clearGroup("highlight_pa") %>%
    leaflet::addPolygons(
      data = data,
      group = "highlight_pa",
      fill = F,
      stroke = F,
      weight = 0,
      options = leaflet::pathOptions(className = "blink-outline")
    )
}

#' addNewProtectedArea applies consistent leaflet behaviour to each Expand button
addNewProtectedArea <- function(map, data, layerId) {
  # TODO: implement check for preventing multiple areas added on top with repeated clicks
  # Maybe don't even need to once checkbox instead of actionButton trigger
  map %>%
    # leaflet::removeShape(layerId) %>%
    leaflet::addPolygons(
      # layerId = layerId,
      group = "added_pa",
      data = data,
      fillColor = "orange",
      color = "lightgrey",
      label = ~lapply(
        paste0("<div style='color:orange; font-size:14px;'>",
               "<b>", name, "</b><br/>",
               "<span style='color:gray;'>Type: Proposed</span>",
               "</div>"),
        HTML
      ),
      highlightOptions = leaflet::highlightOptions(
        bringToFront = TRUE
      )
    )
}

#' Action button
expandProtectionUI <- function(session) {
  actionButton(
    session$ns("new_protected_areas"),
    "Expand Protection"
  )
}
