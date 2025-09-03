if (interactive()) {
  reactlog::reactlog_enable()
}

#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  sf::sf_use_s2(FALSE)
  #### Active Panel ####
  r <- reactiveValues(active_panel = "start")
  observeEvent(input$start_sidebar_btn, {
    r$active_panel <- "start"
    mod_start_server("start_1",
                     map_id = "map",
                     parent_session = session)
  })
  observeEvent(input$species_sidebar_btn, {
    r$active_panel <- "species"
    mod_species_server("species_1",
                       map_id = "map",
                       parent_session = session)
  })
  observeEvent(input$habitats_sidebar_btn, {
    r$active_panel <- "habitats"
    mod_habitats_server("habitats_1",
                        map_id = "map",
                        parent_session = session)
  })
  observeEvent(input$people_sidebar_btn, {
    r$active_panel <- "people"
    mod_people_server("people_1",
                      map_id = "map",
                      parent_session = session)
  })
  observeEvent(input$values_sidebar_btn, {
    r$active_panel <- "values"
    mod_values_server("values_1",
                      map_id = "map",
                      parent_session = session)
  })
  observeEvent(input$threats_sidebar_btn, {
    r$active_panel <- "threats"
    mod_threats_server("threats_1",
                       map_id = "map",
                       parent_session = session)
  })
  observeEvent(input$action_sidebar_btn, {
    r$active_panel <- "action"
    mod_action_server("action_1",
                      map_id = "map",
                      parent_session = session)
  })
  observeEvent(input$protected_areas_sidebar_btn, {
    r$active_panel <- "protected_areas"
    mod_protected_areas_server("protected_areas_1",
                      map_id = "map",
                      parent_session = session)
  })
  observeEvent(input$overlay_sidebar_btn, {
    r$active_panel <- "overlay"
    mod_overlay_server("overlay_1",
                       map_id = "map",
                       parent_session = session)
  })
  #### Update Active Panel ####
  observeEvent(r$active_panel, {
    updateTabsetPanel(inputId = "main_sidebar", selected = r$active_panel)
    leaflet::leafletProxy("map") %>%
      leaflet::clearControls() %>%
      leaflet::clearImages() %>%
      leaflet::clearShapes()
  })

  #### Init Main Map ####
  #### Add Bowen Admin Boundary ####
  bowen_boundary_4326 <- bowen_boundary %>%
    sf::st_transform(crs = 4326)
  bowen_boundary_group <- "Admin Boundary"

  maptiler_key <- "UznHc4ZNqi19zHP8eYLY"
  output$map <- leaflet::renderLeaflet({
    leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = TRUE,
                                              zoomSnap = 0.25,
                                              zoomDelta = 0.25)
                     ) %>%
      leaflet::setView(-123.3698, 49.3738, zoom = 13) %>%
      leaflet::addTiles(
        urlTemplate = paste0(
          "https://api.maptiler.com/maps/backdrop/{z}/{x}/{y}.png?key=",
          maptiler_key
        ),
        attribution = 'Map data © <a href="https://www.maptiler.com/">MapTiler</a>'
      ) %>%
      leaflet::addPolygons(data = bowen_boundary_4326,
                           group = bowen_boundary_group,
                           stroke = TRUE,
                           color = "#333333",
                           fill = FALSE)
  })

  # #### Add Bowen Admin Boundary ####
  # bowen_boundary <- sf::st_read(here::here("inst/extdata/bowen_boundary")) %>%
  #   sf::st_transform(crs = 4326)
  # bowen_boundary_group <- "Admin Boundary"
  # leaflet::leafletProxy("map") %>%
  #   leaflet::addPolygons(data = bowen_boundary,
  #                        group = bowen_boundary_group,
  #                        stroke = TRUE,
  #                        color = "darkgrey",
  #                        fill = FALSE)
  # groups <- append(groups, bowen_boundary_group)
  # #### Add Bowen Island Roads ####
  # bowen_roads <- sf::st_read(here::here("inst/extdata/bowen_roads")) %>%
  #   sf::st_transform(crs = 4326)
  # # addPolylines can't handle multilinestrings for some reason, need to convert to LINESTRINGS
  # bowen_roads_ls <- sf::st_cast(bowen_roads, "LINESTRING")
  # bowen_roads_group <- "Roads"
  # leaflet::leafletProxy("map") %>%
  #   leaflet::addPolylines(data = bowen_roads_ls,
  #                         group = bowen_roads_group,
  #                         stroke = T,
  #                         weight = 3,
  #                         color = "grey",
  #                         opacity = 1.0)
  # groups <- append(groups, bowen_roads_group)
  # #### Add Bowen Island Trails ####
  # bowen_trails <- sf::st_read(here::here("inst/extdata/bowen_trails")) %>%
  #   sf::st_transform(crs = 4326)
  # bowen_trails_group <- "Trails"
  # leaflet::leafletProxy("map") %>%
  #   leaflet::addPolylines(data = bowen_trails,
  #                         group = bowen_trails_group,
  #                         stroke = T,
  #                         weight = 2,
  #                         color = "brown",
  #                         opacity = 1.0)
  # groups <- append(groups, bowen_trails_group)

}
