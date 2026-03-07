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

  #### Welcome Modal ####
  welcome_modal <- function() {
    modalDialog(
      title = NULL,
      mod_start_ui("start_1"),
      footer = modalButton("Get Started"),
      size = "l",
      easyClose = TRUE
    )
  }
  showModal(welcome_modal())

  observeEvent(input$about_btn, {
    showModal(welcome_modal())
  })

  #### Shared raster layer state (only one raster active across all modules) ####
  active_raster_module <- reactiveVal(NULL)

  #### Module Servers ####
  mod_species_server("species_1", map_id = "map", parent_session = session, active_raster = active_raster_module)
  mod_habitats_server("habitats_1", map_id = "map", parent_session = session, active_raster = active_raster_module)
  mod_people_server("people_1", map_id = "map", parent_session = session, active_raster = active_raster_module)
  mod_values_server("values_1", map_id = "map", parent_session = session, active_raster = active_raster_module)
  mod_threats_server("threats_1", map_id = "map", parent_session = session, active_raster = active_raster_module)
  mod_land_management_server("land_management_1", map_id = "map", parent_session = session)
  # mod_overlay_server("overlay_1", map_id = "map", parent_session = session)

  #### Init Main Map ####
  bowen_boundary <- vect_layer("1_base/boundary.gpkg") %>%
    sf::st_transform(crs = 4326)
  bowen_boundary_group <- "Admin Boundary"

  maptiler_key <- "UznHc4ZNqi19zHP8eYLY"

  long_center <- -123.3698
  lat_center <- 49.3738
  long_bound <- 0.1
  lat_bound <- 0.05
  output$map <- leaflet::renderLeaflet({
    leaflet::leaflet(options = leaflet::leafletOptions(
      zoomControl = TRUE,
      zoomSnap = 0.25,
      zoomDelta = 0.25,
      minZoom = 13
    )
    ) %>%
      leaflet::setView(long_center, lat_center, zoom = 13) %>%
      leaflet::setMaxBounds(
        (long_center+long_bound),
        (lat_center+lat_bound),
        (long_center-long_bound),
        (lat_center-lat_bound)
        ) %>%
      leaflet::addTiles(
        urlTemplate = paste0(
          "https://api.maptiler.com/maps/backdrop/{z}/{x}/{y}.png?key=",
          maptiler_key
        ),
        attribution = 'Map data \u00a9 <a href="https://www.maptiler.com/">MapTiler</a>'
      ) %>%
      leaflet::addPolygons(data = bowen_boundary,
                           group = bowen_boundary_group,
                           stroke = TRUE,
                           color = "#333333",
                           fill = FALSE)
  })

  #### Add Base Layers ####
  bowen_roads <- vect_layer("1_base/roads.gpkg") %>%
    sf::st_transform(crs = 4326)
  bowen_roads_ls <- sf::st_cast(bowen_roads, "LINESTRING")

  bowen_trails <- vect_layer("1_base/trails.gpkg") %>%
    sf::st_transform(crs = 4326)

  bowen_ocean_sf <- vect_layer("1_base/ocean.gpkg") %>%
    sf::st_transform(crs = 4326)

  leaflet::leafletProxy("map") %>%
    leaflet::addMapPane("ocean", 501) %>%
    leaflet::addMapPane("roads", 500) %>%
    leaflet::addMapPane("trails", 499) %>%
    leaflet::addPolygons(data = bowen_ocean_sf,
                         group = "Oceans",
                         stroke = TRUE,
                         color = "grey",
                         weight = 5,
                         fill = TRUE,
                         fillColor = "#d9f8fc",
                         fillOpacity = 1.0,
                         options = leaflet::pathOptions(pane = "ocean")
                         ) %>%
    leaflet::addPolylines(data = bowen_roads_ls,
                          group = "Roads",
                          stroke = T,
                          weight = 3,
                          color = "grey",
                          opacity = 1.0,
                          options = leaflet::pathOptions(pane = "roads")) %>%
    leaflet::addPolylines(data = bowen_trails,
                          group = "Trails",
                          stroke = T,
                          weight = 2,
                          color = "brown",
                          opacity = 1.0,
                          options = leaflet::pathOptions(pane = "trails")) %>%
    leaflet::addLayersControl(
      overlayGroups = c("Roads", "Trails")
    )

}
