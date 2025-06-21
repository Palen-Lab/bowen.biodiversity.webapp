#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  #### Active Panel ####
  r <- reactiveValues(active_panel = "start")
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
  #### Update Active Panel ####
  observeEvent(r$active_panel, {
    updateTabsetPanel(inputId = "main_sidebar", selected = r$active_panel)
  })

  #### Init Main Map ####
  # TODO: add interactive legend, to reveal top % of zonation output
  output$map <- leaflet::renderLeaflet({
    leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = TRUE,
                                              zoomSnap = 0.25,
                                              zoomDelta = 1)
                     ) %>%
      leaflet::setView(-123.3698, 49.3738, zoom = 12) %>%
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron,
                                options = leaflet::providerTileOptions(noWrap = TRUE, minZoom = 10, maxZoom = 18)
      )
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
