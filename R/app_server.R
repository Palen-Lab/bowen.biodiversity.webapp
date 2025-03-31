#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # opacity <- reactive({input$opacity_slider})
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
  # Create groups list to add overlay groups for visibility control
  groups <- list()
  #### Add Bowen Admin Boundary ####
  bowen_boundary <- sf::st_read(here::here("inst/extdata/bowen_boundary")) %>%
    sf::st_transform(crs = 4326)
  bowen_boundary_group <- "Admin Boundary"
  leaflet::leafletProxy("map") %>%
    leaflet::addPolygons(data = bowen_boundary,
                         group = bowen_boundary_group,
                         stroke = TRUE,
                         color = "darkgrey",
                         fill = FALSE)
  groups <- append(groups, bowen_boundary_group)
  #### Add Bowen Island Roads ####
  bowen_roads <- sf::st_read(here::here("inst/extdata/bowen_roads")) %>%
    sf::st_transform(crs = 4326)
  # addPolylines can't handle multilinestrings for some reason, need to convert to LINESTRINGS
  bowen_roads_ls <- sf::st_cast(bowen_roads, "LINESTRING")
  bowen_roads_group <- "Roads"
  leaflet::leafletProxy("map") %>%
    leaflet::addPolylines(data = bowen_roads_ls,
                          group = bowen_roads_group,
                          stroke = T,
                          weight = 3,
                          color = "grey",
                          opacity = 1.0)
  groups <- append(groups, bowen_roads_group)
  #### Add Bowen Island Trails ####
  bowen_trails <- sf::st_read(here::here("inst/extdata/bowen_trails")) %>%
    sf::st_transform(crs = 4326)
  bowen_trails_group <- "Trails"
  leaflet::leafletProxy("map") %>%
    leaflet::addPolylines(data = bowen_trails,
                          group = bowen_trails_group,
                          stroke = T,
                          weight = 2,
                          color = "brown",
                          opacity = 1.0)
  groups <- append(groups, bowen_trails_group)
  #### Add Zonation Output Raster ####
  zonation <- terra::rast(here::here("inst/extdata/rankmap.tif")) %>%
    terra::project("epsg:4326")
  zonation_group <- "Relative Conservation Value"
  groups <- append(groups, zonation_group)
  zonation_pal <- leaflet::colorNumeric(c("#FFFFCC", "#41B6C4", "#0C2C84"), c(0,1),
                                        na.color = "transparent")
  observe({
    leaflet::leafletProxy("map") %>%
      leaflet::removeControl("zonation_legend") %>%
      leaflet::removeImage("zonation_raster") %>%
      leaflet::addRasterImage(x = zonation,
                              layerId = "zonation_raster",
                              group = zonation_group,
                              colors = zonation_pal) %>%
      leaflet::addLegend(pal = zonation_pal,
                         layerId = "zonation_legend",
                         values =  c(0,1),
                         title = "Rel. Conservation Value",
                         group = zonation_group) %>%
      leaflet::hideGroup(zonation_group)
  })
  #### Add Human Footprint Raster ####
  human_footprint <- terra::rast(here::here("inst/extdata/bowen_human_footprint.tif")) %>%
    terra::project("epsg:4326")
  human_footprint_group <- "Human Footprint"
  groups <- append(groups, human_footprint_group)
  human_footprint_domain <- c(0, terra::minmax(human_footprint)[2])
  human_footprint_pal <- leaflet::colorNumeric(c("#1a9641", "#a6d96a", "#ffffbf", "#fdae61", "#d7191c"), human_footprint_domain,
                                        na.color = "transparent")
  observe({
    leaflet::leafletProxy("map") %>%
      leaflet::removeControl("human_footprint_legend") %>%
      leaflet::removeImage("human_footprint_raster") %>%
      leaflet::addRasterImage(x = human_footprint,
                              layerId = "human_footprint_raster",
                              group = human_footprint_group,
                              colors = human_footprint_pal) %>%
      leaflet::addLegend(pal = human_footprint_pal,
                         layerId = "human_footprint_legend",
                         values =  human_footprint_domain,
                         title = "Human Footprint",
                         group = human_footprint_group) %>%
      leaflet::hideGroup(human_footprint_group)
  })

  #### Add Species Richness ####
  species_richness <- terra::rast(here::here("inst/extdata/bowen_sdm_richness.tif")) %>%
    terra::project("epsg:4326")
  species_richness_group <- "Species Richness"
  groups <- append(groups, species_richness_group)
  species_richness_domain <- c(0, terra::minmax(species_richness)[2])
  species_richness_pal <- leaflet::colorNumeric(c('#edf8fb','#b2e2e2','#66c2a4','#2ca25f','#006d2c'), species_richness_domain,
                                               na.color = "transparent")
  observe({
    leaflet::leafletProxy("map") %>%
      leaflet::removeControl("species_richness_legend") %>%
      leaflet::removeImage("species_richness_raster") %>%
      leaflet::addRasterImage(x = species_richness,
                              layerId = "species_richness_raster",
                              group = species_richness_group,
                              colors = species_richness_pal) %>%
      leaflet::addLegend(pal = species_richness_pal,
                         layerId = "species_richness_legend",
                         values =  species_richness_domain,
                         title = "Species Richness",
                         group = species_richness_group) %>%
      leaflet::hideGroup(species_richness_group)
  })
  # Add Legend with Visibility Control
  leaflet::leafletProxy("map") %>%
    leaflet::addLayersControl(
      overlayGroups = unique(groups),
      options = leaflet::layersControlOptions(collapsed = F)
    )
}
