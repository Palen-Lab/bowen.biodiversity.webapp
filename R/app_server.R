#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # load(here::here("inst/extdata/weights_df.rda"))
  # layers_df <- weights_df %>%
  #   dplyr::mutate(package = "terra") %>%
  #   dplyr::select(package, relpath, group)
  # layers_sf <- data.frame(package = "sf",
  #                         relpath = here::here("inst/extdata/bowen_boundary"),
  #                         group = "Admin Boundary")
  # layers_output <- dplyr::bind_rows(layers_df, layers_sf)

  #### DEFINE LAYERS FOR APP ####
  # Create layers_df that holds information about all layers on the web map
  layers_df <- data.frame(package = as.character(), # either "sf" or "terra"
                          type = as.character(), # raster, point, line, polygon
                          relpath = as.character(), # relative path within repository
                          group = as.character()) # name to provide for display in LayersControl
                          # pal = as.character()) # pal

  # Add Bowen Admin Boundary to layers_df
  bowen_boundary_df <- data.frame(package = "sf",
                                  type = "polygon",
                                  relpath = here::here("inst/extdata/bowen_boundary"),
                                  group = "Admin Boundary")
  bowen_roads_df <- data.frame(package = "sf",
                               type = "line",
                               relpath = here::here("inst/extdata/bowen_roads"),
                               group = "Roads")
  bowen_trails_df <- data.frame(package = "sf",
                                type = "line",
                                relpath = here::here("inst/extdata/bowen_trails"),
                                group = "Trails")
  layers_df <- layers_df %>% dplyr::bind_rows(bowen_boundary_df, bowen_roads_df, bowen_trails_df)

  # Add rasters
  zonation <- data.frame(package = "terra",
                         type = "raster",
                         relpath = here::here("inst/extdata/rankmap.tif"),
                         group = "Conservation Value")
  layers_df <- layers_df %>% dplyr::bind_rows(zonation)

  #### MAIN MAP MODULE ####
  mod_main_map_server("main_map", layers_df)
}
