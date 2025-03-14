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
  layers_df <- data.frame(package = as.character(),
                          relpath = as.character(),
                          group = as.character())

  # Add Bowen Admin Boundary to layers_df
  bowen_boundary_df <- data.frame(package = "sf",
                                  relpath = here::here("inst/extdata/bowen_boundary"),
                                  group = "Admin Boundary")
  layers_df <- layers_df %>% dplyr::bind_rows(bowen_boundary_df)

  #### MAIN MAP MODULE ####
  mod_main_map_server("main_map", layers_df)
}
