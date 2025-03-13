#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  layers_df <- data.frame(package = c("sf", "terra"),
                          full_path = c("data-raw/bowen_boundary", "inst/extdata/bowen_human_footprint.tif"),
                          group = c("Admin Boundary", "Human Footprint"))


  # Main map module ----
  mod_main_map_server("main_map", layers_df)
}
