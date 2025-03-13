#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic


  # Main map module ----
  mod_main_map_server("main_map")
  # test_raster <- terra::rast("inst/extdata/bowen_human_footprint.tif")
  # leaflet::leafletProxy("main_map") %>%
  #   leaflet::addRasterImage(test_raster)
#
#   bowen_island_admin <- sf::st_read(here::here("data-raw/bowen_boundary")) %>%
#     sf::st_transform(crs = 4326)
#
#   observeEvent("submit_button", {
#
#   })
}
