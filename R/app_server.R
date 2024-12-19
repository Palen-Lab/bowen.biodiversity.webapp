#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  main_raster <- reactive({
    terra::rast("inst/app/rasters/rankmap.tif")
  })
  mod_main_map_server("main_map", main_raster)
}
