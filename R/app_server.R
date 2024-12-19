#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  observeEvent(input$test_button, {
    zonation5()
  })

  main_raster <- reactive({
    terra::rast("inst/app/zonation/zonation_output/rankmap.tif")
  })
  mod_main_map_server("main_map", main_raster)
}
