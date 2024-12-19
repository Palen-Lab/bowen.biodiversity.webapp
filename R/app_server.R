#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  # Zonation5 input layers logic ----
  features <- list.files(here::here("inst", "app", "rasters"),
                         recursive = T,
                         full.names = T)
  layers_df <- data.frame(full_path = features)
  layers_df$filename <- basename(layers_df$full_path)
  layers_df$weights <- 1.0

  # Zonation5 layer selection module ----
  zonation5_input_df <- mod_zonation_param_server("zonation_param", layers_df)
  output$test_table <- DT::renderDT(
    zonation5_input_df()
  )

  # Trigger Zonation5 run ----
  main_raster <- reactiveVal(terra::rast("inst/app/zonation/zonation_output/rankmap.tif"))
  observeEvent(input$test_button, {
    zonation5(zonation5_input_df())
    main_raster(terra::rast("inst/app/zonation/zonation_output/rankmap.tif"))
  })

  # Main map module ----
  mod_main_map_server("main_map", main_raster)

}
