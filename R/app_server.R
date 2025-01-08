#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  # Zonation5 input layers logic ----
  species_df <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1ehO30w4i7EDafilNyRpq3sgc86fm8g_TmYMI4ql3qms/edit?usp=sharing") %>%
    dplyr::rename(layer_name = sci_name)
  environmental_df <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1S7U53ukf74y3YaCDZUUSCh2Rv_wpbdZ4veH-BotoQKo/edit?gid=0#gid=0")
  # Combine Species and Environmental df for now
  layers_df <- bind_rows(species_df, environmental_df)

  # Zonation5 layer selection module ----
  zonation5_input_df <- mod_zonation_param_server("zonation_param", layers_df)
  output$test_table <- DT::renderDT(
    zonation5_input_df()
  )

  # Trigger Zonation5 run ----
  main_raster <- reactiveVal(terra::rast("inst/app/zonation/zonation_output/rankmap.tif"))
  observeEvent(input$zonation_button, {
    zonation5(zonation5_input_df())
    main_raster(terra::rast("inst/app/zonation/zonation_output/rankmap.tif"))
  })

  # Main map module ----
  mod_main_map_server("main_map", main_raster)

}
