#' zonation_param UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_zonation_param_ui <- function(id) {
  ns <- NS(id)
  tagList(
    DT::DTOutput(ns("lyrs"))
  )
}

#' zonation_param Server Functions
#'
#' @noRd
mod_zonation_param_server <- function(id, data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # TODO: set weights for selected layers
    # TODO: zonation settings

    output$lyrs <- DT::renderDT({
      data
    })
    reactive({
      message("Selected layers updated")
      data.frame(data[input$lyrs_rows_selected,])
    })

  })
}

## To be copied in the UI
# mod_zonation_param_ui("zonation_param_1")

## To be copied in the server
# mod_zonation_param_server("zonation_param_1")
