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
    h2("Available Layers"),
    DT::DTOutput(ns("lyrs")),
    hr(),
    h2("Selected Layers"),
    DT::DTOutput(ns("select_lyrs"))
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

    output$select_lyrs <- DT::renderDT({
      select_df()
    })

    select_df <- reactive({
      message("Selected layers updated")
      data.frame(data[input$lyrs_rows_selected,])
    })

    return(select_df)
  })
}

## To be copied in the UI
# mod_zonation_param_ui("zonation_param_1")

## To be copied in the server
# mod_zonation_param_server("zonation_param_1")
