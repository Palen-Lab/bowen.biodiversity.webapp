#' start UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_start_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h1("Start")
  )
}

#' start Server Functions
#'
#' @noRd
mod_start_server <- function(id, map_id, parent_session){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Update map
    leaflet::leafletProxy(mapId = map_id,
                          session = parent_session) %>%
      leaflet::clearControls() %>%
      leaflet::clearImages()
  })
}


## To be copied in the UI
# mod_start_ui("start_1")

## To be copied in the server
# mod_start_server("start_1")
