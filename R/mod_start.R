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
  bslib::card(
    bslib::card_body(
      tagList(
        h1("Welcome!"),
        p("The Bowen Island Biodiversity Data Atlas serves as the interactive information center for supporting decision-making around sustainable development on Bowen Island."),
        p("Knowledge and data was gathered from a wide range of sources, including academia, government, NGO, and local community members, to be integrated into this Bowen Island Biodiversity Data Atlas."),
        p("The existing information are used as inputs to generate Conservation Values across Bowen Island."),
        p("This Conservation Values is then overlaid with potential threats, identifying key locations for further attention."),
        p("Finally, based on the Conservation Values, we explore Action that could mitigate risk towards biodiversity."),
        actionButton(
          inputId = "start_page_button",
          label = "START",
          icon = icon("circle-play")
        ),
        h2("Collaborators:"),
        img(src = "www/SFU_horizontal_logo_rgb.png",
            height = "100%",
            width = "100%"),
        img(src = "www/Bowen-Conservancy-Oval-Logo-6-green-text-transparent-background-400x112-1.png",
            height = "100%",
            width = "100%"),
        img(src = "www/bowenisland_brandmark.png",
            height = "70%",
            width = "70%")
      )
    )
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
      leaflet::clearImages() %>%
      leaflet::clearGroup(group = "clear_each_update")
  })
}


## To be copied in the UI
# mod_start_ui("start_1")

## To be copied in the server
# mod_start_server("start_1")
