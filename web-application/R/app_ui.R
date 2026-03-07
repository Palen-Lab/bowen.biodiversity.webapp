#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # necessary for some interactive functions
    shinyjs::useShinyjs(),
    # Your application UI logic
    bslib::page_navbar(
      title = div(
        img(
          src = "www/bowenisland_brandmark.png",
          id = "website_logo"
        ),
        "Biodiversity Data Atlas"
      ),
      fillable = c("Map"),
      bslib::nav_spacer(),
      bslib::nav_item(
        actionLink("about_btn", "About", icon = icon("circle-info"))
      ),
      bslib::nav_panel(
        "Map",
        bslib::layout_sidebar(
          fillable = TRUE,
          class = "p-0",
          sidebar = bslib::sidebar(
            width = "380px",
            bslib::accordion(
              id = "main_accordion",
              multiple = FALSE,
              bslib::accordion_panel(
                "Species",
                icon = icon("dove"),
                mod_species_ui("species_1")
              ),
              bslib::accordion_panel(
                "Habitats",
                icon = icon("tree"),
                mod_habitats_ui("habitats_1")
              ),
              bslib::accordion_panel(
                "Human Footprint",
                icon = icon("person"),
                mod_people_ui("people_1")
              ),
              bslib::accordion_panel(
                "Conservation Values",
                icon = icon("chart-simple"),
                mod_values_ui("values_1")
              ),
              bslib::accordion_panel(
                "Threats",
                icon = icon("triangle-exclamation"),
                mod_threats_ui("threats_1")
              ),
              bslib::accordion_panel(
                "Protected Areas",
                icon = icon("seedling"),
                mod_protected_areas_ui("protected_areas_1")
              ),
              bslib::accordion_panel(
                "Overlay",
                icon = icon("layer-group"),
                mod_overlay_ui("overlay_1")
              )
            )
          ),
          tags$style(type = "text/css", "#map {height: calc(100vh - 90px) !important;}"),
          leaflet::leafletOutput("map"),
          border_radius = FALSE
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "bowen.biodiversity.webapp"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
