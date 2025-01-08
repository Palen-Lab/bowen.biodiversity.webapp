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
    # Your application UI logic
    bslib::page_navbar(
      bslib::nav_spacer(),
      bslib::nav_panel(
        "Map",
        mod_main_map_ui("main_map")
      ),
      bslib::nav_panel(
        "Zonation",
        h2("Available Layers"),
        mod_zonation_param_ui("zonation_param"),
        hr(),
        h2("Selected Layers"),
        DT::DTOutput("test_table")
      ),
      fillable = c("Map"),
      sidebar = bslib::sidebar({
        tagList(
          h1("Zonation5 Tool"),
          actionButton(label = "Zonation","zonation_button")
        )
      }),
      title = div(
        img(
          src = "www/bowenisland_brandmark.png",
          id = "website_logo"
        ),
        "Biodiversity Data Atlas"
      ),
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
