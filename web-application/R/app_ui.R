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
      bslib::nav_panel(
        "Map",
        bslib::layout_sidebar(
          fillable = T,
          class = "p-0",
          # Simple small sidebar, for selecting different modules / groupings
          sidebar = bslib::sidebar(
            id = "mini_sidebar",
            width = "80px",
            padding = 0,
            gap = 0,
            open = "always",
            actionButton(
              "start_sidebar_btn",
              "Start",
              icon = icon("play"),
              class = "mini_sidebar_btn"
            ),
            tags$p("Inputs", class = "mini_sidebar_heading"),
            actionButton(
              "species_sidebar_btn",
              "Species",
              icon = icon("dove"),
              class = "mini_sidebar_btn"
            ),
            actionButton(
              "habitats_sidebar_btn",
              "Habitats",
              icon = icon("tree"),
              class = "mini_sidebar_btn"
            ),
            actionButton(
              "people_sidebar_btn",
              "People",
              icon = icon("person"),
              class = "mini_sidebar_btn"
            ),
            tags$p("Analysis", class = "mini_sidebar_heading"),
            actionButton(
              "values_sidebar_btn",
              "Values",
              icon = icon("chart-simple"),
              class = "mini_sidebar_btn"
            ),
            actionButton(
              "threats_sidebar_btn",
              "Threats",
              icon = icon("triangle-exclamation"),
              class = "mini_sidebar_btn"
            ),
            tags$p("Outputs", class = c("mini_sidebar_heading")),
            # actionButton(
            #   "action_sidebar_btn",
            #   "Action",
            #   icon = icon("handshake-simple")
            # ),
            actionButton(
              "protected_areas_sidebar_btn",
              "Protected Areas",
              icon = icon("seedling"),
              class = "mini_sidebar_btn"
            ),
            actionButton(
              "overlay_sidebar_btn",
              "Overlay",
              icon = icon("seedling"),
              class = "mini_sidebar_btn"
            )
          ),
          # Main Content, with interactive information sidebar and map
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              width = "500px",
              # Interactive Sidebar (modules go here)
              tabsetPanel(
                id = "main_sidebar",
                type = "hidden",
                tabPanel(
                  "start",
                  mod_start_ui("start_1")
                ),
                tabPanel(
                  "species",
                  mod_species_ui("species_1")
                ),
                tabPanel(
                  "habitats",
                  mod_habitats_ui("habitats_1")
                ),
                tabPanel(
                  "people",
                  mod_people_ui("people_1")
                ),
                tabPanel(
                  "values",
                  mod_values_ui("values_1")
                ),
                tabPanel(
                  "threats",
                  mod_threats_ui("threats_1")
                ),
                # tabPanel(
                #   "action",
                #   mod_action_ui("action_1")
                # ),
                tabPanel(
                  "protected_areas",
                  mod_protected_areas_ui("protected_areas_1")
                ),
                tabPanel(
                  "overlay",
                  mod_overlay_ui("overlay_1")
                )
              )
            ),
            tags$style(type = "text/css", "#map {height: calc(100vh - 90px) !important;}"),
            leaflet::leafletOutput("map"),
            border_radius = F,
            fillable = T,
            class = "p-0"
          )
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
