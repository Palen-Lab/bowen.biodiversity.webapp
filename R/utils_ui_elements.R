#' util_ui_simple_legend
#'
#' @description A utils function
#'
#' @return tagList object
#'
#' @noRd
util_ui_simple_legend <- function(low_colour, high_colour, low_label, high_label) {
  tagList(
    util_ui_simple_legend_element(high_colour, high_label),
    util_ui_simple_legend_element(low_colour, low_label)
  )
}

#' util_ui_simple_legend_element
#'
#' @param colour Hex code for colour
#' @param border_colour Hex code for border colour
#' @param label Label for circle legend item
#'
#' @returns HTML
#' @noRd
util_ui_simple_legend_element <- function(colour, border_colour = "#555", label) {
  div(
    style = "display: inline-flex",
    div(
      style = paste0("height: 24px; width: 24px; background-color: ", colour, "; border-radius: 50%; border-style: solid; border-color: ", border_colour, ";")
    ),
    strong(label, style = "margin-left: 5px;")
  )
}

docs_link <- tagList(
  h4("More Details"),
  p("The full code for producing these maps can be found at our documentation site. Many of these layers can also be downloaded."),
  a(icon("up-right-from-square"), "Visit Documentation",
    href = "https://palen-lab.github.io/bowen.biodiversity.webapp/",
    target = "_blank",
    class = "btn btn-primary")
)
