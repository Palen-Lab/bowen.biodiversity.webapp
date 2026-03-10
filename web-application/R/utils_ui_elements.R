# Accordion panel styled as a section header (dark green, white text).
# Use for top-level section panels (e.g. Biodiversity Values, Planning...).
section_accordion_panel <- function(title, ..., ico = NULL) {
  panel <- bslib::accordion_panel(title, ..., icon = ico)
  htmltools::tagQuery(panel)$
    find("button.accordion-button")$
    addAttrs(style = "background-color: #2d6a4f; color: #ffffff;")$
    allTags()
}

# Non-interactive accordion-style section heading.
# Renders with the same visual weight as an accordion panel header but has no
# collapse behaviour — purely a visual divider/label.
accordion_section_heading <- function(label, ico = NULL) {
  tags$div(
    class = "accordion-item",
    tags$div(
      class = "accordion-button pe-none",
      style = "cursor: default; font-weight: 400; --bs-accordion-btn-icon-width: 0; background-color: #2d6a4f; color: #ffffff;",
      if (!is.null(ico)) tagList(ico, tags$span(label, class = "ms-2")) else label
    )
  )
}

#' hover_popover
#' @description Wrapper around bslib::popover that triggers on hover + focus
#' @noRd
hover_popover <- function(...) {
  bslib::popover(..., options = list(trigger = "hover focus"))
}

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
util_ui_simple_legend_element <- function(colour, label, border_colour = "#555") {
  div(
    style = "display: inline-flex",
    div(
      style = paste0("height: 24px; width: 24px; background-color: ", colour, "; border-radius: 50%; border-style: solid; border-color: ", border_colour, ";")
    ),
    strong(label, style = "margin-left: 5px;")
  )
}

#' docs_link
#' @param href string, link to documentation page
#' @return UI component
docs_link <- function(href = "https://palen-lab.github.io/bowen.biodiversity.webapp/") {
  tagList(
    h4("More Details"),
    p("The full code for producing these maps can be found at our documentation site. Many of these layers can also be downloaded."),
    a(icon("up-right-from-square"), "Visit Documentation",
      href = href,
      target = "_blank",
      class = "btn btn-primary")
  )
}
