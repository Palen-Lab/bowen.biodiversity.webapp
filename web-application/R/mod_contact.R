#' contact UI Function
#'
#' @description Contact page content.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_contact_ui <- function(id) {
  div(
    class = "container py-5",
    style = "max-width: 800px;",
    h1("Contact Us"),
    p("This atlas was developed by the ",
      tags$a("Palen Lab", href = "https://palenlab.wordpress.com/", target = "_blank"),
      " at Simon Fraser University in partnership with the Bowen Island Conservancy and the Municipality of Bowen Island."
    ),
    p("The Palen Lab conducts applied ecology research to inform conservation policy, with a focus on aquatic community ecology across the Pacific Northwest. Led by Professor Wendy Palen, the lab uses field experiments and landscape surveys to understand ecological patterns — from species physiology to food web interactions — and translate findings into actionable conservation guidance."),
    h3("Authors"),
    tags$table(
      style = "border-collapse: collapse; margin-bottom: 1rem;",
      tags$tr(
        tags$td(style = "padding-right: 1rem; font-weight: bold;", "Wendy J. Palen, PhD."),
        tags$td("Professor of Biological Sciences, Simon Fraser University")
      ),
      tags$tr(
        tags$td(style = "padding-right: 1rem; font-weight: bold;", "Thomas D. Sisk, PhD."),
        tags$td("Adjunct Professor of Biological Sciences, Simon Fraser University")
      ),
      tags$tr(
        tags$td(style = "padding-right: 1rem; font-weight: bold;", "Jay Matsushiba, MSc."),
        tags$td(
          "Lead Analyst and Developer, Simon Fraser University — ",
          tags$a("hello@jmatsushiba.com", href = "mailto:hello@jmatsushiba.com")
        )
      )
    ),
    h3("Research Group"),
    p(
      tags$a("Palen Lab — Earth2Ocean Research Group", href = "https://palenlab.wordpress.com/", target = "_blank"), tags$br(),
      "Department of Biological Sciences", tags$br(),
      "Simon Fraser University", tags$br(),
      "Burnaby, BC, Canada"
    )
  )
}
