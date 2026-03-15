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
    h1("Contributions and Acknowledgements"),
    p("This atlas was developed by the authors in collaboration with the Bowen Island Conservancy and the Caring for Nature initiative. We are thankful for the substantial contributions made by the Caring For Nature team, including B. Turner, J. Matheson, W. Husby, J. Dowler, S.E. Fast, A. Whitehead, J. Gedye, S. Johnson, E. Wachtman, P. Hay, E. Olsen, B. Brokenshire, P. Matthews, L. Gilday, and M. Toom, as well as the public participants in our technical workshop and community meetings. Financial support was provided by the Sitka Foundation and the National Science and Engineering Research Council. We acknowledge the valuable technical input and scientific support provided by V. Kwok, K. Rolfe, M. Persram, M. Tylo (SFU Research Team), D. Martin, C. Skuce (Bowen Island Municipality), and W. Shulba (Islands Trust)."),
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
