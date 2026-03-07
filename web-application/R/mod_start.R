#' start UI Function
#'
#' @description Welcome modal content.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_start_ui <- function(id) {
  tagList(
    h1("Welcome!"),
    p("The Bowen Island Biodiversity Data Atlas serves as the interactive information center for supporting decision-making around sustainable development on Bowen Island."),
    h3("Inputs"),
    p("Knowledge and data was gathered from a wide range of sources, including academia, government, NGO, and local community members, to be integrated into this Bowen Island Biodiversity Data Atlas."),
    h3("Analysis"),
    p("Values showcases the Conservation Values output, which takes all of our inputs and algorithmically identifies the highest conservation value locations. In Threats, we conduct additional analyses to generate spatial layers that map potential threats to biodiversity across Bowen Island."),
    h3("Outputs"),
    p("These Conservation Values are used to create outputs, identifying key locations for further attention. Protected Areas shows potential future areas of protection. Overlay allows interactive exploration to compare the Conservation Values layer to other important layers."),
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
}
