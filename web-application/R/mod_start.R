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
    p("Bowen Island — known traditionally as Nexwlélexwm, on the unceded territory of the Squamish Nation — is among the most biodiverse sites in British Columbia. This interactive atlas is the companion tool to the Bowen Island Biodiversity Plan, bringing together the data, maps, and analyses that form its scientific foundation."),
    h3("Patterns of Biodiversity"),
    p("Species richness, habitat diversity, freshwater resources, and ecological intactness data were compiled from a wide range of sources — including citizen science, government databases, academic research, and local expertise — to assess and map biodiversity across the island."),
    h3("Conservation Analysis"),
    p("Integrated Conservation Values are calculated using Zonation5, a spatial conservation prioritization tool, combining all species, habitat, and intactness layers to identify where biodiversity is highest and where conservation efforts will have the greatest impact."),
    h3("Planning for a Biodiverse Future"),
    p("These conservation values are used to identify candidates for protection, assess development pressure on high-value areas, and support the land use planning and stewardship decisions needed to protect Bowen Island's natural heritage."),
    h2("Collaborators:"),
    div(style = "display: flex; align-items: center; justify-content: space-around; gap: 16px;",
      img(src = "www/SFU_horizontal_logo_rgb.png",
          style = "width: 28%; height: auto; object-fit: contain;"),
      img(src = "www/Bowen-Conservancy-Oval-Logo-6-green-text-transparent-background-400x112-1.png",
          style = "width: 28%; height: auto; object-fit: contain;"),
      img(src = "www/bowenisland_brandmark.png",
          style = "width: 28%; height: auto; object-fit: contain;")
    )
  )
}
