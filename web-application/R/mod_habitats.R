#' habitats UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_habitats_ui <- function(id) {
  tagList(
    div(class = "d-flex align-items-center gap-2",
      checkboxInput(NS(id, "hab_all"), "All Habitats", value = FALSE),
      hover_popover(
        icon("circle-info", style = "cursor:pointer;"),
        title = "All Habitats",
        p("Bowen Island features a rich mosaic of habitats—ranging from mature temperate rainforests and dry coastal bluffs to freshwater wetlands, lakes, streams, and coastal shores—each supporting distinct communities of plants, animals, and fungi.")
      )
    ),
    tags$p(tags$strong("Freshwater"), class = "mb-0 mt-2"),
    div(class = "d-flex align-items-center gap-2",
      checkboxInput(NS(id, "hab_fw_richness"), "Freshwater Richness", value = FALSE),
      hover_popover(
        icon("circle-info", style = "cursor:pointer;"),
        title = "Freshwater Richness",
        p("Bowen Island hosts a diverse range of freshwater habitats, including lakes, wetlands, and streams.")
      )
    ),
    div(class = "d-flex align-items-center gap-2",
      checkboxInput(NS(id, "hab_fw_lakes"), "Lakes", value = FALSE),
      hover_popover(
        icon("circle-info", style = "cursor:pointer;"),
        title = "Lakes",
        p("The lakes on Bowen Island, such as Killarney and Grafton Lakes, are ecologically significant.")
      )
    ),
    div(class = "d-flex align-items-center gap-2",
      checkboxInput(NS(id, "hab_fw_ponds"), "Ponds", value = FALSE),
      hover_popover(
        icon("circle-info", style = "cursor:pointer;"),
        title = "Ponds",
        p("The ponds on Bowen Island—including natural and beaver-created wetlands—are ecologically vital.")
      )
    ),
    div(class = "d-flex align-items-center gap-2",
      checkboxInput(NS(id, "hab_fw_riparian"), "Riparian", value = FALSE),
      hover_popover(
        icon("circle-info", style = "cursor:pointer;"),
        title = "Riparian",
        p("Riparian zones on Bowen Island are ecologically crucial for watershed health and biodiversity.")
      )
    ),
    div(class = "d-flex align-items-center gap-2",
      checkboxInput(NS(id, "hab_fw_streams"), "Streams", value = FALSE),
      hover_popover(
        icon("circle-info", style = "cursor:pointer;"),
        title = "Streams",
        p("Streams on Bowen Island play a vital ecological role supporting salmonid populations.")
      )
    ),
    div(class = "d-flex align-items-center gap-2",
      checkboxInput(NS(id, "hab_fw_wetlands"), "Wetlands", value = FALSE),
      hover_popover(
        icon("circle-info", style = "cursor:pointer;"),
        title = "Wetlands",
        p("Wetlands on Bowen Island regulate water flow, improve water quality, and support high biodiversity.")
      )
    ),
    tags$p(tags$strong("Terrestrial"), class = "mb-0 mt-2"),
    div(class = "d-flex align-items-center gap-2",
      checkboxInput(NS(id, "hab_terr_old"), "Old Forest", value = FALSE),
      hover_popover(
        icon("circle-info", style = "cursor:pointer;"),
        title = "Old Forest",
        p("Old forests are generally conifer-dominated forest with complex vertical structure, where canopy tree ages are mostly 250 years old or older.")
      )
    ),
    div(class = "d-flex align-items-center gap-2",
      checkboxInput(NS(id, "hab_terr_mature"), "Mature Forest", value = FALSE),
      hover_popover(
        icon("circle-info", style = "cursor:pointer;"),
        title = "Mature Forest",
        p("Mature forests are generally greater than 80 years old and less than 250 years old.")
      )
    ),
    div(class = "d-flex align-items-center gap-2",
      checkboxInput(NS(id, "hab_terr_young"), "Young Forest", value = FALSE),
      hover_popover(
        icon("circle-info", style = "cursor:pointer;"),
        title = "Young Forest",
        p("Young forests are generally greater than 30 to 40 years old and less than 80 years old.")
      )
    ),
    div(class = "d-flex align-items-center gap-2",
      checkboxInput(NS(id, "hab_terr_young_small"), "Young Forest (small)", value = FALSE),
      hover_popover(
        icon("circle-info", style = "cursor:pointer;"),
        title = "Young Forest (small)",
        p("These are patches of young forests that are less than 5 ha.")
      )
    ),
    div(class = "d-flex align-items-center gap-2",
      checkboxInput(NS(id, "hab_terr_other"), "Other Terrestrial", value = FALSE),
      hover_popover(
        icon("circle-info", style = "cursor:pointer;"),
        title = "Other Terrestrial",
        p("The more detailed habitat types captured by the Terrestrial Ecosystem Mapping project.")
      )
    ),
    div(class = "d-flex align-items-center gap-2",
      checkboxInput(NS(id, "hab_terr_coastal"), "Coastal", value = FALSE),
      hover_popover(
        icon("circle-info", style = "cursor:pointer;"),
        title = "Coastal",
        p("Coastal ecosystems on Bowen Island play a vital ecological role by supporting high biodiversity, including shellfish, seaweeds, and invertebrates.")
      )
    )
  )
}

#' habitats Server Functions
#'
#' @noRd
mod_habitats_server <- function(id, map_id, parent_session, active_raster = NULL){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    hab_boxes <- c(
      "hab_all",
      "hab_fw_richness", "hab_fw_lakes", "hab_fw_ponds",
      "hab_fw_riparian", "hab_fw_streams", "hab_fw_wetlands",
      "hab_terr_old", "hab_terr_mature", "hab_terr_young",
      "hab_terr_young_small", "hab_terr_other", "hab_terr_coastal"
    )

    #### Enforce mutual exclusivity ####
    lapply(hab_boxes, function(checked_box) {
      observeEvent(input[[checked_box]], {
        if (isTRUE(input[[checked_box]])) {
          for (other in setdiff(hab_boxes, checked_box)) {
            updateCheckboxInput(session, other, value = FALSE)
          }
        }
      }, ignoreInit = TRUE)
    })

    #### Reactive: which box is currently checked ####
    active_group <- reactive({
      for (box in hab_boxes) {
        if (isTRUE(input[[box]])) return(box)
      }
      NULL
    })

    #### Cross-module raster exclusivity ####
    observe({
      if (!is.null(active_group())) active_raster("habitats")
    })
    observeEvent(active_raster(), {
      if (!is.null(active_raster()) && active_raster() != "habitats") {
        for (box in hab_boxes) updateCheckboxInput(session, box, value = FALSE)
      }
    }, ignoreInit = TRUE)

    select_raster <- reactiveVal(NULL)

    #### Update map when selection changes ####
    observe({
      group <- active_group()
      map <- leaflet::leafletProxy(mapId = map_id, session = parent_session)

      if (is.null(group)) {
        map %>%
          leaflet::removeImage(layerId = "habitats_raster") %>%
          leaflet::removeControl(layerId = "habitats_legend")
        return()
      }

      if (group == "hab_all") {
        raster_group <- "Habitat Richness"
        rast_layer("3_habitats/total_habitat_richness.tif") %>% select_raster()
        vals <- terra::values(select_raster()) %>% na.omit()
        raster_domain <- seq(min(vals), max(vals), by = 1)
        raster_labels <- raster_domain
        raster_pal <- leaflet::colorNumeric(
          c('#edf8fb','#b2e2e2','#66c2a4','#2ca25f','#006d2c'),
          domain = range(raster_domain), na.color = "transparent"
        )
      } else if (group == "hab_fw_richness") {
        raster_group <- "Freshwater Richness"
        rast_layer("3_habitats/fw_richness.tif") %>% select_raster()
        raster_domain <- terra::values(select_raster()) %>% unique()
        raster_labels <- raster_domain
        raster_colour_ramp <- viridis::mako(5, end = 0.8, direction = -1)
        raster_pal <- leaflet::colorNumeric(raster_colour_ramp, raster_domain, na.color = "transparent")
      } else if (group == "hab_fw_lakes") {
        raster_group <- "Lakes"
        rast_layer("3_habitats/fw_lakes.tif") %>% select_raster()
        raster_domain <- 1; raster_labels <- "Present"
        raster_pal <- leaflet::colorFactor(c('#41B6C4'), raster_domain, na.color = "transparent")
      } else if (group == "hab_fw_ponds") {
        raster_group <- "Ponds"
        rast_layer("3_habitats/fw_ponds.tif") %>% select_raster()
        raster_domain <- 1; raster_labels <- "Present"
        raster_pal <- leaflet::colorFactor(c("#2C7FB8"), raster_domain, na.color = "transparent")
      } else if (group == "hab_fw_riparian") {
        raster_group <- "Riparian"
        rast_layer("3_habitats/fw_riparian.tif") %>% select_raster()
        raster_domain <- 1; raster_labels <- "Present"
        raster_pal <- leaflet::colorFactor(c("#A1DAB4"), raster_domain, na.color = "transparent")
      } else if (group == "hab_fw_streams") {
        raster_group <- "Streams"
        rast_layer("3_habitats/fw_streams.tif") %>% select_raster()
        raster_domain <- c(1, 2, 3)
        raster_labels <- c("non-fish bearing", "tributary to fish-bearing", "fish-bearing")
        raster_pal <- leaflet::colorFactor(c('#ece7f2', '#a6bddb', '#2b8cbe'), raster_domain, na.color = "transparent")
      } else if (group == "hab_fw_wetlands") {
        raster_group <- "Wetlands"
        rast_layer("3_habitats/fw_wetlands.tif") %>% select_raster()
        raster_domain <- 1; raster_labels <- "Present"
        raster_pal <- leaflet::colorFactor(c("#253494"), raster_domain, na.color = "transparent")
      } else if (group == "hab_terr_old") {
        raster_group <- "Old Forest"
        rast_layer("3_habitats/forests_OF.tif") %>% select_raster()
        raster_domain <- 1; raster_labels <- "Present"
        raster_pal <- leaflet::colorFactor(c('#2ca25f'), raster_domain, na.color = "transparent")
      } else if (group == "hab_terr_mature") {
        raster_group <- "Mature Forest"
        rast_layer("3_habitats/forests_MF.tif") %>% select_raster()
        raster_domain <- 1; raster_labels <- "Present"
        raster_pal <- leaflet::colorFactor(c('#2ca25f'), raster_domain, na.color = "transparent")
      } else if (group == "hab_terr_young") {
        raster_group <- "Young Forest"
        rast_layer("3_habitats/forests_YF.tif") %>% select_raster()
        raster_domain <- 1; raster_labels <- "Present"
        raster_pal <- leaflet::colorFactor(c('#2ca25f'), raster_domain, na.color = "transparent")
      } else if (group == "hab_terr_young_small") {
        raster_group <- "Young Forest (small)"
        rast_layer("3_habitats/forests_YS.tif") %>% select_raster()
        raster_domain <- 1; raster_labels <- "Present"
        raster_pal <- leaflet::colorFactor(c('#2ca25f'), raster_domain, na.color = "transparent")
      } else if (group == "hab_terr_other") {
        raster_group <- "Other Terrestrial"
        rast_layer("3_habitats/other_tem.tif") %>% select_raster()
        bowen_TEM_habitat_types <- readRDS(here::here("inst/extdata/3_habitats/other_tem_types.rds"))
        raster_domain <- seq(from = 0, to = (length(bowen_TEM_habitat_types$SITEMC_S1_)-1))
        raster_labels <- bowen_TEM_habitat_types$SITE_S1_LA
        raster_pal <- leaflet::colorFactor("viridis", raster_domain, na.color = "transparent")
      } else if (group == "hab_terr_coastal") {
        raster_group <- "Coastal"
        rast_layer("3_habitats/intertidal.tif") %>% select_raster()
        raster_domain <- seq(from = 0, to = 4)
        raster_labels <- c("Beaches", "Coastal Herbaceous", "Eelgrass", "Mudflats", "Vegetated Shoreline")
        raster_colours <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99")
        raster_pal <- leaflet::colorFactor(raster_colours, raster_domain, na.color = "transparent")
      } else {
        return()
      }

      map %>%
        leaflet::removeImage(layerId = "habitats_raster") %>%
        leaflet::removeControl(layerId = "habitats_legend") %>%
        leaflet::addRasterImage(
          x = select_raster(),
          layerId = "habitats_raster",
          colors = raster_pal
        ) %>%
        leaflet::addLegend(
          layerId = "habitats_legend",
          colors = raster_pal(raster_domain),
          labels = raster_labels,
          labFormat = labelFormat(),
          title = raster_group
        )
    })
  })
}

## To be copied in the UI
# mod_habitats_ui("habitats_1")

## To be copied in the server
# mod_habitats_server("habitats_1")
