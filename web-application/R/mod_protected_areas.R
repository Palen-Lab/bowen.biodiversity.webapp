# selectGroup choices
selectGroupChoices <- c(
  "All Existing",
  "Fairy Fen Nature Reserve",
  "Bowen Island Ecological Reserve",
  "Art Rennison Nature Park",
  "Crippen Regional Park",
  "Proposed: Mount Collins Reserve",
  "Full 30 by 30 Scenario"
)

#' protected_areas UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_protected_areas_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "action_panel",
    # selectInput(
    #   NS(id, "selectGroup"),
    #   "Select Layer:",
    #   choices = selectGroupChoices,
    #   selected = selectGroupChoices[1]
    # ),
    # actionButton(
    #   NS(id, "next_button"),
    #   "Next"
    # ),
    bslib::card(
      bslib::card_body(
        htmlOutput(NS(id, "sidebarInfo")),
        htmlOutput(NS(id, "specific_sidebarInfo"))
      )
    ),
    docs_link("https://palen-lab.github.io/bowen.biodiversity.webapp/vignettes/protected_areas.html")
  )
}

#' protected_areas Server Functions
#'
#' @noRd
mod_protected_areas_server <- function(id, map_id, parent_session){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    #### Tracking layers in map ####
    layerList <- reactiveVal(character(0))
    #### Update raster when input changes ####
    select_raster <- reactiveVal({
      terra::rast(here::here("inst/extdata/bowen_mask.tif")) %>%
        terra::project("epsg:4326")
    })
    #### Load vector layers ####
    bowen_pa <- here::here("inst/extdata/7_protected_areas/existing_protected_areas.gpkg") %>%
      sf::st_read()
    bowen_ogma <- ogma %>%
      sf::st_transform(sf::st_crs(bowen_pa))
    bowen_new_pa <- here::here("inst/extdata/7_protected_areas/new_protected_areas.gpkg") %>%
      sf::st_read() %>%
      sf::st_transform(sf::st_crs(bowen_pa))

    #### Manage page selection ####
    selectPage <- reactiveVal(selectGroupChoices[1])
    nextCount <- reactiveVal(1)
    observeEvent(input$next_button, {
      # Mod to length of list to continue Next button cycle
      i <- (nextCount() %% length(selectGroupChoices)) + 1
      selectPage(selectGroupChoices[i])
      # Increment for next button press
      # Need to use this instead of just using the input$next_button value
      # Creating a new button with each sidebar update, therefore the count is not maintained by the actionButton
      nextCount(nextCount() + 1)
    })
    output$nextCount <- renderText(nextCount())

    #### Update raster layer and specific_sidebarInfo on Leaflet ####
    # Triggered by changes in both selectGroup and subselectGroup inputs
    # For pages without subselectGroups, need to put them before in the else-if
    observeEvent(selectPage(), {
      #### SINGLE PAGE SELECT GROUPS ####
      #### MULTIPLE PAGE SUBSELECT GROUPS ####
      # Existing
      if (selectPage() == "All Existing") {
        # Reset reactiveVals
        nextCount(1)
        # Update Specific Sidebar
        output$sidebarInfo <- renderUI({
          tagList(
            h1("Expanding Protected Areas on Bowen Island"),
            util_ui_simple_legend_element(label = "Existing Protected Areas", colour = "#a1d76a", border_colour = "lightgrey"),
            util_ui_simple_legend_element(label = "Old Growth Management Areas", colour = "beige", border_colour = "lightgrey"),
            p("This is a simple map to show all existing protected areas on Bowen Island. This part of this website will walk through the existing major protected areas, their significance, and where to expand them based on the results of our Conservation Values analyses."),
          )
        })
        output$specific_sidebarInfo <- renderUI({
          tagList(
            nextButton(session)
          )
        })
        # Update Leaflet Map Parameters
        ## Read / Prepare Map Layers
        ## Symbology

        # Update Leaflet Map
        map <- leaflet::leafletProxy(mapId = map_id,
                              session = parent_session) %>%
          leaflet::clearControls() %>%
          leaflet::clearImages() %>%
          leaflet::clearGroup(group = "highlight_pa") %>%
          leaflet::clearGroup(group = "added_pa") %>%
          leaflet::clearGroup(group = "clear_each_update") %>%
          leaflet::flyTo(-123.370, 49.374, 13) %>%
          # Add Protected Areas
          leaflet::addPolygons(
            data = bowen_pa,
            group = "clear_each_update",
            fillColor = "#a1d76a",
            color = "lightgrey",
            weight = 3,
            fillOpacity = 1,
            smoothFactor = 0.2,
            label = ~lapply(
              paste0("<div style='color:darkgreen; font-size:14px;'>",
                     "<b>", name, "</b><br/>",
                     "<span style='color:gray;'>Type: ", type, "</span>",
                     "</div>"),
              HTML
            ),
            highlightOptions = leaflet::highlightOptions(
              color = "orange",
              weight = 3,
              bringToFront = F
            )
          ) %>%
          leaflet::addLegend(
            colors = "#a1d76a",
            labels = "Present",
            title = "Exist. Protected Areas",
          ) %>%
          # Add OGMAs
          leaflet::addPolygons(
            data = bowen_ogma,
            group = "clear_each_update",
            fillColor = "beige",
            color = "lightgrey",
            fillOpacity = 1,
            smoothFactor = 0.2,
            label = ~lapply(
              paste0("<div style='color:brown; font-size:14px;'>",
                     "<b>", name, "</b><br/>",
                     "<span style='color:gray;'>Type: ", type, "</span>",
                     "</div>"),
              HTML
            ),
            highlightOptions = leaflet::highlightOptions(
              color = "orange",
              weight = 3,
              bringToFront = F
            )
          ) %>%
          leaflet::addLegend(
            colors = "beige",
            labels = "Present",
            title = "OGMAs",
          )

        # Clear layerList
        lapply(layerList(), function(map, x, layerList) {
          removeNewProtectedArea(map, x, layerList)
        }, map = map, layerList = layerList)
      }
      else if (selectPage() == "Fairy Fen Nature Reserve") {
        # Update Specific Sidebar
        output$sidebarInfo <- renderUI({
          tagList(
            h1(selectPage()),
            tags$figure(
              a(
                img(
                  src = "https://bowenislandconservancy.org/wp-content/uploads/2016/04/IMG_2872-640x480.jpg",
                  width = "100%",
                  alt = "Fairy Fen–View from the south side. Photo courtesy of Alan Whitehead",
                  title = "Fairy Fen–View from the south side. Photo courtesy of Alan Whitehead"
                ),
                target = "_blank",
                href = "https://bowenislandconservancy.org/our-work/fairy-fen-nature-reserve/"
              ),
              tags$figcaption(
                p("Fairy Fen–View from the south side. Photo courtesy of Alan Whitehead"),
                class = "text-center"
              ),
              class = "p-0 m-0"
            ),
            p("Fairy Fen is a rare, biologically diverse wetland on Bowen Island that supports unique plants, wildlife, and ancient peat deposits dating back over 4,000 years. Protecting the area as part of the “Cove to Cape Greenway” helps prevent logging and development while reducing ecological damage through managed trail use."),
            expandProtectionUI(session)
          )
        })
        output$specific_sidebarInfo <- renderUI({
          tagList(
            if(isTRUE(input$new_protected_areas)) {
              nextButton(session, "Next")
            }
          )
        })
        # Update Leaflet Map Parameters
        # Read / Prepare Map Layers
        selected_pa <- (bowen_pa$name == selectPage()) %>%
          lapply(., function(i) replace(i, is.na(i), FALSE)) %>%
          unlist() %>%
          bowen_pa[.,]
        # Update Leaflet Map
        leaflet::leafletProxy(mapId = map_id,
                              session = parent_session) %>%
          centerViewPolygon(selected_pa) %>%
          highlightProtectedArea(selected_pa)
      }
      else if (selectPage() == "Bowen Island Ecological Reserve") {
        # Update sidebar
        output$sidebarInfo <- renderUI({
          tagList(
            h1(selectPage()),
            tags$figure(
              a(
                img(
                  src = "https://static.inaturalist.org/projects/34875-cover.JPG?1639182079",
                  width = "100%",
                  alt = "Bowen Island Ecological Reserve. Photo by ecoreserves.bc.ca",
                  title = "Bowen Island Ecological Reserve. Photo by ecoreserves.bc.ca"
                ),
                target = "_blank",
                href = "https://inaturalist.ca/projects/bowen-island-ecological-reserve"
              ),
              tags$figcaption(
                p("Bowen Island Ecological Reserve. Photo by ecoreserves.bc.ca"),
                class = "text-center"
              ),
              class = "p-0 m-0"
            ),
            p("The Bowen Island Ecological Reserve was created to preserve dry subzone forest ecosystems in the Coastal Western Hemlock Zone and provide a convenient site for research. While ecological reserves primarily support conservation, research, and education rather than recreation, this reserve allows low-impact activities such as hiking, wildlife observation, and photography."),
            expandProtectionUI(session)
          )
        })
        # Update specific sidebar
        output$specific_sidebarInfo <- renderUI({
          tagList(
            if(isTRUE(input$new_protected_areas)) {
              nextButton(session, "Next")
            }
          )
        })
        # Update Leaflet Map Parameters
        # Read / Prepare Map Layers
        selected_pa <- (bowen_pa$name == selectPage()) %>%
          lapply(., function(i) replace(i, is.na(i), FALSE)) %>%
          unlist() %>%
          bowen_pa[.,]
        # Update Leaflet Map
        leaflet::leafletProxy(mapId = map_id,
                              session = parent_session) %>%
          centerViewPolygon(selected_pa) %>%
          highlightProtectedArea(selected_pa)
      }
      else if (selectPage() == "Art Rennison Nature Park") {
        # Update Sidebar
        output$sidebarInfo <- renderUI({
          tagList(
            h1(selectPage()),
            tags$figure(
              a(
                img(
                  src = "https://i0.wp.com/bowenislandmunicipality.ca/wp-content/uploads/2025/08/Grafton-Lake-scaled.jpeg?resize=1024%2C768&ssl=1",
                  width = "100%",
                  alt = "Grafton Lake – Photo by Raf Izdebski",
                  title = "Grafton Lake – Photo by Raf Izdebski"
                ),
                target = "_blank",
                href = "https://bowenislandmunicipality.ca/2025/08/05/new-water-quality-study-highlights-need-for-ongoing-protection-of-grafton-lake/"
              ),
              tags$figcaption(
                p("Grafton Lake – Photo by Raf Izdebski"),
                class = "text-center"
              ),
              class = "p-0 m-0"
            ),
            p("The Art Rennison Nature Park will permanently protect 230 acres of land around Grafton Lake, safeguarding Bowen Island’s largest drinking water reservoir while offering recreational opportunities through an improved trail system. As part of a larger greenway connecting Mount Gardner to the Ecological Reserve, the park’s wetlands and wildlife corridors play a vital role in conserving native ecosystems and habitats."),
            expandProtectionUI(session)
          )
        })
        # Update specific sidebar
        output$specific_sidebarInfo <- renderUI({
          tagList(
            if(isTRUE(input$new_protected_areas)) {
              nextButton(session, "Next")
            }
          )
        })
        # Update Leaflet Map Parameters
        ## Read / Prepare Map Layers
        sf::sf_use_s2(FALSE)
        selected_pa <- (bowen_pa$name == selectPage()) %>%
          lapply(., function(i) replace(i, is.na(i), FALSE)) %>%
          unlist() %>%
          bowen_pa[.,]
        ## second row is broken, just get the first row geometry
        selected_pa <- selected_pa[1,] %>%
          sf::st_make_valid()
        # Update Leaflet Map
        leaflet::leafletProxy(mapId = map_id,
                              session = parent_session) %>%
          centerViewPolygon(selected_pa) %>%
          highlightProtectedArea(selected_pa)
      }
      else if (selectPage() == "Crippen Regional Park") {
        # Update Leaflet Map Parameters
        ## Read / Prepare Map Layers
        ## Symbology
        # Update Sidebar
        output$sidebarInfo <- renderUI({
          tagList(
            h1(selectPage()),
            tags$figure(
              a(
                img(
                  src = "https://metrovancouver.org/services/regional-parks/ParkPhotos/Crippen-photo1.jpg",
                  width = "100%",
                  alt = "Mist on Killarney Lake in Crippen Regional Park",
                  title = "Mist on Killarney Lake in Crippen Regional Park"
                ),
                target = "_blank",
                href = "https://metrovancouver.org/services/regional-parks/park/crippen-regional-park"
              ),
              tags$figcaption(
                p("Mist on Killarney Lake in Crippen Regional Park"),
                class = "text-center"
              ),
              class = "p-0 m-0"
            ),
            p("Crippen Regional Park on Bowen Island features a rich mix of forested trails, lakeside boardwalks, wildlife viewing opportunities, and historic sites—all easily accessible from Snug Cove. With amenities including picnic areas, an operating salmon hatchery, and clear trail guidelines, the park balances conservation and recreation for a rewarding day-trip experience."),
            expandProtectionUI(session)
          )
        })
        # Update specific sidebar
        output$specific_sidebarInfo <- renderUI({
          tagList(
            if(isTRUE(input$new_protected_areas)) {
              nextButton(session, "Next")
            }
          )
        })
        # Update Leaflet Map
        selected_pa <- (bowen_pa$name == selectPage()) %>%
          lapply(., function(i) replace(i, is.na(i), FALSE)) %>%
          unlist() %>%
          bowen_pa[.,]

        leaflet::leafletProxy(mapId = map_id,
                              session = parent_session) %>%
          centerViewPolygon(selected_pa) %>%
          highlightProtectedArea(selected_pa)
      }
      else if (selectPage() == "Proposed: Mount Collins Reserve") {
        # Update Specific Sidebar
        output$sidebarInfo <- renderUI({
          tagList(
            h1(selectPage()),
            # tags$figure(
            #   a(
            #     img(
            #       src = "https://metrovancouver.org/services/regional-parks/ParkPhotos/Crippen-photo1.jpg",
            #       width = "100%",
            #       alt = "Mist on Killarney Lake in Crippen Regional Park",
            #       title = "Mist on Killarney Lake in Crippen Regional Park"
            #     ),
            #     target = "_blank",
            #     href = "https://metrovancouver.org/services/regional-parks/park/crippen-regional-park"
            #   ),
            #   tags$figcaption(
            #     p("Mist on Killarney Lake in Crippen Regional Park"),
            #     class = "text-center"
            #   ),
            #   class = "p-0 m-0"
            # ),
            p("According to our analyses, this area around Mount Collins has a high conservation value. This is due to the diversity of freshwater and terrestrial habitats that are suitable for many of the species in our analyses. These habitats include a mix of streams, ponds, wetlands, and forests of mixed ages.")
          )
        })
        # Update Leaflet Map Parameters
        # Read / Prepare Map Layers
        selected_pa <- bowen_new_pa[bowen_new_pa$name == "Mount Collins Proposed Reserve" | bowen_new_pa$name == "Mount Collins Proposed Reserve",]
        # Update Leaflet Map
        leaflet::leafletProxy(mapId = map_id,
                              session = parent_session) %>%
          centerViewPolygon(selected_pa) %>%
          addNewProtectedArea(selected_pa, "mount_collins", layerList) %>%
          highlightProtectedArea(selected_pa)
      }
      else if (selectPage() == "Full 30 by 30 Scenario") {
        # Update Specific Sidebar
        output$sidebarInfo <- renderUI({
          tagList(
            h1("30x30 Targets"),
            p("Canada’s 30 by 30 target commits to protecting 30% of the country’s lands and waters by 2030, a goal aimed at conserving biodiversity, mitigating climate change, and safeguarding ecosystem services."),
            p("This proposed expansion would increase the coverage of Protected Areas across Bowen Island to 30% of total land area."),
            p("For Bowen Island, contributing to this target by expanding and connecting protected areas, such as ecological reserves and parks, would provide multiple benefits. It would help preserve rare ecosystems like fens and wetlands, maintain wildlife corridors essential for species movement, and protect forests and watersheds that regulate drinking water and reduce wildfire risk."),
            p("Meeting the 30 by 30 goals locally would also strengthen Bowen Island’s resilience to climate impacts, enhance opportunities for recreation and education, and position the community as a leader in conservation stewardship within the region.")
          )
        })
        output$specific_sidebarInfo <- renderUI({
          nextButton(session, label = "Return to Start")
        })
        # Update Leaflet Map Parameters
        # Read / Prepare Map Layers

        # Update Leaflet Map
        leaflet::leafletProxy(mapId = map_id,
                              session = parent_session) %>%
          # TODO: flow from All Existing to Full 30 by 30
          leaflet::clearGroup("highlight_pa") %>%
          highlightProtectedArea(bowen_new_pa) %>%
          leaflet::flyTo(-123.370, 49.374, 13)
      }
    })
    #### Logic for Expand Protected Areas checkboxInput ####
    observeEvent(input$new_protected_areas, {
      if(input$new_protected_areas) {
        # for each new protected area
        if (selectPage() == "Fairy Fen Nature Reserve") {
          selected_pa <- bowen_new_pa[bowen_new_pa$name == "Fairy Fen Nature Reserve Expansion",]
          leaflet::leafletProxy(mapId = map_id,
                                session = parent_session) %>%
            addNewProtectedArea(selected_pa, "fairy_fen_expansion", layerList) %>%
            highlightProtectedArea(selected_pa)
        }
        else if (selectPage() == "Art Rennison Nature Park") {
          selected_pa <- bowen_new_pa[bowen_new_pa$name == "Art Rennison Nature Park Expansion",]
          leaflet::leafletProxy(mapId = map_id,
                                session = parent_session) %>%
            addNewProtectedArea(selected_pa, "art_rennison_expansion", layerList) %>%
            highlightProtectedArea(selected_pa)
        }
        else if (selectPage() == "Bowen Island Ecological Reserve") {
          selected_pa <- bowen_new_pa[bowen_new_pa$name == "Bowen Island Ecological Reserve Expansion",]
          leaflet::leafletProxy(mapId = map_id,
                                session = parent_session) %>%
            addNewProtectedArea(selected_pa, "bowen_ecological_reserve_expansion", layerList) %>%
            highlightProtectedArea(selected_pa)
        }
        else if (selectPage() == "Crippen Regional Park") {
          selected_pa <- bowen_new_pa[bowen_new_pa$name == "Crippen Regional Park North Expansion" | bowen_new_pa$name == "Crippen Regional Park West Expansion",]
          leaflet::leafletProxy(mapId = map_id,
                                session = parent_session) %>%
            addNewProtectedArea(selected_pa, "crippen_expansion", layerList) %>%
            highlightProtectedArea(selected_pa)
        }
      }
      else {
        # for each new protected area
        if (selectPage() == "Fairy Fen Nature Reserve") {
          selected_pa <- bowen_new_pa[bowen_new_pa$name == "Fairy Fen Nature Reserve Expansion",]
          leaflet::leafletProxy(mapId = map_id,
                                session = parent_session) %>%
            removeNewProtectedArea("fairy_fen_expansion", layerList) %>%
            leaflet::clearGroup("highlight_pa")
        }
        else if (selectPage() == "Art Rennison Nature Park") {
          selected_pa <- bowen_new_pa[bowen_new_pa$name == "Art Rennison Nature Park Expansion",]
          leaflet::leafletProxy(mapId = map_id,
                                session = parent_session) %>%
            removeNewProtectedArea("art_rennison_expansion", layerList) %>%
            leaflet::clearGroup("highlight_pa")
        }
        else if (selectPage() == "Bowen Island Ecological Reserve") {
          selected_pa <- bowen_new_pa[bowen_new_pa$name == "Bowen Island Ecological Reserve Expansion",]
          leaflet::leafletProxy(mapId = map_id,
                                session = parent_session) %>%
            removeNewProtectedArea("bowen_ecological_reserve_expansion", layerList) %>%
            leaflet::clearGroup("highlight_pa")
        }
        else if (selectPage() == "Crippen Regional Park") {
          selected_pa <- bowen_new_pa[bowen_new_pa$name == "Crippen Regional Park North Expansion" | bowen_new_pa$name == "Crippen Regional Park West Expansion",]
          leaflet::leafletProxy(mapId = map_id,
                                session = parent_session) %>%
            removeNewProtectedArea("crippen_expansion", layerList) %>%
            leaflet::clearGroup("highlight_pa")
        }
      }
    })
  })
}

## To be copied in the UI
# mod_protected_areas_ui("protected_areas_1")

## To be copied in the server
# mod_protected_areas_server("protected_areas_1")

## Utils
#' Fly to center of data
centerViewPolygon <- function(map, data, zoom = 14.5) {
  ## get coordinates for zoom
  bb <- sf::st_bbox(data)
  x_cent <- (bb["xmin"] + bb["xmax"]) / 2
  names(x_cent) <- NULL
  y_cent <- (bb["ymin"] + bb["ymax"]) / 2
  names(y_cent) <- NULL

  map %>%
    leaflet::flyTo(x_cent, y_cent, 14.5)
}

#' Update Leaflet Map for each Existing Protected Area ####
highlightProtectedArea <- function(map, data) {
  map %>%
    leaflet::clearGroup("highlight_pa") %>%
    leaflet::addPolygons(
      data = data,
      group = "highlight_pa",
      fill = F,
      stroke = F,
      weight = 0,
      options = leaflet::pathOptions(className = "blink-outline")
    )
}

#' addNewProtectedArea applies consistent leaflet behaviour to each Expand button
addNewProtectedArea <- function(map, data, layerId, layerList) {
  if(!is.reactive(layerList)) {
    stop("layerList is not reactive")
  }
  # Create new ids, handle multiple rows
  ids <- lapply(seq_len(nrow(data)), function(x) {paste0(layerId, "_", x)})
  # Append new ids to the reactiveVal layerList
  layerList(c(layerList(), ids))
  # Add new protected area to map
  map %>%
    leaflet::addPolygons(
      layerId = ids,
      group = "added_pa",
      data = data,
      fillColor = "orange",
      color = "lightgrey",
      label = ~lapply(
        paste0("<div style='color:orange; font-size:14px;'>",
               "<b>", name, "</b><br/>",
               "<span style='color:gray;'>Type: Proposed</span>",
               "</div>"),
        HTML
      ),
      highlightOptions = leaflet::highlightOptions(
        bringToFront = TRUE
      )
    )
}
removeNewProtectedArea <- function(map, layerId, layerList) {
  if(!is.reactive(layerList)) {
    stop("layerList is not reactive")
  }
  # Find matching layerIds
  ids <- layerList()
  ids_to_remove <- ids[grepl(layerId, ids)]
  # Remove matching
  for (id in ids_to_remove) {
    map %>% leaflet::removeShape(id)
  }
  # Update layerList
  layerList(setdiff(ids, ids_to_remove))
  # Return map
  map
}

#' Expand Protected Areas UI
expandProtectionUI <- function(session) {
  tagList(
    checkboxInput(
      session$ns("new_protected_areas"),
      "Expand Protection",
      value = F
    ),
  )
}

#' Next button
nextButton <- function(session, label = "Next") {
  actionButton(
    session$ns("next_button"),
    label
  )
}
