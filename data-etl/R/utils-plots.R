#' Deal with edges
#' @description
#' Increases resolution for the purposes of plotting only. Disaggregates the
#' input raster without manipulating values. Raster is then masked by the Bowen
#' Island ocean sf, so that the jagged appearance along coastline is less
#' pronounced in produced maps.
#'
#' @param raster_layer rast
#' @param ocean_sf sf; Bowen Island ocean polygon used for coastline masking
#' @returns rast
higher_res_edges <- function(raster_layer, ocean_sf) {
  terra::disagg(raster_layer, fact = 10) %>%
    terra::mask(terra::vect(ocean_sf), inverse = TRUE)
}


#' @import terra
template_plot <- function(mask, ocean_sf, basemap) {
  #### Define plot extent ####
  mask <- mask %>%
    project('EPSG: 3857')
  
  mask_ext <- mask %>% # Project to Web Mercator for basemap
    ext()

  #### Base map for plots ####
  # basemap_for_plot <- basemaps::basemap_terra(ext = raster_layer, map_service = "carto", map_type = "voyager")
  # basemap_for_plot <- basemaps::basemap_terra(
  #   ext = mask,
  #   map_service = "maptiler",
  #   map_type = "backdrop",
  #   map_token = "baL4WLstSFqSHP2fnYrE"
  # ) %>%
  #   mask(vect(ocean_sf), inverse = T)

  #### Consistent elements for plots ####
  bowen_ocean_linewidth <- 0.5
  bowen_ocean_colour <- "#dbdbdc"
  trails_colour <- "brown"
  roads_colour <- "darkgrey"
  caption <- glue(
    "Map created: {date()}. Palen Lab."
  )

  ggplot2::ggplot() +
    ggplot2::theme_bw() +
    tidyterra::geom_spatraster_rgb(data = basemap) +
    ggplot2::scale_x_continuous(
      expand = c(0, 0),
      limits = c(mask_ext[1], mask_ext[2])
    ) +
    ggplot2::scale_y_continuous(
      expand = c(0, 0),
      limits = c(mask_ext[3], mask_ext[4])
    ) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(
        angle = 90,
        vjust = 1,
        hjust = 0.5
      ),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(size = 10),
      legend.key.height = ggplot2::unit(0.5, "cm"),
      legend.key.width = ggplot2::unit(1.5, "cm"),
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.box.margin = margin(1, 1, 1, 1),
      legend.background = element_rect(fill = "transparent", color = NA),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = bowen_ocean_colour, color = NA),
      panel.grid = element_blank()
    ) +
    ggnewscale::new_scale_fill() +
    ggnewscale::new_scale_colour()
}

template_plot_overlay <- function(ocean_sf, trails_sf, roads_sf) {
  #### Consistent elements for plots ####
  ocean_linewidth <- 0.5
  trails_colour <- "brown"
  roads_colour <- "darkgrey"

  list(
    ggnewscale::new_scale_fill(),
    ggnewscale::new_scale_colour(),
    geom_sf(
      data = ocean_sf,
      linewidth = ocean_linewidth,
      fill = NA
    ),
    geom_sf(
      data = trails_sf,
      linewidth = 0.3,
      aes(color = trails_colour)
    ),
    geom_sf(
      data = roads_sf,
      linewidth = 0.5,
      aes(color = roads_colour)
    ),
    scale_colour_identity(
      name = "",
      breaks = c(trails_colour, roads_colour),
      labels = c("Trails", "Roads"),
      guide = "legend"
    )
  )
}

add_annotation <- function(plot) {
  template_plot_annotations <- function() {
    list(
      ggspatial::annotation_scale(
        location = "br",
        bar_cols = c("grey60", "white")
      ),
      ggspatial::annotation_north_arrow(
        location = "br",
        which_north = "true",
        pad_x = unit(0.2, "in"),
        pad_y = unit(0.4, "in"),
        style = ggspatial::north_arrow_nautical(
          fill = c("grey40", "white"),
          line_col = "grey20"
        )
      ),
      theme(
        legend.key = element_rect(fill = "white", color = NA)
      )
    )
  }

  plot + 
    template_plot_annotations()
} 

remove_annotation <- function(plot) {
  template_plot_remove <- function() {
    list(
      theme(
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_blank()
      )
    )
  }

  plot +
    template_plot_remove()
}
