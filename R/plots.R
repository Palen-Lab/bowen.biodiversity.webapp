#' Bowen Map Plotting
#'
#' @param raster_layer SpatRaster, input raster
#' @param title Text, title for map
#' @param subtitle Text, subtitle for map
#' @param caption Text, caption for map
#' @param legend_label Text, legend label
#'
#' @returns ggplot
#'
#' @import ggplot2
#' @export
#'
bowen_map <- function(raster_layer,
                      title,
                      subtitle,
                      caption,
                      legend_label) {
  bowen_trails <- here::here("data-raw/Trails/Trails.shp") %>% sf::st_read()
  bowen_roads <- here::here("data-raw/Roads/Bowen_Road_Inventory.shp") %>% sf::st_read()
  bowen_shoreline <- here::here("data-raw/shoreline_dem_smoothed2/shoreline_dem_smoothed2.shp") %>% sf::st_read()

  # basemap_for_plot <- basemaps::basemap_terra(ext = raster_layer, map_service = "carto", map_type = "voyager")
  basemap_for_plot <- basemaps::basemap_terra(ext = raster_layer,
                      map_service = "maptiler",
                      map_type = "backdrop",
                      map_token = "baL4WLstSFqSHP2fnYrE")


  output_plot <- ggplot2::ggplot() +
    ggplot2::theme_bw() +
    tidyterra::geom_spatraster_rgb(data = basemap_for_plot) +
    tidyterra::geom_spatraster(data = raster_layer) +
    ggplot2::geom_sf(data = bowen_shoreline, fill = NA) +
    ggplot2::scale_fill_continuous(
      na.value = NA,
      type = "viridis",
      # limits = c(0,1),
      # breaks = c(0, 0.5, 1),
      guide = ggplot2::guide_colourbar(nbin = 100,
                                       draw.ulim = FALSE,
                                       draw.llim = FALSE,
                                       title.position = "top"
                                       # title.hjust = 0.5
      ),
      name = legend_label
    ) +
    ggplot2::geom_sf(data = bowen_trails,
                     aes(color = "Trails")
    ) +
    ggplot2::geom_sf(data = bowen_roads,
                     aes(color = "Roads")
    ) +
    scale_color_manual(
      values = c("#5c5c5c", "darkgrey", "lightgrey"),
      guide = ggplot2::guide_legend(title = NULL,
                                    theme = ggplot2::theme(
                                      legend.text = element_text(size = 10, margin = margin(r = 5, l = 5, b = 5)),
                                      legend.text.position = "top"
                                    ))
    ) +
    ggplot2::scale_x_continuous(expand = c(0,0)) +
    ggplot2::scale_y_continuous(expand = c(0,0)) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      caption = caption
    ) +
    ggplot2::theme(
      plot.title = ggtext::element_textbox_simple(
        size = 25,
        margin = ggplot2::margin(0, 0, 10, 0)
      ),
      plot.subtitle = ggtext::element_textbox_simple(
        lineheight = 1.5,
        margin= ggplot2::margin(0, 0, 10, 0)
      ),
      axis.text.y = ggplot2::element_text(
        angle = 90, vjust = 1, hjust = 0.5
      ),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(size = 10),
      legend.key.height = ggplot2::unit(0.5, "cm"),
      legend.key.width = ggplot2::unit(1.5, "cm"),
      legend.direction = "horizontal",
      legend.box = "horizontal",
      plot.margin = unit(c(1.3,0.3,0.8,0), "cm")
    ) +
    ggspatial::annotation_scale(
      location = "br",
      bar_cols = c("grey60", "white")
    ) +
    ggspatial::annotation_north_arrow(
      location = "br", which_north = "true",
      pad_x = unit(0.2, "in"), pad_y = unit(0.4, "in"),
      style = ggspatial::north_arrow_nautical(
        fill = c("grey40", "white"),
        line_col = "grey20"
      )
    ) +
    ggnewscale::new_scale_fill() +
    ggnewscale::new_scale_colour()

}
