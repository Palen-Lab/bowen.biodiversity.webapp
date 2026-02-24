## Example Candidate Locations for Protected Areas
candidate_pa_plot <- function(pa, pa_candidates, template_plot, template_plot_overlay) {
  #### Plotting ####
  protected_colour <- "purple"
  ogma_colour <- "#a1d76a"
  new_colour <- "orange"
  template_plot +
  geom_sf(
    data = pa,
    aes(fill = protected_colour, color = protected_colour),
    alpha = 0.1,
    linewidth = 0.5
  ) +
  geom_sf(
    data = pa_candidates,
    aes(fill = new_colour, color = new_colour),
    alpha = 0.3,
    linewidth = 0.5
  ) +
  scale_fill_identity(
    name = "",
    breaks = c(protected_colour, new_colour),
    labels = c(
      "Existing Protected Areas",
      "Candidate Protected Areas"
    ),
    guide = guide_legend(order = 1)
  ) +
  scale_color_identity(
    name = "",
    breaks = c(protected_colour, new_colour),
    labels = c(
      "Existing Protected Areas",
      "Candidate Protected Areas"
    ),
    guide = guide_legend(order = 1)
  ) +
  template_plot_overlay +
  ggplot2::theme(
    legend.margin = margin(-9, 6, 6, 6),
    legend.key.width = ggplot2::unit(0.5, "cm"),
    legend.key.height = ggplot2::unit(0.5, "cm"),
    legend.key.spacing.y = ggplot2::unit(0, "lines"),
    legend.key.spacing.x = ggplot2::unit(0, "cm"),
    legend.direction = "vertical",
    legend.box = "vertical",
    legend.box.background = element_rect(fill = "white", colour = "black"),
    legend.box.margin = margin(10, 5, 5, 5),
    legend.position = c(.05, .95),
    legend.justification = c("left", "top"),
    legend.box.just = "left"
  )
}

# Wildfire Vulnerability Index
wvi_plot <- function(wvi, template_plot, template_plot_overlay) {
  #### Preparing raster for plotting ####
  fire_index_mp <- wvi %>%
    project("EPSG: 3857") %>%
    terra::subst(4294967296, NA) %>% # Max value should be NA
    terra::scale_linear()
  #### Plotting ####
  # TODO: change colour palette from cool to warm
  template_plot +
    tidyterra::geom_spatraster(data = fire_index_mp) +
    colorspace::scale_fill_continuous_sequential(
      na.value = NA,
      palette = "Batlow",
      rev = FALSE,
      limits = c(0, 1),
      breaks = c(0, 1),
      labels = c("Lower", "Higher"),
      guide = ggplot2::guide_colourbar(
        order = 1,
        nbin = 100,
        draw.ulim = FALSE,
        draw.llim = FALSE,
        title.position = "top"
        # title.hjust = 0.5
      ),
      name = "Wildfire Vulnerability Index"
    ) +
    template_plot_overlay
}

# Wildland Urban Interface
wui_plot <- function(wui, template_plot, template_plot_overlay) {
  #### Prepare layers for plotting ####
  bowen_wui <- wui %>%
    select(Name, PSTA) %>%
    rename(WUI = Name) %>%
    mutate(WUI_PSTA = paste0(WUI, " / ", PSTA))
  wui_factor_lvls <- c(
    "Water / 1-4",
    "No Data (Private Land) / 1-4",
    "Low / 1-4",
    "Moderate / 1-4",
    "Moderate / 5-6"
  )
  wui_factor <- bowen_wui %>%
    mutate(
      WUI_PSTA = factor(WUI_PSTA, levels = wui_factor_lvls)
    )
  wui_factor_cols <- c("lightblue", "#fff9cfff", "#fdef86ff", "#fcbb3aff", "#d95f0e")
  names(wui_factor_cols) <- wui_factor_lvls

  #### Plotting ####
  template_plot +
    geom_sf(data = bowen_wui, aes(fill = WUI_PSTA), colour = NA) +
    scale_fill_manual(
      values = wui_factor_cols,
      name = NULL,
      guide = guide_legend(order = 1)
    ) +
    ggnewscale::new_scale_fill() +
    ggnewscale::new_scale_colour() +
    template_plot_overlay +
    ggplot2::theme(
      legend.margin = margin(-9, 6, 6, 6),
      legend.key.width = ggplot2::unit(0.5, "cm"),
      legend.key.height = ggplot2::unit(0.5, "cm"),
      legend.key.spacing.y = ggplot2::unit(0, "lines"),
      legend.key.spacing.x = ggplot2::unit(0, "cm"),
      legend.direction = "vertical",
      legend.box = "vertical",
      legend.box.background = element_rect(fill = "white", colour = "black"),
      legend.box.margin = margin(20, 5, 5, 5),
      legend.position = c(.05, .95),
      legend.justification = c("left", "top"),
      legend.box.just = "left"
    )
}

# Subdivision Capacity
subd_capacity_plot <- function(parcelmap_subdiv, parcelmap, template_plot, template_plot_overlay) {
  template_plot +
    geom_sf(
      data = parcelmap,
      aes(geometry = geom, fill = "grey"),
      color = NA
    ) +
    scale_fill_identity(
      name = "No Capacity",
      breaks = c("grey40"),
      labels = c(""),
      guide = guide_legend(order = 2)
    ) +
    ggnewscale::new_scale_fill() +
    ggnewscale::new_scale_color() +
    geom_sf(
      data = parcelmap_subdiv,
      aes(fill = `Can Subdivide?`, geometry = geom),
      color = NA
    ) +
    scale_fill_distiller(
      palette = "RdPu",
      direction = 1,
      name = "Subdivision Capacity",
      guide = ggplot2::guide_colourbar(
        order = 1,
        nbin = 100,
        draw.ulim = FALSE,
        draw.llim = FALSE,
        title.position = "top"
        # title.hjust = 0.5
      )
    ) +
    template_plot_overlay
}

# Biodiversity Value by Parcel
biod_val_parcel_plot <- function(biod_val_parcel, template_plot, template_plot_overlay) {
  template_plot +
    geom_sf(
      data = biod_val_parcel,
      aes(fill = rankmap),
      color = NA
    ) +
    scale_fill_continuous(
      palette = "YlGn",
      name = "Relative Biodiversity / Parcel",
      breaks = c(
        min(biod_val_parcel$rankmap, na.rm = T),
        max(biod_val_parcel$rankmap, na.rm = T)
      ),
      labels = c("Lower", "Higher"),
      guide = ggplot2::guide_colourbar(
        order = 1,
        nbin = 100,
        draw.ulim = FALSE,
        draw.llim = FALSE,
        title.position = "top"
        # title.hjust = 0.5
      )
    ) +
    template_plot_overlay
}


# Wildfire Vulnerability Index: Top 30% Biodiversity Value overlapping Top 30% WVI
# ocean_sf is passed explicitly because higher_res_edges() references bowen_ocean_sf
# as a global, which is not available inside a targets execution.
wvi_top30_plot <- function(wvi, rankmap, ocean_sf, template_plot, template_plot_overlay) {
  top30_wvi <- wvi %>%
    terra::project("EPSG: 3857") %>%
    terra::subst(4294967296, NA) %>%
    terra::scale_linear() %>%
    remove_by_quantile(0.7)

  rankmap_top30_lowres <- rankmap %>%
    terra::project("EPSG: 3857") %>%
    remove_by_quantile(0.7)

  rankmap_top30_mask <- rankmap_top30_lowres %>%
    terra::not.na() %>%
    terra::classify(cbind(FALSE, NA))

  rankmap_mask_re <- rankmap_top30_mask %>%
    terra::resample(top30_wvi)

  grey_rast <- rankmap_mask_re %>%
    terra::disagg(fact = 10) %>%
    terra::mask(terra::vect(ocean_sf), inverse = TRUE) %>%
    as.factor()

  fire_biod_val <- terra::mask(top30_wvi, rankmap_mask_re)
  fire_range    <- as.numeric(terra::minmax(fire_biod_val))

  template_plot +
    tidyterra::geom_spatraster(data = grey_rast) +
    ggplot2::scale_fill_manual(
      values = c("grey40"),
      labels = c(""),
      na.value = NA,
      na.translate = FALSE,
      guide = "none",
      # guide = ggplot2::guide_legend(title.position = "top", order = 2),
      name = ""
    ) +
    ggnewscale::new_scale_fill() +
    ggnewscale::new_scale_colour() +
    tidyterra::geom_spatraster(data = fire_biod_val) +
    colorspace::scale_fill_continuous_sequential(
      na.value = NA,
      palette = "Batlow",
      rev = FALSE,
      begin = fire_range[1],
      end   = fire_range[2],
      limits = fire_range,
      breaks = fire_range,
      labels = c("Medium", "Higher"),
      guide = ggplot2::guide_colourbar(
        order = 1,
        nbin = 100,
        draw.ulim = FALSE,
        draw.llim = FALSE,
        title.position = "top"
      ),
      name = "Wildfire Vulnerability Index"
    ) +
    template_plot_overlay
}

#### Loading layers ####
# bowen_pa <- here(
#   "output-data/7_protected_areas/existing_protected_areas.gpkg"
# ) %>%
#   st_read()
# bowen_ogma <- ogma %>%
#   st_transform(st_crs(bowen_pa))
# bowen_new_pa <- here(
#   "output-data/7_protected_areas/new_protected_areas.gpkg"
# ) %>%
#   st_read() %>%
#   st_transform(st_crs(bowen_pa))
#### Statistics ####
# TODO: calculate % of orange protential protected areas coverage of bowen island

