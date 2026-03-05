## Example Candidate Locations for Protected Areas
candidate_pa_plot <- function(pa, pa_candidates, tmpl, overlay) {
  #### Plotting ####
  protected_colour <- "purple"
  ogma_colour <- "#a1d76a"
  new_colour <- "orange"
  tmpl +
  geom_sf(
    data = pa,
    aes(fill = protected_colour, color = protected_colour),
    alpha = 0.4,
    linewidth = 0.7
  ) +
  geom_sf(
    data = pa_candidates,
    aes(fill = new_colour, color = new_colour),
    alpha = 0.4,
    linewidth = 0.7
  ) +
  scale_fill_identity(
    name = "",
    breaks = c(protected_colour, new_colour),
    labels = c(
      "Existing Protected Areas",
      "Candidate Protected Areas (Crown Lands Only)"
    ),
    guide = guide_legend(order = 1)
  ) +
  scale_color_identity(
    name = "",
    breaks = c(protected_colour, new_colour),
    labels = c(
      "Existing Protected Areas",
      "Candidate Protected Areas (Crown Lands Only)"
    ),
    guide = guide_legend(order = 1)
  ) +
  overlay +
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
wildfire_vulnerability_plot <- function(wvi, tmpl, overlay) {
  #### Preparing raster for plotting ####
  fire_index_mp <- wvi %>%
    project("EPSG: 3857") %>%
    terra::subst(4294967296, NA) %>% # Max value should be NA
    terra::scale_linear()
  #### Plotting ####
  # TODO: change colour palette from cool to warm
  tmpl +
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
    overlay
}

# Wildland Urban Interface
wildland_urban_interface_plot <- function(wui, tmpl, overlay) {
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
  tmpl +
    geom_sf(data = bowen_wui, aes(fill = WUI_PSTA), colour = NA) +
    scale_fill_manual(
      values = wui_factor_cols,
      name = NULL,
      guide = guide_legend(order = 1)
    ) +
    ggnewscale::new_scale_fill() +
    ggnewscale::new_scale_colour() +
    overlay +
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
subdivision_capacity_plot <- function(parcelmap_subdiv, parcelmap, tmpl, overlay) {
  tmpl +
    # geom_sf(
    #   data = parcelmap,
    #   aes(geometry = geom, fill = "grey"),
    #   color = NA
    # ) +
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
    overlay
}

# Biodiversity Value by Parcel
parcel_biodiversity_plot <- function(biod_val_parcel, tmpl, overlay) {
  tmpl +
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
    overlay
}


# Wildfire Vulnerability Index: Top 30% Biodiversity Value overlapping Top 30% WVI
wildfire_vulnerability_top30_plot <- function(wvi, rankmap, ocean_sf, tmpl, overlay) {
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

  grey_rast <- higher_res_edges(rankmap_mask_re, ocean_sf) %>%
    as.factor()

  fire_biod_val <- terra::mask(top30_wvi, rankmap_mask_re)
  fire_range    <- as.numeric(terra::minmax(fire_biod_val))

  tmpl +
    tidyterra::geom_spatraster(data = grey_rast) +
    ggplot2::scale_fill_manual(
      values = c("grey40"),
      labels = c(""),
      na.value = NA,
      na.translate = FALSE,
      # guide = "none",
      guide = ggplot2::guide_legend(title.position = "top", order = 2),
      name = "Low Vulnerability"
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
    overlay
}

# Protected Areas: Top 30% Biodiversity Values
protected_areas_top30_plot <- function(rankmap, pa, ocean_sf, tmpl, overlay) {
  rankmap_top30 <- rankmap %>%
    remove_by_quantile(0.7) %>%
    terra::project("EPSG: 3857") %>%
    higher_res_edges(ocean_sf)

  union_pa <- pa %>%
    sf::st_make_valid() %>%
    sf::st_union()

  protectedareas_colour <- "purple"

  tmpl +
    tidyterra::geom_spatraster(data = rankmap_top30) +
    colorspace::scale_fill_continuous_sequential(
      na.value = NA,
      palette = "ag_GrnYl",
      limits = c(0, 1),
      breaks = c(0, 1),
      labels = c("Lower", "Higher"),
      guide = ggplot2::guide_colourbar(
        order = 1,
        nbin = 100,
        draw.ulim = FALSE,
        draw.llim = FALSE,
        title.position = "top"
      ),
      name = "Relative Biodiversity"
    ) +
    ggnewscale::new_scale_fill() +
    ggnewscale::new_scale_colour() +
    ggplot2::geom_sf(
      data = union_pa,
      aes(fill = protectedareas_colour, color = protectedareas_colour),
      alpha = 0.4,
      linewidth = 0.7
    ) +
    ggplot2::scale_fill_identity(
      name = "",
      breaks = c(protectedareas_colour),
      labels = c("Protected Areas"),
      guide = "legend"
    ) +
    ggplot2::scale_colour_identity(
      name = "",
      breaks = c(protectedareas_colour),
      labels = c("Protected Areas"),
      guide = "legend"
    ) +
    overlay
}

# Species Richness
species_richness_plot <- function(species_richness, ocean_sf, tmpl, overlay) {
  species_richness_mp <- species_richness %>%
    terra::project("EPSG: 3857") %>%
    higher_res_edges(ocean_sf)
  tmpl +
    tidyterra::geom_spatraster(data = species_richness_mp) +
    colorspace::scale_fill_continuous_sequential(
      name = "Species Richness",
      na.value = NA,
      palette = "ag_GrnYl",
      guide = ggplot2::guide_colourbar(
        order = 1,
        nbin = 100,
        draw.ulim = FALSE,
        draw.llim = FALSE,
        title.position = "top"
      )
    ) +
    overlay
}

# Freshwater Habitat Richness
fw_richness_plot <- function(fw_richness, ocean_sf, tmpl, overlay) {
  fw_richness_mp <- fw_richness %>%
    terra::project("EPSG: 3857") %>%
    higher_res_edges(ocean_sf)
  tmpl +
    tidyterra::geom_spatraster(data = fw_richness_mp) +
    colorspace::scale_fill_continuous_sequential(
      name = "Freshwater Habitat Richness",
      na.value = NA,
      palette = "Mako",
      guide = ggplot2::guide_colourbar(
        order = 1,
        nbin = 100,
        draw.ulim = FALSE,
        draw.llim = FALSE,
        title.position = "top"
      )
    ) +
    overlay
}

# Total Habitat Richness
habitat_richness_plot <- function(habitat_richness, ocean_sf, tmpl, overlay) {
  habitat_richness_mp <- habitat_richness %>%
    terra::project("EPSG: 3857") %>%
    higher_res_edges(ocean_sf)
  tmpl +
    tidyterra::geom_spatraster(data = habitat_richness_mp) +
    colorspace::scale_fill_continuous_sequential(
      name = "Habitat Richness",
      na.value = NA,
      palette = "ag_GrnYl",
      guide = ggplot2::guide_colourbar(
        order = 1,
        nbin = 100,
        draw.ulim = FALSE,
        draw.llim = FALSE,
        title.position = "top"
      )
    ) +
    overlay
}

# Biodiversity Values (Conservation Values)
biodiversity_values_plot <- function(rankmap, ocean_sf, tmpl, overlay) {
  rankmap_mp <- rankmap %>%
    terra::project("EPSG: 3857") %>%
    higher_res_edges(ocean_sf)
  tmpl +
    tidyterra::geom_spatraster(data = rankmap_mp) +
    colorspace::scale_fill_continuous_sequential(
      name = "Relative Biodiversity",
      na.value = NA,
      palette = "ag_GrnYl",
      limits = c(0, 1),
      breaks = c(0, 1),
      labels = c("Lower", "Higher"),
      guide = ggplot2::guide_colourbar(
        order = 1,
        nbin = 100,
        draw.ulim = FALSE,
        draw.llim = FALSE,
        title.position = "top"
      )
    ) +
    overlay
}

# Ecological Intactness (Human Footprint: Continuous)
ecological_intactness_plot <- function(human_footprint, ocean_sf, tmpl, overlay) {
  hfp_mp <- human_footprint %>%
    terra::project("EPSG: 3857") %>%
    higher_res_edges(ocean_sf)
  max_hfp <- terra::global(human_footprint, fun = "max", na.rm = TRUE) %>% unlist()
  min_hfp <- terra::global(human_footprint, fun = "min", na.rm = TRUE) %>% unlist()
  tmpl +
    tidyterra::geom_spatraster(data = hfp_mp) +
    colorspace::scale_fill_continuous_sequential(
      name = "Intactness",
      na.value = NA,
      palette = "Inferno",
      rev = FALSE,
      limits = c(min_hfp, max_hfp),
      breaks = c(min_hfp, max_hfp),
      labels = c("Higher", "Lower"),
      guide = ggplot2::guide_colourbar(
        order = 1,
        nbin = 100,
        draw.ulim = FALSE,
        draw.llim = FALSE,
        title.position = "top"
      )
    ) +
    overlay
}

# Ecological Intactness: Top 30% Biodiversity Values
ecological_intactness_top30_plot <- function(human_footprint, rankmap, ocean_sf, tmpl, overlay) {
  rankmap_top30_mask <- rankmap %>%
    remove_by_quantile(0.7) %>%
    terra::not.na() %>%
    terra::classify(cbind(FALSE, NA))
  hfp_top30 <- human_footprint %>%
    terra::project(rankmap_top30_mask) %>%
    terra::mask(rankmap_top30_mask) %>%
    terra::project("EPSG: 3857") %>%
    higher_res_edges(ocean_sf)
  max_hfp <- terra::global(human_footprint, fun = "max", na.rm = TRUE) %>% unlist()
  min_hfp <- terra::global(human_footprint, fun = "min", na.rm = TRUE) %>% unlist()
  tmpl +
    tidyterra::geom_spatraster(data = hfp_top30) +
    colorspace::scale_fill_continuous_sequential(
      name = "Intactness",
      na.value = NA,
      palette = "Inferno",
      limits = c(min_hfp, max_hfp),
      breaks = c(min_hfp, max_hfp),
      labels = c("Higher", "Lower"),
      guide = ggplot2::guide_colourbar(
        order = 1,
        nbin = 100,
        draw.ulim = FALSE,
        draw.llim = FALSE,
        title.position = "top"
      )
    ) +
    overlay
}

# Private Land: Top 30% Biodiversity Values
private_land_top30_plot <- function(privateland, rankmap, ocean_sf, tmpl, overlay) {
  rankmap_top30 <- rankmap %>%
    remove_by_quantile(0.7) %>%
    terra::project("EPSG: 3857") %>%
    higher_res_edges(ocean_sf)
  properties_colour <- "brown"
  tmpl +
    tidyterra::geom_spatraster(data = rankmap_top30) +
    colorspace::scale_fill_continuous_sequential(
      na.value = NA,
      palette = "ag_GrnYl",
      limits = c(0, 1),
      breaks = c(0, 1),
      labels = c("Lower", "Higher"),
      guide = ggplot2::guide_colourbar(
        order = 1,
        nbin = 100,
        draw.ulim = FALSE,
        draw.llim = FALSE,
        title.position = "top"
      ),
      name = "Relative Biodiversity"
    ) +
    ggnewscale::new_scale_fill() +
    ggnewscale::new_scale_colour() +
    ggplot2::geom_sf(
      data = privateland,
      aes(fill = properties_colour, color = properties_colour),
      alpha = 0.4,
      linewidth = 0.7
    ) +
    ggplot2::scale_fill_identity(
      name = "",
      breaks = c(properties_colour),
      labels = c("Private Land"),
      guide = "legend"
    ) +
    ggplot2::scale_colour_identity(
      name = "",
      breaks = c(properties_colour),
      labels = c("Private Land"),
      guide = "legend"
    ) +
    overlay
}

# Public Land (Crown): Top 30% Biodiversity Values
public_land_top30_plot <- function(unprotected_crown, rankmap, ocean_sf, tmpl, overlay) {
  rankmap_top30 <- rankmap %>%
    remove_by_quantile(0.7) %>%
    terra::project("EPSG: 3857") %>%
    higher_res_edges(ocean_sf)
  crown_colour <- "green"
  tmpl +
    tidyterra::geom_spatraster(data = rankmap_top30) +
    colorspace::scale_fill_continuous_sequential(
      na.value = NA,
      palette = "ag_GrnYl",
      limits = c(0, 1),
      breaks = c(0, 1),
      labels = c("Lower", "Higher"),
      guide = ggplot2::guide_colourbar(
        order = 1,
        nbin = 100,
        draw.ulim = FALSE,
        draw.llim = FALSE,
        title.position = "top"
      ),
      name = "Relative Biodiversity"
    ) +
    ggnewscale::new_scale_fill() +
    ggnewscale::new_scale_colour() +
    ggplot2::geom_sf(
      data = unprotected_crown,
      aes(fill = crown_colour, color = crown_colour),
      alpha = 0.4,
      linewidth = 0.7
    ) +
    ggplot2::scale_fill_identity(
      name = "",
      breaks = c(crown_colour),
      labels = c("Other Public Lands"),
      guide = "legend"
    ) +
    ggplot2::scale_colour_identity(
      name = "",
      breaks = c(crown_colour),
      labels = c("Other Public Lands"),
      guide = "legend"
    ) +
    overlay
}

# Land Ownership: Top 30% Biodiversity Values
land_ownership_top30_plot <- function(land_ownership_rast, rankmap, ocean_sf, tmpl, overlay) {
  top30_mask <- rankmap %>%
    remove_by_quantile(0.7) %>%
    terra::not.na() %>%
    terra::classify(cbind(FALSE, NA)) %>%
    terra::resample(land_ownership_rast, method = "near")
  comb_rast_top30_for_map <- terra::mask(land_ownership_rast, top30_mask) %>%
    terra::as.factor() %>%
    terra::project("EPSG: 3857") %>%
    higher_res_edges(ocean_sf) %>%
    terra::as.factor()
  authority_factor_lvls <- c(1, 2, 3, 4)
  authority_factor_cols <- c("brown", "purple", "lightgreen", "blue")
  names(authority_factor_cols) <- authority_factor_lvls
  tmpl +
    tidyterra::geom_spatraster(
      data = comb_rast_top30_for_map,
      aes(fill = layer),
      alpha = 0.3,
      na.rm = TRUE
    ) +
    scale_fill_manual(
      values = authority_factor_cols,
      aesthetics = c("colour", "fill"),
      labels = c("Private", "Protected", "Public", "Mixed"),
      name = "",
      na.value = NA,
      na.translate = FALSE,
      guide = guide_legend(order = 1)
    ) +
    overlay +
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

# Land Ownership / Authority
land_ownership_plot <- function(land_ownership_rast, ocean_sf, tmpl, overlay) {
  comb_rast_for_map <- land_ownership_rast %>%
    terra::as.factor() %>%
    terra::project("EPSG: 3857") %>%
    higher_res_edges(ocean_sf) %>%
    terra::as.factor()

  authority_factor_lvls <- c(1, 2, 3, 4)
  authority_factor_cols <- c("brown", "purple", "lightgreen", "blue")
  names(authority_factor_cols) <- authority_factor_lvls

  # TODO: investigate why gaps between the land ownership types appear
  # When terra::plot(), the comb_rast_for_map looks as expected
  tmpl +
    tidyterra::geom_spatraster(
      data = comb_rast_for_map,
      aes(fill = layer),
      alpha = 0.3,
      na.rm = TRUE
    ) +
    scale_fill_manual(
      values = authority_factor_cols,
      aesthetics = c("colour", "fill"),
      labels = c("Private", "Protected", "Public", "Mixed"),
      name = "",
      na.value = NA,
      na.translate = FALSE,
      guide = guide_legend(order = 1)
    ) +
    overlay +
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
