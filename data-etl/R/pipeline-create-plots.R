## Example Candidate Locations for Protected Areas 
# Mapping optimal Crown land for reaching 30x30 targets, using the conservation values analysis and existing protected areas. This output considers connectivity by applying boundary penalties, which prioritizes shorter boundaries relative to area. Currently, about 20% of Bowen Island is under some protection.

candidate_pa_plot_annotated <- function(pa, pa_candidates, template_plot, template_plot_overlay, template_plot_annotations, template_plot_remove) {
  #### Plotting ####
  protected_colour <- "purple"
  ogma_colour <- "#a1d76a"
  new_colour <- "orange"
  protectedareas_example_plot_base <- template_plot +
  geom_sf(
    data = pa,
    aes(fill = protected_colour, color = protected_colour),
    alpha = 0.1,
    linewidth = 0.5
  ) +
  geom_sf(
    data = pa_candidates,
    aes(fill = new_colour, color = new_colour),
    alpha = 0.1,
    linewidth = 0.5
  ) +
  scale_fill_identity(
    name = "",
    breaks = c(protected_colour, ogma_colour, new_colour),
    labels = c(
      "Current Protected",
      "Old Growth Management Areas",
      "Proposed Protected"
    ),
    guide = guide_legend(order = 1)
  ) +
  scale_color_identity(
    name = "",
    breaks = c(protected_colour, ogma_colour, new_colour),
    labels = c(
      "Current Protected",
      "Old Growth Management Areas",
      "Proposed Protected"
    ),
    guide = guide_legend(order = 1)
  ) +
  template_plot_overlay() +
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
  pa_example_plot_annotations <- protectedareas_example_plot_base +
  template_plot_annotations()
  pa_example_plot_no_annotations <- protectedareas_example_plot_base +
  template_plot_remove()
  # Save plots
  ggsave_drive(
    here(output_dir_annotated, "7_8_candidate_protected_areas.png"),
    pa_example_plot_annotations,
    drive_folder_id_annotated,
    device = ragg::agg_png,
    width = 9,
    height = 12,
    units = "in",
    res = 300
  )
  ggsave_drive(
    here(
      output_dir_unannotated,
      "7_8_candidate_protected_areas_no_annotation.png"
    ),
    pa_example_plot_no_annotations,
    drive_folder_id_unannotated,
    device = ragg::agg_png,
    width = 9,
    height = 12,
    units = "in",
    res = 300
  )
}

save_ggplot <- function(plot, path) {

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

