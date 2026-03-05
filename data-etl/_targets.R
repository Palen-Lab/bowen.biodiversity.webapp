library(targets)
library(tarchetypes)
library(geotargets)
library(here)

tar_option_set(
  packages = c(
    "sf",
    "terra",
    "tidyterra",
    "dplyr",
    "here",
    "magrittr",
    "stringr",
    "glue",
    "basemaps",
    "ggplot2",
    "googledrive",
    "ggspatial",
    "ggnewscale",
    "rmapshaper",
    "readxl",
    "colorspace"
  )
)
tar_source()

list(

  # SETUP ####
  # Project constants, Google Drive folder IDs, and local output directories.

  tar_target(project_crs, load_project_crs()),

  ## Google Drive folder IDs — one per data category ####
  tar_target(drive_folder_id_base,            {"1zX3cteFb9JrM3r3C1R3gNHMqvjr94yf6"}),
  tar_target(drive_folder_id_species,         {"1HJEQmrMzXctbdDvR5fG6uslF-4QrenFY"}),
  tar_target(drive_folder_id_habitats,        {"184f9InZ2jEgmnwNiegsx2Y64hbxuBke6"}),
  tar_target(drive_folder_id_people,          {"1YyXSEklX8RFdd2e_5F2BzOwIwWm7lSi5"}),
  tar_target(drive_folder_id_values,          {"1pYZjm8dyazPm8lvZCAXn46RT0Rx_4w5L"}),
  tar_target(drive_folder_id_threats,         {"1eA-6hmVSRO8ZuQMdIlQ5t4XF2xYEnMs3"}),
  tar_target(drive_folder_id_land_management, {"1c7T_1GLAAylUPByJRZhSnWNg-kyH2UNq"}),

  ## Output figure folder IDs — annotated and unannotated versions ####
  tar_target(drive_folder_id_annotated,   {"1mi0iC0OKSJ-x3nC0AI0tZjpJy_yN7-JJ"}),
  tar_target(drive_folder_id_unannotated, {"1XkDA2Oc4zNQquNM3MbNV9To7GyE8BKA8"}),

  ## Local output directories for saving figure PNGs ####
  tar_target(output_dir,             here("output-figures/data-atlas")),
  tar_target(output_dir_annotated,   here(output_dir, "annotated")),
  tar_target(output_dir_unannotated, here(output_dir, "unannotated")),

  # 1: BASE ####
  # Spatial base layers shared across all sections: boundary, shoreline, roads,
  # trails, raster mask, ocean polygon, and the background basemap tile.
  # format = "file" on path targets triggers downstream re-runs on file changes.

  ## Boundary
  tar_target(boundary_path, here("data-1-raw/datasets/boundary/Bowen_boundary.shp"), format = "file"),
  tar_target(boundary, load_boundary(boundary_path, project_crs)),
  tar_target(boundary_upload, upload_gdrive(boundary, boundary_path, drive_folder_id_base), format = "file"),

  ## Shoreline
  tar_target(shoreline_path, here("data-1-raw/datasets/bowen_island_shoreline_w_hutt.gpkg"), format = "file"),
  tar_target(shoreline, load_shoreline(shoreline_path, project_crs)),
  tar_target(shoreline_upload, upload_gdrive(shoreline, shoreline_path, drive_folder_id_base), format = "file"),

  ## Roads
  tar_target(roads_path, here("data-1-raw/datasets/roads/Bowen_Road_Inventory.shp"), format = "file"),
  tar_target(roads, load_roads(roads_path, project_crs)),
  tar_target(roads_upload, upload_gdrive(roads, roads_path, drive_folder_id_base), format = "file"),

  ## Trails
  tar_target(trails_path, here("data-1-raw/datasets/trails/Trails.shp"), format = "file"),
  tar_target(trails, load_trails(trails_path, project_crs)),
  tar_target(trails_upload, upload_gdrive(trails, trails_path, drive_folder_id_base), format = "file"),

  ## Derived — raster mask and ocean polygon used as plot backgrounds
  tar_terra_rast(mask, create_mask(shoreline, zoning, project_crs)),
  tar_target(ocean_sf, create_ocean(mask, shoreline)),

  ## Basemap — cached satellite/backdrop tile used as the background in all map plots
  tar_terra_rast(
    basemap,
    {
      basemaps::basemap_terra(
        ext = mask,
        map_service = "maptiler",
        map_type = "backdrop",
        map_token = "baL4WLstSFqSHP2fnYrE"
      ) %>%
        mask(vect(ocean_sf), inverse = T)
    }
  ),

  # 3: FOCAL VERTEBRATE SPECIES ####
  # SDM-derived species richness raster: total number of predicted species per cell.

  ## Species Richness (→ figure 3_1)
  tar_target(species_richness_path, here("data-3-outputs/2_species/total_richness.tif"), format = "file"),
  tar_terra_rast(species_richness, {rast(species_richness_path)}),
  tar_target(species_richness_upload, upload_gdrive(species_richness, species_richness_path, drive_folder_id_species, name = "species_richness.tif"), format = "file"),

  # 4: HABITATS ####
  # Richness rasters for freshwater habitats and all habitat types combined.

  ## Freshwater Habitat Richness (→ figure 4_2)
  tar_target(fw_richness_path, here("data-3-outputs/3_habitats/fw_richness.tif"), format = "file"),
  tar_terra_rast(fw_richness, {rast(fw_richness_path)}),
  tar_target(fw_richness_upload, upload_gdrive(fw_richness, fw_richness_path, drive_folder_id_habitats, name = "fw_richness.tif"), format = "file"),

  ## Total Habitat Richness (→ figure 4_3)
  tar_target(habitat_richness_path, here("data-3-outputs/3_habitats/total_habitat_richness.tif"), format = "file"),
  tar_terra_rast(habitat_richness, {rast(habitat_richness_path)}),
  tar_target(habitat_richness_upload, upload_gdrive(habitat_richness, habitat_richness_path, drive_folder_id_habitats, name = "habitat_richness.tif"), format = "file"),

  # 5: CONSERVATION VALUES ####
  # Zonation priority rankmap combining species and habitat inputs into a single
  # conservation value score per cell (0 = lowest, 1 = highest priority).

  ## Conservation Value Rankmap (→ figure 5_1)
  tar_target(rankmap_path, here("data-3-outputs/5_values/rankmap.tif"), format = "file"),
  tar_terra_rast(rankmap, load_rankmap(rankmap_path, project_crs)),
  tar_target(rankmap_upload, upload_gdrive(rankmap, rankmap_path, drive_folder_id_values, name = "conservation_values.tif"), format = "file"),

  # 6: THREATS ####
  # Human disturbance (ecological intactness), wildfire vulnerability index,
  # and wildland-urban interface layers.

  ## Human Footprint / Ecological Intactness (→ figures 6_1, 6_2)
  tar_target(human_footprint_path, here("data-3-outputs/4_people/bowen_human_footprint.tif"), format = "file"),
  tar_terra_rast(human_footprint, {rast(human_footprint_path)}),
  tar_target(human_footprint_upload, upload_gdrive(human_footprint, human_footprint_path, drive_folder_id_people, name = "human_footprint.tif"), format = "file"),

  ## Wildfire Vulnerability Index (→ figures 6_3, 6_4)
  tar_target(fire_index_path, here("data-3-outputs/6_threats/fire_index_40m.tif"), format = "file"),
  tar_terra_rast(fire_index, {rast(fire_index_path)}),
  tar_target(fire_index_upload, upload_gdrive(fire_index, fire_index_path, drive_folder_id_threats, name = "wildfire_vulnerability_index.tif"), format = "file"),

  ## Wildland Urban Interface (→ figure 6_5)
  tar_target(wui_path, here("data-1-raw/datasets/bc_wui/wui.gpkg"), format = "file"),
  tar_target(wui, {st_read(wui_path, quiet = TRUE)}),
  tar_target(wui_upload, upload_gdrive(wui, wui_path, drive_folder_id_threats), format = "file"),

  # 7: LAND MANAGEMENT ####
  # Zoning, protected areas, crown land, private parcels, subdivision potential,
  # land ownership raster, and candidate protected area locations.

  ## Zoning
  tar_target(zoning_path, here("data-1-raw/datasets/zoning/BM_ZONING.shp"), format = "file"),
  tar_target(zoning, load_zoning(zoning_path, project_crs)),
  tar_target(zoning_upload, upload_gdrive(zoning, zoning_path, drive_folder_id_land_management), format = "file"),

  ## Protected Areas
  tar_target(pa, load_protected_areas(project_crs)),
  tar_target(pa_export, {
    pa %>%
      st_drop_geometry() %>%
      write.csv(file = here("data-3-outputs/protectedareas.csv"))
  }, format = "file"),
  tar_target(dissolved_pa, dissolve_protected_areas(pa)),
  # tar_target(ogma, load_ogma(project_crs)),

  ## Crown (Public) Land
  tar_target(crown_path, here("data-1-raw/datasets/protectedareas/Crown-Land-JD.gpkg"), format = "file"),
  tar_target(crown, load_crown(crown_path, project_crs)),
  tar_target(crown_upload, upload_gdrive(crown, crown_path, drive_folder_id_land_management), format = "file"),
  tar_target(unprotected_crown, create_unprotected_crown(crown, dissolved_pa)),

  ## Parcelmap (Private Land)
  tar_target(parcelmap_path, here("data-1-raw/datasets/parcelmap_bowen/parcelmap_bowen.gpkg"), format = "file"),
  tar_target(parcelmap, load_parcelmap(parcelmap_path, project_crs)),
  tar_target(parcelmap_upload, upload_gdrive(parcelmap, parcelmap_path, drive_folder_id_land_management), format = "file"),
  tar_target(privateland, create_privateland(parcelmap, dissolved_pa)),

  ## Biodiversity Value by Parcel (→ figure 7_1)
  tar_target(biod_val_parcel, compute_parcel_biod_val(parcelmap_subdiv, rankmap)),

  ## Subdivision Potential (→ figure 7_2)
  tar_target(subdiv_path, here("data-1-raw/datasets/development_potential_danielmartin/zoning subdivision potential.xlsx"), format = "file"),
  tar_target(parcelmap_subdiv, create_parcel_subdiv(parcelmap, subdiv_path)),

  ## Protected Areas: Top 30% Coverage Stats (→ figure 7_3)
  tar_target(pa_top30_coverage, compute_pa_top30_coverage(pa, rankmap)),

  ## Candidate Protected Areas (→ figure 7_8)
  ## TODO: manual step — move this to post-Zonation calculation
  tar_target(pa_candidates_path, here("data-3-outputs/7_protected_areas/new_protected_areas.gpkg"), format = "file"),
  tar_target(pa_candidates, load_pa_candidates(pa_candidates_path, project_crs)),
  tar_target(pa_candidates_upload, upload_gdrive(pa_candidates, pa_candidates_path, drive_folder_id_land_management, name = "candidate_protected_areas.gpkg"), format = "file"),

  ## Land Ownership / Authority Raster (→ figures 7_6, 7_7)
  # Categories: 1 = Private, 2 = Protected Areas, 3 = Crown Land, 4 = Mixed
  tar_terra_rast(land_ownership_rast, create_land_ownership_rast(pa, unprotected_crown, privateland, rankmap)),
  ## Cell counts and percent coverage for each ownership category (overall and top-30% BV)
  tar_target(land_ownership_combined_stats, {
    land_ownership_stats       <- compute_land_ownership_stats(land_ownership_rast)
    land_ownership_top30_stats <- compute_land_ownership_top30_stats(land_ownership_rast, rankmap)
    combined <- bind_rows(land_ownership_stats, land_ownership_top30_stats)
    combined$description <- c("total", "top_30%_conservation_values")
    combined
  }),
  ## Save stats CSV to disk and upload
  tar_target(land_ownership_stats_path, here("data-3-outputs/7_land_management/land_ownership_stats.csv")),
  tar_target(land_ownership_stats_save, {write.csv(land_ownership_combined_stats, land_ownership_stats_path, row.names = FALSE); land_ownership_stats_path}, format = "file"),
  tar_target(land_ownership_stats_upload, upload_gdrive(land_ownership_stats_save, land_ownership_stats_path, drive_folder_id_land_management, name = "land_ownership_stats.csv"), format = "file"),
  ## Save raster to disk and upload
  tar_target(land_ownership_rast_path, {here("data-3-outputs/7_land_management/land_ownership_rast.tif")}),
  tar_target(land_ownership_rast_save, {writeRaster(land_ownership_rast, land_ownership_rast_path, overwrite = T)}),
  tar_target(land_ownership_rast_upload, {upload_gdrive(land_ownership_rast, land_ownership_rast_path, drive_folder_id_land_management, name = "land_ownership.tif")}, format = "file"),

  # OUTPUT FIGURES ####
  # Annotated and unannotated PNG exports for every atlas figure, in atlas section order.
  # Each figure produces 4 targets: annotated, annotated_upload, unannotated, unannotated_upload.

  # 3_1: Species Richness ----
  tar_target(
    species_richness_annotated,
    {
      tmpl <- template_plot(mask, ocean_sf, basemap)
      overlay <- template_plot_overlay(ocean_sf, trails, roads)
      p <- species_richness_plot(species_richness, ocean_sf, tmpl, overlay)
      path <- here(output_dir_annotated, "3_1_species_richness.png")
      ggsave_template(path, add_annotation(p))
    },
    format = "file"
  ),
  tar_target(species_richness_annotated_upload, upload_gdrive(species_richness_annotated, species_richness_annotated, drive_folder_id_annotated), format = "file"),
  tar_target(
    species_richness_unannotated,
    {
      tmpl <- template_plot(mask, ocean_sf, basemap)
      overlay <- template_plot_overlay(ocean_sf, trails, roads)
      p <- species_richness_plot(species_richness, ocean_sf, tmpl, overlay)
      path <- here(output_dir_unannotated, "3_1_species_richness_no_annotation.png")
      ggsave_template(path, remove_annotation(p))
    },
    format = "file"
  ),
  tar_target(species_richness_unannotated_upload, upload_gdrive(species_richness_unannotated, species_richness_unannotated, drive_folder_id_unannotated), format = "file"),

  # 4_2: Freshwater Habitat Richness ----
  tar_target(
    fw_richness_annotated,
    {
      tmpl <- template_plot(mask, ocean_sf, basemap)
      overlay <- template_plot_overlay(ocean_sf, trails, roads)
      p <- fw_richness_plot(fw_richness, ocean_sf, tmpl, overlay)
      path <- here(output_dir_annotated, "4_2_freshwater_richness.png")
      ggsave_template(path, add_annotation(p))
    },
    format = "file"
  ),
  tar_target(fw_richness_annotated_upload, upload_gdrive(fw_richness_annotated, fw_richness_annotated, drive_folder_id_annotated), format = "file"),
  tar_target(
    fw_richness_unannotated,
    {
      tmpl <- template_plot(mask, ocean_sf, basemap)
      overlay <- template_plot_overlay(ocean_sf, trails, roads)
      p <- fw_richness_plot(fw_richness, ocean_sf, tmpl, overlay)
      path <- here(output_dir_unannotated, "4_2_freshwater_richness_no_annotation.png")
      ggsave_template(path, remove_annotation(p))
    },
    format = "file"
  ),
  tar_target(fw_richness_unannotated_upload, upload_gdrive(fw_richness_unannotated, fw_richness_unannotated, drive_folder_id_unannotated), format = "file"),

  # 4_3: Total Habitat Richness ----
  tar_target(
    habitat_richness_annotated,
    {
      tmpl <- template_plot(mask, ocean_sf, basemap)
      overlay <- template_plot_overlay(ocean_sf, trails, roads)
      p <- habitat_richness_plot(habitat_richness, ocean_sf, tmpl, overlay)
      path <- here(output_dir_annotated, "4_3_habitat_richness.png")
      ggsave_template(path, add_annotation(p))
    },
    format = "file"
  ),
  tar_target(habitat_richness_annotated_upload, upload_gdrive(habitat_richness_annotated, habitat_richness_annotated, drive_folder_id_annotated), format = "file"),
  tar_target(
    habitat_richness_unannotated,
    {
      tmpl <- template_plot(mask, ocean_sf, basemap)
      overlay <- template_plot_overlay(ocean_sf, trails, roads)
      p <- habitat_richness_plot(habitat_richness, ocean_sf, tmpl, overlay)
      path <- here(output_dir_unannotated, "4_3_habitat_richness_no_annotation.png")
      ggsave_template(path, remove_annotation(p))
    },
    format = "file"
  ),
  tar_target(habitat_richness_unannotated_upload, upload_gdrive(habitat_richness_unannotated, habitat_richness_unannotated, drive_folder_id_unannotated), format = "file"),

  # 5_1: Biodiversity Values (Conservation Values) ----
  tar_target(
    biodiversity_values_annotated,
    {
      tmpl <- template_plot(mask, ocean_sf, basemap)
      overlay <- template_plot_overlay(ocean_sf, trails, roads)
      p <- biodiversity_values_plot(rankmap, ocean_sf, tmpl, overlay)
      path <- here(output_dir_annotated, "5_1_biodiversity_values.png")
      ggsave_template(path, add_annotation(p))
    },
    format = "file"
  ),
  tar_target(biodiversity_values_annotated_upload, upload_gdrive(biodiversity_values_annotated, biodiversity_values_annotated, drive_folder_id_annotated), format = "file"),
  tar_target(
    biodiversity_values_unannotated,
    {
      tmpl <- template_plot(mask, ocean_sf, basemap)
      overlay <- template_plot_overlay(ocean_sf, trails, roads)
      p <- biodiversity_values_plot(rankmap, ocean_sf, tmpl, overlay)
      path <- here(output_dir_unannotated, "5_1_biodiversity_values_no_annotation.png")
      ggsave_template(path, remove_annotation(p))
    },
    format = "file"
  ),
  tar_target(biodiversity_values_unannotated_upload, upload_gdrive(biodiversity_values_unannotated, biodiversity_values_unannotated, drive_folder_id_unannotated), format = "file"),

  # 6_1: Ecological Intactness (Human Footprint: Continuous) ----
  tar_target(
    ecological_intactness_annotated,
    {
      tmpl <- template_plot(mask, ocean_sf, basemap)
      overlay <- template_plot_overlay(ocean_sf, trails, roads)
      p <- ecological_intactness_plot(human_footprint, ocean_sf, tmpl, overlay)
      path <- here(output_dir_annotated, "6_1_ecological_intactness.png")
      ggsave_template(path, add_annotation(p))
    },
    format = "file"
  ),
  tar_target(ecological_intactness_annotated_upload, upload_gdrive(ecological_intactness_annotated, ecological_intactness_annotated, drive_folder_id_annotated), format = "file"),
  tar_target(
    ecological_intactness_unannotated,
    {
      tmpl <- template_plot(mask, ocean_sf, basemap)
      overlay <- template_plot_overlay(ocean_sf, trails, roads)
      p <- ecological_intactness_plot(human_footprint, ocean_sf, tmpl, overlay)
      path <- here(output_dir_unannotated, "6_1_ecological_intactness_no_annotation.png")
      ggsave_template(path, remove_annotation(p))
    },
    format = "file"
  ),
  tar_target(ecological_intactness_unannotated_upload, upload_gdrive(ecological_intactness_unannotated, ecological_intactness_unannotated, drive_folder_id_unannotated), format = "file"),

  # 6_2: Ecological Intactness: Top 30% Biodiversity Values ----
  tar_target(
    ecological_intactness_top30_annotated,
    {
      tmpl <- template_plot(mask, ocean_sf, basemap)
      overlay <- template_plot_overlay(ocean_sf, trails, roads)
      p <- ecological_intactness_top30_plot(human_footprint, rankmap, ocean_sf, tmpl, overlay)
      path <- here(output_dir_annotated, "6_2_ecological_intactness_top30.png")
      ggsave_template(path, add_annotation(p))
    },
    format = "file"
  ),
  tar_target(ecological_intactness_top30_annotated_upload, upload_gdrive(ecological_intactness_top30_annotated, ecological_intactness_top30_annotated, drive_folder_id_annotated), format = "file"),
  tar_target(
    ecological_intactness_top30_unannotated,
    {
      tmpl <- template_plot(mask, ocean_sf, basemap)
      overlay <- template_plot_overlay(ocean_sf, trails, roads)
      p <- ecological_intactness_top30_plot(human_footprint, rankmap, ocean_sf, tmpl, overlay)
      path <- here(output_dir_unannotated, "6_2_ecological_intactness_top30_no_annotation.png")
      ggsave_template(path, remove_annotation(p))
    },
    format = "file"
  ),
  tar_target(ecological_intactness_top30_unannotated_upload, upload_gdrive(ecological_intactness_top30_unannotated, ecological_intactness_top30_unannotated, drive_folder_id_unannotated), format = "file"),

  # 6_3: Wildfire Vulnerability Index ----
  tar_target(
    fire_index_plot_annotated,
    {
      tmpl <- template_plot(mask, ocean_sf, basemap)
      overlay <- template_plot_overlay(ocean_sf, trails, roads)
      p <- wildfire_vulnerability_plot(fire_index, tmpl, overlay)
      path <- here(output_dir_annotated, "6_3_wildfire_vulnerability.png")
      ggsave_template(path, add_annotation(p))
    },
    format = "file"
  ),
  tar_target(fire_index_plot_annotated_upload, upload_gdrive(fire_index_plot_annotated, fire_index_plot_annotated, drive_folder_id_annotated), format = "file"),
  tar_target(
    fire_index_plot_unannotated,
    {
      tmpl <- template_plot(mask, ocean_sf, basemap)
      overlay <- template_plot_overlay(ocean_sf, trails, roads)
      p <- wildfire_vulnerability_plot(fire_index, tmpl, overlay)
      path <- here(output_dir_unannotated, "6_3_wildfire_vulnerability_no_annotation.png")
      ggsave_template(path, remove_annotation(p))
    },
    format = "file"
  ),
  tar_target(fire_index_plot_unannotated_upload, upload_gdrive(fire_index_plot_unannotated, fire_index_plot_unannotated, drive_folder_id_unannotated), format = "file"),

  # 6_4: Wildfire Vulnerability Index: Top 30% Biodiversity Values ----
  tar_target(
    wvi_top30_annotated,
    {
      tmpl <- template_plot(mask, ocean_sf, basemap)
      overlay <- template_plot_overlay(ocean_sf, trails, roads)
      p <- wildfire_vulnerability_top30_plot(fire_index, rankmap, ocean_sf, tmpl, overlay)
      path <- here(output_dir_annotated, "6_4_wildfire_vulnerability_top30.png")
      ggsave_template(path, add_annotation(p))
    },
    format = "file"
  ),
  tar_target(wvi_top30_annotated_upload, upload_gdrive(wvi_top30_annotated, wvi_top30_annotated, drive_folder_id_annotated), format = "file"),
  tar_target(
    wvi_top30_unannotated,
    {
      tmpl <- template_plot(mask, ocean_sf, basemap)
      overlay <- template_plot_overlay(ocean_sf, trails, roads)
      p <- wildfire_vulnerability_top30_plot(fire_index, rankmap, ocean_sf, tmpl, overlay)
      path <- here(output_dir_unannotated, "6_4_wildfire_vulnerability_top30_no_annotation.png")
      ggsave_template(path, remove_annotation(p))
    },
    format = "file"
  ),
  tar_target(wvi_top30_unannotated_upload, upload_gdrive(wvi_top30_unannotated, wvi_top30_unannotated, drive_folder_id_unannotated), format = "file"),

  # 6_5: Wildland Urban Interface ----
  tar_target(
    wui_annotated,
    {
      tmpl <- template_plot(mask, ocean_sf, basemap)
      overlay <- template_plot_overlay(ocean_sf, trails, roads)
      p <- wildland_urban_interface_plot(wui, tmpl, overlay)
      path <- here(output_dir_annotated, "6_5_wildland_urban_interface.png")
      ggsave_template(path, add_annotation(p))
    },
    format = "file"
  ),
  tar_target(wui_annotated_upload, upload_gdrive(wui_annotated, wui_annotated, drive_folder_id_annotated), format = "file"),
  tar_target(
    wui_unannotated,
    {
      tmpl <- template_plot(mask, ocean_sf, basemap)
      overlay <- template_plot_overlay(ocean_sf, trails, roads)
      p <- wildland_urban_interface_plot(wui, tmpl, overlay)
      path <- here(output_dir_unannotated, "6_5_wildland_urban_interface_no_annotation.png")
      ggsave_template(path, remove_annotation(p))
    },
    format = "file"
  ),
  tar_target(wui_unannotated_upload, upload_gdrive(wui_unannotated, wui_unannotated, drive_folder_id_unannotated), format = "file"),

  # 7_1: Biodiversity Values by Parcel ----
  tar_target(
    biod_val_parcel_annotated,
    {
      tmpl <- template_plot(mask, ocean_sf, basemap)
      overlay <- template_plot_overlay(ocean_sf, trails, roads)
      p <- parcel_biodiversity_plot(biod_val_parcel, tmpl, overlay)
      path <- here(output_dir_annotated, "7_1_parcel_biodiversity_values.png")
      ggsave_template(path, add_annotation(p))
    },
    format = "file"
  ),
  tar_target(biod_val_parcel_annotated_upload, upload_gdrive(biod_val_parcel_annotated, biod_val_parcel_annotated, drive_folder_id_annotated), format = "file"),
  tar_target(
    biod_val_parcel_unannotated,
    {
      tmpl <- template_plot(mask, ocean_sf, basemap)
      overlay <- template_plot_overlay(ocean_sf, trails, roads)
      p <- parcel_biodiversity_plot(biod_val_parcel, tmpl, overlay)
      path <- here(output_dir_unannotated, "7_1_parcel_biodiversity_values_no_annotation.png")
      ggsave_template(path, remove_annotation(p))
    },
    format = "file"
  ),
  tar_target(biod_val_parcel_unannotated_upload, upload_gdrive(biod_val_parcel_unannotated, biod_val_parcel_unannotated, drive_folder_id_unannotated), format = "file"),

  # 7_2: Subdivision Potential ----
  tar_target(
    subd_capacity_annotated,
    {
      tmpl <- template_plot(mask, ocean_sf, basemap)
      overlay <- template_plot_overlay(ocean_sf, trails, roads)
      p <- subdivision_capacity_plot(parcelmap_subdiv, parcelmap, tmpl, overlay)
      path <- here(output_dir_annotated, "7_2_subdivision_potential.png")
      ggsave_template(path, add_annotation(p))
    },
    format = "file"
  ),
  tar_target(subd_capacity_annotated_upload, upload_gdrive(subd_capacity_annotated, subd_capacity_annotated, drive_folder_id_annotated), format = "file"),
  tar_target(
    subd_capacity_unannotated,
    {
      tmpl <- template_plot(mask, ocean_sf, basemap)
      overlay <- template_plot_overlay(ocean_sf, trails, roads)
      p <- subdivision_capacity_plot(parcelmap_subdiv, parcelmap, tmpl, overlay)
      path <- here(output_dir_unannotated, "7_2_subdivision_potential_no_annotation.png")
      ggsave_template(path, remove_annotation(p))
    },
    format = "file"
  ),
  tar_target(subd_capacity_unannotated_upload, upload_gdrive(subd_capacity_unannotated, subd_capacity_unannotated, drive_folder_id_unannotated), format = "file"),

  # 7_3: Protected Areas: Top 30% Biodiversity Values ----
  tar_target(
    pa_top30_annotated,
    {
      tmpl <- template_plot(mask, ocean_sf, basemap)
      overlay <- template_plot_overlay(ocean_sf, trails, roads)
      p <- protected_areas_top30_plot(rankmap, pa, ocean_sf, tmpl, overlay)
      path <- here(output_dir_annotated, "7_3_protected_areas_top30.png")
      ggsave_template(path, add_annotation(p))
    },
    format = "file"
  ),
  tar_target(pa_top30_annotated_upload, upload_gdrive(pa_top30_annotated, pa_top30_annotated, drive_folder_id_annotated), format = "file"),
  tar_target(
    pa_top30_unannotated,
    {
      tmpl <- template_plot(mask, ocean_sf, basemap)
      overlay <- template_plot_overlay(ocean_sf, trails, roads)
      p <- protected_areas_top30_plot(rankmap, pa, ocean_sf, tmpl, overlay)
      path <- here(output_dir_unannotated, "7_3_protected_areas_top30_no_annotation.png")
      ggsave_template(path, remove_annotation(p))
    },
    format = "file"
  ),
  tar_target(pa_top30_unannotated_upload, upload_gdrive(pa_top30_unannotated, pa_top30_unannotated, drive_folder_id_unannotated), format = "file"),

  # 7_4: Private Land: Top 30% Biodiversity Values ----
  tar_target(
    private_land_top30_annotated,
    {
      tmpl <- template_plot(mask, ocean_sf, basemap)
      overlay <- template_plot_overlay(ocean_sf, trails, roads)
      p <- private_land_top30_plot(privateland, rankmap, ocean_sf, tmpl, overlay)
      path <- here(output_dir_annotated, "7_4_private_land_top30.png")
      ggsave_template(path, add_annotation(p))
    },
    format = "file"
  ),
  tar_target(private_land_top30_annotated_upload, upload_gdrive(private_land_top30_annotated, private_land_top30_annotated, drive_folder_id_annotated), format = "file"),
  tar_target(
    private_land_top30_unannotated,
    {
      tmpl <- template_plot(mask, ocean_sf, basemap)
      overlay <- template_plot_overlay(ocean_sf, trails, roads)
      p <- private_land_top30_plot(privateland, rankmap, ocean_sf, tmpl, overlay)
      path <- here(output_dir_unannotated, "7_4_private_land_top30_no_annotation.png")
      ggsave_template(path, remove_annotation(p))
    },
    format = "file"
  ),
  tar_target(private_land_top30_unannotated_upload, upload_gdrive(private_land_top30_unannotated, private_land_top30_unannotated, drive_folder_id_unannotated), format = "file"),

  # 7_5: Public Land (Crown): Top 30% Biodiversity Values ----
  tar_target(
    public_land_top30_annotated,
    {
      tmpl <- template_plot(mask, ocean_sf, basemap)
      overlay <- template_plot_overlay(ocean_sf, trails, roads)
      p <- public_land_top30_plot(unprotected_crown, rankmap, ocean_sf, tmpl, overlay)
      path <- here(output_dir_annotated, "7_5_public_land_top30.png")
      ggsave_template(path, add_annotation(p))
    },
    format = "file"
  ),
  tar_target(public_land_top30_annotated_upload, upload_gdrive(public_land_top30_annotated, public_land_top30_annotated, drive_folder_id_annotated), format = "file"),
  tar_target(
    public_land_top30_unannotated,
    {
      tmpl <- template_plot(mask, ocean_sf, basemap)
      overlay <- template_plot_overlay(ocean_sf, trails, roads)
      p <- public_land_top30_plot(unprotected_crown, rankmap, ocean_sf, tmpl, overlay)
      path <- here(output_dir_unannotated, "7_5_public_land_top30_no_annotation.png")
      ggsave_template(path, remove_annotation(p))
    },
    format = "file"
  ),
  tar_target(public_land_top30_unannotated_upload, upload_gdrive(public_land_top30_unannotated, public_land_top30_unannotated, drive_folder_id_unannotated), format = "file"),

  # 7_6: Land Ownership / Authority ----
  tar_target(
    land_ownership_annotated,
    {
      tmpl <- template_plot(mask, ocean_sf, basemap)
      overlay <- template_plot_overlay(ocean_sf, trails, roads)
      p <- land_ownership_plot(land_ownership_rast, ocean_sf, tmpl, overlay)
      path <- here(output_dir_annotated, "7_6_land_ownership.png")
      ggsave_template(path, add_annotation(p))
    },
    format = "file"
  ),
  tar_target(land_ownership_annotated_upload, upload_gdrive(land_ownership_annotated, land_ownership_annotated, drive_folder_id_annotated), format = "file"),
  tar_target(
    land_ownership_unannotated,
    {
      tmpl <- template_plot(mask, ocean_sf, basemap)
      overlay <- template_plot_overlay(ocean_sf, trails, roads)
      p <- land_ownership_plot(land_ownership_rast, ocean_sf, tmpl, overlay)
      path <- here(output_dir_unannotated, "7_6_land_ownership_no_annotation.png")
      ggsave_template(path, remove_annotation(p))
    },
    format = "file"
  ),
  tar_target(land_ownership_unannotated_upload, upload_gdrive(land_ownership_unannotated, land_ownership_unannotated, drive_folder_id_unannotated), format = "file"),

  # 7_7: Land Ownership: Top 30% Biodiversity Values ----
  tar_target(
    land_ownership_top30_annotated,
    {
      tmpl <- template_plot(mask, ocean_sf, basemap)
      overlay <- template_plot_overlay(ocean_sf, trails, roads)
      p <- land_ownership_top30_plot(land_ownership_rast, rankmap, ocean_sf, tmpl, overlay)
      path <- here(output_dir_annotated, "7_7_land_ownership_top30.png")
      ggsave_template(path, add_annotation(p))
    },
    format = "file"
  ),
  tar_target(land_ownership_top30_annotated_upload, upload_gdrive(land_ownership_top30_annotated, land_ownership_top30_annotated, drive_folder_id_annotated), format = "file"),
  tar_target(
    land_ownership_top30_unannotated,
    {
      tmpl <- template_plot(mask, ocean_sf, basemap)
      overlay <- template_plot_overlay(ocean_sf, trails, roads)
      p <- land_ownership_top30_plot(land_ownership_rast, rankmap, ocean_sf, tmpl, overlay)
      path <- here(output_dir_unannotated, "7_7_land_ownership_top30_no_annotation.png")
      ggsave_template(path, remove_annotation(p))
    },
    format = "file"
  ),
  tar_target(land_ownership_top30_unannotated_upload, upload_gdrive(land_ownership_top30_unannotated, land_ownership_top30_unannotated, drive_folder_id_unannotated), format = "file"),

  # 7_8: Candidate Protected Areas ----
  tar_target(
    candidate_pa_plot_annotated,
    {
      tmpl <- template_plot(mask, ocean_sf, basemap)
      overlay <- template_plot_overlay(ocean_sf, trails, roads)
      p <- candidate_pa_plot(pa, pa_candidates, tmpl, overlay)
      path <- here(output_dir_annotated, "7_8_candidate_protected_areas.png")
      ggsave_template(path, add_annotation(p))
    },
    format = "file"
  ),
  tar_target(candidate_pa_plot_annotated_upload, upload_gdrive(candidate_pa_plot_annotated, candidate_pa_plot_annotated, drive_folder_id_annotated), format = "file"),
  tar_target(
    candidate_pa_plot_unannotated,
    {
      tmpl <- template_plot(mask, ocean_sf, basemap)
      overlay <- template_plot_overlay(ocean_sf, trails, roads)
      p <- candidate_pa_plot(pa, pa_candidates, tmpl, overlay)
      path <- here(output_dir_unannotated, "7_8_candidate_protected_areas_no_annotation.png")
      ggsave_template(path, remove_annotation(p))
    },
    format = "file"
  ),
  tar_target(candidate_pa_plot_unannotated_upload, upload_gdrive(candidate_pa_plot_unannotated, candidate_pa_plot_unannotated, drive_folder_id_unannotated), format = "file")

)
