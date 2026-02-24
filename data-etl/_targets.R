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

#### Google Drive upload ####
drive_folder_id_annotated <- "1mi0iC0OKSJ-x3nC0AI0tZjpJy_yN7-JJ"
drive_folder_id_unannotated <- "1XkDA2Oc4zNQquNM3MbNV9To7GyE8BKA8"
#### Output directories ####
output_dir <- here("output-figures/data-atlas")
output_dir_annotated <- here(output_dir, "annotated")
output_dir_unannotated <- here(output_dir, "unannotated")

list(
  # ── Phase 0: Load Constants ────────────────────────────────────────────────────
  tar_target(project_crs, load_project_crs()),

  # # ── Phase 1: Load Foundation Layers ────────────────────────────────────────────────────────
  tar_target(boundary,  load_boundary(project_crs)),
  tar_target(shoreline, load_shoreline(project_crs)),

  # SpatRaster — stored natively via geotargets
  tar_terra_rast(mask, create_mask(shoreline, zoning, project_crs)),

  tar_target(ocean_sf,  create_ocean(mask, shoreline)),
  tar_target(roads,     load_roads(project_crs)),
  tar_target(trails,    load_trails(project_crs)),

  # # ── Phase 2: Load Landuse ───────────────────────────────────────────────────
  tar_target(zoning, load_zoning(project_crs)),

  # # Protected Areas
  tar_target(pa, load_protected_areas(project_crs)),
  tar_target(pa_export, {
    pa %>%
      st_drop_geometry() %>%
      write.csv(file = here("data-3-outputs/protectedareas.csv"))
  }, format = "file"),
  # tar_target(dissolved_pa, dissolve_protected_areas(pa)),
  # tar_target(ogma,                     load_ogma(project_crs)),
  # # TODO: manual step, move this to post Zonation calculation
  tar_target(pa_candidates, load_pa_candidates(project_crs)),

  # # Public Lands
  # tar_target(crown,                    load_crown(project_crs)),
  # tar_target(unprotected_crown,        create_unprotected_crown(crown, dissolved_pa)),

  # # Private Lands
  tar_target(parcelmap, load_parcelmap(project_crs)),
  # tar_target(privateland,              create_privateland(parcelmap_bowen, dissolved_pa)),

  # # ── Phase 3: Load Species ──────────────────────────────────────────────────────
  # # iNaturalist
  # # Track the raw iNat zip by file hash — re-runs if the export is updated
  # tar_target(inat_raw, here::here("data-1-raw/datasets/inat/observations-610962.csv.zip"),
  #            format = "file"),
  # # gpkg written to data-2-processed/03_species/inat.gpkg
  # tar_target(inat_gpkg, process_inat(inat_raw), format = "file"),
  # # SpatRaster of observation counts — stored natively via geotargets
  # tar_terra_rast(inat_rast, rasterize_inat(inat_gpkg, mask)),

  # # SpatRaster of SDMs masked to Bowen Island
  # # tar_terra_rast(),

  # # ── Phase 4: Load Habitats ─────────────────────────────────────────────────────
  # # Whitehead consultant datasets — written to data-2-processed/04_habitats/
  # tar_target(ponds_wc,        process_whitehead_ponds(),    format = "file"),
  # tar_target(wetlands_wc,     process_whitehead_wetlands(), format = "file"),
  # tar_target(fish_streams_wc, process_whitehead_fish(),     format = "file"),

  #### Phase 5: Conservation Value ####
  # Use Zonation
  tar_terra_rast(rankmap, load_rankmap(project_crs)),

  #### Output Figures ####
  # Base map
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
  # Wildfire Vulnerability Index
  tar_terra_rast(fire_index, {rast(here("data-3-outputs/6_threats/fire_index_40m.tif"))}),
  tar_target(
    fire_index_plot_annotated,
    {
      tmpl    <- template_plot(mask, ocean_sf, basemap)
      overlay <- template_plot_overlay(ocean_sf, trails, roads)
      p       <- wvi_plot(fire_index, tmpl, overlay)
      path    <- here(output_dir_annotated, "6_3_wildfire_vulnerability.png")
      ggsave_drive(path, add_annotation(p), drive_folder_id_annotated)
    },
    format = "file"
  ),
  tar_target(
    fire_index_plot_unannotated,
    {
      tmpl    <- template_plot(mask, ocean_sf, basemap)
      overlay <- template_plot_overlay(ocean_sf, trails, roads)
      p       <- wvi_plot(fire_index, tmpl, overlay)
      path    <- here(output_dir_unannotated, "6_3_wildfire_vulnerability_no_annotation.png")
      ggsave_drive(path, remove_annotation(p), drive_folder_id_unannotated)
    },
    format = "file"
  ),

  # Wildfire Vulnerability Index: Top 30% Biodiversity Value
  tar_target(
    wvi_top30_annotated,
    {
      tmpl    <- template_plot(mask, ocean_sf, basemap)
      overlay <- template_plot_overlay(ocean_sf, trails, roads)
      p       <- wvi_top30_plot(fire_index, rankmap, ocean_sf, tmpl, overlay)
      path    <- here(output_dir_annotated, "6_4_wildfire_vulnerability_top30.png")
      ggsave_drive(path, add_annotation(p), drive_folder_id_annotated)
    },
    format = "file"
  ),
  tar_target(
    wvi_top30_unannotated,
    {
      tmpl    <- template_plot(mask, ocean_sf, basemap)
      overlay <- template_plot_overlay(ocean_sf, trails, roads)
      p       <- wvi_top30_plot(fire_index, rankmap, ocean_sf, tmpl, overlay)
      path    <- here(output_dir_unannotated, "6_4_wildfire_vulnerability_top30_no_annotation.png")
      ggsave_drive(path, remove_annotation(p), drive_folder_id_unannotated)
    },
    format = "file"
  ),

  # Wildland Urban Interface
  tar_target(wui, {st_read(here("data-1-raw/datasets/bc_wui/wui.gpkg"))}),
  tar_target(
    wui_annotated,
    {
      tmpl    <- template_plot(mask, ocean_sf, basemap)
      overlay <- template_plot_overlay(ocean_sf, trails, roads)
      p       <- wui_plot(wui, tmpl, overlay)
      path    <- here(output_dir_annotated, "6_5_wildland_urban_interface.png")
      ggsave_drive(path, add_annotation(p), drive_folder_id_annotated)
    },
    format = "file"
  ),
  tar_target(
    wui_unannotated,
    {
      tmpl    <- template_plot(mask, ocean_sf, basemap)
      overlay <- template_plot_overlay(ocean_sf, trails, roads)
      p       <- wui_plot(wui, tmpl, overlay)
      path    <- here(output_dir_annotated, "6_5_wildland_urban_interface_no_annotation.png")
      ggsave_drive(path, remove_annotation(p), drive_folder_id_unannotated)
    },
    format = "file"
  ),
  # Subdivision Potential
  tar_target(parcelmap_subdiv, parcel_subdiv(parcelmap)),
  tar_target(
    subd_capacity_annotated,
    {
      tmpl    <- template_plot(mask, ocean_sf, basemap)
      overlay <- template_plot_overlay(ocean_sf, trails, roads)
      p       <- subd_capacity_plot(parcelmap_subdiv, parcelmap, tmpl, overlay)
      path    <- here(output_dir_annotated, "7_2_subdivision_potential.png")
      ggsave_drive(path, add_annotation(p), drive_folder_id_annotated)
    },
    format = "file"
  ),
  tar_target(
    subd_capacity_unannotated,
    {
      tmpl    <- template_plot(mask, ocean_sf, basemap)
      overlay <- template_plot_overlay(ocean_sf, trails, roads)
      p       <- subd_capacity_plot(parcelmap_subdiv, parcelmap, tmpl, overlay)
      path    <- here(output_dir_annotated, "7_2_subdivision_potential_no_annotation.png")
      ggsave_drive(path, remove_annotation(p), drive_folder_id_unannotated)
    },
    format = "file"
  ),
  # Biodiversity Values by Parcel
  tar_target(biod_val_parcel, parcel_biod_val(parcelmap_subdiv, rankmap)),
  tar_target(
    biod_val_parcel_annotated,
    {
      tmpl    <- template_plot(mask, ocean_sf, basemap)
      overlay <- template_plot_overlay(ocean_sf, trails, roads)
      p       <- biod_val_parcel_plot(biod_val_parcel, tmpl, overlay)
      path    <- here(output_dir_annotated, "7_1_parcel_biodiversity_values.png")
      ggsave_drive(path, add_annotation(p), drive_folder_id_annotated)
    },
    format = "file"
  ),
  tar_target(
    biod_val_parcel_unannotated,
    {
      tmpl    <- template_plot(mask, ocean_sf, basemap)
      overlay <- template_plot_overlay(ocean_sf, trails, roads)
      p       <- biod_val_parcel_plot(biod_val_parcel, tmpl, overlay)
      path    <- here(output_dir_annotated, "7_1_parcel_biodiversity_values_no_annotation.png")
      ggsave_drive(path, remove_annotation(p), drive_folder_id_unannotated)
    },
    format = "file"
  ),
  # Candidate Protected Areas
  tar_target(
    candidate_pa_plot_annotated,
    {
      tmpl    <- template_plot(mask, ocean_sf, basemap)
      overlay <- template_plot_overlay(ocean_sf, trails, roads)
      p       <- candidate_pa_plot(pa, pa_candidates, tmpl, overlay)
      path    <- here(output_dir_annotated, "7_8_candidate_protected_areas.png")
      ggsave_drive(path, add_annotation(p), drive_folder_id_annotated)
    },
    format = "file"
  ),
  tar_target(
    candidate_pa_plot_unannotated,
    {
      tmpl    <- template_plot(mask, ocean_sf, basemap)
      overlay <- template_plot_overlay(ocean_sf, trails, roads)
      p       <- candidate_pa_plot(pa, pa_candidates, tmpl, overlay)
      path    <- here(output_dir_unannotated, "7_8_candidate_protected_areas_no_annotation.png")
      ggsave_drive(path, remove_annotation(p), drive_folder_id_unannotated)
    },
    format = "file"
  )
)
