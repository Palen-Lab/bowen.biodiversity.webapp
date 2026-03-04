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
  # Phase 0: Constants
  tar_target(project_crs, load_project_crs()),

  #### Drive folder ID targets ####
  tar_target(drive_folder_id_foundation, {"1zX3cteFb9JrM3r3C1R3gNHMqvjr94yf6"}),
  tar_target(drive_folder_id_landuse, {"1wZ2In-UJUi-QNsgoaqskCy5hGFcrWRL9"}),
  tar_target(drive_folder_id_values, {"1pYZjm8dyazPm8lvZCAXn46RT0Rx_4w5L"}),
  tar_target(drive_folder_id_threats, {"1eA-6hmVSRO8ZuQMdIlQ5t4XF2xYEnMs3"}),
  tar_target(drive_folder_id_annotated, {"1mi0iC0OKSJ-x3nC0AI0tZjpJy_yN7-JJ"}),
  tar_target(drive_folder_id_unannotated, {"1XkDA2Oc4zNQquNM3MbNV9To7GyE8BKA8"}),
  tar_target(drive_folder_id_land_management, {"1c7T_1GLAAylUPByJRZhSnWNg-kyH2UNq"}), 

  #### Plot Output Directories ####
  tar_target(output_dir, here("output-figures/data-atlas")),
  tar_target(output_dir_annotated, here(output_dir, "annotated")),
  tar_target(output_dir_unannotated, here(output_dir, "unannotated")),

  # 1: BASE ####
  ## format = "file" on path targets triggers downstream re-runs on file changes
  ## Boundary
  tar_target(boundary_path, here("data-1-raw/datasets/boundary/Bowen_boundary.shp"), format = "file"),
  tar_target(boundary, load_boundary(boundary_path, project_crs)),
  tar_target(boundary_upload, upload_gdrive(boundary, boundary_path, drive_folder_id_foundation), format = "file"),

  ## Shoreline
  tar_target(shoreline_path, here("data-1-raw/datasets/bowen_island_shoreline_w_hutt.gpkg"), format = "file"),
  tar_target(shoreline, load_shoreline(shoreline_path, project_crs)),
  tar_target(shoreline_upload, upload_gdrive(shoreline, shoreline_path, drive_folder_id_foundation), format = "file"),

  ## Roads
  tar_target(roads_path, here("data-1-raw/datasets/roads/Bowen_Road_Inventory.shp"), format = "file"),
  tar_target(roads, load_roads(roads_path, project_crs)),
  tar_target(roads_upload, upload_gdrive(roads, roads_path, drive_folder_id_foundation), format = "file"),

  ## Trails
  tar_target(trails_path, here("data-1-raw/datasets/trails/Trails.shp"), format = "file"),
  tar_target(trails, load_trails(trails_path, project_crs)),
  tar_target(trails_upload, upload_gdrive(trails, trails_path, drive_folder_id_foundation), format = "file"),

  ## Derived — SpatRasters stored natively via geotargets
  tar_terra_rast(mask, create_mask(shoreline, zoning, project_crs)),
  tar_target(ocean_sf, create_ocean(mask, shoreline)),

  # Phase 2: Landuse
  ## Zoning
  tar_target(zoning_path, here("data-1-raw/datasets/zoning/BM_ZONING.shp"), format = "file"),
  tar_target(zoning, load_zoning(zoning_path, project_crs)),
  tar_target(zoning_upload, upload_gdrive(zoning, zoning_path, drive_folder_id_landuse), format = "file"),

  ## Protected Areas
  tar_target(pa, load_protected_areas(project_crs)),
  tar_target(pa_export, {
    pa %>%
      st_drop_geometry() %>%
      write.csv(file = here("data-3-outputs/protectedareas.csv"))
  }, format = "file"),
  tar_target(dissolved_pa, dissolve_protected_areas(pa)),
  # tar_target(ogma, load_ogma(project_crs)),

  ## TODO: manual step, move this to post Zonation calculation
  tar_target(pa_candidates_path, here("data-3-outputs/7_protected_areas/new_protected_areas.gpkg"), format = "file"),
  tar_target(pa_candidates, load_pa_candidates(pa_candidates_path, project_crs)),
  tar_target(pa_candidates_upload, upload_gdrive(pa_candidates, pa_candidates_path, drive_folder_id_land_management, name = "candidate_protected_areas.gpkg"), format = "file"),

  ## Crown (Public) Land
  tar_target(crown_path, here("data-1-raw/datasets/protectedareas/Crown-Land-JD.gpkg"), format = "file"),
  tar_target(crown, load_crown(crown_path, project_crs)),
  tar_target(crown_upload, upload_gdrive(crown, crown_path, drive_folder_id_landuse), format = "file"),
  tar_target(unprotected_crown, create_unprotected_crown(crown, dissolved_pa)),

  ## Parcelmap (Private Land)
  tar_target(parcelmap_path, here("data-1-raw/datasets/parcelmap_bowen/parcelmap_bowen.gpkg"), format = "file"),
  tar_target(parcelmap, load_parcelmap(parcelmap_path, project_crs)),
  tar_target(parcelmap_upload, upload_gdrive(parcelmap, parcelmap_path, drive_folder_id_landuse), format = "file"),
  tar_target(privateland, create_privateland(parcelmap, dissolved_pa)),

  ## Subdivision Potential
  tar_target(subdiv_path, here("data-1-raw/datasets/development_potential_danielmartin/zoning subdivision potential.xlsx"), format = "file"),
  tar_target(parcelmap_subdiv, create_parcel_subdiv(parcelmap, subdiv_path)),

  ## Biodiversity Value by Subdividable Parcels
  tar_target(biod_val_parcel, compute_parcel_biod_val(parcelmap_subdiv, rankmap)),

  ## Wildland Urban Interface
  tar_target(wui_path, here("data-1-raw/datasets/bc_wui/wui.gpkg"), format = "file"),
  tar_target(wui, {st_read(wui_path, quiet = TRUE)}),
  tar_target(wui_upload, upload_gdrive(wui, wui_path, drive_folder_id_threats), format = "file"),

  # 2: SPECIES ####
  ## iNaturalist
  # tar_target(inat_raw, here::here("data-1-raw/datasets/inat/observations-610962.csv.zip"), format = "file"),
  # tar_target(inat_gpkg, process_inat(inat_raw), format = "file"),
  # tar_terra_rast(inat_rast, rasterize_inat(inat_gpkg, mask)),
  
  ## Species Richness

  # 3: HABITATS ####
  ## Whitehead consultant datasets — written to data-2-processed/04_habitats/
  # tar_target(ponds_wc, process_whitehead_ponds(), format = "file"),
  # tar_target(wetlands_wc, process_whitehead_wetlands(), format = "file"),
  # tar_target(fish_streams_wc, process_whitehead_fish(), format = "file"),

  # 4: PEOPLE ####

  # 5: CONSERVATION VALUE ####
  ## Zonation rankmap
  tar_target(rankmap_path, here("data-3-outputs/5_values/rankmap.tif"), format = "file"),
  tar_terra_rast(rankmap, load_rankmap(rankmap_path, project_crs)),
  tar_target(rankmap_upload, upload_gdrive(rankmap, rankmap_path, drive_folder_id_values, name = "conservation_values.tif"), format = "file"),

  # 6: THREATS ####
  ## Wildfire Vulnerability Index
  tar_target(fire_index_path, here("data-3-outputs/6_threats/fire_index_40m.tif"), format = "file"),
  tar_terra_rast(fire_index, {rast(fire_index_path)}),
  tar_target(fire_index_upload, upload_gdrive(fire_index, fire_index_path, drive_folder_id_threats, name = "wildfire_vulnerability_index.tif"), format = "file"),

  # 7: LAND MANAGEMENT ####

  ## Protected Areas: Top 30% Biodiversity Values
  

  ## Land Ownership / Authority
  tar_terra_rast(land_ownership_rast, create_land_ownership_rast(pa, unprotected_crown, privateland, rankmap)),
  ## Calculate n cells and percent coverage for each land ownership type 
  tar_target(land_ownership_combined_stats, {
    land_ownership_stats <- compute_land_ownership_stats(land_ownership_rast)
    land_ownership_top30_stats <- compute_land_ownership_top30_stats(land_ownership_rast, rankmap)
    
    combined <- bind_rows(land_ownership_stats, land_ownership_top30_stats)
    combined$description <- c("total", "top_30%_conservation_values")
    combined
  }),
  ## Save raster 
  tar_target(land_ownership_rast_path, {here("data-3-outputs/7_land_management/land_ownership_rast.tif")}), 
  tar_target(land_ownership_rast_save, {writeRaster(land_ownership_rast, land_ownership_rast_path, overwrite=T)}),
  tar_target(land_ownership_rast_upload, {upload_gdrive(land_ownership_rast, land_ownership_rast_path, drive_folder_id_land_management, name = "land_ownership.tif")}, format = "file"),

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
  tar_target(fire_index_plot_annotated,
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
  tar_target(fire_index_plot_unannotated,
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

  # Wildfire Vulnerability Index: Top 30% Biodiversity Value
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

  # Wildland Urban Interface
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

  # Subdivision Potential
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

  # Biodiversity Values by Parcel
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

  # Candidate Protected Areas
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
  tar_target(candidate_pa_plot_unannotated_upload, upload_gdrive(candidate_pa_plot_unannotated, candidate_pa_plot_unannotated, drive_folder_id_unannotated), format = "file"),

  # Land Ownership / Authority
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
  tar_target(land_ownership_unannotated_upload, upload_gdrive(land_ownership_unannotated, land_ownership_unannotated, drive_folder_id_unannotated), format = "file")
)
