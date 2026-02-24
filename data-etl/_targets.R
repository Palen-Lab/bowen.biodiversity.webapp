library(targets)
library(tarchetypes)
library(geotargets)

tar_source()
tar_option_set(
  packages = c("sf", "terra", "dplyr", "here", "magrittr", "stringr", "glue", "basemaps"),
  format = "qs"
)

list(

  # ── Phase 0: Load Constants ────────────────────────────────────────────────────
  tar_target(project_crs, load_project_crs()),

  # # ── Phase 1: Load Foundation Layers ────────────────────────────────────────────────────────
  # tar_target(bowen_boundary,  load_bowen_boundary(project_crs)),
  # tar_target(bowen_shoreline, load_bowen_shoreline(project_crs)),

  # SpatRaster — stored natively via geotargets
  tar_terra_rast(bowen_mask, create_bowen_mask(bowen_shoreline, bowen_zoning, project_crs)),

  tar_target(bowen_ocean_sf,  create_bowen_ocean(bowen_mask, bowen_shoreline)),
  # tar_target(bowen_roads,     load_bowen_roads(project_crs)),
  # tar_target(bowen_trails,    load_bowen_trails(project_crs)),

  # # ── Phase 2: Load Landuse ───────────────────────────────────────────────────
  # tar_target(bowen_zoning, load_bowen_zoning(project_crs)),

  # # Protected Areas
  # tar_target(pa, load_protected_areas(project_crs)),
  # tar_target(dissolved_pa, dissolve_protected_areas(pa)),
  # tar_target(ogma,                     load_ogma(project_crs)),
  # # TODO: manual step, move this to post Zonation calculation
  # tar_target(pa_candidates, load_pa_candidates(project_crs)),

  # # Public Lands
  # tar_target(crown,                    load_crown(project_crs)),
  # tar_target(unprotected_crown,        create_unprotected_crown(crown, dissolved_pa)),

  # # Private Lands
  # tar_target(parcelmap_bowen,          load_parcelmap(project_crs)),
  # tar_target(privateland,              create_privateland(parcelmap_bowen, dissolved_pa)),

  # # ── Phase 3: Load Species ──────────────────────────────────────────────────────
  # # iNaturalist 
  # # Track the raw iNat zip by file hash — re-runs if the export is updated
  # tar_target(inat_raw, here::here("data-1-raw/datasets/inat/observations-610962.csv.zip"),
  #            format = "file"),
  # # gpkg written to data-2-processed/03_species/inat.gpkg
  # tar_target(inat_gpkg, process_inat(inat_raw), format = "file"),
  # # SpatRaster of observation counts — stored natively via geotargets
  # tar_terra_rast(inat_rast, rasterize_inat(inat_gpkg, bowen_mask)),

  # # SpatRaster of SDMs masked to Bowen Island
  # # tar_terra_rast(),

  # # ── Phase 4: Load Habitats ─────────────────────────────────────────────────────
  # # Whitehead consultant datasets — written to data-2-processed/04_habitats/
  # tar_target(ponds_wc,        process_whitehead_ponds(),    format = "file"),
  # tar_target(wetlands_wc,     process_whitehead_wetlands(), format = "file"),
  # tar_target(fish_streams_wc, process_whitehead_fish(),     format = "file"),

  # Phase 5: Conservation Value 
  # Use Zonation 

  # ── Quarto site ───────────────────────────────────────────────────────────
  # Renders all analyses/ documents. Re-renders a doc when any tar_load() /
  # tar_read() target it references is updated.
  # tar_quarto(site, path = ".")

  # Output Figures
  # Candidate Protected Areas plot
  tar_target(template_plot, template_plot(bowen_mask, bowen_ocean_sf)),
  tar_target(candidate_pa_plot, candidate_pa_plot(pa, pa_candidates, template_plot), format = "file")

)
