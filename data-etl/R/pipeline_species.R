# Pipeline functions: Phase 3 — Species Data
# iNaturalist observations → gpkg (file target) and raster (geotargets)
# SDM processing lives in analyses/02_input_species.qmd

# Reads iNaturalist zip export, writes a GeoPackage.
# Returns file path — used with format = "file" in _targets.R.
process_inat <- function(raw_zip_path) {
  out_dir <- here::here("data-2-processed/03_species")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  tmp_dir <- tempdir()
  utils::unzip(raw_zip_path, exdir = tmp_dir)
  csv_files <- list.files(tmp_dir, pattern = "\\.csv$", full.names = TRUE)
  csv_path <- csv_files[grepl("observations", csv_files)][1]

  bowen_inat <- sf::st_read(
    csv_path,
    options = c("X_POSSIBLE_NAMES=longitude", "Y_POSSIBLE_NAMES=latitude"),
    quiet = TRUE
  ) %>%
    sf::st_set_crs(4326) %>%
    dplyr::select(-dplyr::any_of(c("private_longitude", "private_latitude")))

  out_path <- file.path(out_dir, "inat.gpkg")
  sf::st_write(bowen_inat, out_path, append = FALSE, quiet = TRUE)
  out_path
}

# Rasterizes iNaturalist points to observation counts per 100m cell.
# Returns a SpatRaster — used with tar_terra_rast() in _targets.R.
# inat_gpkg_path is the file path from the process_inat() target.
# bowen_mask is the SpatRaster from the tar_terra_rast(bowen_mask) target.
rasterize_inat <- function(inat_gpkg_path, bowen_mask) {
  bowen_inat <- sf::st_read(inat_gpkg_path, quiet = TRUE)

  bowen_inat_vect <- terra::vect(bowen_inat) %>%
    terra::project(bowen_mask)

  terra::rasterize(bowen_inat_vect, bowen_mask, fun = "count") %>%
    terra::mask(bowen_mask)
}
