# Pipeline functions: Phase 1 — Foundation
# CRS, boundary, shoreline, mask, ocean, roads, trails
# These functions are called as targets in _targets.R.
#
# Raster targets use geotargets::tar_terra_rast() — functions return SpatRaster
# directly; no manual writeRaster() needed.

load_project_crs <- function() {
  "+proj=aea +lat_0=45 +lon_0=-126 +lat_1=50 +lat_2=58.5 +x_0=1000000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
}

load_boundary <- function(project_crs) {
  sf::st_read(here::here("data-1-raw/datasets/boundary"), quiet = TRUE) %>%
    sf::st_transform(project_crs)
}

load_shoreline <- function(project_crs) {
  sf::st_read(
    here::here("data-1-raw/datasets/bowen_island_shoreline_w_hutt.gpkg"),
    layer = "bowen_island_shoreline_w_hutt__bowen_islands",
    quiet = TRUE
  ) %>%
    sf::st_transform(project_crs)
}

# Returns a SpatRaster — used with tar_terra_rast() in _targets.R.
create_mask <- function(shoreline, zoning, project_crs) {
  sdm_folder <- here::here("data-1-raw/datasets/species_distribution_models")
  sdm_files <- list.files(sdm_folder, recursive = TRUE, pattern = "\\.tif$")
  sdm_files_nas <- sdm_files[grepl("_NAs_", sdm_files)]

  sample_sdm <- terra::rast(file.path(sdm_folder, sdm_files_nas[1])) %>%
    terra::project(y = project_crs) %>%
    terra::crop(terra::vect(zoning), snap = "out") %>%
    terra::disagg(fact = 4)

  land <- terra::vect(shoreline)

  dsm <- terra::rast(here::here("data-1-raw/datasets/dsm/chm.tif"))
  dsm_smooth <- dsm %>%
    terra::focal(w = 9, fun = mean, na.policy = "only", na.rm = TRUE) %>%
    terra::project(y = project_crs) %>%
    terra::resample(sample_sdm, method = "average") %>%
    terra::crop(land, mask = TRUE)

  m <- matrix(c(-1, 35, 1), ncol = 3, byrow = TRUE)
  terra::classify(dsm_smooth, m) %>%
    terra::as.int()
}

# Returns an sf polygon — used with a standard tar_target() in _targets.R.
# mask is the SpatRaster from the tar_terra_rast() target.
create_ocean <- function(mask, shoreline) {
  mask_reproj <- terra::project(mask, "EPSG:3857")
  mask_sf <- terra::as.polygons(mask_reproj, extent = TRUE) %>%
    sf::st_as_sf() %>%
    sf::st_transform(3857) %>%
    sf::st_buffer(10000)

  sf::st_difference(
    sf::st_union(mask_sf),
    sf::st_union(sf::st_transform(shoreline, 3857))
  )
}

load_roads <- function(project_crs) {
  sf::st_read(
    here::here("data-1-raw/datasets/roads/Bowen_Road_Inventory.shp"),
    quiet = TRUE
  ) %>%
    sf::st_transform(project_crs)
}

load_trails <- function(project_crs) {
  sf::st_read(
    here::here("data-1-raw/datasets/trails/Trails.shp"),
    quiet = TRUE
  ) %>%
    sf::st_transform(project_crs)
}
