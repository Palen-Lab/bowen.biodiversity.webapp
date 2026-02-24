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

#' Load Species Distribution Models 
#' The species distribution models (SDMs) included in the Bowen Island Conservation Plan 
#' analyses were originally produced by Viorel Popescu (Popescu et al., 2020). Out of the 
#' 341 SDMs available, we included 193 of the SDMs. Occurrence locations of small vertebrate 
#' species were gathered from Global Biodiversity Information Facility (data.gbif.org) and 
#' Nature Counts (birdscanada.org/birdmon) open-access databases. Inclusion of small mammal 
#' species was based on Appendix 2 & 3 from the 2005 Environmental Inventory report, which 
#' states species presence on Bowen Island. We used the iNaturalist and eBird databases of 
#' observations for the inclusion criteria for reptiles, amphibians, and birds (described in 
#' further detail in Methods).

mask_sdm <- function(mask, path) {

}

# #### SPECIES DISTRIBUTION MODELS ####
# # Contents of "data-raw/species_distribution_models/" downloaded from Cumulative effects - SFU Teams Sharepoint 
# # https://1sfu.sharepoint.com/:f:/r/teams/Cumulativeeffects-SFUTeams9/Shared%20Documents/BCH_project_backup/SPECIES?csf=1&web=1&e=FpNkgc 
# # Directory Path: BCH_project_backup/SPECIES/
# # Paper: https://www.nature.com/articles/s41598-020-64501-7#MOESM1
# # See Wendy Palen (SFU) for access 
# #### PROCESSING SDM ####
# # Get list of available SDMs
# sdm_folder <- here("data-raw/species_distribution_models")
# sdm_folder_tif <- list.files(sdm_folder, recursive = T, pattern = "*.tif$") # Get only .tif files
# # Correspondence with Viorel Popescu confirms that _NAs_ should be SDMs with rock, ice, large lakes layers masked
# sdm_folder_tif_NAs <- sdm_folder_tif[grepl("_NAs_", sdm_folder_tif)] # Get only .tif with "_NAs_" in filename
# # Directory to save SDMs
# sdm_output_dir <- here("inst/extdata/bowen_sdm")
# if(!dir.exists(sdm_output_dir)) {
#   dir.create(sdm_output_dir)
# }

# # Load Bowen Island Mask (created with vignettes/input_mask.qmd)
# bowen_mask <- rast(here("inst/extdata/bowen_mask.tif"))
# # Bowen Land, from zoning 
# bowen_land <- bowen_zoning[!bowen_zoning$ZONE_CODE %in% c("WG1", "WG1(a)"),] %>% 
#   vect()

# #### PREPARE SDMs TO BOWEN ISLAND ####
# # Weights for terra::focal() function that provides moving window average
# # - Smoothing algorithm 
# # - Use this to fill some empty cells on Bowen Island with weighted mean of the neighbouring cells.
# weights <- matrix(c(0, 0, 1, 1, 1, 0, 0,
#                     0, 1, 1, 2, 1, 1, 0,
#                     1, 1, 3, 3, 3, 1, 1,
#                     1, 2, 3, 5, 3, 2, 1,
#                     1, 1, 3, 3, 3, 1, 1,
#                     0, 1, 1, 2, 1, 1, 0,
#                     0, 0, 1, 1, 1, 0, 0
# ), nrow=7)
# # Create species list from available SDMs
# species_list <- sdm_folder_tif_NAs %>%
#   basename() %>%
#   stringr::str_extract("^\\S+\\.[a-zA-Z]+_") %>%
#   stringr::str_remove("_") %>%
#   stringr::str_replace("\\.", " ")
# # Create taxon list 
# taxon_list <- sdm_folder_tif_NAs %>%
#   dirname() %>%
#   str_remove("_mask")

# # Loop through SDMs
# sdm_inat <- foreach(i = 1:length(sdm_folder_tif_NAs), .combine = "rbind") %do% {
#   #### iNaturalist CHECK ####
#   # Progress Message 
#   print(paste0("Currently working on species ", i, " out of ", length(species_list),". This is ", species_list[i], "."))
#   # Query iNaturalist by species name and Bowen Island Administrative Boundaries
#   rinat_results_sf <- tryCatch({
#     Sys.sleep(3) # Need to slow down queries to prevent requests being blocked
#     rinat::get_inat_obs_sf(taxon_name = species_list[i],
#                            area = st_as_sf(bowen_island_admin),
#                            maxresults = 10000)
#   }, error = function(cond) {
#     NA
#   })
#   # Get number of iNaturalist observations
#   if(!is.data.frame(rinat_results_sf)) {
#     inat_n_obs <- 0
#   } else {
#     inat_n_obs <- nrow(rinat_results_sf)
#   }
  
#   #### PREP RASTERS ####
#   sdm_tif <- sdm_folder_tif_NAs[i]
#   sdm_path <- here(sdm_folder, sdm_tif) 
#   sdm_output_path <- here(sdm_output_dir, basename(sdm_tif))
#   sdm <- rast(sdm_path)
#   # Normalize SDMs that range from 0 to 1000
#   sdm_minmax <- terra::minmax(sdm)
#   if(sdm_minmax[2] > 1) {
#     sdm <- sdm / 1000
#   }
#   sdm_output <- sdm %>%
#     #### Reproject SDM ####
#     terra::project(y = project_crs) %>%
#     #### Crop SDM ####
#     # Figured out why the SDMs were cropped incorrectly
#     # The default for cropping a raster by a polygon is to snap = "near"
#     # This means that the polygon extent is snapped to the closest raster
#     # We need snap = "out" to make sure the full extent of the polygon is covered by the output raster
#     terra::crop(bowen_mask, snap = "out") %>%
#     #### Extrapolate for Southern Parts of Bowen Island
#     # - fills the NA values missing in south part of Bowen Island
#     terra::focal(w = weights,
#                  fun = "mean",
#                  na.policy = "only") %>%
#     #### Downsamples ####
#     # - from 400 m to 100 m resolution
#     terra::disagg(fact = 4) %>%
#     #### Smooths the raster ####
#     terra::focal(w = weights,
#                  fun = "mean",
#                  na.rm = T,
#                  na.policy = "omit") %>%
#     terra::crop(bowen_mask, mask = T)
#   writeRaster(sdm_output, sdm_output_path, overwrite = T)
  
#   # Get highest value in SDM
#   sdm_max <- sdm_output %>% 
#     minmax() %>% 
#     max()
  
#     # PREPARE OUTPUT ROW
#   # 1. scientific species name
#   # 2. highest SDM cell value within area 
#   # 3. observation count from iNaturalist
#   # 4. taxon group
#   # 5. filepath where SDM was saved
#   result <- data.frame(species = species_list[i],
#                        sdm_max_value = sdm_max,
#                        inat_n_obs = inat_n_obs,
#                        taxon_group = taxon_list[i],
#                        filepath = sdm_output_path)
# }