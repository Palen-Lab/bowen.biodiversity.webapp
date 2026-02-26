## code to prepare `bowen_mask` analysis mask goes here
#
# This script creates the analysis mask for Bowen Island using Digital Surface Model (DSM) data.
# The mask is used to exclude water areas and steep terrain from species distribution analyses.
#
# Inputs:
#   - data-raw/datasets/dsm/chm.tif (Digital Surface Model)
#   - data/bowen_shoreline.rda (from 01_setup/03_bowen_shoreline.R)
#   - Sample SDM for template raster (from species_distribution_models/)
#
# Outputs:
#   - output-data/bowen_mask.tif (Analysis mask raster)
#
# Runtime: ~2-5 minutes

library(terra)
library(magrittr)
library(here)

# Get sample SDM to use as template
sdm_folder <- here("data-raw/datasets/species_distribution_models")
sdm_folder_tif <- list.files(sdm_folder, recursive = TRUE, pattern = "*.tif$")
# Correspondence with Viorel Popescu confirms that _NAs_ should be SDMs
# with rock, ice, large lakes layers masked
sdm_folder_tif_NAs <- sdm_folder_tif[grepl("_NAs_", sdm_folder_tif)]

# Create template raster matching SDM resolution and extent
sample_sdm <- rast(here(sdm_folder, sdm_folder_tif_NAs[1])) %>%
  terra::project(y = project_crs) %>%
  terra::crop(bowen_zoning, snap = "out") %>%
  terra::disagg(fact = 4)

# Exclude water zones from analysis
bowen_land <- bowen_shoreline %>%
  vect()

# Load and process Digital Surface Model
dsm <- rast(here("data-raw/datasets/dsm/chm.tif"))
dsm_smooth <- dsm %>%
  focal(w = 9, fun = mean, na.policy = "only", na.rm = TRUE) %>%
  terra::project(y = project_crs) %>%
  terra::resample(sample_sdm, method = "average") %>%
  terra::crop(bowen_land, mask = TRUE)

# Reclassify: keep areas with elevation -1 to 35m as valid (1), rest as NA
m <- c(-1, 35, 1) %>% matrix(ncol = 3, byrow = TRUE)
bowen_mask <- dsm_smooth %>%
  classify(m) %>%
  as.int()

# Save mask to output-data/
writeRaster(bowen_mask, here("data/bowen_mask.tif"), overwrite = TRUE)

message("✓ Analysis mask created: data/bowen_mask.tif")
