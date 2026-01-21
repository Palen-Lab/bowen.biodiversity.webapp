## code to prepare `bowen_inat` dataset goes here
library(sf)
library(terra)
library(tidyverse)

#### Creating sf object from all iNaturalist observations on Bowen Island ####
# Updated as of 2025/09/02
# Unzip records exported from iNaturalist
unzip("data-raw/datasets/inat/observations-610962.csv.zip", exdir = "data-raw/")
file.remove("data-raw/datasets/inat/observations-610962.csv.zip")
# Load records
bowen_inat_raw <- st_read("data-raw/datasets/inat/observations-610962.csv", options=c("X_POSSIBLE_NAMES=longitude","Y_POSSIBLE_NAMES=latitude"))
bowen_inat <- bowen_inat_raw %>%
  st_set_crs(4326) %>%
  dplyr::select(!c("private_longitude", "private_latitude"))
# Export to data
usethis::use_data(bowen_inat, overwrite = TRUE)

#### Creating raster with iNaturalist observation counts per cell ####
# Matches the Zonation raster cells
# Load Bowen Mask to create raster
bowen_mask <- rast("inst/extdata/bowen_mask.tif")
# Convert sf to vect
bowen_inat_vect <- bowen_inat %>%
  vect() %>%
  project(bowen_mask)
# Count points in each pixel
bowen_inat_count_rast <- terra::rasterize(bowen_inat_vect, bowen_mask, fun = "count") %>%
  mask(bowen_mask) # Mask to cells with Zonation values
# Export to inst/extdata
writeRaster(bowen_inat_count_rast, "inst/extdata/4_people/bowen_inat.tif", overwrite = T)

