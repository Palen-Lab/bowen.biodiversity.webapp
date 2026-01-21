## code to prepare `bowen_island_shoreline_w_hutt` dataset goes here
library(sf)
library(tidyverse)

# Bowen Island mask raster
bowen_mask <- terra::rast(here::here("inst/extdata/bowen_mask.tif")) %>%
  project("EPSG: 3857")
bowen_mask_ext <- bowen_mask %>%
  ext()
bowen_mask_sf <- as.polygons(bowen_mask, extent=T) %>%
  st_as_sf() %>%
  st_transform(3857) %>%
  st_buffer(10000)
# Shoreline
bowen_shoreline <- st_read(
  here("data-raw/datasets/island_shoreline_w_hutt.gpkg"),
  layer = "bowen_island_shoreline_w_hutt__bowen_islands",
  quiet = T
) %>%
  st_transform(3857)
# Create ocean polygon, need this to reduce appearance of jagged edges from raster along coastline
bowen_ocean_sf <- st_difference(st_union(bowen_mask_sf), st_union(bowen_shoreline))

usethis::use_data(bowen_ocean_sf, overwrite = TRUE)
