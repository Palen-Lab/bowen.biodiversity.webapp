## code to prepare `bowen_fish_whitehead_consultants` dataset goes here
library(here)
library(sf)
library(dplyr)

output_dir <- here("data-2-processed/04_habitats")
# Ponds from Alan Whitehead
ponds_wc <- st_read(here("data-raw/datasets/wetlands_whitehead_consultants/ponds-aw.gpkg"))
st_write(ponds_wc, here(output_dir, "ponds_wc.gpkg"), append = FALSE)

# Wetlands from Alan Whitehead
wetlands_wc <- st_read(here("data-raw/datasets/wetlands_whitehead_consultants/wetlands2015.gpkg"))
st_write(wetlands_wc, here(output_dir, "wetlands_wc.gpkg"),append = FALSE)

# Fishbearing Streams from Alan Whitehead
fish_streams_wc <- st_read(here("data-raw/datasets/fish_whitehead_consultants/streams_fish.shp"))
fish_streams_wc[319, "OCP_CLASS"] <- "drainage channel"
st_write(fish_streams_wc, here(output_dir, "fish_streams_wc.gpkg"),append = FALSE)
