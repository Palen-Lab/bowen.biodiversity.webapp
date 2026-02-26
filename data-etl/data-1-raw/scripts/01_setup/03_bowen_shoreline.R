## code to prepare `bowen_island_shoreline_w_hutt` dataset goes here
library(sf)
library(tidyverse)

# Shoreline
bowen_shoreline <- st_read(
  here("data-raw/datasets/bowen_island_shoreline_w_hutt.gpkg"),
  layer = "bowen_island_shoreline_w_hutt__bowen_islands",
  quiet = T
) %>%
  st_transform(project_crs)

usethis::use_data(bowen_shoreline, overwrite = TRUE)
