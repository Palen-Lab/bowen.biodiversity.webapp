## code to prepare `DATASET` dataset goes here
library(sf)
# Bowen Island Administrative Boundary (from Metro Vancouver)
bowen_boundary <- st_read(here("data-raw/bowen_boundary")) %>%
  st_transform(project_crs)
usethis::use_data(bowen_boundary, overwrite = TRUE)
