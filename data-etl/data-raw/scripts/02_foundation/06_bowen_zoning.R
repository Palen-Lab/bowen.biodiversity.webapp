## code to prepare `bowen_zoning` dataset goes here
library(sf)
library(here)
library(magrittr)

bowen_zoning <- st_read(here("data-raw/bowen_zoning/BM_ZONING.shp")) %>%
  st_transform(crs = project_crs)

usethis::use_data(bowen_zoning, overwrite = TRUE)
