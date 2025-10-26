## code to prepare `bowen_wetlands_whitehead_consultants` dataset goes here
library(sf)
library(here)
library(dplyr)

bowen_ponds_whitehead_consultants <- st_read(here("data-raw/bowen_wetlands_whitehead_consultants/ponds-aw.gpkg"))
usethis::use_data(bowen_ponds_whitehead_consultants, overwrite = TRUE)

bowen_wetlands_whitehead_consultants <- st_read(here("data-raw/bowen_wetlands_whitehead_consultants/wetlands2015.gpkg"))
usethis::use_data(bowen_wetlands_whitehead_consultants, overwrite = TRUE)
