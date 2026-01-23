## code to prepare `bowen_fish_whitehead_consultants` dataset goes here
library(here)
library(sf)

bowen_fish_whitehead_consultants <- st_read(here("data-raw/datasets/fish_whitehead_consultants/streams_fish.shp"))
bowen_fish_whitehead_consultants[319, "OCP_CLASS"] <- "drainage channel"
usethis::use_data(bowen_fish_whitehead_consultants, overwrite = TRUE)
