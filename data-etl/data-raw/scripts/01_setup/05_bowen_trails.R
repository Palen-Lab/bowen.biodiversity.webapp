## code to prepare `bowen_trails` dataset goes here
library(sf)
bowen_trails <- st_read("data-raw/datasets/trails/Trails.shp") %>%
  st_transform(project_crs)

usethis::use_data(bowen_trails, overwrite = TRUE)
