## code to prepare `bowen_roads` dataset goes here
library(sf)
bowen_roads <- st_read("data-raw/bowen_roads/Bowen_Road_Inventory.shp") %>%
  st_transform(project_crs)

usethis::use_data(bowen_roads, overwrite = TRUE)
