# Create ocean polygon, need this to reduce appearance of jagged edges from raster along coastline
# Bowen Island mask raster
bowen_mask <- terra::rast(here::here("inst/extdata/bowen_mask.tif")) %>%
  project("EPSG: 3857")
bowen_mask_ext <- bowen_mask %>%
  ext()
bowen_mask_sf <- as.polygons(bowen_mask, extent = T) %>%
  st_as_sf() %>%
  st_transform(3857) %>%
  st_buffer(10000)
bowen_ocean_sf <- st_difference(
  st_union(bowen_mask_sf),
  st_union(bowen_shoreline)
)
usethis::use_data(bowen_ocean_sf, overwrite = TRUE)
