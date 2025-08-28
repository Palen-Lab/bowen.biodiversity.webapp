## code to prepare `bowen_parcelmap` dataset goes here
library(sf)
library(here)
library(dplyr)
library(rmapshaper)
sf_use_s2(FALSE)

# The original layer used for private lands is the "ParcelMap BC Parcel Fabric - Parcel Fabric File Geodatabase (NAD83(CSRS) / BC Albers)".
# Download Information:
# https://open.canada.ca/data/en/dataset/4cf233c2-f020-4f7a-9b87-1923252fbc24/resource/262e86d1-8957-458e-9369-b36f07ed7415#additional-info
# ArcGIS Online:
# https://governmentofbc.maps.arcgis.com/home/item.html?id=ce7fd87476b54100a3b158c9dae7e9b7
bc_parcelmap_path <-  here("data-raw/bc_parcelmap")
if(!dir.exists(bc_parcelmap_path)) {
  dir.create(bc_parcelmap_path)
  download.file("https://pub.data.gov.bc.ca/datasets/4cf233c2-f020-4f7a-9b87-1923252fbc24/ParcelMapBCExtract.zip",
                here(bc_parcelmap_path, "ParcelMapBCExtract.zip"))
  unzip(bc_parcelmap_path, exdir = bc_parcelmap_path)
  bowen_parcelmap <- st_read(here(bc_parcelmap_path, "ParcelMapBCExtract.gdb"),
                             query = "SELECT * FROM Parcel_Polygon WHERE Municipality LIKE 'Bowen Island, Municipality'")
  dir.create(here("data-raw/bowen_parcelmap"))
  st_write(bowen_parcelmap, here("data-raw/bowen_parcelmap/bowen_parcelmap.gpkg"))
}

bowen_parcelmap <- st_read(here("data-raw/bowen_parcelmap/bowen_parcelmap.gpkg")) %>%
  st_transform(project_crs) %>%
  st_cast("MULTIPOLYGON") # Need this due to MULTISURFACE - see: https://gis.stackexchange.com/questions/389814/r-st-centroid-geos-error-unknown-wkb-type-12

usethis::use_data(bowen_parcelmap, overwrite = TRUE)

bowen_parcelmap_union <- bowen_parcelmap %>%
  dplyr::filter(OwnerType == "Private") %>%
  st_make_valid() %>%
  st_union() %>%
  st_difference(dissolved_protectedareas) %>%
  st_snap(x = ., y = ., tolerance = 0.1) %>%
  st_buffer(5)
privateland <- bowen_parcelmap_union %>%
  ms_simplify(keep = 0.03, keep_shapes = FALSE)

usethis::use_data(privateland, overwrite = TRUE)
