## code to prepare `bowen_parcelmap` dataset goes here
library(sf)
library(here)

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

bowen_parcelmap <- st_read(here("data-raw/bowen_parcelmap/bowen_parcelmap.gpkg"))
usethis::use_data(bowen_parcelmap, overwrite = TRUE)
