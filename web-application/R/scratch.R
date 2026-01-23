# test_raster <- terra::rast(here::here("inst/extdata/3_habitats/fw_streams.tif"))
#
# levels(test_raster) <- data.frame(id=1:3, cover=c("one", "two", "three"))
#
# test_raster <- test_raster %>%
#   terra::project("epsg:3857")
#
# raster_labels <- c("one", "two", "three")
# raster_domain <- c(1, 2, 3)
# raster_pal <- leaflet::colorFactor(
#   palette = c('red', "blue", "green"),
#   domain = raster_domain,
#   na.color = "transparent"
# )
#
# leaflet::leaflet() %>%
#   leaflet::addRasterImage(x = test_raster,
#                           colors = raster_pal) %>%
#   leaflet::addLegend(
#     colors = raster_pal(raster_domain),
#     labels = raster_labels
#   )
#
# ######
# test_raster <- terra::rast(here::here("inst/extdata/3_habitats/fw_richness.tif"))
# test_raster
#
# values(test_raster) %>%
#   unique()
#
# test_raster %>%
#   project("epsg:3857", method = "near") %>%
#   values() %>%
#   unique()

