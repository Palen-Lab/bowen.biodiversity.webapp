# Supabase Storage layer loading helpers
#
# These replace bare here::here("inst/extdata/...") calls throughout the modules.
# In development (local files present) they read from inst/extdata/ directly.
# In production (files absent) they stream from the public Supabase bucket.
#
# Requires env var (set in .Renviron):
#   SUPABASE_URL   e.g. https://abcdefgh.supabase.co
#
# The bucket "bowen-biodiversity" must be set to PUBLIC in Supabase dashboard.

.supabase_public_url <- function(key) {
  base <- Sys.getenv("SUPABASE_URL", unset = "")
  if (!nzchar(base)) stop("Set SUPABASE_URL in .Renviron")
  paste0(base, "/storage/v1/object/public/Assets/", key)
}

# Load a raster layer.
# `key` matches the inst/extdata/ relative path, e.g. "2_species/total_richness.tif"
# In prod, uses GDAL /vsicurl/ to stream the GeoTIFF without downloading it.
rast_layer <- function(key) {
  local <- here::here("inst/extdata", key)
  # if (file.exists(local)) {
  #   return(terra::rast(local))
  # }
  url <- .supabase_public_url(key)
  terra::rast(paste0("/vsicurl/", url))
}

# Load a vector layer (gpkg, shp, etc.).
# `key` matches the inst/extdata/ relative path, e.g. "7_protected_areas/existing_protected_areas.gpkg"
# In prod, downloads to a temp file then reads with sf.
vect_layer <- function(key) {
  local <- here::here("inst/extdata", key)
  if (file.exists(local)) {
    return(sf::read_sf(local))
  }
  url  <- .supabase_public_url(key)
  tmp  <- tempfile(fileext = paste0(".", tools::file_ext(key)))
  utils::download.file(url, tmp, quiet = TRUE, mode = "wb")
  sf::read_sf(tmp)
}
