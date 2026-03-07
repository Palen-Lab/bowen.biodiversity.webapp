# Test reading layers from Supabase Storage
#
# Run from the web-application/ directory.
# Requires SUPABASE_URL set in web-application/.Renviron.

library(here)
readRenviron(here(".Renviron"))
source(here("R/utils-supabase.R"))

base_url <- Sys.getenv("SUPABASE_URL")
cat("SUPABASE_URL:", base_url, "\n\n")

# Helper: HTTP HEAD check — returns status code without downloading the file.
check_url <- function(key) {
  url  <- .supabase_public_url(key)
  code <- tryCatch(
    httr::status_code(httr::HEAD(url)),
    error = function(e) NA_integer_
  )
  cat(sprintf("  HEAD %-55s -> %s\n", key, code))
  invisible(code)
}

# Helper: try reading, print result or error.
try_rast <- function(key) {
  cat("-- rast_layer:", key, "\n")
  tryCatch({
    r <- rast_layer(key)
    cat("  OK | dims:", paste(dim(r), collapse = "x"), "| CRS:", terra::crs(r, describe = TRUE)$code, "\n")
  }, error = function(e) cat("  ERROR:", conditionMessage(e), "\n"))
}

try_vect <- function(key) {
  cat("-- vect_layer:", key, "\n")
  tryCatch({
    v <- vect_layer(key)
    cat("  OK | nrow:", nrow(v), "| geom:", sf::st_geometry_type(v)[1], "\n")
  }, error = function(e) cat("  ERROR:", conditionMessage(e), "\n"))
}

# All keys used by the web application
rast_keys <- c(
  # Species
  "2_species/total_richness.tif",
  "2_species/threatened_richness.tif",
  "2_species/birds_richness.tif",
  "2_species/sm_mammals_richness.tif",
  "2_species/herptiles_richness.tif",
  # Habitats
  "3_habitats/fw_richness.tif",
  "3_habitats/total_habitat_richness.tif",
  "3_habitats/fw_lakes.tif",
  "3_habitats/fw_ponds.tif",
  "3_habitats/fw_riparian.tif",
  "3_habitats/fw_streams.tif",
  "3_habitats/fw_wetlands.tif",
  "3_habitats/forests_OF.tif",
  "3_habitats/forests_MF.tif",
  "3_habitats/forests_YF.tif",
  "3_habitats/forests_YS.tif",
  "3_habitats/other_tem.tif",
  "3_habitats/intertidal.tif",
  # People
  "4_people/bowen_human_footprint.tif",
  "4_people/bowen_inat.tif",
  # Conservation Values
  "5_values/rankmap.tif",
  # Threats
  "6_threats/fire_index_40m.tif",
  "6_threats/fire_wui_40m.tif"
)

vect_keys <- c(
  # Base
  "1_base/boundary.gpkg",
  "1_base/ocean.gpkg",
  "1_base/roads.gpkg",
  "1_base/trails.gpkg",
  # Land Management — Protected Areas
  "7_land_management/existing_protected_areas.gpkg",
  "7_land_management/pa_candidates.gpkg",
  # Land Management — Development
  "7_land_management/biod_val_parcel.gpkg"
)

# --- 1. Probe URLs ----------------------------------------------------------
cat("=== URL availability (200 = exists, 400/404 = missing) ===\n")
cat("-- Rasters --\n")
for (k in rast_keys) check_url(k)
cat("-- Vectors --\n")
for (k in vect_keys) check_url(k)
cat("\n")

# --- 2. Read tests ----------------------------------------------------------
cat("=== Read tests ===\n")
for (k in rast_keys) try_rast(k)
for (k in vect_keys) try_vect(k)
