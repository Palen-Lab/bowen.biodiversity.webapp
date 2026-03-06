# Supabase S3-compatible storage upload utilities
#
# Mirrors utils-gdrive.R but targets Supabase Storage instead.
# Requires env vars (set in .Renviron):
#   SUPABASE_URL        e.g. https://abcdefgh.supabase.co
#   SUPABASE_S3_KEY_ID  from Supabase dashboard → Storage → S3 Access Keys
#   SUPABASE_S3_SECRET  (same location)
#
# Bucket layout mirrors inst/extdata/:
#   2_species/total_richness.tif
#   3_habitats/fw_richness.tif
#   ...

SUPABASE_BUCKET <- "Assets"

# Internal: authenticated S3 client pointed at Supabase
.supabase_s3 <- function() {
  url <- Sys.getenv("SUPABASE_URL", unset = NA)
  key <- Sys.getenv("SUPABASE_S3_KEY_ID", unset = NA)
  sec <- Sys.getenv("SUPABASE_S3_SECRET", unset = NA)
  if (any(is.na(c(url, key, sec)))) {
    stop("Set SUPABASE_URL, SUPABASE_S3_KEY_ID, SUPABASE_S3_SECRET in .Renviron")
  }
  paws.storage::s3(config = list(
    credentials = list(creds = list(access_key_id = key, secret_access_key = sec)),
    endpoint    = paste0(url, "/storage/v1/s3"),
    region      = "ca-central-1"   # required by Supabase even though unused
  ))
}

# Upload a single file to Supabase Storage.
# `key` is the path within the bucket — should match the inst/extdata/ relative
# path so the app helper resolves it correctly (e.g. "2_species/total_richness.tif").
#
# dependency_obj is accepted only to create the dependency link in the targets
# graph; the file at file_path is uploaded directly.
upload_supabase <- function(dependency_obj, file_path,
                            key    = basename(file_path),
                            bucket = SUPABASE_BUCKET) {
  .supabase_s3()$put_object(
    Bucket = bucket,
    Key    = key,
    Body   = file_path
  )
  invisible(file_path)
}

# Upload all files in a shapefile folder to Supabase Storage.
# Set `prefix` to the category subfolder (e.g. "1_base") so the key layout
# mirrors inst/extdata/ (e.g. "1_base/boundary/boundary.shp").
upload_shapefile_folder_supabase <- function(dependency_obj, folder_path,
                                             prefix = "",
                                             bucket = SUPABASE_BUCKET) {
  s3         <- .supabase_s3()
  layer_name <- basename(folder_path)
  files      <- list.files(folder_path, full.names = TRUE)
  lapply(files, function(f) {
    key <- if (nzchar(prefix)) {
      paste(prefix, layer_name, basename(f), sep = "/")
    } else {
      paste(layer_name, basename(f), sep = "/")
    }
    s3$put_object(Bucket = bucket, Key = key, Body = f)
  })
  invisible(folder_path)
}

# Upload a single GeoPackage file to Supabase Storage.
# Set `prefix` to the category subfolder (e.g. "7_land_management") so the key
# layout mirrors inst/extdata/ (e.g. "7_land_management/zoning.gpkg").
upload_gpkg_supabase <- function(dependency_obj, file_path,
                                 prefix = "",
                                 bucket = SUPABASE_BUCKET) {
  key <- if (nzchar(prefix)) {
    paste(prefix, basename(file_path), sep = "/")
  } else {
    basename(file_path)
  }
  .supabase_s3()$put_object(Bucket = bucket, Key = key, Body = file_path)
  invisible(file_path)
}
