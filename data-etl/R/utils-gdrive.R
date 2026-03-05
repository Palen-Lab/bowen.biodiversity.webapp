ggsave_template <- function(file_path, plot, ...) {
  ggplot2::ggsave(
    file_path,
    plot,
    width = 9,
    height = 12,
    units = "in",
    res = 300,
    device = ragg::agg_png,
    ...)
  invisible(file_path)
}

# dependency_obj is accepted only to create the dependency link in the targets
# graph; the file at file_path is uploaded directly without re-writing.
upload_gdrive <- function(dependency_obj, file_path, drive_folder_id, name = basename(file_path)) {
  googledrive::drive_put(
    media = file_path,
    path  = googledrive::as_id(drive_folder_id),
    name  = name
  )
  invisible(file_path)
}

# Writes an sf object as a shapefile into a dedicated subfolder.
# The subfolder name becomes the layer name (e.g. "boundary/boundary.shp").
# Returns the folder path so targets can track it with format = "file".
write_shapefile_folder <- function(sf_obj, folder_path) {
  dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)
  layer_name <- basename(folder_path)
  sf::st_write(sf_obj, file.path(folder_path, paste0(layer_name, ".shp")), delete_layer = TRUE)
  invisible(folder_path)
}

# Uploads all files in a local shapefile folder to a new Google Drive subfolder.
# Creates (or overwrites) a Drive subfolder named after the local folder's basename,
# then puts each shapefile component file into it.
upload_shapefile_folder_gdrive <- function(dependency_obj, folder_path, drive_folder_id) {
  layer_name <- basename(folder_path)
  subfolder <- googledrive::drive_mkdir(
    name      = layer_name,
    path      = googledrive::as_id(drive_folder_id),
    overwrite = TRUE
  )
  files <- list.files(folder_path, full.names = TRUE)
  lapply(files, function(f) {
    googledrive::drive_put(media = f, path = subfolder, name = basename(f))
  })
  invisible(folder_path)
}
