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
