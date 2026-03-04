ggsave_drive <- function(file_path, plot, drive_folder_id, ...) {
  plot_width = 9
  plot_height = 12
  plot_res = 300

  ggplot2::ggsave(
    file_path,
    plot,
    width = plot_width,
    height = plot_height,
    units = "in",
    res = plot_res,
    device = ragg::agg_png,
    ...)
  googledrive::drive_put(
    media = file_path,
    path  = googledrive::as_id(drive_folder_id),
    name  = basename(file_path)
  )
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
