load_rankmap <- function(filepath, project_crs) {
  filepath %>%
    rast() %>%
    project(project_crs)
}