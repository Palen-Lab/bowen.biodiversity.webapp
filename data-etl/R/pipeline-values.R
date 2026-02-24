load_rankmap <- function(project_crs) {
  here("data-3-outputs/5_values/rankmap.tif") %>%
    rast() %>%
    project(project_crs)
}