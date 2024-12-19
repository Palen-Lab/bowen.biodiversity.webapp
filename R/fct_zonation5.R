#' zonation5
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
zonation5 <- function() {
  # Create features.txt from files available in data
  features <- list.files("inst/app/rasters",
                         recursive = T,
                         full.names = T)

  weights <- lapply(features, function(x) {"1.0"})

  cat('"weight" "filename"',
      file = "inst/app/zonation/zonation_input/features.txt",
      sep = "\n")

  for(i in 1:length(features)) {
    next_row <- paste0(weights[i], " ", features[i])
    cat(next_row,
        file = "inst/app/zonation/zonation_input/features.txt",
        append = TRUE,
        sep = "\n")
  }

  zonation5_location <- file.path("inst/app/zonation/zonation5") %>%
    normalizePath()

  zonation5_mode <- "CAZ2"

  zonation5_settings <- file.path("inst/app/zonation/zonation_input/settings.z5") %>%
    normalizePath()

  zonation5_output <- file.path("inst/app/zonation/zonation_output") %>%
    normalizePath()

  zonation5_command <- stringr::str_glue(
    '{zonation5_location} --mode={zonation5_mode} {zonation5_settings} {zonation5_output}',
    zonation5_location = zonation5_location,
    zonation5_mode = zonation5_mode,
    zonation5_settings = zonation5_settings,
    zonation5_output = zonation5_output
  ) %>%
    stringr::str_c()

  system(command = zonation5_command)

}
