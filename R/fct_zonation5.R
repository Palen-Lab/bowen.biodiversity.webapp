#' zonation5
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' @export
zonation5 <- function(layers_df) {
  features_txt_path <- here::here("inst", "app", "zonation", "zonation_input", "features.txt")
  cat('"weight" "filename"',
      file = features_txt_path,
      sep = "\n")

  for(i in 1:nrow(layers_df)) {
    next_row <- paste0(layers_df[i, "weights"], " ", layers_df[i, "full_path"])
    cat(next_row,
        file = features_txt_path,
        append = TRUE,
        sep = "\n")
  }

  zonation5_location <- here::here("inst","app","zonation","zonation5")

  zonation5_mode <- "CAZ2"

  zonation5_settings <- here::here("inst","app","zonation","zonation_input","settings.z5")

  zonation5_output <- here::here("inst","app","zonation","zonation_output")

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
