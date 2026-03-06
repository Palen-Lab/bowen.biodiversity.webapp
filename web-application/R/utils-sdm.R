#' Only keep top or bottom cell values by quantile
#'
#' @param x SpatRaster
#' @param prob Quantile between 0 and 1
#' @param direction "top" or "bottom", for direction of keep / discard cell values
#'
#' @returns SpatRaster
#' @export
remove_by_quantile <- function(x, prob, direction = "top") {
  val <- x %>%
    terra::values() %>%
    quantile(probs = prob, na.rm = T)
  min_val <- x %>%
    terra::values() %>%
    min(na.rm = T)
  max_val <- x %>%
    terra::values() %>%
    max(na.rm = T)
  if(direction == "top") {
    m <- matrix(c(
      min_val, val, NA,
      val, max_val, 1
    ), ncol = 3, byrow = T)
  } else if (direction == "bottom") {
    m <- matrix(c(
      min_val, val, 1,
      val, max_val, NA
    ), ncol = 3, byrow = T)
  }
  mask_quantile <- x %>%
    terra::classify(m, include.lowest = T)
  mask_quantile * x
}
