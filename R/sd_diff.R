#' Standard Deviation of First Differences
#'
#' Calculates the standard deviation of consecutive differences
#' in a time series.
#'
#' @param y A numeric vector containing time series observations.
#'
#' @return The standard deviation of the first differences.
#'
#' @examples
#' y <- c(1, 2, 3, 10, 11)
#' sd_diff(y)
#'
#' @export
sd_diff <- function(y) {
  if (length(y) < 3 || !is.numeric(y) || anyNA(y)) {
    return(NA_real_)
  }
  
  stats::sd(diff(y))
}