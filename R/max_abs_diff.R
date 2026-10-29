#' Maximum Absolute Difference
#'
#' Calculates the largest absolute difference between consecutive
#' observations in a time series.
#'
#' @param y A numeric vector containing time series observations.
#'
#' @return A numeric value giving the maximum absolute difference.
#'
#' @examples
#' y <- c(1, 2, 3, 10, 11)
#' max_abs_diff(y)
#'
#' @export
max_abs_diff <- function(y) {
  if (length(y) < 2 || !is.numeric(y) || anyNA(y)) {
    return(NA_real_)
  }
  
  max(abs(diff(y)))
}