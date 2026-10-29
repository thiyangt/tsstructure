#' Jump Score_sd
#'
#' Measures the size of the largest absolute change relative to the
#' standard deviation of consecutive differences.
#'
#' @param y A numeric vector containing time series observations.
#'
#' @return The ratio of the maximum absolute difference to the
#' standard deviation of first differences. Returns \code{NA_real_}
#' if the standard deviation is zero or undefined.
#'
#' @examples
#' y <- c(1, 2, 3, 10, 11)
#' jump_score(y)
#'
#' @seealso [max_abs_diff()], [sd_diff()]
#'
#' @export
jump_score_sd <- function(y) {
  numerator <- max_abs_diff(y)
  denominator <- sd_diff(y)
  
  if (is.na(numerator) || is.na(denominator) ||
      denominator == 0) {
    return(NA_real_)
  }
  
  numerator / denominator
}