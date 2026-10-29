#' Count the Number of  Large Jumps
#'
#' Counts unusually large changes between consecutive observations
#' using an automatically estimated threshold.
#'
#' @param y A numeric vector containing time series observations.
#'
#' @return An integer giving the number of detected large jumps.
#'
#' @details
#' The function calculates first differences and centers them at their
#' median. An automatic threshold is estimated using the Tukey upper
#' fence applied to the absolute deviations from the median difference.
#' If the absolute deviations have zero IQR, the function falls back
#' to a quantile-based threshold. If no meaningful threshold can be
#' estimated, zero is returned.
#'
#' @examples
#' y <- c(1, 2, 3, 10, 11, 100)
#' n_large_jumps(y)
#' y <- c(1, 2, 3, 10, 11, 15)
#' n_large_jumps(y)
#' y <- c(rnorm(100), rnorm(mean=50, n=100))
#' n_large_jumps(y)
#' @export
n_large_jumps <- function(y) {
  if (!is.numeric(y) || length(y) < 3 ||
      anyNA(y) || any(!is.finite(y))) {
    return(NA_integer_)
  }
  
  d <- diff(y)
  deviations <- abs(d - stats::median(d))
  
  q1 <- stats::quantile(deviations, 0.25, names = FALSE)
  q3 <- stats::quantile(deviations, 0.75, names = FALSE)
  iqr_dev <- q3 - q1
  
  # Automatic Tukey upper fence
  threshold <- q3 + 1.5 * iqr_dev
  
  # Fallback when the IQR is zero
  if (iqr_dev == 0) {
    positive_dev <- deviations[deviations > 0]
    
    if (length(positive_dev) == 0) {
      return(0L)
    }
    
    threshold <- stats::median(positive_dev)
  }
  
  as.integer(sum(deviations > threshold))
}
