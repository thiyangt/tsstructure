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

#' Detect Large Jumps Automatically
#'
#' Detects unusually large changes in a time series using an
#' automatically estimated threshold.
#'
#' @param y A numeric vector of time series observations.
#' @scale scale parameter to control upper fence (q3 + scale * iqr_dev)
#'
#' @return A list containing the estimated threshold, jump positions,
#'   and number of detected jumps. Positions refer to the observation
#'   immediately after each detected jump.
#'
#' @examples
#' y1 <- c(10, 11, 12, 13, 14, 25, 26)
#' detect_jumps(y1)
#' 
#' y2 <- c(rnorm(100), rnorm(mean=3, n=100))
#' detect_jumps(y2)
#' n <- length(y2)
#' t <- 1:n
#' plot(y2~t, type="l")
#' @export
detect_jumps <- function(y, scale=3) {
  if (!is.numeric(y) || length(y) < 3 ||
      anyNA(y) || any(!is.finite(y))) {
    stop("y must be a finite numeric vector of length at least 3.")
  }
  
  d <- abs(diff(y))
  deviations <- abs(d - stats::median(d))
  
  q1 <- stats::quantile(deviations, 0.25, names = FALSE)
  q3 <- stats::quantile(deviations, 0.75, names = FALSE)
  iqr_dev <- q3 - q1
  
  threshold <- q3 + scale * iqr_dev
  
  # Handle zero-IQR cases
  if (iqr_dev == 0) {
    positive_dev <- deviations[deviations > 0]
    
    if (length(positive_dev) == 0) {
      jump_positions <- integer(0)
    } else {
      threshold <- min(positive_dev) / 2
      jump_positions <- which(deviations > threshold)
    }
  } else {
    jump_positions <- which(deviations > threshold)
  }
  
  list(
    threshold = threshold,
    jump_positions = jump_positions + 1L,
    n_jumps = length(jump_positions)
  )
}
