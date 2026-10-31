#' Detect Zig-Zag Patterns in a Time Series
#'
#' Identifies direction reversals in a time series by examining the
#' signs of consecutive first differences.
#'
#' @param y A numeric vector of time series observations.
#' @param min_reversals Minimum number of direction reversals required
#'   to classify a series as zig-zag. Defaults to 3.
#' @param detailed if detailed=TRUE, you get all detailed otherwise only zigzagrate 
#'
#' @return if detailed=FALSE returns zigzag rate, otherwise a list containing:
#' \describe{
#'   \item{is_zigzag}{Whether the series meets the reversal criterion.}
#'   \item{n_reversals}{Number of direction reversals.}
#'   \item{zigzag_rate}{Proportion of successive non-zero differences
#'   that reverse direction. Zigzag rate measures how frequently a time series changes direction. It indicates whether the series has a smooth pattern or frequently alternates between increasing and decreasing.}
#'   \item{reversal_positions}{Indices of observations where reversals
#'   occur.}
#' }
#'
#' @details
#' Zero differences are ignored when identifying direction reversals.
#' A reversal is recorded at observation i when the non-zero
#' differences on either side of that observation have opposite signs.
#' The zig-zag classification is based on the specified minimum
#' number of reversals.
#'
#' @examples
#' y <- c(1, 5, 2, 6, 3, 7, 4)
#' detect_zigzag(y)
#'
#' @export
detect_zigzag <- function(y, min_reversals = 3, detailed = FALSE) {
  
  if (!is.numeric(y) || length(y) < 3 ||
      anyNA(y) || any(!is.finite(y))) {
    stop("y must be a finite numeric vector of length at least 3.")
  }
  
  if (length(min_reversals) != 1 ||
      !is.finite(min_reversals) ||
      min_reversals < 1 ||
      min_reversals != as.integer(min_reversals)) {
    stop("min_reversals must be a positive integer.")
  }
  
  d <- diff(y)
  
  # Remove zero differences
  nonzero_idx <- which(d != 0)
  d_nonzero <- d[nonzero_idx]
  
  # Not enough non-zero differences
  if (length(d_nonzero) < 2) {
    
    if (detailed) {
      return(list(
        is_zigzag = FALSE,
        n_reversals = 0L,
        zigzag_rate = 0,
        reversal_positions = integer(0)
      ))
    } else {
      return(0)
    }
  }
  
  # Identify changes in direction
  reversals <- which(
    sign(d_nonzero[-1]) != sign(d_nonzero[-length(d_nonzero)])
  )
  
  # Map reversals to observation positions
  reversal_positions <- nonzero_idx[reversals] + 1L
  
  n_reversals <- length(reversals)
  
  zigzag_rate <- n_reversals / (length(d_nonzero) - 1L)
  
  if (detailed) {
    
    return(list(
      is_zigzag = n_reversals >= min_reversals,
      n_reversals = n_reversals,
      zigzag_rate = zigzag_rate,
      reversal_positions = reversal_positions
    ))
    
  } else {
    
    return(zigzag_rate)
    
  }
}