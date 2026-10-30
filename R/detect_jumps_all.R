#' Detect Large Jumps Automatically
#'
#' Detects unusually large changes in a time series using an
#' automatically estimated threshold.
#'
#' @param y A numeric vector of time series observations.
#' @param quant supper tail probability
#' @param detailed If detailed true you will get locations of the jumps, number of jumps and threshold
#'
#' @return If detailed=TRUE, A list containing the estimated threshold, jump positions,
#'   and number of detected jumps. Positions refer to the observation
#'   immediately after each detected jump otherwise only number o jumps.
#'
#' @examples
#' y1 <- c(10, 11, 12, 13, 14, 25, 26)
#' detect_jumps_all(y1)
#' 
#' y2 <- c(rnorm(100), rnorm(mean=3, n=100))
#' detect_jumps_all(y2)
#' n <- length(y2)
#' t <- 1:n
#' plot(y2~t, type="l")
#' @export
detect_jumps_all <- function(y, quant=0.97, detailed=FALSE) {
  if (!is.numeric(y) || length(y) < 3 ||
      anyNA(y) || any(!is.finite(y))) {
    stop("y must be a finite numeric vector of length at least 3.")
  }
  
  deviations <- abs(diff(y))
 # deviations <- abs(d - stats::median(d))
  
 # q1 <- stats::quantile(deviations, 0.25, names = FALSE)
  q3 <- stats::quantile(deviations, quant, names = FALSE)
 # iqr_dev <- q3 - q1
  
  threshold <- q3 
  

    jump_positions <- which(deviations > threshold)
 
  
  if(detailed==TRUE){
  list(
    threshold = threshold,
    jump_positions = jump_positions + 1L,
    n_jumps = length(jump_positions)
  )} else{
    n_jumps = length(jump_positions)
  }
}

