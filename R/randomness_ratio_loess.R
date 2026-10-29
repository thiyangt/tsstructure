#' Randomness Ratio Based on LOESS
#'
#' Measures the proportion of variation remaining after fitting a
#' locally weighted regression (LOESS) curve to a time series.
#'
#' @param y A numeric time series.
#' @param plot Logical; if TRUE, plots the observed series and fitted curve.
#'
#' @return A numeric value representing the ratio of residual variance
#'   to the variance of the original series.
#'   
#' @examples
#' set.seed(123)
#' t <- seq_len(200)
#' y <- 10 + 3 * sin(2 * pi * t / 50) + rnorm(200, sd = 0.3)
#' randomness_ratio_loess(y, plot=TRUE)
#'
#' @export
randomness_ratio_loess <- function(y, plot = FALSE) {
  
  n <- length(y)
  t <- seq_len(n)
  
  if (n < 10L) {
    stop("`y` must contain at least 10 observations.", call. = FALSE)
  }
  
  if (!is.numeric(y) || any(!is.finite(y))) {
    stop("`y` must contain finite numeric values.", call. = FALSE)
  }
  
  if (stats::var(y) == 0) {
    return(0)
  }
  
  # Automatically select the smoothing span
  sp <- min(0.75, max(0.2, 10 / n))
  
  # Fit LOESS
  fit <- stats::loess(
    y ~ t,
    span = sp,
    control = stats::loess.control(surface = "direct")
  )
  
  fitted_values <- stats::predict(fit)
  remainder <- y - fitted_values
  
  ratio <- stats::var(remainder) / stats::var(y)
  
  if (plot) {
    plot(
      t, y,
      type = "l",
      col = "grey70",
      main = "Actual versus LOESS",
      xlab = "Time",
      ylab = "Value"
    )
    
    lines(t, fitted_values, col = "red", lwd = 2)
  }
  
  ratio
}
