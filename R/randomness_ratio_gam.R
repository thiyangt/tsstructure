#' Randomness Ratio Based on a GAM
#'
#' Measures the proportion of variation in a time series that remains
#' unexplained after fitting a smooth nonlinear trend using a GAM.
#'
#' @param y A numeric time series.
#' @param plot Logical; if TRUE, plots the observed series and fitted GAM.
#'
#' @return A numeric value between 0 and 1 representing the proportion
#'   of variation remaining after the GAM fit.
#' @examples
#' set.seed(123)
#' t <- seq_len(200)
#' y <- 10 + 3 * sin(2 * pi * t / 50) + rnorm(200, sd = 0.3)
#' randomness_ratio_gam(y, plot=TRUE)
#' @export
randomness_ratio_gam <- function(y, plot = FALSE) {
  
  n <- length(y)
  t <- seq_len(n)
  
  if (stats::var(y) == 0)
    return(0)
  
  n <- length(y)
  t <- seq_len(n)
  
  if (n < 3)
    stop("`y` must contain at least 3 observations.")
  
  if (stats::var(y) == 0)
    return(0)
  
  # ------------------------------------------------------------
  # Short series: use a simple first-difference S/N measure
  # ------------------------------------------------------------
  if (n < 20) {
    
    dy <- diff(y)
    
    # Estimate noise variance from successive differences
    noise_variance <- stats::var(dy) / 2
    
    ratio <- noise_variance / stats::var(y)
    
    # Keep the measure in [0, 1]
    ratio <- min(max(ratio, 0), 1)
    
    if (plot) {
      plot(t, y, type = "b",
           main = "Short-series randomness",
           xlab = "Time", ylab = "Value")
    }
    
    return(ratio)
  }
  
  # Detect a candidate period using the ACF
  acf_values <- as.numeric(
    stats::acf(y, lag.max = floor(n / 2), plot = FALSE)$acf
  )[-1]
  
  candidate_lags <- seq_along(acf_values)
  
  # Find positive local maxima
  peaks <- which(
    acf_values[2:(length(acf_values) - 1)] >
      acf_values[1:(length(acf_values) - 2)] &
      acf_values[2:(length(acf_values) - 1)] >
      acf_values[3:length(acf_values)]
  ) + 1L
  
  period <- if (length(peaks)) {
    candidate_lags[peaks[which.max(acf_values[peaks])]]
  } else {
    NA_integer_
  }
  
  # Fit a cyclic GAM if a plausible period is detected
  if (!is.na(period) && period >= 4 && period <= n / 2) {
    
    season <- (t - 1) %% period + 1
    
    fit <- mgcv::gam(
      y ~ s(season, bs = "cc", k = min(period, n - 1)),
      method = "REML",
      knots = list(season = c(0.5, period + 0.5))
    )
    
  } else {
    
    k <- min(10 + floor(sqrt(n)), floor(n / 2))
    
    fit <- mgcv::gam(
      y ~ s(t, k = k),
      method = "REML"
    )
  }
  
  fitted_values <- stats::predict(fit)
  remainder <- y - fitted_values
  
  ratio <- stats::var(remainder) / stats::var(y)
  
  if (plot) {
    plot(t, y, type = "l", col = "grey70",
         main = paste("Actual versus GAM; period =", period),
         xlab = "Time", ylab = "Value")
    lines(t, fitted_values, col = "blue", lwd = 2)
  }
  
  ratio
}
