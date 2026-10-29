#' Calculate the Randomness Ratio of a Time Series
#'
#' Fits a generalized additive model (GAM) with a smooth function of time
#' to capture linear and nonlinear temporal patterns. The randomness ratio
#' is calculated as the variance of the residuals divided by the variance
#' of the original observations.
#'
#' A low randomness ratio indicates that a large proportion of the
#' variation is explained by the fitted smooth trend. A high ratio
#' indicates that more variation remains unexplained by the fitted model.
#'
#' @param y A numeric vector containing the time series observations.
#' @param span span parameter to control smoothing
#' @param plot TRUE or FALSE indicating whether we need to output plot or not.
#'
#' @return Plot and a numeric value representing the randomness ratio:
#' \deqn{\frac{\operatorname{Var}(y_t - \hat{f}(t))}
#' {\operatorname{Var}(y_t)}}
#' where \eqn{\hat{f}(t)} is the fitted smooth function of time.
#'
#' @details
#' The function uses [mgcv::gam()] with a penalized smooth term,
#' \code{s(t)}, and selects the smoothing parameter using restricted
#' maximum likelihood (\code{method = "REML"}).
#'
#' The ratio is not necessarily bounded by zero and one because the
#' residual variance and original variance are calculated separately.
#' Its value depends on the smoothness of the fitted model and the
#' characteristics of the time series.
#'
#' @references
#' Wood, S. N. (2017). \emph{Generalized Additive Models: An Introduction
#' with R} (2nd ed.). Chapman and Hall/CRC.
#'
#' @seealso [mgcv::gam()], [mgcv::s()]
#'
#' @examples
#' set.seed(123)
#' t <- seq_len(200)
#' A smooth periodic series with small random noise
#' y <- 10 + 3 * sin(2 * pi * t / 50) + rnorm(200, sd = 0.3)
#' randomness_ratio(y)
#' 
#'
#' @importFrom mgcv gam
#' @export
randomness_ratio_loess <- function(y, sp = 0.75, plot=TRUE) {
  n <- length(y)
  t <- seq_len(n)
  
  # Fit a flexible nonlinear function
  fit <-  loess(y ~ t, span = sp)

  
  fitted_values <- predict(fit)
  remainder <- y - fitted_values
  
  # Compare fitted curves
  if(plot==TRUE){
  plot(t, y, type = "l", col = "grey70",
       main = "Actual versus LOESS",
       xlab = "Time", ylab = "Value")
  
  lines(t, fitted_values, col = "red", lwd = 2)
 # lines(t, pred_gam, col = "red", lwd = 2)
  }
  
  var(remainder) / var(y)
  
}

