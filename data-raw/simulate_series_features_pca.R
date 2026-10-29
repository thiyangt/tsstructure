# ============================================================
# Simulate 100 time series, compute temporal features, and run PCA
# Each series has 120 observations (> 20)
# ============================================================

# Install once if needed:
# install.packages("mgcv")

set.seed(20261029)

# -----------------------------
# 1. Generate 100 time series
# -----------------------------
n_series <- 100
n_time <- 120
time <- seq_len(n_time)

zigzag_ids <- 1:8
jump_ids   <- 9:16
sine_ids   <- 17:24

series_list <- vector("list", n_series)
metadata <- data.frame(
  series_id = sprintf("series_%03d", seq_len(n_series)),
  pattern = character(n_series),
  n = rep(n_time, n_series),
  jump_position = rep(NA_integer_, n_series),
  jump_size = rep(NA_real_, n_series)
)

for (i in seq_len(n_series)) {
  level <- rnorm(1, mean = 20, sd = 5)
  slope <- runif(1, min = -0.05, max = 0.08)
  noise_sd <- runif(1, min = 0.8, max = 2.8)
  noise <- rnorm(n_time, mean = 0, sd = noise_sd)
  trend <- slope * time

  if (i %in% zigzag_ids) {
    # Strong alternating up/down movement
    amplitude <- runif(1, 3, 6.5)
    y <- level + trend + amplitude * (-1)^time + 0.4 * noise
    metadata$pattern[i] <- "zigzag"

  } else if (i %in% jump_ids) {
    # One large sudden level shift
    jump_at <- sample(30:90, 1)
    jump_size <- sample(c(-1, 1), 1) * runif(1, 8, 18)
    y <- level + trend + noise
    y[(jump_at + 1):n_time] <- y[(jump_at + 1):n_time] + jump_size
    metadata$pattern[i] <- "large_jump"
    metadata$jump_position[i] <- jump_at + 1L
    metadata$jump_size[i] <- jump_size

  } else if (i %in% sine_ids) {
    # Clear sinusoidal pattern
    period <- runif(1, 18, 42)
    amplitude <- runif(1, 4, 8)
    phase <- runif(1, 0, 2 * pi)
    y <- level + trend +
      amplitude * sin(2 * pi * time / period + phase) +
      0.4 * noise
    metadata$pattern[i] <- "sine"

  } else {
    # Ordinary simulated series: trend + AR(1) noise;
    # a subset has only weak seasonality.
    phi <- runif(1, 0.1, 0.8)
    innovations <- rnorm(n_time, 0, noise_sd)
    ar_noise <- numeric(n_time)
    ar_noise[1] <- innovations[1]

    for (j in 2:n_time) {
      ar_noise[j] <- phi * ar_noise[j - 1] + innovations[j]
    }

    y <- level + trend + ar_noise

    if (runif(1) < 0.25) {
      period <- runif(1, 20, 60)
      weak_amplitude <- runif(1, 0.3, 1.5)
      phase <- runif(1, 0, 2 * pi)
      y <- y + weak_amplitude * sin(2 * pi * time / period + phase)
    }

    metadata$pattern[i] <- "normal"
  }

  series_list[[i]] <- as.numeric(y)
}

stopifnot(length(series_list) == 100)
stopifnot(all(lengths(series_list) > 20))

# Long-format data frame: one row per time point
series_data <- data.frame(
  series_id = rep(metadata$series_id, each = n_time),
  time = rep(time, times = n_series),
  value = unlist(series_list, use.names = FALSE)
)

# Optional overview plot of all generated series
old_par <- par(no.readonly = TRUE)
par(mfrow = c(2, 2), mar = c(3, 3, 2, 1))
for (p in c("zigzag", "large_jump", "sine", "normal")) {
  ids <- which(metadata$pattern == p)
  plot(time, series_list[[ids[1]]], type = "l",
       main = paste("Example:", p), xlab = "Time", ylab = "Value")
}
par(old_par)

# -----------------------------
# 2. Feature functions
# -----------------------------

max_abs_diff <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) < 2) return(NA_real_)
  max(abs(diff(x)))
}

sd_diff <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) < 3) return(NA_real_)
  stats::sd(diff(x))
}

jump_score_sd <- function(x) {
  x <- as.numeric(x)
  d <- diff(x)
  d <- d[is.finite(d)]
  if (length(d) < 2) return(NA_real_)
  s <- stats::sd(d)
  if (!is.finite(s) || s == 0) return(0)
  max(abs(d - stats::median(d))) / s
}

# Number of differences that are unusually large relative to the
# distribution of first differences. This is a simple threshold rule,
# not a formal change-point test.
detect_jumps_all <- function(x, threshold = 3) {
  x <- as.numeric(x)
  d <- diff(x)
  d <- d[is.finite(d)]
  if (length(d) < 2) return(0L)

  center <- stats::median(d)
  robust_sd <- stats::mad(d, center = center, constant = 1.4826)

  if (!is.finite(robust_sd) || robust_sd == 0) {
    # Fall back to the usual standard deviation if MAD is zero
    robust_sd <- stats::sd(d)
  }
  if (!is.finite(robust_sd) || robust_sd == 0) return(0L)

  as.integer(sum(abs(d - center) > threshold * robust_sd))
}

# Returns TRUE when a high proportion of consecutive differences
# change sign. Zigzag detection can also flag noisy alternating series.
detect_zigzag <- function(x, proportion_threshold = 0.70) {
  x <- as.numeric(x)
  d <- diff(x)
  d <- d[is.finite(d)]
  signs <- sign(d)
  signs <- signs[signs != 0]

  if (length(signs) < 3) return(FALSE)
  reversal_rate <- mean(signs[-1] != signs[-length(signs)])
  reversal_rate >= proportion_threshold
}

jump_score_mean <- function(x) {
  x <- as.numeric(x)
  d <- diff(x)
  d <- d[is.finite(d)]
  if (length(d) < 2) return(NA_real_)
  mean_abs_diff <- mean(abs(d))
  if (!is.finite(mean_abs_diff) || mean_abs_diff == 0) return(0)
  max(abs(d)) / mean_abs_diff
}

# Fraction of total variation left unexplained by a smooth GAM.
# Values closer to 0 indicate a series well represented by a smooth
# trend; values closer to 1 indicate more variation remains unexplained.
randomness_ratio_gam <- function(x) {
  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop("Package 'mgcv' is required. Run install.packages('mgcv').")
  }

  x <- as.numeric(x)
  ok <- is.finite(x)
  x <- x[ok]
  n <- length(x)

  if (n < 10 || !is.finite(stats::sd(x)) || stats::sd(x) == 0) {
    return(NA_real_)
  }

  dat <- data.frame(y = x, t = seq_len(n))
  k <- min(10L, max(4L, floor(n / 5)))
  fit <- mgcv::gam(y ~ s(t, k = k), data = dat, method = "REML")

  rss <- sum(stats::residuals(fit)^2)
  tss <- sum((x - mean(x))^2)
  if (tss == 0) return(0)
  max(0, min(1, rss / tss))
}

# Fraction of total variation left unexplained by LOESS.
randomness_ratio_loess <- function(x, span = 0.75) {
  x <- as.numeric(x)
  ok <- is.finite(x)
  x <- x[ok]
  n <- length(x)

  if (n < 10 || !is.finite(stats::sd(x)) || stats::sd(x) == 0) {
    return(NA_real_)
  }

  dat <- data.frame(y = x, t = seq_len(n))
  fit <- stats::loess(y ~ t, data = dat, span = span,
                      na.action = stats::na.exclude)
  fitted_values <- stats::predict(fit, newdata = dat)
  ok_fit <- is.finite(fitted_values)

  if (sum(ok_fit) < 3) return(NA_real_)
  rss <- sum((x[ok_fit] - fitted_values[ok_fit])^2)
  tss <- sum((x[ok_fit] - mean(x[ok_fit]))^2)
  if (tss == 0) return(0)
  max(0, min(1, rss / tss))
}

feature_names <- c(
  "max_abs_diff",
  "sd_diff",
  "jump_score_sd",
  "detect_jumps_all",
  "detect_zigzag",
  "jump_score_mean",
  "randomness_ratio_gam",
  "randomness_ratio_loess"
)

compute_features <- function(series) {
  c(
    max_abs_diff = max_abs_diff(series),
    sd_diff = sd_diff(series),
    jump_score_sd = jump_score_sd(series),
    detect_jumps_all = detect_jumps_all(series),
    detect_zigzag = as.numeric(detect_zigzag(series)),
    jump_score_mean = jump_score_mean(series),
    randomness_ratio_gam = randomness_ratio_gam(series),
    randomness_ratio_loess = randomness_ratio_loess(series)
  )
}

# -----------------------------
# 3. Compute features
# -----------------------------
feature_matrix <- t(vapply(
  series_list,
  compute_features,
  FUN.VALUE = setNames(numeric(length(feature_names)), feature_names)
))

feature_df <- data.frame(
  series_id = metadata$series_id,
  pattern = metadata$pattern,
  as.data.frame(feature_matrix, check.names = FALSE),
  check.names = FALSE
)

# -----------------------------
# 4. PCA on standardized features
# -----------------------------
X <- feature_df[, feature_names, drop = FALSE]

# PCA cannot use missing/infinite values. Median-impute any such values.
for (nm in names(X)) {
  bad <- !is.finite(X[[nm]])
  if (any(bad)) {
    good_values <- X[[nm]][!bad]
    replacement <- if (length(good_values)) median(good_values) else 0
    X[[nm]][bad] <- replacement
  }
}

# Remove features with zero variance before scaling.
keep <- vapply(X, function(z) stats::sd(z) > 0, logical(1))
if (sum(keep) < 2) stop("At least two non-constant features are needed for PCA.")

pca_fit <- stats::prcomp(X[, keep, drop = FALSE],
                         center = TRUE, scale. = TRUE)

pca_scores <- data.frame(
  series_id = metadata$series_id,
  pattern = metadata$pattern,
  PC1 = pca_fit$x[, 1],
  PC2 = if (ncol(pca_fit$x) >= 2) pca_fit$x[, 2] else 0
)

pca_variance <- data.frame(
  component = paste0("PC", seq_along(pca_fit$sdev)),
  standard_deviation = pca_fit$sdev,
  proportion_variance = pca_fit$sdev^2 / sum(pca_fit$sdev^2),
  cumulative_variance = cumsum(pca_fit$sdev^2 / sum(pca_fit$sdev^2))
)

# PCA scores plot
pattern_cols <- c(
  normal = "grey60",
  zigzag = "#D55E00",
  large_jump = "#0072B2",
  sine = "#009E73"
)
plot(
  pca_scores$PC1, pca_scores$PC2,
  col = pattern_cols[pca_scores$pattern],
  pch = 19,
  xlab = "PC1",
  ylab = "PC2",
  main = "PCA of simulated time-series features"
)
legend("topright", legend = names(pattern_cols),
       col = pattern_cols, pch = 19, bty = "n")

# -----------------------------
# 5. Optional tsstruct() call
# -----------------------------
# If tsstruct() is defined in your package/environment, you can also
# compute the features for an individual series using your interface:
#
# tsstruct(
#   x = series_list[[1]],
#   features = c(
#     "max_abs_diff", "sd_diff", "jump_score_sd",
#     "detect_jumps_all", "detect_zigzag", "jump_score_mean",
#     "randomness_ratio_gam", "randomness_ratio_loess"
#   )
# )
#
# The code above computes the same named features directly, so it does
# not depend on a particular tsstruct package implementation.

# -----------------------------
# 6. Export all outputs
# -----------------------------
write.csv(series_data, "simulated_100_series_long.csv", row.names = FALSE)
write.csv(metadata, "simulated_100_series_metadata.csv", row.names = FALSE)
write.csv(feature_df, "simulated_100_series_features.csv", row.names = FALSE)
write.csv(pca_scores, "simulated_100_series_pca_scores.csv", row.names = FALSE)
write.csv(pca_variance, "simulated_100_series_pca_variance.csv", row.names = FALSE)

# Keep objects available in the R session:
# series_list, series_data, metadata, feature_df, pca_fit, pca_scores,
# pca_variance
print(table(metadata$pattern))
print(head(feature_df))
print(pca_variance)
