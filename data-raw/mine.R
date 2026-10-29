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


feature_df <- tsstruct(
x =  series_list,
features = c("max_abs_diff", "sd_diff", "jump_score_sd", 
"detect_jumps_all", "detect_zigzag", "jump_score_mean", "randomness_ratio_gam",
"randomness_ratio_loess"))


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



head(pca_scores)
head(series_list)
