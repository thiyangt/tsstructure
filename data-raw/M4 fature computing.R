library(M4comp2018)
data("M4")
class(M4)
x_list <- lapply(M4, `[[`, "x")
x_list[[1]]
library(tsstructure)
feature_dfM4 <- tsstruct(
  x =  x_list,
  features = c("max_abs_diff", "sd_diff", "jump_score_sd", 
               "detect_jumps_all", "detect_zigzag", "jump_score_mean",
               "randomness_ratio_gam", "randomness_ratio_loess"))
library(tidyverse)
write_csv(feature_dfM4, file="data-raw/features_dfM4.csv")
summary(feature_dfM4)


# Features
features <- c(
  "max_abs_diff", "sd_diff", "jump_score_sd",
  "detect_jumps_all", "detect_zigzag", "jump_score_mean",
  "randomness_ratio_gam", "randomness_ratio_loess"
)
#if NA → replace with min - 0.1 × range
#if the feature has only one observed value → use min - 1
# Extract feature matrix
feature_mat <- as.data.frame(feature_dfM4[, features])

# Replace NA with a value outside the observed range
for (j in seq_along(feature_mat)) {
  
  x <- feature_mat[[j]]
  
  if (anyNA(x)) {
    
    observed <- x[is.finite(x)]
    
    if (length(observed) > 0) {
      
      xmin <- min(observed)
      xmax <- max(observed)
      r <- xmax - xmin
      
      # Value below the minimum
      replacement <- if (r > 0) {
        xmin - 0.1 * r
      } else {
        xmin - 1
      }
      
      x[is.na(x)] <- replacement
      feature_mat[[j]] <- x
    }
  }
}

# PCA
pca_M4 <- prcomp(
  feature_mat,
  center = TRUE,
  scale. = TRUE
)

# PCA scores
pca_scores <- as.data.frame(pca_M4$x)

# Add series ID
pca_scores$series_id <- rownames(feature_mat)

# Variance explained
summary(pca_M4)