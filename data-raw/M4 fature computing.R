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
