
#' Compute Structural Features for a List of Time Series
#'
#' Apply selected feature functions from the tsstructure package
#' to each time series in a list.
#'
#' @param x A list of numeric time series.
#' @param features A character vector containing the names of feature
#'   functions to compute. Each function must accept a numeric vector
#'   as its first argument and return a single value.
#'
#' @return A data frame with one row per time series and one column
#'   per selected feature, in addition to the series name.
#'
#' @examples
#' series <- list(
#'   linear = 1:20,
#'   rand = rnorm(100),
#'   zigzag = c(1, 5, 2, 6, 3, 7, 4, 5, 6, 5, 6, 5, 6, 5, 6, 5, 6, 5, 6, 5, 6, 5,6),
#'   seasonal = sin(2 * pi * (1:100) / 12)
#' )
#'
#' tsstruct(
#'   x = series,
#'   features = c("max_abs_diff", "sd_diff", "jump_score_sd", 
#'   "detect_jumps_all", "detect_zigzag", "jump_score_mean", "randomness_ratio_gam",
#'   "randomness_ratio_loess")
#' )
#'
#' @export
tsstruct <- function(x, features) {
  
  # Validate inputs
  if (!is.list(x) || length(x) == 0L) {
    stop("`x` must be a non-empty list of time series.",
         call. = FALSE)
  }
  
  if (missing(features) || !is.character(features) ||
      length(features) == 0L || anyNA(features)) {
    stop("`features` must be a non-empty character vector of function names.",
         call. = FALSE)
  }
  
  if (anyDuplicated(features)) {
    stop("`features` must not contain duplicate function names.",
         call. = FALSE)
  }
  
  # Check that each feature function exists in the package namespace
  available <- vapply(
    features,
    exists,
    logical(1),
    envir = asNamespace("tsstructure"),
    inherits = FALSE
  )
  
  if (any(!available)) {
    stop(
      "Unknown feature function(s): ",
      paste(features[!available], collapse = ", "),
      call. = FALSE
    )
  }
  
  # Retrieve the selected functions
  feature_functions <- lapply(
    features,
    get,
    envir = asNamespace("tsstructure"),
    inherits = FALSE
  )
  names(feature_functions) <- features
  
  # Assign default names if needed
  if (is.null(names(x))) {
    names(x) <- paste0("series_", seq_along(x))
  }
  
  # Compute features for each series
  results <- lapply(seq_along(x), function(i) {
    
    y <- x[[i]]
    
    if (!is.numeric(y) || length(y) < 2L ||
        any(!is.finite(y))) {
      stop(
        sprintf(
          "Series '%s' must contain at least two finite numeric values.",
          names(x)[i]
        ),
        call. = FALSE
      )
    }
    
    values <- lapply(feature_functions, function(f) f(y))
    
    # Require one result per feature
    valid <- vapply(values, length, integer(1)) == 1L
    
    if (any(!valid)) {
      stop(
        "Each feature function must return one value. Check: ",
        paste(features[!valid], collapse = ", "),
        call. = FALSE
      )
    }
    
    data.frame(
      series = names(x)[i],
      as.data.frame(
        setNames(values, features),
        optional = TRUE
      ),
      check.names = FALSE
    )
  })
  
  rownames(results) <- NULL
  do.call(rbind, results)
}
