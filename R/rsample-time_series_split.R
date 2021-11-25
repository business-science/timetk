#' Simple Training/Test Set Splitting for Time Series
#'
#' `time_series_split` creates resample splits using [time_series_cv()] but
#' returns only a __single split.__ This is useful when creating a single
#' train/test split.
#'
#' @inheritParams time_series_cv
#' @param slice Returns a single slice from [time_series_cv]
#'
#' @details
#'
#' __Time-Based Specification__
#'
#'  The `initial`, `assess`, `skip`, and `lag` variables can be specified as:
#'
#' - Numeric: `initial = 24`
#' - Time-Based Phrases: `initial = "2 years"`, if the `data` contains
#'  a `date_var` (date or datetime)
#'
#' __Initial (Training Set) and Assess (Testing Set)__
#'
#' The main options, `initial` and `assess`, control the number of
#'  data points from the original data that are in the analysis (training set)
#'  and the assessment (testing set), respectively.
#'
#'
#' __Skip__
#'
#' `skip` enables the function to not use every data point in the resamples.
#'  When `skip = 1`, the resampling data sets will increment by one position.
#'
#'  Example: Suppose that the rows of a data set are consecutive days. Using `skip = 7`
#'  will make the analysis data set operate on *weeks* instead of days. The
#'  assessment set size is not affected by this option.
#'
#' __Lag__
#'
#' The Lag parameter creates an overlap between the Testing set. This is needed
#' when lagged predictors are used.
#'
#' __Cumulative vs Sliding Window__
#'
#' When `cumulative = TRUE`, the `initial` parameter is ignored and the
#' analysis (training) set will grow as
#'  resampling continues while the assessment (testing) set size will always remain
#'  static.
#'
#' When `cumulative = FALSE`, the `initial` parameter fixes the analysis (training)
#' set and resampling occurs over a fixed window.
#'
#' __Slice__
#'
#' This controls which slice is returned. If `slice = 1`, only the most recent
#' slice will be returned.
#'
#'
#' @return An `rsplit` object that can be used with the `training` and `testing`
#'  functions to extract the data in each split.
#'
#' @seealso
#'
#' - [time_series_cv()] and [rsample::rolling_origin()] - Functions used to create
#'   time series resample specifications.
#'
#' @examples
#' library(tidyverse)
#' library(timetk)
#'
#' # DATA ----
#' m750 <- m4_monthly %>% filter(id == "M750")
#'
#' # Get the most recent 3 years as testing, and previous 10 years as training
#' m750 %>%
#'     time_series_split(initial = "10 years", assess = "3 years")
#'
#' # Skip the most recent 3 years
#' m750 %>%
#'     time_series_split(
#'         initial = "10 years",
#'         assess  = "3 years",
#'         skip    = "3 years",
#'         slice   = 2          # <- Returns 2nd slice, 3-years back
#'     )
#'
#' # Add 1 year lag for testing overlap
#' m750 %>%
#'     time_series_split(
#'         initial = "10 years",
#'         assess  = "3 years",
#'         skip    = "3 years",
#'         slice   = 2,
#'         lag     = "1 year"   # <- Overlaps training/testing by 1 year
#'     )
#'
#'
#' @export
time_series_split <- function(data, date_var = NULL, initial = 5, assess = 1,
                              skip = 1, lag = 0, cumulative = FALSE, slice = 1,
                              point_forecast = FALSE, ...) {

    date_var_expr <- rlang::enquo(date_var)

    time_series_cv(
        data,
        date_var       = !! date_var_expr,
        initial        = initial,
        assess         = assess,
        skip           = skip,
        lag            = lag,
        cumulative     = cumulative,
        slice_limit    = slice,
        point_forecast = point_forecast
    ) %>%
        dplyr::slice(slice) %>%
        purrr::pluck("splits", 1)
}

#' @export
#' @importFrom rsample complement
complement.ts_cv_split <- function(x, ...) {
    if (!all(is.na(x$out_id))) {
        return(x$out_id)
    } else {
        setdiff(1:nrow(x$data), x$in_id)
    }
}
