#' Lag Transformation
#'
#' `lag_vec()` applies a Lag Transformation.
#'
#' @param x A numeric vector to be lagged.
#' @param lag Which lag (how far back) to be included in the differencing calculation.
#'  Negative lags are leads.
#'
#' @return A numeric vector
#'
#' @details
#'
#' __Benefits:__
#'
#' This function is `NA` padded by default so it works well with `dplyr::mutate()` operations.
#' The function allows both lags and leads (negative lags).
#'
#' __Lag Calculation__
#'
#' A lag is an offset of `lag` periods. `NA` values are returned for the number of `lag` periods.
#'
#' __Lead Calculation__
#'
#' A negative lag is considered a lead.
#'
#' @seealso
#'
#' Modeling and Advanced Lagging:
#' - [recipes::step_lag()] - Recipe for adding lags in `tidymodels` modeling
#' - [tk_augment_lags()] - Add many lags group-wise to a data.frame (tibble)
#'
#' Vectorized Transformations:
#'   - Box Cox Transformation: [box_cox_vec()]
#'   - Lag Transformation: [lag_vec()]
#'   - Differencing Transformation: [diff_vec()]
#'   - Rolling Window Transformation: [slidify_vec()]
#'   - Loess Smoothing Transformation: [smooth_vec()]
#'   - Fourier Series: [fourier_vec()]
#'   - Missing Value Imputation for Time Series: [ts_impute_vec()], [ts_clean_vec()]
#'
#' @examples
#' library(dplyr)
#' library(timetk)
#'
#' # --- VECTOR ----
#'
#' # Lag
#' 1:10 %>% lag_vec(lag = 1)
#'
#' # Lead
#' 1:10 %>% lag_vec(lag = -1)
#'
#'
#' # --- MUTATE ----
#'
#' m4_daily %>%
#'     group_by(id) %>%
#'     mutate(lag_1 = lag_vec(value, lag = 1))
#'
#'
#' @name lag_vec
#' @export
lag_vec <- function(x, lag = 1) {
    # Checks
    if (length(lag) > 1) stop(call. = FALSE, "lag_vec(length(lag) > 1): Multiple lags detected. Use tk_augment_lags().")

    UseMethod("lag_vec", x)
}

#' @export
lag_vec.default <- function(x, lag = 1) {
    stop(paste0("lag_vec: No method for class ", class(x)[[1]], "."), call. = FALSE)
}

#' @export
lag_vec.double <- function(x, lag = 1) {
    lag_calc(x, lag)
}

#' @export
lag_vec.integer <- function(x, lag = 1) {
    lag_calc(x, lag)
}

lag_calc <- function(x, lag) {
    ret_vec <- xts::lag.xts(
        x           = x,
        k           = lag,
        na.pad      = TRUE
    )

    pad_len <- length(x) - length(ret_vec)
    if (pad_len > 0) {
        ret_vec <- c(rep(NA, pad_len), ret_vec)
    }

    return(ret_vec)
}

