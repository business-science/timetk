#' Log-Interval Transformation for Constrained Interval Forecasting
#'
#' The `log_interval_vec()` transformation constrains a forecast to an interval
#' specified by an `upper_limit` and a `lower_limit`. The transformation provides
#' similar benefits to `log()` transformation, while ensuring the inverted transformation
#' stays within an upper and lower limit.
#'
#' @param x A positive numeric vector.
#' @param limit_lower A lower limit. Must be less than the minimum value.
#'  If set to "auto", selects zero.
#' @param limit_upper An upper limit. Must be greater than the maximum value.
#'  If set to "auto",  selects a value that is 10% greater than the maximum value.
#' @param offset An offset to include in the log transformation.
#'  Useful when the data contains values less than or equal to zero.
#' @param silent Whether or not to report the parameter selections as a message.
#'
#' @details
#'
#' __Log Interval Transformation__
#'
#' The Log Interval Transformation constrains values to specified upper and lower limits.
#' The transformation maps limits to a function:
#'
#' `log(((x + offset) - a)/(b - (x + offset)))`
#'
#' where `a` is the lower limit and `b` is the upper limit
#'
#' __Inverse Transformation__
#'
#' The inverse transformation:
#'
#' `(b-a)*(exp(x)) / (1 + exp(x)) + a - offset`
#'
#'
#'
#' @seealso
#'   - Box Cox Transformation: [box_cox_vec()]
#'   - Lag Transformation: [lag_vec()]
#'   - Differencing Transformation: [diff_vec()]
#'   - Rolling Window Transformation: [slidify_vec()]
#'   - Loess Smoothing Transformation: [smooth_vec()]
#'   - Fourier Series: [fourier_vec()]
#'   - Missing Value Imputation & Anomaly Cleaning for Time Series: [ts_impute_vec()], [ts_clean_vec()]
#'
#' Other common transformations to reduce variance: `log()`, `log1p()` and `sqrt()`
#'
#' @references
#' - [Forecasting: Principles & Practices: Forecasts constrained to an interval](https://otexts.com/fpp2/limits.html)
#'
#' @examples
#' library(dplyr)
#' library(timetk)
#'
#' values_trans <- log_interval_vec(1:10, limit_lower = 0, limit_upper = 11)
#' values_trans
#'
#' values_trans_forecast <- c(values_trans, 3.4, 4.4, 5.4)
#'
#' values_trans_forecast %>%
#'     log_interval_inv_vec(limit_lower = 0, limit_upper = 11) %>%
#'     plot()
#'
#'
#' @name log_interval_vec
#' @export
NULL

#' @rdname log_interval_vec
#' @export
log_interval_vec <- function(x, limit_lower = "auto", limit_upper = "auto", offset = 0, silent = FALSE) {

    x <- x + offset
    if (any(x <= 0)) rlang::abort("x <= 0: Try using an offset to avoid values less than or equal to zero.")

    max_x   <- max(x)
    min_x   <- min(x)
    range_x <- abs(max_x - min_x)

    # Convert character strings to numeric
    limit_lower <- auto_limit_lower(limit_lower, min_x, range_x)
    limit_upper <- auto_limit_upper(limit_upper, max_x, range_x)

    # Checks
    if (any(is.na(x))) rlang::abort("Missing values detected. Try replacing missing values.")
    if (limit_upper <= max_x) rlang::abort("limit_upper <= max(x): This results in NaN. Try increasing limit_upper to a value greater than or equal to max(x).")
    if (limit_lower >= min_x) rlang::abort("limit_lower >= min(x): This results in NaN. Try decreasing limit_lower to a value less than or equal to min(x).")

    # Message
    if (!silent) message("log_interval_vec(): \n Using limit_lower: ", limit_lower, "\n Using limit_upper: ", limit_upper, "\n Using offset: ", offset)

    scaled <- (x - limit_lower) / (limit_upper - x)
    log(scaled)

}

#' @rdname log_interval_vec
#' @export
log_interval_inv_vec <- function(x, limit_lower, limit_upper, offset = 0) {

    if (rlang::is_missing(limit_lower)) {
        rlang::abort("log_interval_inv_vec(limit_lower): Is missing. Please provide a value.")
    }
    if (rlang::is_missing(limit_upper)) {
        rlang::abort("log_interval_inv_vec(limit_upper): Is missing. Please provide a value.")
    }

    a <- limit_lower
    b <- limit_upper

    (b-a)*(exp(x)) / (1 + exp(x)) + a - offset
}


auto_limit_lower <- function(limit_lower, min_x, range_x) {
    if (limit_lower == "auto") {
        limit_lower <- 0
    }
    return(limit_lower)
}

auto_limit_upper <- function(limit_upper, max_x, range_x) {
    if (limit_upper == "auto") {
        limit_upper <- max_x + (0.1 * range_x)
    }
    return(limit_upper)
}


