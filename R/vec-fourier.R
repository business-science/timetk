#' Fourier Series
#'
#' `fourier_vec()` calculates a Fourier Series from a date or date-time index.
#'
#'
#' @param x A date, POSIXct, yearmon, yearqtr, or numeric sequence (scaled to difference 1 for `period` alignment)
#'  to be converted to a fourier series.
#' @param period The number of observations that complete one cycle.
#' @param K The fourier term order.
#' @param type Either "sin" or "cos" for the appropriate type of fourier term.
#'
#' @return A numeric vector
#'
#' @details
#'
#' __Benefits:__
#'
#' This function is `NA` padded by default so it works well with `dplyr::mutate()` operations.
#'
#' __Fourier Series Calculation__
#'
#' The internal calculation is relatively straightforward:
#' `fourier(x) = sin(2 * pi * term * x) or cos(2 * pi * term * x)`,
#' where `term = K / period`.
#'
#' __Period Alignment, period__
#'
#' The `period` alignment with the sequence is an essential part of fourier series calculation.
#'
#' - __Date, Date-Time, and Zoo (yearqtr and yearmon) Sequences__ - Are scaled to unit difference of 1. This happens internally,
#'   so there's nothing you need to do or to worry about. Future time series will be scaled appropriately.
#' - __Numeric Sequences__ - Are not scaled, which means you should transform them to a unit difference of 1 so that
#'   your x is a sequence that increases by 1. Otherwise your period and fourier order will be incorrectly calculated.
#'   The solution is to just take your sequence and divide by the median difference between values.
#'
#' __Fourier Order, K__
#'
#' The fourier order is a parameter that increases the frequency. `K = 2` doubles the frequency.
#' It's common in time series analysis to add multiple fourier orders (e.g. 1 through 5) to account for
#' seasonalities that occur faster than the primary seasonality.
#'
#' __Type (Sin/Cos)__
#'
#' The type of the fourier series can be either `sin` or `cos`. It's common in time series analysis
#' to add both sin and cos series.
#'
#'
#' @seealso
#'
#' Fourier Modeling Functions:
#'   - [step_fourier()] - Recipe for `tidymodels` workflow
#'   - [tk_augment_fourier()] - Adds many fourier series to a `data.frame` (`tibble`)
#'
#' Additional Vector Functions:
#'   - Fourier Series: [fourier_vec()]
#'   - Box Cox Transformation: [box_cox_vec()]
#'   - Lag Transformation: [lag_vec()]
#'   - Differencing Transformation: [diff_vec()]
#'   - Rolling Window Transformation: [roll_apply_vec()]
#'   - Loess Smoothing Transformation: [smooth_vec()]
#'
#' @examples
#' library(tidyverse)
#' library(timetk)
#'
#' options(max.print = 50)
#'
#' date_sequence <- tk_make_date_sequence("2016-01-01", "2016-01-31", by = "hour")
#'
#' # --- VECTOR ---
#'
#' fourier_vec(date_sequence, period = 7 * 24, K = 1, type = "sin")
#'
#' # --- MUTATE ---
#'
#' tibble(date = date_sequence) %>%
#'     # Add cosine series that oscilates at a 7-day period
#'     mutate(
#'         C1_7 = fourier_vec(date, period = 7*24, K = 1, type = "cos"),
#'         C2_7 = fourier_vec(date, period = 7*24, K = 2, type = "cos")
#'     ) %>%
#'     # Visualize
#'     pivot_longer(cols = contains("_"), names_to = "name", values_to = "value") %>%
#'     plot_time_series(
#'         date, value, .color_var = name,
#'         .smooth = FALSE,
#'         .interactive = FALSE,
#'         .title = "7-Day Fourier Terms"
#'     )
#'
#'
#'
#' @name fourier_vec
#' @export
fourier_vec <- function(x, period, K = 1, type = c("sin", "cos")) {

    UseMethod("fourier_vec", x)
}

#' @export
fourier_vec.integer <- function(x, period, K = 1, type = c("sin", "cos")) {
    calc_fourier(x = x, period = period, K = K, type = type)
}

#' @export
fourier_vec.double <- function(x, period, K = 1, type = c("sin", "cos")) {
    calc_fourier(x = x, period = period, K = K, type = type)
}

#' @export
fourier_vec.Date <- function(x, period, K = 1, type = c("sin", "cos")) {

    x_num    <- as.POSIXct(x) %>% as.numeric() %>% as.integer()
    x_scaled <- x_num / date_to_seq_scale_factor(x)

    calc_fourier(x = x_scaled, period = period, K = K, type = type)
}

#' @export
fourier_vec.POSIXct <- function(x, period, K = 1, type = c("sin", "cos")) {

    x_num    <- as.numeric(x) %>% as.integer()
    x_scaled <- x_num / date_to_seq_scale_factor(x)

    calc_fourier(x = x_scaled, period = period, K = K, type = type)
}

#' @export
fourier_vec.yearmon <- function(x, period, K = 1, type = c("sin", "cos")) {
    x_scaled <- x * 12
    calc_fourier(x = x, period = period, K = K, type = type)
}

#' @export
fourier_vec.yearqtr <- function(x, period, K = 1, type = c("sin", "cos")) {
    x_scaled <- x * 4
    calc_fourier(x = x_scaled, period = period, K = K, type = type)
}

#' @export
fourier_vec.default <- function(x, period, K = 1, type = c("sin", "cos")) {
    rlang::abort(paste0("fourier_vec(x): No method for class: ", class(x)[[1]]))
}

calc_fourier <- function(x, period, K = 1, type = c("sin", "cos")) {

    x_scaled <- x
    term     <- K / period
    type     <- tolower(type[1])

    if (type == "sin") {
        ret <- sin(2 * pi * term * x)
    } else {
        ret <- cos(2 * pi * term * x)
    }

    return(ret)

}

date_to_seq_scale_factor <- function(idx) {
    tk_get_timeseries_summary(idx) %>% dplyr::pull(diff.median)
}
