#' Differencing Transformation
#'
#' `diff_vec()` applies a Differencing Transformation.
#' `diff_inv_vec()` inverts the differencing transformation.
#'
#'
#' @param x A numeric vector to be differenced or inverted.
#' @param lag Which lag (how far back) to be included in the differencing calculation.
#' @param difference The number of differences to perform.
#'  - 1 Difference is equivalent to measuring period change.
#'  - 2 Differences is equivalent to measuring period acceleration.
#' @param log If log differences should be calculated.
#'  _Note that difference inversion of a log-difference is approximate._
#' @param initial_values Only used in the `diff_vec_inv()` operation.
#'  A numeric vector of the initial values, which are used to invert differences.
#'  This vector is the original values that are the length of the `NA` missing differences.
#' @param silent Whether or not to report the initial values used to invert the difference
#'  as a message.
#'
#' @return A numeric vector
#'
#' @details
#'
#' __Benefits:__
#'
#' This function is `NA` padded by default so it works well with `dplyr::mutate()` operations.
#'
#' __Difference Calculation__
#'
#' Single differencing, `diff_vec(x_t)` is equivalent to: `x_t - x_t1`,
#' where the subscript _t1 indicates the first lag.
#' _This transformation can be interpereted as change._
#'
#' __Double Differencing Calculation__
#'
#' Double differencing, `diff_vec(x_t, difference = 2)` is equivalent to:
#' `(x_t - x_t1) - (x_t - x_t1)_t1`, where the subscript _t1 indicates the first lag.
#' _This transformation can be interpereted as acceleration._
#'
#' __Log Difference Calculation__
#'
#' Log differencing, `diff_vec(x_t, log = TRUE)` is equivalent to:
#' `log(x_t) - log(x_t1) = log(x_t / x_t1)`, where x_t is the series and x_t1 is the first lag.
#'
#' The 1st difference `diff_vec(difference = 1, log = TRUE)` has an interesting property
#' where `diff_vec(difference = 1, log = TRUE) %>% exp()` is approximately _1 + rate of change._
#'
#' @seealso
#'
#' Advanced Differencing and Modeling:
#'   - [step_diff()] - Recipe for `tidymodels` workflow
#'   - [tk_augment_differences()] - Adds many differences to a `data.frame` (`tibble`)
#'
#' Additional Vector Functions:
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
#'
#' # --- USAGE ----
#'
#' diff_vec(1:10, lag = 2, difference = 2) %>%
#'     diff_inv_vec(lag = 2, difference = 2, initial_values = 1:4)
#'
#' # --- VECTOR ----
#'
#' # Get Change
#' 1:10 %>% diff_vec()
#'
#' # Get Acceleration
#' 1:10 %>% diff_vec(difference = 2)
#'
#' # Get approximate rate of change
#' 1:10 %>% diff_vec(log = TRUE) %>% exp() - 1
#'
#'
#' # --- MUTATE ----
#'
#' m4_daily %>%
#'     group_by(id) %>%
#'     mutate(difference = diff_vec(value, lag = 1)) %>%
#'     mutate(
#'         difference_inv = diff_inv_vec(
#'             difference,
#'             lag = 1,
#'             # Add initial value to calculate the inverse difference
#'             initial_values = value[1]
#'         )
#'     )
#'
#'
#'
#' @name diff_vec
#' @export

# DIFF ----

#' @export
#' @rdname diff_vec
diff_vec <- function(x, lag = 1, difference = 1, log = FALSE, initial_values = NULL, silent = FALSE) {

    # Checks
    if (!is.numeric(x)) rlang::abort("Non-numeric data detected. 'x' must be numeric.")
    if (length(lag) > 1) rlang::abort("length(lag) > 1): Multiple lags detected. Use tk_augment_diff().")
    if (length(difference) > 1) rlang::abort("diff_vec(length(difference) > 1): Multiple differences detected. Use tk_augment_diff().")
    if (!is.null(initial_values)) rlang::warn("`initial_values` are not required for the `diff_vec()` calculation.")

    if (!silent) message("diff_vec(): Initial values: ",
                         stringr::str_c(x[1:(lag * difference)], collapse = ", "))

    ret_vec <- xts::diff.xts(
        x           = x,
        lag         = lag,
        differences = difference,
        arithmetic  = TRUE,
        log         = log,
        na.pad      = TRUE
    )

    pad_len <- length(x) - length(ret_vec)
    if (pad_len > 0) {
        ret_vec <- c(rep(NA, pad_len), ret_vec)
    }

    return(ret_vec)
}

# DIFF INVERSE ----

#' @rdname diff_vec
#' @export
diff_inv_vec <- function(x, lag = 1, difference = 1, log = FALSE, initial_values = NULL) {
    # Checks
    if (!is.numeric(x)) rlang::abort("Non-numeric data detected. 'x' must be numeric.")
    if (length(lag) > 1) stop(call. = FALSE, "diff_inv_vec(length(lag) > 1): Multiple lags detected. Use tk_augment_diff().")
    if (length(difference) > 1) stop(call. = FALSE, "diff_inv_vec(length(difference) > 1): Multiple differences detected. Use tk_augment_diff().")

    if (!is.null(initial_values)) {
        if (length(initial_values) != (lag * difference)) {
            stop(call. = FALSE, "diff_inv_vec(initial_values): Size of lag values must match the number of missing values generated by the differencing operation.")
        }
    }

    na_len <- is.na(x) %>% sum()
    x_trim <- x[!is.na(x)]

    if (!log) {
        # Not log transform
        if (!is.null(initial_values)) {
            ret_vec <- stats::diffinv(x = x_trim, lag = lag, differences = difference, xi = initial_values)
        } else {
            ret_vec <- stats::diffinv(x = x_trim, lag = lag, differences = difference)
        }
    }  else {
        # Log transform

        if (difference > 1) {
            stop(call. = FALSE, "diff_inv_vec(log = TRUE): Log-Difference inversion for multiple differences is not yet implemented.")
        }

        if (is.null(initial_values)) {
            initial_values <- rep(1, na_len)
        }

        ret_vec <- stats::diffinv(
            x = x_trim,
            lag = lag,
            differences = difference
        )

        ret_vec <- exp(ret_vec) * initial_values[1]

    }

    return(ret_vec)
}


