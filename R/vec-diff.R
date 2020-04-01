#' Differencing Transformation
#'
#' `diff_vec()` applies a Differencing Transformation.
#'
#'
#' @param .x A numeric vector to be differenced or inverted.
#' @param .lag Which lag (how far back) to be included in the differencing calculation.
#' @param .differences The number of differences to perform.
#'  - 1 Difference is equivalent to period change.
#'  - 2 Differences is equivalent to acceleration.
#' @param .log If log differences should be calculated.
#'  _Note that difference inversion of a log-difference is approximate._
#' @param .lag_values A numeric vector of the initial values, which are used for difference inversion.
#'
#' @return A numeric vector
#'
#' @details
#'
#' __Benefits:__
#'
#' This function is `NA` padded by default so it works well with `dplyr::mutate()` operations.
#'
#'
#' @examples
#' library(dplyr)
#' library(timetk)
#'
#' # --- MUTATE ----
#' m4_daily %>%
#'     group_by(id) %>%
#'     mutate(difference = diff_vec(value, .lag = 1)) %>%
#'     mutate(
#'         difference_inv = diff_inv_vec(
#'             difference,
#'             .lag        = 1,
#'             .lag_values = value[1]
#'         )
#'     )
#'
#' @seealso
#'   - Rolling Window Calculations: [roll_apply_vec()], [slider::slide_vec()], [zoo::rollapply()]
#'   - Loess Modeling: [stats::loess()]
#'
#'
#' @name diff_vec
#' @export
diff_vec <- function(.x, .lag = 1, .differences = 1, .log = FALSE) {
    # Checks
    if (length(.lag) > 1) stop(call. = FALSE, "diff_vec(length(.lag) > 1): Multiple lags detected. Use tk_augment_diff().")
    if (length(.differences) > 1) stop(call. = FALSE, "diff_vec(length(.differences) > 1): Multiple differences detected. Use tk_augment_diff().")

    UseMethod("diff_vec", .x)
}

#' @export
diff_vec.default <- function(.x, .lag = 1, .differences = 1, .log = FALSE) {
    stop(paste0("diff_vec: No method for class ", class(.x)[[1]], "."), call. = FALSE)
}

#' @export
diff_vec.double <- function(.x, .lag = 1, .differences = 1, .log = FALSE) {
    diff_calc(.x, .lag, .differences, .log)
}

#' @export
diff_vec.integer <- function(.x, .lag = 1, .differences = 1, .log = FALSE) {
    diff_calc(.x, .lag, .differences, .log)
}

diff_calc <- function(.x, .lag, .differences, .log) {
    ret_vec <- xts::diff.xts(
        x           = .x,
        lag         = .lag,
        differences = .differences,
        arithmetic  = TRUE,
        log         = .log,
        na.pad      = TRUE
    )

    pad_len <- length(.x) - length(ret_vec)
    if (pad_len > 0) {
        ret_vec <- c(rep(NA, pad_len), ret_vec)
    }

    return(ret_vec)
}

# DIFF INVERSE ----

#' @rdname diff_vec
#' @export
diff_inv_vec <- function(.x, .lag = 1, .differences = 1, .log = FALSE, .lag_values = NULL) {
    # Checks
    if (length(.lag) > 1) stop(call. = FALSE, "diff_inv_vec(length(.lag) > 1): Multiple lags detected. Use tk_augment_diff().")
    if (length(.differences) > 1) stop(call. = FALSE, "diff_inv_vec(length(.differences) > 1): Multiple differences detected. Use tk_augment_diff().")

    if (!is.null(.lag_values)) {
        if (length(.lag_values) != (.lag * .differences)) {
            stop(call. = FALSE, "diff_inv_vec(.lag_values): Size of lag values must match the lag used.")
        }
    }
    UseMethod("diff_inv_vec", .x)
}

#' @export
diff_inv_vec.default <- function(.x, .lag = 1, .differences = 1, .log = FALSE, .lag_values = NULL) {
    stop(paste0("diff_inv_vec: No method for class ", class(.x)[[1]], "."), call. = FALSE)
}

#' @export
diff_inv_vec.double <- function(.x, .lag = 1, .differences = 1, .log = FALSE, .lag_values = NULL) {
    diff_inv_calc(.x, .lag, .differences, .log, .lag_values)
}

#' @export
diff_inv_vec.integer <- function(.x, .lag = 1, .differences = 1, .log = FALSE, .lag_values = NULL) {
    diff_inv_calc(.x, .lag, .differences, .log, .lag_values)
}

diff_inv_calc <- function(.x, .lag, .differences, .log, .lag_values = NULL) {

    # Checks


    na_len <- is.na(.x) %>% sum()
    x_trim <- .x[!is.na(.x)]

    if (!.log) {
        # Not log transform
        if (!is.null(.lag_values)) {
            ret_vec <- stats::diffinv(x = x_trim, lag = .lag, differences = .differences, xi = .lag_values)
        } else {
            ret_vec <- stats::diffinv(x = x_trim, lag = .lag, differences = .differences)
        }
    } else {
        # Log transform
        warning(call. = FALSE, "diff_inv_vec(.log = TRUE): Log-Difference Inversion is approximate.")
        if (!is.null(.lag_values)) {
            ret_vec <- exp(stats::diffinv(x = x_trim, lag = .lag, differences = .differences)) * .lag_values[1]
            ret_vec[1:na_len] <- .lag_values
        } else {
            ret_vec <- exp(stats::diffinv(x = x_trim, lag = .lag, differences = .differences))
        }

    }

    return(ret_vec)
}


