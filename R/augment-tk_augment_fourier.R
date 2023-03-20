#' Add many fourier series to the data
#'
#' A handy function for adding multiple fourier series to a data frame.
#' Works with `dplyr` groups too.
#'
#' @param .data A tibble.
#' @param .date_var A date or date-time column used to calculate a fourier series
#' @param .periods One or more periods for the fourier series
#' @param .K The maximum number of fourier orders.
#' @param .names A vector of names for the new columns. Must be of same length as the
#' number of output columns. Use "auto" to automatically rename the columns.
#'
#'
#' @return Returns a `tibble` object describing the timeseries.
#'
#' @details
#'
#' __Benefits__
#'
#' This is a scalable function that is:
#'
#' - Designed to work with grouped data using `dplyr::group_by()`
#' - Add multiple differences by adding a sequence of differences using
#'  the `.periods` argument (e.g. `lags = 1:20`)
#'
#'
#' @seealso
#'
#' Augment Operations:
#'
#' - [tk_augment_timeseries_signature()] - Group-wise augmentation of timestamp features
#' - [tk_augment_holiday_signature()] - Group-wise augmentation of holiday features
#' - [tk_augment_slidify()] - Group-wise augmentation of rolling functions
#' - [tk_augment_lags()] - Group-wise augmentation of lagged data
#' - [tk_augment_differences()] - Group-wise augmentation of differenced data
#' - [tk_augment_fourier()] - Group-wise augmentation of fourier series
#'
#' Underlying Function:
#'
#' - [`fourier_vec()`] - Underlying function that powers `tk_augment_fourier()`
#'
#' @examples
#' library(dplyr)
#' library(timetk)
#'
#' m4_monthly %>%
#'     group_by(id) %>%
#'     tk_augment_fourier(date, .periods = c(6, 12), .K = 2)
#'
#' @name tk_augment_fourier
NULL

#' @export
#' @rdname tk_augment_fourier
tk_augment_fourier <- function(.data,
                               .date_var,
                               .periods,
                               .K = 1,
                               .names = "auto"
) {
    # Checks
    column_expr <- enquo(.date_var)
    if (rlang::quo_is_missing(column_expr)) stop(call. = FALSE, "tk_augment_fourier(.date_var) is missing.")
    if (rlang::is_missing(.periods)) stop(call. = FALSE, "tk_augment_fourier(.periods) is missing.")
    # if (rlang::is_missing(.K)) stop(call. = FALSE, "tk_augment_fourier(.K) is missing.")
    if (!any(.names == "auto")) {
        if (length(.names) != length(.periods) * 2) {
            rlang::abort(".names must be a vector of length ", length(.periods) * 2)
        }
    }
    vals <- .data %>% dplyr::pull(!! rlang::enquo(.date_var))
    if (!is_date_class(vals)) {
        rlang::abort(paste0("Please ensure that .date_var is a valid date, date-time, yearmon, or yearqtr class. Class detected: ", class(vals)[1]))
    }

    UseMethod("tk_augment_fourier", .data)
}

#' @export
tk_augment_fourier.default <- function(.data,
                                       .date_var,
                                       .periods = 1,
                                       .K = 1,
                                       .names = "auto") {
    stop(paste0("`tk_augment_fourier` has no method for class ", class(data)[[1]]))
}

#' @export
tk_augment_fourier.data.frame <- function(.data,
                                             .date_var,
                                             .periods = 1,
                                             .K = 1,
                                             .names = "auto") {

    column_expr <- enquo(.date_var)

    make_call <- function(col, period_val, K_val, type_val) {
        rlang::call2(
            "fourier_vec",
            x          = rlang::sym(col),
            period     = period_val,
            K          = K_val,
            type       = type_val,
            .ns        = "timetk"
        )
    }

    grid <- expand.grid(
        col         = rlang::quo_name(column_expr),
        type_val    = c("sin", "cos"),
        K_val       = 1:max(.K),
        period_val  = .periods,
        stringsAsFactors = FALSE)

    calls   <- purrr::pmap(.l = list(grid$col, grid$period_val, grid$K_val, grid$type_val), make_call)

    if (any(.names == "auto")) {
        newname <- paste0(grid$col, "_", grid$type_val, round(grid$period_val, 2), "_K", grid$K_val)
    } else {
        newname <- as.list(.names)
    }

    calls   <- purrr::set_names(calls, newname)

    ret <- tibble::as_tibble(dplyr::mutate(.data, !!!calls))

    return(ret)

}

#' @export
tk_augment_fourier.grouped_df <- function(.data,
                                             .date_var,
                                             .periods = 1,
                                             .K = 1,
                                             .names = "auto") {

    # Tidy Eval Setup
    column_expr <- enquo(.date_var)
    group_names <- dplyr::group_vars(.data)

    .data %>%
        tidyr::nest() %>%
        dplyr::mutate(nested.col = purrr::map(
            .x         = data,
            .f         = function(df) tk_augment_fourier(
                .data       = df,
                .date_var   = !! enquo(.date_var),
                .periods    = .periods,
                .K          = .K,
                .names      = .names
            )
        )) %>%
        dplyr::select(-data) %>%
        tidyr::unnest(cols = nested.col) %>%
        dplyr::group_by_at(.vars = group_names)
}



