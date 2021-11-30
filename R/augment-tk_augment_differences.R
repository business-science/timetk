#' Add many differenced columns to the data
#'
#' A handy function for adding multiple lagged difference values to a data frame.
#' Works with `dplyr` groups too.
#'
#' @param .data A tibble.
#' @param .value One or more column(s) to have a transformation applied. Usage
#'  of `tidyselect` functions (e.g. `contains()`) can be used to select multiple columns.
#' @param .lags One or more lags for the difference(s)
#' @param .differences The number of differences to apply.
#' @param .log If TRUE, applies log-differences.
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
#'  the `.lags` argument (e.g. `lags = 1:20`)
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
#' - [`diff_vec()`] - Underlying function that powers `tk_augment_differences()`
#'
#' @examples
#' library(tidyverse)
#' library(timetk)
#'
#' m4_monthly %>%
#'     group_by(id) %>%
#'     tk_augment_differences(value, .lags = 1:20)
#'
#' @name tk_augment_differences
NULL

#' @export
#' @rdname tk_augment_differences
tk_augment_differences <- function(.data,
                                  .value,
                                  .lags = 1,
                                  .differences = 1,
                                  .log = FALSE,
                                  .names = "auto") {
    # Checks
    column_expr <- enquo(.value)
    if (rlang::quo_is_missing(column_expr)) stop(call. = FALSE, "tk_augment_differences(.value) is missing.")
    # if (rlang::is_missing(.lags)) stop(call. = FALSE, "tk_augment_differences(.lags) is missing.")
    # if (rlang::is_missing(.differences)) stop(call. = FALSE, "tk_augment_differences(.differences) is missing.")
    if (!any(.names == "auto")) {
        if (length(.names) != length(.lags) * length(.differences)) {
            rlang::abort(".names must be a vector of length ", length(.lags) * length(.differences))
        }
    }

    UseMethod("tk_augment_differences", .data)
}

#' @export
tk_augment_differences.data.frame <- function(.data,
                                             .value,
                                             .lags = 1,
                                             .differences = 1,
                                             .log = FALSE,
                                             .names = "auto") {

    # column_expr <- enquo(.value)
    col_nms   <- names(tidyselect::eval_select(rlang::enquo(.value), .data))

    make_call <- function(col, lag_val, diff_val) {
        rlang::call2(
            "diff_vec",
            x          = rlang::sym(col),
            lag        = lag_val,
            difference = diff_val,
            log        = .log,
            silent     = TRUE,
            .ns        = "timetk"
        )
    }

    grid <- expand.grid(
        col      = col_nms,
        lag_val  = .lags,
        diff_val = .differences,
        stringsAsFactors = FALSE
    )

    calls   <- purrr::pmap(.l = list(grid$col, grid$lag_val, grid$diff_val), make_call)

    if (any(.names == "auto")) {
        newname <- paste0(grid$col, "_lag", grid$lag_val, "_diff", grid$diff_val)
    } else {
        newname <- as.list(.names)
    }

    calls   <- purrr::set_names(calls, newname)

    ret <- tibble::as_tibble(dplyr::mutate(.data, !!!calls))

    return(ret)

}

#' @export
tk_augment_differences.grouped_df <- function(.data,
                                             .value,
                                             .lags = 1,
                                             .differences = 1,
                                             .log = FALSE,
                                             .names = "auto") {

    # Tidy Eval Setup
    column_expr <- enquo(.value)
    group_names <- dplyr::group_vars(.data)

    .data %>%
        tidyr::nest() %>%
        dplyr::mutate(nested.col = purrr::map(
            .x         = data,
            .f         = function(df) tk_augment_differences(
                .data       = df,
                .value     = !! enquo(.value),
                .lags       = .lags,
                .differences = .differences,
                .log        = .log,
                .names      = .names
            )
        )) %>%
        dplyr::select(-data) %>%
        tidyr::unnest(cols = nested.col) %>%
        dplyr::group_by_at(.vars = group_names)
}


#' @export
tk_augment_differences.default <- function(.data,
                                          .value,
                                          .lags = 1,
                                          .differences = 1,
                                          .log = FALSE,
                                          .names = "auto") {
    stop(paste0("`tk_augment_differences` has no method for class ", class(data)[[1]]))
}
