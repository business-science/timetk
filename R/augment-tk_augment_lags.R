#' Add many lags to the data
#'
#' A handy function for adding multiple lagged columns to a data frame.
#' Works with `dplyr` groups too.
#'
#' @param .data A tibble.
#' @param .value A column to have a difference transformation applied
#' @param .lags One or more lags for the difference(s)
#' @param .names A vector of names for the new columns. Must be of same length as `.lags`.
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
#' - Add multiple lags by adding a sequence of lags using
#'  the `.lags` argument (e.g. `.lags = 1:20`)
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
#' - [lag_vec()] - Underlying function that powers `tk_augment_lags()`
#'
#' @examples
#' library(tidyverse)
#' library(timetk)
#'
#' m4_monthly %>%
#'     group_by(id) %>%
#'     tk_augment_lags(value, .lags = 1:20)
#'
#' @name tk_augment_lags
NULL

#' @export
#' @rdname tk_augment_lags
tk_augment_lags <- function(.data,
                            .value,
                            .lags = 1,
                            .names = "auto") {

    # Checks
    column_expr <- enquo(.value)

    if (rlang::quo_is_missing(column_expr)) stop(call. = FALSE, "tk_augment_lags(.value) is missing.")
    if (rlang::is_missing(.lags)) stop(call. = FALSE, "tk_augment_lags(.lags) is missing.")

    UseMethod("tk_augment_lags", .data)
}

#' @export
tk_augment_lags.data.frame <- function(.data,
                                       .value,
                                       .lags = 1,
                                       .names = "auto") {

    column_expr <- enquo(.value)

    ret_1 <- .data

    ret_2 <- .lags %>%
        purrr::map_dfc(.f = function(lag) {
            .data %>%
                dplyr::pull(!! column_expr) %>%
                lag_vec(lag = lag)
        })

    # Adjust Names
    if (any(.names == "auto")) {
        grid <- expand.grid(
            col      = rlang::quo_name(column_expr),
            lag_val  = .lags,
            stringsAsFactors = FALSE)
        newname <- paste0(grid$col, "_lag", grid$lag_val)
    } else {
        newname <- .names
    }
    ret_2 <- ret_2 %>%
        purrr::set_names(newname)

    ret <- dplyr::bind_cols(ret_1, ret_2)

    return(ret)

}

tk_augment_lags.grouped_df <- function(.data,
                                       .value,
                                       .lags = 1,
                                       .names = "auto") {

    # Tidy Eval Setup
    column_expr <- enquo(.value)
    group_names <- dplyr::group_vars(.data)

    .data %>%
        tidyr::nest() %>%
        dplyr::mutate(nested.col = purrr::map(
            .x         = data,
            .f         = function(df) tk_augment_lags(
                .data       = df,
                .value     = !! enquo(.value),
                .lags       = .lags,
                .names      = .names
            )
        )) %>%
        dplyr::select(-data) %>%
        tidyr::unnest(cols = nested.col) %>%
        dplyr::group_by_at(.vars = group_names)
}


#' @export
tk_augment_lags.default <- function(.data,
                                    .value,
                                    .lags = 1,
                                    .names = "auto") {
    stop(paste0("`tk_augment_lags` has no method for class ", class(data)[[1]]))
}
