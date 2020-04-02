#' Augment columns with lagged data
#'
#' A handy function for adding multiple lagged columns to a data frame.
#' Works group-wise.
#'
#' @param .data A tibble.
#' @param .column A column to have a difference transformation applied
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
#' - [tk_augment_roll_apply()] - Group-wise augmentation of rolling functions
#' - [tk_augment_lags()] - Group-wise augmentation of lagged data
#' - [tk_augment_differences()] - Group-wise augmentation of differenced data
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
                            .column,
                            .lags = 1,
                            .names = paste0("lag_", .lags)) {

    # Checks
    column_expr <- enquo(.column)

    if (rlang::quo_is_missing(column_expr)) stop(call. = FALSE, "tk_augment_lags(.column) is missing.")
    if (rlang::is_missing(.lags)) stop(call. = FALSE, "tk_augment_lags(.lags) is missing.")

    UseMethod("tk_augment_lags", .data)
}

#' @export
tk_augment_lags.data.frame <- function(.data,
                                       .column,
                                       .lags = 1,
                                       .names = paste0("lag_", .lags)) {

    column_expr <- enquo(.column)

    ret_1 <- .data

    ret_2 <- .lags %>%
        purrr::map_dfc(.f = function(lag) {
            .data %>%
                dplyr::pull(!! column_expr) %>%
                lag_vec(.lag = lag)
        }) %>%
        purrr::set_names(.names)

    ret <- dplyr::bind_cols(ret_1, ret_2)

    return(ret)

}

tk_augment_lags.grouped_df <- function(.data,
                                       .column,
                                       .lags = 1,
                                       .names = paste0("lag_", .lags)) {

    # Tidy Eval Setup
    column_expr <- enquo(.column)
    group_names <- dplyr::group_vars(.data)

    .data %>%
        tidyr::nest() %>%
        dplyr::mutate(nested.col = purrr::map(
            .x         = data,
            .f         = function(df) tk_augment_lags(
                .data       = df,
                .column     = !! enquo(.column),
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
                                    .column,
                                    .lags = 1,
                                    .names = paste0("lag_", .lags)) {
    stop(paste0("`tk_augment_lags` has no method for class ", class(data)[[1]]))
}
