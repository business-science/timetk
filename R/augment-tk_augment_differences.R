#' Add many differenced columns to the data
#'
#' A handy function for adding multiple lagged difference values to a data frame.
#' Works with `dplyr` groups too.
#'
#' @param .data A tibble.
#' @param .column A column to have a difference transformation applied
#' @param .lags One or more lags for the difference(s)
#' @param .difference The number of differences to apply.
#' @param .log If TRUE, applies log-differences.
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
#' - [tk_augment_roll_apply()] - Group-wise augmentation of rolling functions
#' - [tk_augment_lags()] - Group-wise augmentation of lagged data
#' - [tk_augment_differences()] - Group-wise augmentation of differenced data
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
                                  .column,
                                  .lags = 1,
                                  .difference = 1,
                                  .log = FALSE,
                                  .names = paste0("diff_", .lags)) {
    UseMethod("tk_augment_differences", .data)
}

#' @export
tk_augment_differences.data.frame <- function(.data,
                                             .column,
                                             .lags = 1,
                                             .difference = 1,
                                             .log = FALSE,
                                             .names = paste0("diff_", .lags)) {

    column_expr <- enquo(.column)

    if (rlang::quo_is_missing(column_expr)) stop(call. = FALSE, "tk_augment_differences(.column) is missing.")
    if (rlang::is_missing(.lags)) stop(call. = FALSE, "tk_augment_differences(.lags) is missing.")
    if (rlang::is_missing(.difference)) stop(call. = FALSE, "tk_augment_differences(.difference) is missing.")

    ret_1 <- .data

    ret_2 <- .lags %>%
        purrr::map_dfc(.f = function(lag) {
            .data %>%
                dplyr::pull(!! column_expr) %>%
                diff_vec(
                    .lag        = lag,
                    .difference = .difference,
                    .log        = .log
                )
        }) %>%
        purrr::set_names(.names)

    ret <- dplyr::bind_cols(ret_1, ret_2)

    return(ret)

}

tk_augment_differences.grouped_df <- function(.data,
                                             .column,
                                             .lags = 1,
                                             .difference = 1,
                                             .log = FALSE,
                                             .names = paste0("diff_", .lags)) {

    # Tidy Eval Setup
    column_expr <- enquo(.column)
    group_names <- dplyr::group_vars(.data)

    # Checks
    if (rlang::quo_is_missing(column_expr)) stop(call. = FALSE, "tk_augment_differences(.column) is missing.")
    if (rlang::is_missing(.lags)) stop(call. = FALSE, "tk_augment_differences(.lags) is missing.")
    if (rlang::is_missing(.difference)) stop(call. = FALSE, "tk_augment_differences(.difference) is missing.")

    .data %>%
        tidyr::nest() %>%
        dplyr::mutate(nested.col = purrr::map(
            .x         = data,
            .f         = function(df) tk_augment_differences(
                .data       = df,
                .column     = !! enquo(.column),
                .lags       = .lags,
                .difference = .difference,
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
                                          .column,
                                          .lags = 1,
                                          .difference = 1,
                                          .log = FALSE,
                                          .names = paste0("diff_", .lags)) {
    stop(paste0("`tk_augment_differences` has no method for class ", class(data)[[1]]))
}
