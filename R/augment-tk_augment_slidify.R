#' Add many rolling window calculations to the data
#'
#' Quickly use any function as a rolling function and apply to multiple `.periods`.
#' Works with `dplyr` groups too.
#'
#' @param .data A tibble.
#' @param .value A numeric column to have a rolling window transformation applied
#' @param .period One or more periods for the rolling window(s)
#' @param .f A summary `[function / formula]`,
#' @param ... Optional arguments for the summary function
#' @param .align Rolling functions generate `.period - 1` fewer values than the incoming vector.
#' Thus, the vector needs to be aligned. Select one of "center", "left", or "right".
#' @param .partial .partial Should the moving window be allowed to return partial (incomplete) windows instead of `NA` values.
#'  Set to FALSE by default, but can be switched to TRUE to remove `NA`'s.
#' @param .names A vector of names for the new columns. Must be of same length as `.period`. Default is "auto".
#'
#'
#' @return Returns a `tibble` object describing the timeseries.
#'
#' @details
#' `tk_augment_slidify()` scales the [`slidify_vec()`] function to multiple
#' time series `.periods`. See [`slidify_vec()`] for examples and usage of the core function
#' arguments.
#'
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
#' - [`slidify_vec()`] - The underlying function that powers `tk_augment_slidify()`
#'
#' @examples
#' library(tidyverse)
#' library(tidyquant)
#' library(timetk)
#'
#' FANG %>%
#'     select(symbol, date, adjusted) %>%
#'     group_by(symbol) %>%
#'     tk_augment_slidify(
#'         .value  = adjusted,
#'         # Multiple rolling windows
#'         .period  = c(10, 30, 60, 90),
#'         .f       = AVERAGE,
#'         .partial = TRUE,
#'         .names   = str_c("MA_", c(10, 30, 60, 90))
#'     )
#'
#' @name tk_augment_slidify
NULL

#' @export
#' @rdname tk_augment_slidify
tk_augment_slidify <- function(.data,
                                  .value,
                                  .period,
                                  .f,
                                  ...,
                                  .align = c("center", "left", "right"),
                                  .partial = FALSE,
                                  .names = "auto") {
    UseMethod("tk_augment_slidify", .data)
}

#' @export
tk_augment_slidify.data.frame <- function(.data,
                                             .value,
                                             .period,
                                             .f,
                                             ...,
                                             .align = c("center", "left", "right"),
                                             .partial = FALSE,
                                             .names = "auto") {

    column_expr <- enquo(.value)

    if (rlang::quo_is_missing(column_expr)) stop(call. = FALSE, "tk_augment_slidify(.value) is missing.")
    if (rlang::is_missing(.period)) stop(call. = FALSE, "tk_augment_slidify(.period) is missing.")
    if (rlang::is_missing(.f)) stop(call. = FALSE, "tk_augment_slidify(.f) is missing.")

    ret_1 <- .data

    ret_2 <- .period %>%
        purrr::map(.f = function(period) {
            .data %>%
                dplyr::pull(!! column_expr) %>%
                slidify_vec(
                    .period  = period,
                    .f       = .f,
                    ...,
                    .align   = .align[1],
                    .partial = .partial[1]
                )
        })

    # Adjust Names
    if (any(.names == "auto")) {
        grid <- expand.grid(
            col         = rlang::quo_name(column_expr),
            period_val  = .period,
            stringsAsFactors = FALSE)
        newname <- paste0(grid$col, "_roll_", grid$period_val)
    } else {
        newname <- .names
    }
    ret_2 <- ret_2 %>%
        purrr::set_names(newname) %>%
        dplyr::bind_cols()

    # Perform Overwrite
    ret <- bind_cols_overwrite(ret_1, ret_2)

    return(ret)

}

#' @export
tk_augment_slidify.grouped_df <- function(.data,
                                             .value,
                                             .period,
                                             .f,
                                             ...,
                                             .align = c("center", "left", "right"),
                                             .partial = FALSE,
                                             .names = "auto") {

    # Tidy Eval Setup
    column_expr <- enquo(.value)
    group_names <- dplyr::group_vars(.data)

    # Checks
    if (rlang::quo_is_missing(column_expr)) stop(call. = FALSE, "tk_augment_slidify(.value) is missing.")
    if (rlang::is_missing(.period)) stop(call. = FALSE, "tk_augment_slidify(.period) is missing.")
    if (rlang::is_missing(.f)) stop(call. = FALSE, "tk_augment_slidify(.f) is missing.")

    .data %>%
        tidyr::nest() %>%
        dplyr::mutate(nested.col = purrr::map(
            .x         = data,
            .f         = function(df) tk_augment_slidify(
                .data      = df,
                .value     = !! enquo(.value),
                .period    = .period,
                .f         = .f,
                ...,
                .align     = .align[1],
                .partial   = .partial[1],
                .names     = .names
            )
        )) %>%
        dplyr::select(-data) %>%
        tidyr::unnest(cols = nested.col) %>%
        dplyr::group_by_at(.vars = group_names)
}


#' @export
tk_augment_slidify.default <- function(.data,
                                          .value,
                                          .period,
                                          .f,
                                          ...,
                                          .align = c("center", "left", "right"),
                                          .partial = FALSE,
                                          .names = "auto") {
    stop(paste0("`tk_augment_slidify` has no method for class ", class(data)[[1]]))
}
