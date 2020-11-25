#' Add many lags to the data
#'
#' A handy function for adding multiple lagged columns to a data frame.
#' Works with `dplyr` groups too.
#'
#' @param .data A tibble.
#' @param .value One or more column(s) to have a transformation applied. Usage
#'  of `tidyselect` functions (e.g. `contains()`) can be used to select multiple columns.
#' @param .lags One or more lags for the difference(s)
#' @param .names A vector of names for the new columns. Must be of same length as `.lags`.
#'
#'
#' @return Returns a `tibble` object describing the timeseries.
#'
#' @details
#'
#' __Lags vs Leads__
#'
#' A _negative lag_ is considered a lead. The `tk_augment_leads()` function is
#' identical to `tk_augment_lags()` with the exception that the
#' automatic naming convetion (`.names = 'auto'`) will convert column names with negative lags to
#' leads.
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
#' # Lags
#' m4_monthly %>%
#'     group_by(id) %>%
#'     tk_augment_lags(contains("value"), .lags = 1:20)
#'
#' # Leads
#' m4_monthly %>%
#'     group_by(id) %>%
#'     tk_augment_leads(value, .lags = 1:-20)
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

    # column_expr <- enquo(.value)
    col_nms   <- names(tidyselect::eval_select(rlang::enquo(.value), .data))

    make_call <- function(col, lag_val) {
        rlang::call2(
            "lag_vec",
            x          = rlang::sym(col),
            lag        = lag_val,
            .ns        = "timetk"
        )
    }

    grid <- expand.grid(
        col      = col_nms,
        lag_val  = .lags,
        stringsAsFactors = FALSE
    )

    calls   <- purrr::pmap(.l = list(grid$col, grid$lag_val), make_call)

    if (any(.names == "auto")) {
        newname <- paste0(grid$col, "_lag", grid$lag_val)
    } else {
        newname <- as.list(.names)
    }

    calls   <- purrr::set_names(calls, newname)

    ret <- tibble::as_tibble(dplyr::mutate(.data, !!!calls))

    return(ret)

}

#' @export
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



#' @export
#' @rdname tk_augment_lags
tk_augment_leads <- function(.data,
                             .value,
                             .lags = -1,
                             .names = "auto") {

    # Checks
    column_expr <- enquo(.value)

    if (rlang::quo_is_missing(column_expr)) stop(call. = FALSE, "tk_augment_leads(.value) is missing.")
    if (rlang::is_missing(.lags)) stop(call. = FALSE, "tk_augment_leads(.lags) is missing.")

    UseMethod("tk_augment_leads", .data)
}

#' @export
tk_augment_leads.data.frame <- function(.data,
                                        .value,
                                        .lags = -1,
                                        .names = "auto") {

    # column_expr <- enquo(.value)
    col_nms   <- names(tidyselect::eval_select(rlang::enquo(.value), .data))

    make_call <- function(col, lag_val) {
        rlang::call2(
            "lag_vec",
            x          = rlang::sym(col),
            lag        = lag_val,
            .ns        = "timetk"
        )
    }

    grid <- expand.grid(
        col      = col_nms,
        lag_val  = .lags,
        stringsAsFactors = FALSE
    )

    calls   <- purrr::pmap(.l = list(grid$col, grid$lag_val), make_call)

    if (any(.names == "auto")) {
        newname <- paste0(grid$col, "_lag", grid$lag_val) %>%
            stringr::str_replace_all("lag-","lead")
    } else {
        newname <- as.list(.names)
    }

    calls   <- purrr::set_names(calls, newname)

    ret <- tibble::as_tibble(dplyr::mutate(.data, !!!calls))

    return(ret)

}

#' @export
tk_augment_leads.grouped_df <- function(.data,
                                        .value,
                                        .lags = -1,
                                        .names = "auto") {

    # Tidy Eval Setup
    column_expr <- enquo(.value)
    group_names <- dplyr::group_vars(.data)

    .data %>%
        tidyr::nest() %>%
        dplyr::mutate(nested.col = purrr::map(
            .x         = data,
            .f         = function(df) tk_augment_leads(
                .data       = df,
                .value      = !! enquo(.value),
                .lags       = .lags,
                .names      = .names
            )
        )) %>%
        dplyr::select(-data) %>%
        tidyr::unnest(cols = nested.col) %>%
        dplyr::group_by_at(.vars = group_names)
}


#' @export
tk_augment_leads.default <- function(.data,
                                     .value,
                                     .lags = 1,
                                     .names = "auto") {
    stop(paste0("`tk_augment_leads` has no method for class ", class(data)[[1]]))
}
