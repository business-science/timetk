#' Add many time series features to the data
#'
#' @param .data A time-based tibble or time-series object.
#' @param .date_var For `tibbles`, a column containing either date or date-time values.
#'  If `NULL`, the time-based column will interpret from the object (tibble, xts, zoo, etc).
#'
#' @return Returns a `tibble` object describing the timeseries.
#'
#' @details
#' `tk_augment_timeseries_signature()` adds 25+ time series features including:
#'
#' - __Trend in Seconds Granularity:__ index.num
#' - __Yearly Seasonality:__ Year, Month, Quarter
#' - __Weekly Seasonality:__ Week of Month, Day of Month, Day of Week, and more
#' - __Daily Seasonality:__ Hour, Minute, Second
#' - __Weekly Cyclic Patterns:__ 2 weeks, 3 weeks, 4 weeks
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
#' - [tk_get_timeseries_signature()] - Returns timeseries features from an index
#'
#' @examples
#' library(dplyr)
#' library(timetk)
#'
#' m4_daily %>%
#'     group_by(id) %>%
#'     tk_augment_timeseries_signature(date)
#'
#' @name tk_augment_timeseries
NULL

#' @export
#' @rdname tk_augment_timeseries
tk_augment_timeseries_signature <- function(.data, .date_var = NULL) {

    # Checks
    if (is.data.frame(.data)) {
        if (rlang::quo_is_null(rlang::enquo(.date_var))) {
            # .date_var is NULL
            date_var <- tk_get_timeseries_variables(.data)[[1]]
            if (length(date_var) == 0 ) stop(call. = FALSE, "tk_augment_timeseries_signature(): No date variable detected.")
            message("tk_augment_timeseries_signature(): Using the following .date_var variable: ", date_var)
        }
    }

    UseMethod("tk_augment_timeseries_signature", .data)
}

#' @export
tk_augment_timeseries_signature.data.frame <- function(.data, .date_var = NULL) {

    date_var_expr <- rlang::enquo(.date_var)

    # Get date_var
    if (rlang::quo_is_null(date_var_expr)) {
        # .date_var is NULL
        date_var <- tk_get_timeseries_variables(.data)[[1]]
    } else {
        date_var <- .data %>%
            dplyr::ungroup() %>%
            dplyr::select(!! date_var_expr) %>%
            colnames()
    }


    # Arrange by date_var
    .data <- .data %>% dplyr::arrange(!! rlang::sym(date_var))

    # Bind Time Series Signature
    ret_1 <- .data

    ret_2 <- .data %>%
        dplyr::pull(date_var) %>%
        tk_get_timeseries_signature() %>%
        dplyr::select(-1)

    ret <- dplyr::bind_cols(ret_1, ret_2)

    return(ret)

}

#' @export
tk_augment_timeseries_signature.grouped_df <- function(.data, .date_var = NULL) {

    # Tidy Eval Setup
    group_names <- dplyr::group_vars(.data)
    date_var_expr <- enquo(.date_var)

    if (rlang::quo_is_null(date_var_expr)) {
        ret_tbl <- .data %>%
            tidyr::nest() %>%
            dplyr::mutate(nested.col = purrr::map(
                .x         = data,
                .f         = function(df) tk_augment_timeseries_signature.data.frame(
                    .data       = df,
                    .date_var   = NULL
                )
            )) %>%
            dplyr::select(-"data") %>%
            tidyr::unnest(cols = nested.col) %>%
            dplyr::group_by_at(.vars = group_names)
    } else {
        ret_tbl <- .data %>%
            tidyr::nest() %>%
            dplyr::mutate(nested.col = purrr::map(
                .x         = data,
                .f         = function(df) tk_augment_timeseries_signature.data.frame(
                    .data       = df,
                    .date_var   = !! date_var_expr
                )
            )) %>%
            dplyr::select(-"data") %>%
            tidyr::unnest(cols = nested.col) %>%
            dplyr::group_by_at(.vars = group_names)
    }

    return(ret_tbl)

}

#' @export
tk_augment_timeseries_signature.xts <- function(.data, .date_var = NULL) {

    ret_1 <- .data

    ret_2 <- .data %>%
        tk_index() %>%
        tk_get_timeseries_signature() %>%
        tk_xts(silent = TRUE)

    ret <- xts::merge.xts(ret_1, ret_2)

    return(ret)

}

#' @export
tk_augment_timeseries_signature.zoo <- function(.data, .date_var = NULL) {

    ret_1 <- .data %>%
        tk_xts(silent = TRUE)

    ret_2 <- .data %>%
        tk_index() %>%
        tk_get_timeseries_signature() %>%
        tk_xts(silent = TRUE)

    ret <- xts::merge.xts(ret_1, ret_2) %>%
        tk_zoo(silent = TRUE)

    return(ret)

}

#' @export
tk_augment_timeseries_signature.default <- function(.data, .date_var = NULL) {
    stop(paste0("`tk_augment_timeseries_signature` has no method for class ", class(.data)[[1]]))
}
