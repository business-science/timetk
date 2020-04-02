#' Augment the time series signature to the data
#'
#' @param .data A time-based tibble or time-series object.
#'
#' @return Returns a `tibble` object describing the timeseries.
#'
#' @details
#' `tk_augment_timeseries_signature` adds the time series signature
#' features including
#' numeric value, differences,
#' year, month, day, day of week, day of month,
#' day of year, hour, minute, second
#' to the input data.
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
#' - [tk_get_timeseries_signature()] - Returns timeseries features from an index
#'
#' @examples
#' library(dplyr)
#' library(tidyquant)
#' library(timetk)
#'
#' FANG %>%
#'     filter(symbol == "FB") %>%
#'     tk_augment_timeseries_signature()
#'
#' @name tk_augment_timeseries
NULL

#' @export
#' @rdname tk_augment_timeseries
tk_augment_timeseries_signature <- function(.data) {
    UseMethod("tk_augment_timeseries_signature", .data)
}

#' @export
tk_augment_timeseries_signature.data.frame <- function(.data) {

    date_var <- tk_get_timeseries_variables(.data)[[1]]

    # Arrange by date_var
    .data <- .data %>% dplyr::arrange(!! sym(date_var))

    # Bind Time Series Signature
    ret_1 <- .data

    ret_2 <- .data %>%
        tk_index() %>%
        tk_get_timeseries_signature() %>%
        dplyr::select(-1)

    ret <- dplyr::bind_cols(ret_1, ret_2)

    return(ret)

}

#' @export
tk_augment_timeseries_signature.xts <- function(.data) {

    ret_1 <- .data

    ret_2 <- .data %>%
        tk_index() %>%
        tk_get_timeseries_signature() %>%
        tk_xts(silent = TRUE)

    ret <- xts::merge.xts(ret_1, ret_2)

    return(ret)

}

#' @export
tk_augment_timeseries_signature.zoo <- function(.data) {

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
tk_augment_timeseries_signature.default <- function(.data) {
    stop(paste0("`tk_augment_timeseries_signature` has no method for class ", class(.data)[[1]]))
}
