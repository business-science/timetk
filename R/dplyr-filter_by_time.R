#' Filter for Time-Series Data
#'
#' The easiest way to filter time-based tibbles using shorthand timeseries notation.
#' See [between_time()] for the date and date-time vector implementation.
#'
#' @param .data A tibble with a time-based column.
#' @param .date_var A column containing date or date-time values to filter
#' @param .start_date The starting date for the filter sequence
#' @param .end_date The ending date for the filter sequence
#'
#' @details
#'
#' __Pure Time Series Filtering Flexibilty__
#'
#' The `.start_date`  and `.end_date` parameters are designed with flexibility in mind.
#'
#' Each side of the `time_formula` is specified as the character
#' `'YYYY-MM-DD HH:MM:SS'`, but powerful shorthand is available.
#' Some examples are:
#' * __Year:__ `.start_date = '2013', .end_date = '2015'`
#' * __Month:__ `.start_date = '2013-01', .end_date = '2016-06'`
#' * __Day:__ `.start_date = '2013-01-05', .end_date = '2016-06-04'`
#' * __Second:__ `.start_date = '2013-01-05 10:22:15', .end_date = '2018-06-03 12:14:22'`
#' * __Variations:__ `.start_date = '2013', .end_date = '2016-06'`
#'
#' __Key Words: "start" and "end"__
#'
#' Use the keywords "start" and "end" as shorthand, instead of specifying the
#' actual start and end values. Here are some examples:
#'
#' * __Start of the series to end of 2015:__ `.start_date = 'start', .end_date = '2015'`
#' * __Start of 2014 to end of series:__ `.start_date = '2014', .end_date = 'end'`
#'
#' __Internal Calculations__
#'
#' All shorthand dates are expanded:
#' * The `.start_date` is expanded to be the _first date_ in that period
#' * The `.end_date` side is expanded to be the _last date_ in that period
#'
#' This means that the following examples are equivalent (assuming your
#' index is a POSIXct):
#' * `.start_date = '2015'` is equivalent to `.start_date = '2015-01-01 + 00:00:00' `
#' * `.end_date = '2016'` is equivalent to `2016-12-31 + 23:59:59'`
#'
#' @seealso
#' - [between_time()] - A time-based variant of `dplyr::between()` that powers by
#' `filter_by_time()`
#'
#'
#' @examples
#' library(tidyverse)
#' library(tidyquant)
#' library(timetk)
#'
#' # Filter values in January 1st through end of February, 2013
#' FANG %>%
#'     group_by(symbol) %>%
#'     filter_by_time(date, "start", "2013-02") %>%
#'     plot_time_series(date, adjusted, .facet_ncol = 2, .interactive = FALSE)
#'
#' @name filter_by_time
#' @export
filter_by_time <- function(.data, .date_var, .start_date = "start", .end_date = "end") {
    UseMethod("filter_by_time", .data)
}

#' @export
filter_by_time.default <- function(.data, .date_var, .start_date = "start", .end_date = "end") {
    stop("Object is not of class `tbl_time`.", call. = FALSE)
}

#' @export
filter_by_time.data.frame <- function(.data, .date_var, .start_date = "start", .end_date = "end") {

    date_var_expr <- enquo(.date_var)

    .data %>%
        dplyr::filter(between_time(!! date_var_expr, .start_date, .end_date))

}
