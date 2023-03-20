#' Filter (for Time-Series Data)
#'
#' The easiest way to filter time-based ___start/end ranges___ using shorthand timeseries notation.
#' See [filter_period()] for applying filter expression by period (windows).
#'
#' @param .data A tibble with a time-based column.
#' @param .date_var A column containing date or date-time values to filter.
#'  If missing, attempts to auto-detect date column.
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
#'
#' Time-Based dplyr functions:
#'
#' - [summarise_by_time()] - Easily summarise using a date column.
#' - [mutate_by_time()] - Simplifies applying mutations by time windows.
#' - [pad_by_time()] - Insert time series rows with regularly spaced timestamps
#' - [filter_by_time()] - Quickly filter using date ranges.
#' - [filter_period()] - Apply filtering expressions inside periods (windows)
#' - [slice_period()] - Apply slice inside periods (windows)
#' - [condense_period()] - Convert to a different periodicity
#' - [between_time()] - Range detection for date or date-time sequences.
#' - [slidify()] - Turn any function into a sliding (rolling) function
#'
#' @references
#' - This function is based on the `tibbletime::filter_time()` function developed by Davis Vaughan.
#'
#' @examples
#' library(dplyr)
#' library(tidyquant)
#' library(timetk)
#'
#' # Filter values in January 1st through end of February, 2013
#' FANG %>%
#'     group_by(symbol) %>%
#'     filter_by_time(.start_date = "start", .end_date = "2013-02") %>%
#'     plot_time_series(date, adjusted, .facet_ncol = 2, .interactive = FALSE)
#'
#' @name filter_by_time
#' @export
filter_by_time <- function(.data, .date_var, .start_date = "start", .end_date = "end") {

    if (rlang::quo_is_missing(rlang::enquo(.date_var))) {
        message(".date_var is missing. Using: ", tk_get_timeseries_variables(.data)[1])
    }

    UseMethod("filter_by_time", .data)
}

#' @export
filter_by_time.default <- function(.data, .date_var, .start_date = "start", .end_date = "end") {
    stop("Object is not of class `tbl_time`.", call. = FALSE)
}

#' @export
filter_by_time.data.frame <- function(.data, .date_var, .start_date = "start", .end_date = "end") {

    date_var_expr      <- rlang::enquo(.date_var)

    # Check date_var
    if (rlang::quo_is_missing(date_var_expr)) {
        date_var_text <- tk_get_timeseries_variables(.data)[1]
        date_var_expr <- rlang::sym(date_var_text)
    }

    # Check index exists
    date_var_text <- rlang::quo_name(date_var_expr)
    if (!date_var_text %in% names(.data)) {
        rlang::abort(stringr::str_glue("Attempting to use .date_var = {date_var_text}. Column does not exist in .data. Please specify a date or date-time column."))
    }

    ret <- .data %>%
        dplyr::filter((!! date_var_expr) %>% between_time(!! enquo(.start_date), !! enquo(.end_date)))


    return(ret)

}
