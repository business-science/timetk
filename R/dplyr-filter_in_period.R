#' Filter within a period (e.g. Filter to the Max Value Each Month)
#'
#' @description
#' Applies a dplyr filtering expression inside a time-based period (window).
#' This allows for filtering expressions like:
#' - Filtering to the maximum value each month.
#' - Filtering the first date each month.
#' - Filtering all rows with value greater than a monthly average
#'
#'
#' @param .data A `tbl` object or `data.frame`
#' @param .date_var A column containing date or date-time values.
#'  If missing, attempts to auto-detect date column.
#' @param .period A period to filter within.
#'   Time units are grouped using `lubridate::floor_date()` or `lubridate::ceiling_date()`.
#'
#'   The value can be:
#'   - `second`
#'   - `minute`
#'   - `hour`
#'   - `day`
#'   - `week`
#'   - `month`
#'   - `bimonth`
#'   - `quarter`
#'   - `season`
#'   - `halfyear`
#'   - `year`
#'
#'   Arbitrary unique English abbreviations as in the `lubridate::period()` constructor are allowed:
#'   - `"1 year"`
#'   - `"2 months"`
#'   - `"30 seconds"`
#'
#' @param ... Filtering expression. Expressions that return a logical value, and are defined in
#' terms of the variables in .data. If multiple expressions are included, they are combined with
#' the & operator. Only rows for which all conditions evaluate to TRUE are kept.
#'
#'
#' @return
#' A `tibble` or `data.frame`
#'
#' @seealso
#'
#' Time-Based dplyr functions:
#'
#' - [summarise_by_time()] - Easily summarise using a date column.
#' - [filter_by_time()] - Quickly filter using date ranges.
#' - [filter_in_period()] - Apply filtering expressions inside periods (windows)
#' - [between_time()] - Range detection for date or date-time sequences.
#' - [pad_by_time()] - Insert time series rows with regularly spaced timestamps
#' - [condense_period()] - Convert to a different periodicity
#' - [slidify()] - Turn any function into a sliding (rolling) function
#'
#' @examples
#' # Libraries
#' library(timetk)
#' library(dplyr)
#'
#' # Max value in each month
#' m4_daily %>%
#'     group_by(id) %>%
#'     filter_in_period(.period = "1 month", value == max(value))
#'
#' # First date each month
#' m4_daily %>%
#'     group_by(id) %>%
#'     filter_in_period(.period = "1 month", date == first(date))
#'
#' # All observations that are greater than a monthly average
#' m4_daily %>%
#'     group_by(id) %>%
#'     filter_in_period(.period = "1 month", value > mean(value))
#'
#' @export
filter_in_period <- function(.data, ..., .date_var, .period = "day") {

    if (rlang::quo_is_missing(rlang::enquo(.date_var))) {
        message(".date_var is missing. Using: ", tk_get_timeseries_variables(.data)[1])
    }

    UseMethod("filter_in_period")
}

#' @export
filter_in_period.default <- function(.data, ..., .date_var, .period = "day") {
    stop("Object is not of class `data.frame`.", call. = FALSE)
}

#' @export
filter_in_period.data.frame <- function(.data, ..., .date_var, .period = "day") {

    data_groups_expr   <- rlang::syms(dplyr::group_vars(.data))
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

    # Time-based filtering logic
    ret_tbl <- .data %>%
        dplyr::mutate(..date_agg = lubridate::floor_date(!! date_var_expr, unit = .period)) %>%
        dplyr::group_by(!!! data_groups_expr, ..date_agg) %>%
        dplyr::filter(...) %>%
        dplyr::ungroup() %>%
        dplyr::select(-..date_agg) %>%
        dplyr::group_by(!!! data_groups_expr)

    return(ret_tbl)

}
