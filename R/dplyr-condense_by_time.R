#' Condense the Period (e.g. Daily to Monthly)
#'
#' @description
#' Convert a `tbl` object from daily to monthly,
#' from minute data to hourly, and more. This allows the user to easily
#' aggregate data to a less granular level by taking the value from either
#' the beginning or end of the period.
#'
#'
#' @param .data A `tbl` object or `data.frame`
#' @param .date_var A column containing date or date-time values to summarize.
#'  If missing, attempts to auto-detect date column.
#' @param .by A time unit to aggregate by.
#'   Time units are collapsed using `lubridate::floor_date()` or `lubridate::ceiling_date()`.
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
#' @param .type One of "first" or "last".
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
#' - [between_time()] - Range detection for date or date-time sequences.
#' - [pad_by_time()] - Insert time series rows with regularly spaced timestamps
#' - [condense_by_time()] - Convert to a different periodicity
#' - [slidify()] - Turn any function into a sliding (rolling) function
#'
#' @examples
#' # Libraries
#' library(timetk)
#' library(dplyr)
#'
#' # First value in each month
#' m4_daily %>%
#'     group_by(id) %>%
#'     condense_by_time(.by = "1 month")
#'
#' # Last value in each month
#' m4_daily %>%
#'     group_by(id) %>%
#'     condense_by_time(.by = "1 month", .side = "end")
#'
#'
#'
#' @export
condense_by_time <- function(.data, .date_var, .by = "day", .type = c("first", "last")) {

    if (rlang::quo_is_missing(rlang::enquo(.date_var))) {
        message(".date_var is missing. Using: ", tk_get_timeseries_variables(.data)[1])
    }

    UseMethod("condense_by_time")
}

#' @export
condense_by_time.default <- function(.data, .date_var, .by = "day", .type = c("first", "last")) {
    stop("Object is not of class `data.frame`.", call. = FALSE)
}

#' @export
condense_by_time.data.frame <- function(.data, .date_var, .by = "day", .type = c("first", "last")) {

    date_var_expr      <- rlang::enquo(.date_var)

    # Check date_var
    if (rlang::quo_is_missing(date_var_expr)) {
        date_var_text <- tk_get_timeseries_variables(.data)[1]
        date_var_expr <- rlang::sym(date_var_text)
    }

    # Choose first/last function
    fun_type <- tolower(.type[[1]])
    if (fun_type == "last") {
        .f <- dplyr::last
    } else {
        .f <- dplyr::first
    }

    # Time-based summarization logic
    ret_tbl <- .data %>%
        summarise_by_time(
            .date_var = !! date_var_expr,
            .by       = .by,
            dplyr::across(.fns = .f)
        )

    return(ret_tbl)

}
