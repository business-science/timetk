#' Convert the Period to a Lower Periodicity (e.g. Go from Daily to Monthly)
#'
#' @description
#' Convert a `data.frame` object from daily to monthly,
#' from minute data to hourly, and more. This allows the user to easily
#' aggregate data to a less granular level by taking the value from either
#' the beginning or end of the period.
#'
#'
#' @param .data A `tbl` object or `data.frame`
#' @param .date_var A column containing date or date-time values.
#'  If missing, attempts to auto-detect date column.
#' @param .period A period to condense the time series to.
#'   Time units are condensed using `lubridate::floor_date()` or `lubridate::ceiling_date()`.
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
#' @param .side One of "start" or "end". Determines if the first observation in the period should be returned
#'  or the last.
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
#' - [mutate_by_time()] - Simplifies applying mutations by time windows.
#' - [pad_by_time()] - Insert time series rows with regularly spaced timestamps
#' - [filter_by_time()] - Quickly filter using date ranges.
#' - [filter_period()] - Apply filtering expressions inside periods (windows)
#' - [slice_period()] - Apply slice inside periods (windows)
#' - [condense_period()] - Convert to a different periodicity
#' - [between_time()] - Range detection for date or date-time sequences.
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
#'     condense_period(.period = "1 month")
#'
#' # Last value in each month
#' m4_daily %>%
#'     group_by(id) %>%
#'     condense_period(.period = "1 month", .side = "end")
#'
#'
#'
#' @export
condense_period <- function(.data, .date_var, .period = "1 day", .side = c("start", "end")) {

    if (rlang::quo_is_missing(rlang::enquo(.date_var))) {
        message(".date_var is missing. Using: ", tk_get_timeseries_variables(.data)[1])
    }

    UseMethod("condense_period")
}

#' @export
condense_period.default <- function(.data, .date_var, .period = "1 day", .side = c("start", "end")) {
    stop("Object is not of class `data.frame`.", call. = FALSE)
}

#' @export
condense_period.data.frame <- function(.data, .date_var, .period = "1 day", .side = c("start", "end")) {

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

    # Get the function type logic
    fun_type <- tolower(.side[[1]])

    # Switch alternative expressions
    if (fun_type == "first" || fun_type == "left") {
        message("condense_period: Using .side = 'start'.")
        fun_type <- "start"
    }
    if (fun_type == "last" || fun_type == "right") {
        message("condense_period: Using .side = 'end'.")
        fun_type <- "end"
    }

    # Choose min(start) or max(end) function
    if (fun_type == "end") {
        .f <- max
    } else {
        .f <- min
    }

    # Time-based filtering logic
    ret_tbl <- .data %>%
        filter_period(
            .date_var = !! date_var_expr,
            .period   = .period,
            # Filter max/min date
            !! date_var_expr == .f(!! date_var_expr)
        )

    return(ret_tbl)

}
