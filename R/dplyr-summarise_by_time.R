#' Summarise (for Time Series Data)
#'
#' @description
#' `summarise_by_time()` is a time-based variant of the popular `dplyr::summarise()` function
#' that uses `.date_var` to specify a date or date-time column and `.by` to group the
#' calculation by groups like "5 seconds", "week", or "3 months".
#'
#' `summarise_by_time()` and `summarize_by_time()` are synonyms.
#'
#' @section Useful summary functions:
#'
#' * Sum: [sum()]
#' * Center: [mean()], [median()]
#' * Spread: [sd()], [var()]
#' * Range: [min()], [max()]
#' * Count: [dplyr::n()], [dplyr::n_distinct()]
#' * Position: [dplyr::first()], [dplyr::last()], [dplyr::nth()]
#' * Correlation: [cor()], [cov()]
#'
#'
#' @export
#' @param .data A `tbl` object or `data.frame`
#' @param .date_var A column of date or date-time (e.g. POSIXct) data class
#' @param ... Name-value pairs of summary functions.
#'   The name will be the name of the variable in the result.
#'
#'   The value can be:
#'
#'   * A vector of length 1, e.g. `min(x)`, `n()`, or `sum(is.na(y))`.
#'   * A vector of length `n`, e.g. `quantile()`.
#'   * A data frame, to add multiple columns from a single expression.
#' @param .by A time unit to summarise by.
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
#'   Arbitrary unique English abbreviations as in the `lubridate::period()` constructor are allowed.
#'
#' @param .type One of "floor", "ceiling", or "round. Defaults to "floor". See `lubridate::round_date`.
#'
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
#' - [slidify()] - Turn any function into a sliding (rolling) function
#'
#' @examples
#' # Libraries
#' library(tidyquant)
#' library(timetk)
#' library(dplyr)
#'
#' # First adjusted price in each month
#' FANG %>%
#'     group_by(symbol) %>%
#'     summarise_by_time(
#'         date, .by = "month", # Setup for monthly aggregation
#'         adjusted  = FIRST(adjusted)
#'     )
#'
#' # Last adjused price in each month (day is first day of next month with ceiling option)
#' FANG %>%
#'     group_by(symbol) %>%
#'     summarise_by_time(
#'         .date_var  = date,
#'         .by        = "month",
#'         adjusted   = LAST(adjusted),
#'         .type      = "ceiling") %>%
#'     # Shift to the last day of the month
#'     mutate(date = date %-time% "1 day")
#'
#' # Total Volume each year (.by is set to "year" now)
#' FANG %>%
#'     group_by(symbol) %>%
#'     summarise_by_time(
#'         .date_var  = date,
#'         .by        = "year",
#'         adjusted   = SUM(volume))
#'
#'
#' @export
summarise_by_time <- function(.data, .date_var, ..., .by = "day",
                              .type = c("floor", "ceiling", "round")) {
    UseMethod("summarise_by_time")
}

#' @rdname summarise_by_time
#' @export
summarize_by_time <- summarise_by_time

#' @export
summarise_by_time.default <- function(.data, .date_var, ..., .by = "day",
                                      .type = c("floor", "ceiling", "round")) {

    stop("Object is not of class `data.frame`.", call. = FALSE)

}

#' @export
summarise_by_time.data.frame <- function(.data, .date_var, ..., .by = "day",
                                         .type = c("floor", "ceiling", "round")) {

    data_groups_expr   <- rlang::syms(dplyr::group_vars(.data))
    date_var_expr      <- rlang::enquo(.date_var)

    # Choose lubridate function
    fun_type <- tolower(.type[[1]])
    if (fun_type == "floor") {
        .f <- lubridate::floor_date
    } else if (fun_type == "ceiling") {
        .f <- lubridate::ceiling_date
    } else {
        .f <- lubridate::round_date
    }

    # Time-based summarization logic
    ret_tbl <- .data %>%
        dplyr::mutate(!! date_var_expr := .f(!! date_var_expr, unit = .by)) %>%
        dplyr::group_by_at(.vars = dplyr::vars(!!! data_groups_expr, !! date_var_expr)) %>%
        dplyr::arrange(!! date_var_expr, .by_group = TRUE) %>%
        dplyr::summarize(...)

    return(ret_tbl)

}
