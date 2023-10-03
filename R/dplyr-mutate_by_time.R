#' Mutate (for Time Series Data)
#'
#' @description
#' `mutate_by_time()` is a time-based variant of the popular `dplyr::mutate()` function
#' that uses `.date_var` to specify a date or date-time column and `.by` to group the
#' calculation by groups like "5 seconds", "week", or "3 months".
#'
#'
#' @param .data A `tbl` object or `data.frame`
#' @param .date_var A column containing date or date-time values to summarize.
#'  If missing, attempts to auto-detect date column.
#' @param ... Name-value pairs.
#'   The name gives the name of the column in the output.
#'
#'   The value can be:
#'
#'   * A vector of length 1, which will be recycled to the correct length.
#'   * A vector the same length as the current group (or the whole data frame
#'     if ungrouped).
#'   * `NULL`, to remove the column.
#'   * A data frame or tibble, to create multiple columns in the output.
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
#' library(dplyr)
#'
#' # First value in each month
#' m4_daily_first_by_month_tbl <- m4_daily %>%
#'     group_by(id) %>%
#'     mutate_by_time(
#'         .date_var = date,
#'         .by       = "month", # Setup for monthly aggregation
#'         # mutate recycles a single value
#'         first_value_by_month  = first(value)
#'     )
#' m4_daily_first_by_month_tbl
#'
#' # Visualize Time Series vs 1st Value Each Month
#' m4_daily_first_by_month_tbl %>%
#'     tidyr::pivot_longer(value:first_value_by_month) %>%
#'     plot_time_series(date, value, name,
#'                      .facet_scale = "free", .facet_ncol = 2,
#'                      .smooth = FALSE, .interactive = FALSE)
#'
#' @export
mutate_by_time <- function(.data, .date_var, .by = "day", ...,
                              .type = c("floor", "ceiling", "round")) {

    if (rlang::quo_is_missing(rlang::enquo(.date_var))) {
        message(".date_var is missing. Using: ", tk_get_timeseries_variables(.data)[1])
    }

    UseMethod("mutate_by_time")
}



#' @export
mutate_by_time.default <- function(.data, .date_var, .by = "day", ...,
                                      .type = c("floor", "ceiling", "round")) {

    stop("Object is not of class `data.frame`.", call. = FALSE)

}

#' @export
mutate_by_time.data.frame <- function(.data, .date_var, .by = "day", ...,
                                         .type = c("floor", "ceiling", "round")) {

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

    # Choose lubridate function
    fun_type <- tolower(.type[[1]])
    if (fun_type == "floor") {
        .f <- lubridate::floor_date
    } else if (fun_type == "ceiling") {
        .f <- lubridate::ceiling_date
    } else {
        .f <- lubridate::round_date
    }

    # Time-based mutate logic
    ret_tbl <- .data %>%
        dplyr::mutate(.date_var_collapsed := .f(!! date_var_expr, unit = .by)) %>%
        dplyr::group_by_at(.vars = dplyr::vars(!!! data_groups_expr, .date_var_collapsed)) %>%
        dplyr::arrange(!! date_var_expr, .by_group = TRUE) %>%
        dplyr::mutate(...) %>%
        dplyr::ungroup() %>%
        dplyr::select(-.date_var_collapsed) %>%
        dplyr::group_by_at(.vars = dplyr::vars(!!! data_groups_expr))

    return(ret_tbl)

}
