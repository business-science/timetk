#' Insert time series rows with regularly spaced timestamps
#'
#' The easiest way to fill in missing timestamps or convert to a more
#' granular period (e.g. quarter to month). Wraps the `padr::pad()` function
#' for padding tibbles.
#'
#' @param .data A tibble with a time-based column.
#' @param .date_var A column containing date or date-time values to pad
#' @param .by Either "auto", a time-based frequency like "year", "month", "day", "hour", etc,
#'  or a time expression like "5 min", or "7 days". See Details.
#' @param .pad_value Fills in padded values. Default is `NA`.
#'
#' @details
#'
#' __Padding Missing Observations__
#'
#' The most common use case for `pad_by_time()` is to add rows where timestamps
#' are missing. This could be from sales data that have missing values on weekends and holidays.
#' Or it could be high frequency data where observations are irregularly spaced and should be
#' reset to a regular frequency.
#'
#' __Going from Low to High Frequency__
#'
#' The second use case is going from a low frequency (e.g. day) to high frequency (e.g. hour).
#' This is possible by supplying a higher frequency to `pad_by_time()`.
#'
#' __Interval, .by__
#'
#' Padding can be applied in the following ways:
#' - `.by = "auto"` - `pad_by_time()` will detect the time-stamp frequency and apply padding.
#' - The eight intervals in are: year, quarter, month, week, day, hour, min, and sec.
#' - Intervals like 5 minutes, 6 hours, 10 days are possible.
#'
#' @seealso
#'
#' Imputation:
#' - [impute_ts_vec()] - Impute missing values for time series.
#'
#' Additional Time-Based `dplyr`-style functions:
#'
#' - [summarise_by_time()] - Easily summarise using a date column.
#' - [filter_by_time()] - Quickly filter using date ranges.
#' - [between_time()] - Range detection for date or date-time sequences.
#' - [pad_by_time()] - Insert time series rows with regularly spaced timestamps
#' - [slidify()] - Turn any function into a sliding (rolling) function
#'
#' @references
#' - This function wraps the `padr::pad()` function developed by Edwin Thoen.
#'
#' @examples
#' library(tidyverse)
#' library(tidyquant)
#' library(timetk)
#'
#' # Create a quarterly series with 1 missing value
#' missing_data_tbl <- tibble(
#'     date = tk_make_date_sequence("2014-01-01", "2015-01-01", by = "quarter"),
#'     value = 1:5
#' ) %>%
#'     slice(-4) # Lose the 4th quarter on purpose
#' missing_data_tbl
#'
#'
#' # Detects missing quarter, and pads the missing regularly spaced quarter with NA
#' missing_data_tbl %>% pad_by_time(date, .by = "quarter")
#'
#' # Can specify a shorter period. This fills monthly.
#' missing_data_tbl %>% pad_by_time(date, .by = "month")
#'
#' # Can let pad_by_time() auto-detect date and period
#' missing_data_tbl %>% pad_by_time()
#'
#' # Can specify a .pad_value
#' missing_data_tbl %>% pad_by_time(date, .by = "quarter", .pad_value = 0)
#'
#' # Can then impute missing values
#' missing_data_tbl %>%
#'     pad_by_time(date, .by = "quarter") %>%
#'     mutate(value = impute_ts_vec(value, period = 1))
#'
#' # --- GROUPS ----
#' FANG %>%
#'     group_by(symbol) %>%
#'     pad_by_time(date, .by = "day")
#'
#' @name pad_by_time
#' @export
pad_by_time <- function(.data, .date_var, .by = "auto", .pad_value = NA) {

    if (rlang::quo_is_missing(rlang::enquo(.date_var))) {
        message(".date_var is missing. Using: ", tk_get_timeseries_variables(.data)[1])
    }

    UseMethod("pad_by_time", .data)
}

#' @export
pad_by_time.default <- function(.data, .date_var, .by = "auto", .pad_value = NA) {
    rlang::abort("Sorry, no method for class, ", class(.data)[1])
}

#' @export
pad_by_time.data.frame <- function(.data, .date_var, .by = "auto", .pad_value = NA) {
    padder(.data = .data, .date_var = !! enquo(.date_var), .by = .by, .pad_value = .pad_value)
}

#' @export
pad_by_time.grouped_df <- function(.data, .date_var, .by = "auto", .pad_value = NA) {

    group_names <- dplyr::group_vars(.data)
    date_var_expr <- rlang::enquo(.date_var)

    if (rlang::quo_is_missing(rlang::enquo(.date_var))) {
        date_var_expr <- rlang::ensym(tk_get_timeseries_variables(.data)[1])
    }


    .data %>%
        tidyr::nest() %>%
        dplyr::mutate(nested.col = purrr::map(
            .x         = data,
            .f         = function(df) padder(
                .data        = df,
                .date_var    = !! date_var_expr,
                .by          = .by,
                .pad_value  = .pad_value
            )
        )) %>%
        dplyr::select(-data) %>%
        tidyr::unnest(cols = nested.col) %>%
        dplyr::group_by_at(.vars = group_names)
}

# UTILS ----

padder <- function(.data, .date_var, .by = "auto", .pad_value = NA,
                           .start_date = NULL, .end_date = NULL,
                           .group = NULL, .stop_padding_if = 10e6) {

    date_var_expr <- rlang::enquo(.date_var)

    if (rlang::quo_is_missing(rlang::enquo(.date_var))) {
        date_var_expr <- rlang::sym(tk_get_timeseries_variables(.data)[1])
    }

    # Convert by to NULL
    if (.by == "auto") .by <- NULL

    # Convert start and end dates
    if (!is.null(.start_date)) {
        if (stringr::str_detect(.by, "(hour)|(min)|(sec)")) {
            .start_date <- as.POSIXct(.start_date)
        } else {
            .start_date <- lubridate::as_date(.start_date)
        }
    }
    if (!is.null(.end_date)) {
        if (stringr::str_detect(.by, "(hour)|(min)|(sec)")) {
            .end_date <- as.POSIXct(.end_date)
        } else {
            .end_date <- lubridate::as_date(.end_date)
        }
    }

    # Apply the padding
    ret <- padr::pad(
        x           = .data,
        by          = rlang::quo_name(date_var_expr),
        interval    = .by,
        start_val   = .start_date,
        end_val     = .end_date,
        group       = .group,
        break_above = .stop_padding_if
    )

    # Replace fill values
    if (!is.na(.pad_value)) {
        ret[is.na(ret)] <- .pad_value
    }

    # Make tibble
    if(!tibble::is_tibble(ret)) ret <- tibble::as_tibble(ret)

    return(ret)

}



