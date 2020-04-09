#' Make date and date-time sequences between a start and end date
#'
#' @param start_date Used to define the starting date for date sequence generation.
#'  Provide in "YYYY-MM-DD" format.
#' @param end_date Used to define the ending date for date sequence generation.
#'  Provide in "YYYY-MM-DD" format.
#' @param by A character string, containing one of
#'  `"sec"`, `"min"`, `"hour"`, `"day"`, `"week"`, `"month"`, `"quarter"` or `"year"`.
#'  You can create regularly spaced sequences using phrases like `by = "10 min"`.
#' @param length_out Optional length of the sequence. Can be used instead of one of:
#'  `start_date`, `end_date`, or `by`.
#' @param calendar The calendar to be used in Date Sequence calculations for Holidays
#'  from the `timeDate` package.
#'  Acceptable values are: `"NYSE"`, `"LONDON"`, `"NERC"`, `"TSX"`, `"ZURICH"`.
#' @param skip_values A daily date sequence to skip
#' @param insert_values A daily date sequence to insert
#' @param remove_holidays A logical value indicating whether or not to
#' remove common holidays from the date sequence
#' @param remove_weekends A logical value indicating whether or not to
#' remove weekends (Saturday and Sunday) from the date sequence
#'
#' @details
#'
#' The `tk_make_date_sequence()` function handles both date and date-time sequences
#' automatically.
#'
#' - Parses date and date times from character
#' - Intelligently guesses the sequence desired based on arguments provided
#' - Handles spacing intelligently
#' - When both `by` and `length_out` are missing, guesses either second or day sequences
#' - Can skip and insert values if needed.
#'
#' __Daily Sequences__
#'
#' Make a daily sequence with `tk_make_date_sequence(by)`. Examples:
#'
#'  - Every Day: `by = "day"`
#'  - Every 2-Weeks: `by = "2 weeks"`
#'  - Every 6-months: `by = "6 months"`
#'
#' __Sub-Daily Sequences__
#'
#' Make a sub-daily sequence with `tk_make_date_sequence(by)`. Examples:
#'
#' - Every minute: `by = "min"`
#' - Every 30-seconds: `by = "30 sec"`
#' - Every 2-hours: `by = "2 hours`
#'
#' __Skip / Insert Timestamps__
#'
#' Skips and inserts are performed after the sequence is generated. This means that if you use
#' the `length_out` parameter, the length may differ than the `length_out`.
#'
#' __Holiday Sequences__
#'
#' `tk_make_holiday_sequence()` is a wrapper for various holiday calendars from the `timeDate` package,
#' making it easy to generate holiday sequences for common business calendars:
#'
#' - New York Stock Exchange: `calendar = "NYSE"`
#' - Londo Stock Exchange: `"LONDON"`
#' - North American Reliability Council: `"NERC"`
#' - Toronto Stock Exchange: `"TSX"`
#' - Zurich Stock Exchange: `"ZURICH"`
#'
#'
#' @return A vector containing future dates
#'
#' @seealso
#' - Make future index from existing: [tk_make_future_timeseries()]
#' - Index extraction: [tk_index()]
#' - Index information: [tk_get_timeseries_summary()], [tk_get_timeseries_signature()]
#'
#' @examples
#' library(dplyr)
#' library(tidyquant)
#' library(timetk)
#'
#' options(max.print = 50)
#'
#' # ---- BASIC DATE SEQUENCE EXAMPLES ----
#'
#' # Date Sequence - By Day
#' tk_make_date_sequence("2017-01-01", "2017-12-31") # Guesses by = "day"
#'
#' # Date Sequence - By Day
#' tk_make_date_sequence("2012-01-01", length_out = 6) # Guesses by = "day"
#'
#' # Date Sequence - By Month
#' tk_make_date_sequence("2012-01-01", by = "1 month", length_out = 6) # Switch to month
#'
#'
#' # ---- BASIC DATE-TIME SEQUENCE EXAMPLES ----
#'
#' # Date-Time Sequence - By Second
#' tk_make_date_sequence("2016-01-01 01:01:02", "2016-01-01 01:01:04") # Guesses by second
#'
#' # Date-Time Sequence - By 10 Minutes
#' # - Converts to date-time automatically & applies 10-min interval
#' tk_make_date_sequence("2017-01-01", "2017-01-02", by = "10 min")
#'
#'
#' # ---- HOLIDAYS & WEEKENDS ----
#'
#' # Business Holiday Sequence (only available in day frequency)
#' tk_make_holiday_sequence("2017-01-01", "2017-12-31", calendar = "NYSE")
#'
#' # Weekday Sequence (only available in day frequency)
#' tk_make_weekday_sequence("2017-01-01", "2017-12-31", remove_holidays = TRUE)
#'
#' # Weekday Sequence + Removing Business Holidays (only available in day frequency)
#' tk_make_weekday_sequence("2017-01-01", "2017-12-31", remove_holidays = TRUE)
#'
#'
#' # ---- COMBINE HOLIDAYS WITH MAKE FUTURE TIMESERIES FROM EXISTING ----
#' # - A common machine learning application is creating a future time series data set
#' #   from an existing
#'
#' # Create index of days that FB stock will be traded in 2017 based on 2016 + holidays
#' FB_tbl <- FANG %>% filter(symbol == "FB")
#'
#' holidays <- tk_make_holiday_sequence(
#'     start_date = "2016-12-31",
#'     end_date   = "2017-12-31",
#'     calendar   = "NYSE")
#'
#' weekends <- tk_make_weekend_sequence(
#'     start_date = "2016-12-31",
#'     end_date   = "2017-12-31")
#'
#' # Remove holidays and weekends with skip_values
#' # We could also remove weekends with inspect_weekdays = TRUE
#' FB_tbl %>%
#'     tk_index() %>%
#'     tk_make_future_timeseries(n_future         = 366,
#'                               skip_values      = c(holidays, weekends))
#'
#'
#'
#' @name tk_make_date_sequence
NULL

# DATE SEQUENCE ----

#' @rdname tk_make_date_sequence
#' @export
tk_make_date_sequence <- function(start_date, end_date, by, length_out = NULL,
                                   skip_values = NULL, insert_values = NULL) {

    if (rlang::is_missing(start_date) & rlang::is_missing(end_date)) {
        rlang::abort("Must specify a start_date and/or end_date.")
    }

    # Determine if sequence_type is date or datetime
    if (!rlang::is_missing(by)) {
        if (stringr::str_detect(tolower(by), pattern = "(sec)|(min)|(hour)")) {
            parser <- "datetime"
        } else {
            parser <- "date"
        }
    } else if (!rlang::is_missing(start_date)) {
        parser <- readr::guess_parser(start_date)

    } else {
        parser <- readr::guess_parser(end_date)
    }


    # Apply
    if (parser == "datetime") {
        # Sub-daily

        if (!rlang::is_missing(start_date)) start_date <- readr::parse_datetime(start_date)
        if (!rlang::is_missing(end_date)) end_date <- readr::parse_datetime(end_date)
        if (rlang::is_missing(by)) {
            condition_count <- rlang::is_missing(start_date) + rlang::is_missing(end_date) + is.null(length_out)
            if (condition_count < 3) {
                by <- "sec"
                message("Using by: sec")
            }
        }

        seq <- seq.POSIXt(
            from = start_date,
            to   = end_date,
            by   = by,
            length.out = length_out
        )

        seq <- add_subtract_sequence(seq, skip_values, insert_values)
        return(seq)

    } else {
        # Daily

        if (!rlang::is_missing(start_date)) start_date <- readr::parse_date(start_date)
        if (!rlang::is_missing(end_date)) end_date <- readr::parse_date(end_date)
        if (rlang::is_missing(by)) {
            condition_count <- rlang::is_missing(start_date) + rlang::is_missing(end_date) + is.null(length_out)
            if (condition_count < 3) {
                by <- "day"
                message("Using by: day")
            }
        }

        seq <- seq.Date(
            from = start_date,
            to   = end_date,
            by   = by,
            length.out = length_out
        )

        seq <- add_subtract_sequence(seq, skip_values, insert_values)
        return(seq)
    }

}

# HOLIDAYS -----

#' @rdname tk_make_date_sequence
#' @export
tk_make_holiday_sequence <- function(start_date, end_date,
                                     calendar = c("NYSE", "LONDON", "NERC", "TSX", "ZURICH"),
                                     skip_values = NULL, insert_values = NULL) {

    fun <- switch(
        tolower(calendar[1]),
        "nyse"     = timeDate::holidayNYSE,
        "london"   = timeDate::holidayLONDON,
        "nerc"     = timeDate::holidayNERC,
        "tsx"      = timeDate::holidayTSX,
        "zurich"   = timeDate::holidayZURICH
    )

    date_seq <- tk_make_date_sequence(start_date, end_date, by   = "day")

    # Find holidays
    years            <- date_seq %>% lubridate::year() %>% unique()
    holiday_sequence <- fun(year = years) %>% lubridate::as_date()
    holidays         <- holiday_sequence[holiday_sequence %in% date_seq]

    # Add/Subtract
    holidays <- add_subtract_sequence(holidays, skip_values, insert_values)

    return(holidays)
}

# WEEKENDS ----

#' @rdname tk_make_date_sequence
#' @export
tk_make_weekend_sequence <- function(start_date, end_date) {

    date_sequence <-  tk_make_date_sequence(start_date, end_date, by = "day")

    ret_tbl <- tibble::tibble(date_sequence = date_sequence) %>%
        dplyr::mutate(weekday = lubridate::wday(date_sequence, label = TRUE)) %>%
        dplyr::filter((weekday == "Sat" | weekday == "Sun"))

    ret_tbl %>% dplyr::pull(date_sequence)
}

# WEEKDAYS ----

#' @rdname tk_make_date_sequence
#' @export
tk_make_weekday_sequence <- function(start_date, end_date,
                                     remove_weekends = TRUE, remove_holidays = FALSE,
                                     calendar = c("NYSE", "LONDON", "NERC", "TSX", "ZURICH"),
                                     skip_values = NULL, insert_values = NULL
                                     ) {

    date_sequence <-  tk_make_date_sequence(start_date, end_date, by = "day")

    # Remove weekends
    if (remove_weekends) {
        weekend_sequence <- tk_make_weekend_sequence(start_date, end_date)
        date_sequence    <- date_sequence[!date_sequence %in% weekend_sequence]
    }

    # Remove Holidays
    if (remove_holidays) {
        holiday_sequence <- tk_make_holiday_sequence(start_date, end_date, calendar)
        date_sequence <- date_sequence[!date_sequence %in% holiday_sequence]
    }

    # Skip/Insert
    date_sequence <- add_subtract_sequence(date_sequence, skip_values, insert_values)

    return(date_sequence)
}


