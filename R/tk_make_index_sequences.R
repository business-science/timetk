#' Make date and date-time sequences between a start and end date
#'
#' @param start_date Used to define the starting date for date sequence generation.
#'  Provide in "YYYY-MM-DD" format.
#' @param end_date Used to define the ending date for date sequence generation.
#'  Provide in "YYYY-MM-DD" format.
#' @param by A character string, containing one of
#'  `"sec"`, `"min"`, `"hour"`, `"day"`, `"week"`, `"month"`, `"quarter"` or `"year"`.
#'  You can create regularly spaced sequences using phrases like `by = "10 min"`.
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
#' __Daily Sequences__
#'
#' Make a daily sequence with `tk_make_index_sequence(by)`. Examples:
#'
#'  - Every Day: `by = "day"`
#'  - Every 2-Weeks: `by = "2 weeks"`
#'  - Every 6-months: `by = "6 months"`
#'
#' __Sub-Daily Sequences__
#'
#' Make a sub-daily sequence with `tk_make_index_sequence(by)`. Examples:
#'
#' - Every minute: `by = "min"`
#' - Every 30-seconds: `by = "30 sec"`
#' - Every 2-hours: `by = "2 hours`
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
#' # ---- BASIC EXAMPLES ----
#'
#' # Date Sequence
#' tk_make_index_sequence("2017-01-01", "2017-12-31", by = "day")
#'
#' # Date-Time Sequence
#' tk_make_index_sequence("2017-01-01", "2017-12-31", by = "10 min")
#'
#' # Holiday Sequence
#' tk_make_holiday_sequence("2017-01-01", "2017-12-31", calendar = "NYSE")
#'
#' # Weekday Sequence
#' tk_make_weekday_sequence("2017-01-01", "2017-12-31", remove_holidays = TRUE)
#'
#' # Weekday Sequence + Removing Holidays
#' tk_make_weekday_sequence("2017-01-01", "2017-12-31", remove_holidays = TRUE)
#'
#' # ---- COMBINE WITH MAKE FUTURE TIMESERIES FROM EXISTING ----
#' # - A common machine learning application is creating a future time series data set
#' #   from an existing
#'
#' # Create index of days that FB stock will be traded in 2017 based on 2016 + holidays
#' FB_tbl <- FANG %>% filter(symbol == "FB")
#'
#' holidays <- tk_make_holiday_sequence(
#'     start_date = "2017-01-01",
#'     end_date   = "2017-12-31",
#'     calendar   = "NYSE")
#'
#' # Remove holidays with skip_values, and remove weekends with inspect_weekdays = TRUE
#' FB_tbl %>%
#'     tk_index() %>%
#'     tk_make_future_timeseries(n_future         = 366,
#'                               inspect_weekdays = TRUE,
#'                               skip_values      = holidays)
#'
#'
#'
#' @name tk_make_index_sequence
NULL

# DATE SEQUENCE ----

#' @rdname tk_make_index_sequence
#' @export
tk_make_index_sequence <- function(start_date, end_date, by = "day",
                                   skip_values = NULL, insert_values = NULL) {

    if (stringr::str_detect(tolower(by), pattern = "(sec)|(min)|(hour)")) {
        # Sub-daily
        seq <- seq.POSIXt(
            from = lubridate::as_datetime(start_date),
            to   = lubridate::as_datetime(end_date),
            by   = by
        )

        seq <- add_subtract_sequence(seq, skip_values, insert_values)
        return(seq)

    } else {
        # Daily
        seq <- seq.Date(
            from = lubridate::as_date(start_date),
            to   = lubridate::as_date(end_date),
            by   = by
        )

        seq <- add_subtract_sequence(seq, skip_values, insert_values)
        return(seq)
    }

}

# HOLIDAYS -----

#' @rdname tk_make_index_sequence
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

    date_seq <- tk_make_index_sequence(start_date, end_date, by   = "day")

    # Find holidays
    years            <- date_seq %>% lubridate::year() %>% unique()
    holiday_sequence <- fun(year = years) %>% lubridate::as_date()
    holidays         <- holiday_sequence[holiday_sequence %in% date_seq]

    # Add/Subtract
    holidays <- add_subtract_sequence(holidays, skip_values, insert_values)

    return(holidays)
}

# WEEKENDS ----

#' @rdname tk_make_index_sequence
#' @export
tk_make_weekend_sequence <- function(start_date, end_date) {

    date_sequence <-  tk_make_index_sequence(start_date, end_date, by = "day")

    ret_tbl <- tibble::tibble(date_sequence = date_sequence) %>%
        dplyr::mutate(weekday = lubridate::wday(date_sequence, label = TRUE)) %>%
        dplyr::filter((weekday == "Sat" | weekday == "Sun"))

    ret_tbl %>% dplyr::pull(date_sequence)
}

# WEEKDAYS ----

#' @rdname tk_make_index_sequence
#' @export
tk_make_weekday_sequence <- function(start_date, end_date,
                                     remove_weekends = TRUE, remove_holidays = FALSE,
                                     calendar = c("NYSE", "LONDON", "NERC", "TSX", "ZURICH"),
                                     skip_values = NULL, insert_values = NULL
                                     ) {

    date_sequence <-  tk_make_index_sequence(start_date, end_date, by = "day")

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


