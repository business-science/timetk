#' Make daily Holiday and Weekend date sequences
#'
#'
#' @param start_date Used to define the starting date for date sequence generation.
#'  Provide in "YYYY-MM-DD" format.
#' @param end_date Used to define the ending date for date sequence generation.
#'  Provide in "YYYY-MM-DD" format.
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
#' __Start and End Date Specification__
#'
#' - Accept shorthand notation (i.e. `tk_make_timeseries()` specifications apply)
#' - Only available in Daily Periods.
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
#' __Weekend and Weekday Sequences__
#'
#' These simply populate
#'
#'
#' @return A vector containing future dates
#'
#' @seealso
#' - Intelligent date or date-time sequence creation: [tk_make_timeseries()]
#' - Holidays and weekends: [tk_make_holiday_sequence()], [tk_make_weekend_sequence()], [tk_make_weekday_sequence()]
#' - Make future index from existing: [tk_make_future_timeseries()]
#'
#' @examples
#' library(dplyr)
#' library(tidyquant)
#' library(timetk)
#'
#' options(max.print = 50)
#'
#'
#' # ---- HOLIDAYS & WEEKENDS ----
#'
#' # Business Holiday Sequence
#' tk_make_holiday_sequence("2017-01-01", "2017-12-31", calendar = "NYSE")
#'
#' tk_make_holiday_sequence("2017", calendar = "NYSE") # Same thing as above (just shorter)
#'
#' # Weekday Sequence
#' tk_make_weekday_sequence("2017", "2018", remove_holidays = TRUE)
#'
#' # Weekday Sequence + Removing Business Holidays
#' tk_make_weekday_sequence("2017", "2018", remove_holidays = TRUE)
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
#'     start_date = "2016",
#'     end_date   = "2017",
#'     calendar   = "NYSE")
#'
#' weekends <- tk_make_weekend_sequence(
#'     start_date = "2016",
#'     end_date   = "2017")
#'
#' # Remove holidays and weekends with skip_values
#' # We could also remove weekends with inspect_weekdays = TRUE
#' FB_tbl %>%
#'     tk_index() %>%
#'     tk_make_future_timeseries(length_out       = 366,
#'                               skip_values      = c(holidays, weekends))
#'
#'
#'
#' @name tk_make_holiday_sequence
NULL

# DATE SEQUENCE ----



# HOLIDAYS -----

#' @rdname tk_make_holiday_sequence
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

    date_seq <- tk_make_timeseries(
        start_date = start_date,
        end_date   = end_date,
        by         = "day")

    # Find holidays
    years            <- date_seq %>% lubridate::year() %>% unique()
    holiday_sequence <- fun(year = years) %>% lubridate::as_date()
    holidays         <- holiday_sequence[holiday_sequence %in% date_seq]

    # Add/Subtract
    holidays <- add_subtract_sequence(holidays, skip_values, insert_values)

    return(holidays)
}

# WEEKENDS ----

#' @rdname tk_make_holiday_sequence
#' @export
tk_make_weekend_sequence <- function(start_date, end_date) {

    date_sequence <- tk_make_timeseries(
        start_date = start_date,
        end_date   = end_date,
        by         = "day")

    ret_tbl <- tibble::tibble(date_sequence = date_sequence) %>%
        dplyr::mutate(weekday = lubridate::wday(date_sequence, label = TRUE)) %>%
        dplyr::filter((weekday == "Sat" | weekday == "Sun"))

    ret_tbl %>% dplyr::pull(date_sequence)
}

# WEEKDAYS ----

#' @rdname tk_make_holiday_sequence
#' @export
tk_make_weekday_sequence <- function(start_date, end_date,
                                     remove_weekends = TRUE, remove_holidays = FALSE,
                                     calendar = c("NYSE", "LONDON", "NERC", "TSX", "ZURICH"),
                                     skip_values = NULL, insert_values = NULL
                                     ) {

    date_sequence <- tk_make_timeseries(
        start_date = start_date,
        end_date   = end_date,
        by         = "day")

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


