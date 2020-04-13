#' Add (For Time Series): Add a period to a time series
#'
#' The easiest way to add / subtract a period to a time series date or date-time vector.
#'
#' @param index A date or date-time vector. Can also accept a character representation.
#' @param period A period to add. Accepts character strings like "5 seconds", "2 days",
#'  and complex strings like "1 month 4 days 34 minutes".
#'
#' @return A `date` or datetime (`POSIXct`) vector the same length as `index` with the
#' time values shifted +/- a period.
#'
#' @details
#'
#' A convenient wrapper for `lubridate::period()`. Adds and subtracts a period from a
#' time-based index. Great for:
#'
#' - Finding a timestamp n-periods into the future or past
#' - Shifting a time-based index. Note that `NA` values may be present where dates don't exist.
#'
#' __Period Specification__
#'
#' The `period` argument accepts complex strings like:
#'
#' - "1 month 4 days 43 minutes"
#' - "second = 3, minute = 1, hour = 2, day = 13, week = 1"
#'
#' @seealso
#'
#' Other Time-Based vector functions:
#'
#' - [between_time()] - Range detection for date or date-time sequences.
#'
#' Underlying function:
#'
#' - [lubridate::period()]
#'
#'
#' @examples
#' library(timetk)
#'
#'
#' # ---- LOCATING A DATE N-PERIODS IN FUTURE / PAST ----
#'
#' # Forward
#' "2021-01-01 00:00:00" %+time% "1 hour 34 seconds"
#'
#' # Backward
#' "2021-01-01 00:00:00" %-time% "1 hour 34 seconds"
#'
#' # ---- INDEX SHIFTING ----
#'
#' index_daily <- tk_make_timeseries("2016-01-01", "2016-02-01", by = "day")
#'
#' # ADD TIME
#' # - Note `NA` values created where a daily dates aren't possible
#' #   (e.g. Feb 29 & 30, 2016 doesn't exist).
#' index_daily %+time% "1 month"
#'
#' # Subtracting Time
#' index_daily %-time% "1 month"
#'
#'
#'
#'
#' @name add_time
#' @export

# ADD ----

#' @export
#' @rdname add_time
add_time <- function(index, period) {
    UseMethod("add_time", index)
}

#' @export
add_time.character <- function(index, period) {
    index <- readr::parse_guess(index)
    add_time(index, period)
}

#' @export
add_time.POSIXt <- function(index, period) {
    time_adder(index, period)
}

#' @export
add_time.Date <- function(index, period) {
    time_adder(index, period)
}

#' @export
add_time.yearmon <- function(index, period) {
    index <- lubridate::as_date(index)
    message("Converting to date class")
    time_adder(index, period)
}

#' @export
add_time.yearqtr <- function(index, period) {
    index <- lubridate::as_date(index)
    message("Converting to date class")
    time_adder(index, period)
}

#' @export
add_time.numeric <- function(index, period) {
    stop("Index must be a non-numeric time-based class.")
}

#' @export
add_time.default <- function(index, period) {
    rlang::abort(paste0("No method for class ", class(index)[[1]], "."))
}

# SUBTRACT ----

#' @export
#' @rdname add_time
subtract_time <- function(index, period) {
    UseMethod("subtract_time", index)
}

#' @export
subtract_time.character <- function(index, period) {
    index <- readr::parse_guess(index)
    subtract_time(index, period)
}

#' @export
subtract_time.POSIXt <- function(index, period) {
    time_subtracter(index, period)
}

#' @export
subtract_time.Date <- function(index, period) {
    time_subtracter(index, period)
}

#' @export
subtract_time.yearmon <- function(index, period) {
    index <- lubridate::as_date(index)
    message("Converting to date class")
    time_subtracter(index, period)
}

#' @export
subtract_time.yearqtr <- function(index, period) {
    index <- lubridate::as_date(index)
    message("Converting to date class")
    time_subtracter(index, period)
}

#' @export
subtract_time.numeric <- function(index, period) {
    stop("Index must be a non-numeric time-based class.")
}

#' @export
subtract_time.default <- function(index, period) {
    rlang::abort(paste0("No method for class ", class(index)[[1]], "."))
}


#' @export
#' @rdname add_time
`%+time%` <- function(index, period) {
    add_time(index, period)
}

#' @export
#' @rdname add_time
`%-time%` <- function(index, period) {
    subtract_time(index, period)
}

# Utils ----

time_adder <- function(index, period) {
    check_quarter(period)
    index + lubridate::period(period)
}

time_subtracter <- function(index, period) {
    check_quarter(period)
    index - lubridate::period(period)
}

check_quarter <- function(period) {
    if (tolower(period) %>% stringr::str_detect("quarter")) {
        rlang::abort("`quarter` detected. Try using `3 month` increments instead.")
    }
}




