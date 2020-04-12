#' Between (For Time Series): Range detection for date or date-time sequences
#'
#' The easiest way to filter time series date or date-time vectors. Returns a
#' logical vector indicating which date or date-time values are within a range.
#' See [filter_by_time()] for the `data.frame` (`tibble`) implementation.
#'
#' @param index A date or date-time vector.
#' @param start_date The starting date
#' @param end_date The ending date
#'
#' @return A `logical` vector the same length as `index` indicating whether or not
#' the timestamp value was within the `start_date` and `end_date` range.
#'
#' @details
#'
#' __Pure Time Series Filtering Flexibilty__
#'
#' The `start_date`  and `end_date` parameters are designed with flexibility in mind.
#'
#' Each side of the `time_formula` is specified as the character
#' `'YYYY-MM-DD HH:MM:SS'`, but powerful shorthand is available.
#' Some examples are:
#' * __Year:__ `start_date = '2013', end_date = '2015'`
#' * __Month:__ `start_date = '2013-01', end_date = '2016-06'`
#' * __Day:__ `start_date = '2013-01-05', end_date = '2016-06-04'`
#' * __Second:__ `start_date = '2013-01-05 10:22:15', end_date = '2018-06-03 12:14:22'`
#' * __Variations:__ `start_date = '2013', end_date = '2016-06'`
#'
#' __Key Words: "start" and "end"__
#'
#' Use the keywords "start" and "end" as shorthand, instead of specifying the
#' actual start and end values. Here are some examples:
#'
#' * __Start of the series to end of 2015:__ `start_date = 'start', end_date = '2015'`
#' * __Start of 2014 to end of series:__ `start_date = '2014', end_date = 'end'`
#'
#' __Internal Calculations__
#'
#' All shorthand dates are expanded:
#' * The `start_date` is expanded to be the _first date_ in that period
#' * The `end_date` side is expanded to be the _last date_ in that period
#'
#' This means that the following examples are equivalent (assuming your
#' index is a POSIXct):
#' * `start_date = '2015'` is equivalent to `start_date = '2015-01-01 + 00:00:00' `
#' * `end_date = '2016'` is equivalent to `2016-12-31 + 23:59:59'`
#'
#' @seealso
#'
#' Other Time-Based dplyr functions:
#'
#' - [summarise_by_time()] - Easily summarise using a date column.
#' - [filter_by_time()] - Quickly filter using date ranges.
#' - [between_time()] - Range detection for date or date-time sequences.
#'
#' @references
#' - This function is based on the `tibbletime::filter_time()` function developed by Davis Vaughan.
#'
#'
#' @examples
#' library(tidyverse)
#' library(tidyquant)
#' library(timetk)
#'
#' index_daily <- tk_make_timeseries("2016-01-01", "2017-01-01", by = "day")
#' index_min   <- tk_make_timeseries("2016-01-01", "2017-01-01", by = "min")
#'
#' # How it works
#' # - Returns TRUE/FALSE length of index
#' # - Use sum() to tally the number of TRUE values
#' index_daily %>% between_time("start", "2016-01") %>% sum()
#'
#' # ---- INDEX SLICING ----
#'
#' # Daily Series: Month of January 2016
#' index_daily[index_daily %>% between_time("start", "2016-01")]
#'
#' # Daily Series: March 1st - June 15th, 2016
#' index_daily[index_daily %>% between_time("2016-03", "2016-06-15")]
#'
#' # Minute Series:
#' index_min[index_min %>% between_time("2016-02-01 12:00", "2016-02-01 13:00")]
#'
#' # ---- FILTERING WITH DPLYR ----
#' FANG %>%
#'     group_by(symbol) %>%
#'     filter(date %>% between_time("2016-01", "2016-01"))
#'
#' @name between_time
#' @export
between_time <- function(index, start_date = "start", end_date = "end") {
    UseMethod("between_time", index)
}

#' @export
between_time.default <- function(index, start_date = "start", end_date = "end") {
    stop(call. = FALSE, paste0("`between_time(index)` has no method for class ", class(index)[[1]]))
}

#' @export
between_time.POSIXct <- function(index, start_date = "start", end_date = "end") {
    between_time_vec(index, start_date, end_date)
}

#' @export
between_time.Date <- function(index, start_date = "start", end_date = "end") {
    between_time_vec(index, start_date, end_date)
}

#' @export
between_time.yearmon <- function(index, start_date = "start", end_date = "end") {
    between_time_vec(index, start_date, end_date)
}

#' @export
between_time.yearqtr <- function(index, start_date = "start", end_date = "end") {
    between_time_vec(index, start_date, end_date)
}

#' @export
between_time.hms <- function(index, start_date = "start", end_date = "end") {
    between_time_vec(index, start_date, end_date)
}

between_time_vec <- function(index, start_date = "start", end_date = "end") {

    tz     <- lubridate::tz(index)
    start_date <- as.character(start_date)
    end_date   <- as.character(end_date)

    time_formula <- rlang::new_formula(start_date, end_date)

    # Parse the time_formula, don't convert to dates yet
    tf_list <- parse_time_formula(index, time_formula)

    # Convert to datetime
    from_to <- purrr::map(
        .x = tf_list,
        .f = ~list_to_datetime(index, .x, tz = tz)
    )

    # Get sequence creation pieces ready
    from <- from_to[[1]]
    to   <- from_to[[2]]

    dplyr::between(index, from, to)

}


