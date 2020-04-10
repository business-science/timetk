#' Intelligent date and date-time sequence creation
#'
#' Improves on the `seq.Date()` and `seq.POSIXt()` functions by simplifying
#' into 1 function `tk_make_date_sequence()`. Intelligently handles character dates
#' and logical assumptions based on user inputs.
#'
#' @param start_date Used to define the starting date for date sequence generation.
#'  Provide in "YYYY-MM-DD" format.
#' @param end_date Used to define the ending date for date sequence generation.
#'  Provide in "YYYY-MM-DD" format.
#' @param by A character string, containing one of
#'  `"sec"`, `"min"`, `"hour"`, `"day"`, `"week"`, `"month"`, `"quarter"` or `"year"`.
#'  You can create regularly spaced sequences using phrases like `by = "10 min"`. See Details.
#' @param length_out Optional length of the sequence. Can be used instead of one of:
#'  `start_date`, `end_date`, or `by`.
#' @param skip_values A sequence to skip
#' @param insert_values A sequence to insert
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
#'
#'
#' @return A vector containing date or date-times
#'
#' @seealso
#' - Intelligent date or date-time sequence creation: [tk_make_date_sequence()]
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
#' # ---- SKIP & INSERT VALUES ----
#'
#' tk_make_date_sequence(
#'     "2011-01-01", length_out = 5,
#'     skip_values   = "2011-01-05",
#'     insert_values = "2011-01-06"
#' )
#'
#' @name tk_make_date_sequence
NULL

# DATE SEQUENCE ----

#' @rdname tk_make_date_sequence
#' @export
tk_make_date_sequence <- function(start_date, end_date, by, length_out = NULL,
                                   skip_values = NULL, insert_values = NULL) {

    # Condition count for everything except by. If by is missing, will be guessed.
    condition_count <- c(
        !rlang::is_missing(start_date),
        !rlang::is_missing(end_date),
        !is.null(length_out)
    ) %>% sum()

    # Check at least 2 important conditions being supplied
    if (condition_count < 2) {
        rlang::abort("Must specify at least 2 of start_date, end_date, by, and length_out")
    }

    # Start with character data
    if (!rlang::is_missing(start_date)) {
        start_date <- as.character(start_date)
    }
    if (!rlang::is_missing(end_date)) {
        end_date <- as.character(end_date)
    }

    # Determine if sequence_type is date or datetime. Returns parser selection.
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


    # Apply parser
    if (parser == "datetime") {
        # Sub-daily
        if (!rlang::is_missing(start_date)) {
            tryCatch({
                start_date <- readr::parse_datetime(start_date)
            }, warning = function(w) {
                rlang::abort("Cannot parse start_date specification.")
            })
        }

        if (!rlang::is_missing(end_date)) {
            tryCatch({
                end_date <- readr::parse_datetime(end_date)
            }, warning = function(w) {
                rlang::abort("Cannot parse end_date specification.")
            })
        }

        if (rlang::is_missing(by)) {
            # If length_out is not supplied
            if (condition_count < 3) {
                by <- "sec"
                message("Using by: sec")
            }
        }

        # Use seq.POSIXt to create the sequence
        seq <- seq.POSIXt(
            from = start_date,
            to   = end_date,
            by   = by,
            length.out = length_out
        )

        # Remove skip / insert values
        seq <- add_subtract_sequence(seq, skip_values, insert_values)

    } else {
        # Daily

        if (!rlang::is_missing(start_date)) {
            tryCatch({
                start_date <- readr::parse_date(start_date)
            }, warning = function(w) {
                rlang::abort("Cannot parse start_date specification.")
            })
        }
        if (!rlang::is_missing(end_date)) {
            tryCatch({
                end_date <- readr::parse_date(end_date)
            }, warning = function(w) {
                rlang::abort("Cannot parse end_date specification.")
            })
        }
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

    }

    return(seq)

}

