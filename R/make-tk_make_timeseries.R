#' Intelligent date and date-time sequence creation
#'
#' Improves on the `seq.Date()` and `seq.POSIXt()` functions by simplifying
#' into 1 function `tk_make_timeseries()`. Intelligently handles character dates
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
#'  `start_date`, `end_date`, or `by`. Can be specified as a number or a time-based phrase.
#' @param skip_values A sequence to skip
#' @param insert_values A sequence to insert
#' @param include_endpoints Logical. Whether or not to keep the last value when `length_out` is
#'  a time-based phrase. Default is `TRUE` (keep last value).
#'
#' @details
#'
#' The `tk_make_timeseries()` function handles both date and date-time sequences
#' automatically.
#'
#' - Parses date and date times from character
#' - Intelligently guesses the sequence desired based on arguments provided
#' - Handles spacing intelligently
#' - When both `by` and `length_out` are missing, guesses either second or day sequences
#' - Can skip and insert values if needed.
#'
#' __Start and End Date Specification__
#'
#' Start and end dates can be specified in reduced time-based phrases:
#'
#' - `start_date = "2014"`: Is converted to "2014-01-01" (start of period)
#' - `end_date = "2014"`: Is converted to "2014-12-31" (end of period)
#' - `start_date = "2014-03"`: Is converted to "2014-03-01" (start of period)
#' - `end_date = "2014-03"`: Is converted to "2014-03-31" (end of period)
#'
#' A similar process can be used for date-times.
#'
#' __By: Daily Sequences__
#'
#' Make a daily sequence with `tk_make_timeseries(by)`. Examples:
#'
#'  - Every Day: `by = "day"`
#'  - Every 2-Weeks: `by = "2 weeks"`
#'  - Every 6-months: `by = "6 months"`
#'
#' If missing, will guess `by = "day"`
#'
#' __By: Sub-Daily Sequences__
#'
#' Make a sub-daily sequence with `tk_make_timeseries(by)`. Examples:
#'
#' - Every minute: `by = "min"`
#' - Every 30-seconds: `by = "30 sec"`
#' - Every 2-hours: `by = "2 hours`
#'
#' If missing, will guess `by = "sec"` if the start or end date is a date-time specification.
#'
#' __Length Out__
#'
#' The `length_out` can be specified by number of observation or complex time-based expressions.
#' The following examples are all possible.
#'
#' - `length_out = 12` Creates 12 evenly spaced observations.
#' - `length_out = "12 months"` Adjusts the end date so it falls on the 12th month.
#'
#'
#' __Include Endpoint__
#'
#' Sometimes the last date is not desired.
#' For example, if the user specifies `length_out = 12 months`, the user may want the last value
#' to be the 12th month and not the 13th. Just toggle, `include_endpoint = FALSE` to obtain this
#' behavior.
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
#' # ---- DATE ----
#'
#' # Start + End, Guesses by = "day"
#' tk_make_timeseries("2017-01-01", "2017-12-31")
#' tk_make_timeseries("2017", "2017") # Same result
#'
#' # Start + Length Out, Guesses by = "day"
#' tk_make_timeseries("2012", length_out = 6) # Guesses by = "day"
#'
#' # Start + By + Length Out, Spacing 6 observations by monthly interval
#' tk_make_timeseries("2012-01-01", by = "1 month", length_out = 6)
#'
#' # Start + By + Length Out, Phrase "1 year 6 months"
#' tk_make_timeseries("2012-01-01", by = "1 month",
#'                    length_out = "1 year 6 months", include_endpoints = FALSE)
#'
#' # Going in Reverse, End + Length Out
#' tk_make_timeseries(end_date = "2012-01-01", by = "1 month",
#'                    length_out = "1 year 6 months", include_endpoints = FALSE)
#'
#' # ---- DATE-TIME ----
#'
#' # Start + End, Guesses by second
#' tk_make_timeseries("2016-01-01 01:01:02", "2016-01-01 01:01:04")
#'
#' # Date-Time Sequence - By 10 Minutes
#' # - Converts to date-time automatically & applies 10-min interval
#' tk_make_timeseries("2017-01-01", "2017-01-02", by = "10 min")
#'
#'
#' # --- REMOVE / INCLUDE ENDPOINTS ----
#'
#' # Last value in this case is desired
#' tk_make_timeseries("2017-01-01", by = "30 min", length_out = "6 hours")
#'
#' # Last value in monthly case is not wanted
#' tk_make_timeseries("2012-01-01", by = "1 month",
#'                    length_out = "12 months",
#'                    include_endpoints = FALSE) # Removes unnecessary last value
#'
#'
#' # ---- SKIP & INSERT VALUES ----
#'
#' tk_make_timeseries(
#'     "2011-01-01", length_out = 5,
#'     skip_values   = "2011-01-05",
#'     insert_values = "2011-01-06"
#' )
#'
#' @name tk_make_timeseries
NULL

# DATE SEQUENCE ----

#' @rdname tk_make_timeseries
#' @export
tk_make_timeseries <- function(start_date, end_date, by, length_out = NULL,
                               include_endpoints = TRUE,
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

    # Clean inputs
    # if (!rlang::is_missing(by)) {
    #     by <- by %>% stringr::str_trim()
    # }
    # if (!rlang::is_missing(length_out)) {
    #     length_out <- length_out %>% stringr::str_trim()
    # }

    # PARSER SELECTION ----

    # Determine if sequence_type is date or datetime. Returns parser selection.
    parser <- NULL
    if (!rlang::is_missing(by)) {
        if (stringr::str_detect(tolower(by), pattern = "(sec)|(min)|(hour)")) {
            parser <- "datetime"
        }
    }

    # Determine if datetime class, conversion to character can destroy 00:00:00
    if (is.null(parser)) {
        if (!rlang::is_missing(start_date)) {
            if (inherits(start_date, "POSIXt")) parser <- "datetime"
        } else if (!rlang::is_missing(end_date)) {
            if (inherits(end_date, "POSIXt")) parser <- "datetime"
        }
    }

    # CONVERT TO CHARACTER ----
    # Needed for readr::parse_ functions
    if (!rlang::is_missing(start_date)) {
        start_date <- as.character(start_date)
    }
    if (!rlang::is_missing(end_date)) {
        end_date <- as.character(end_date)
    }

    # If datetime was not detected in by, move to start_date / end_date
    if (is.null(parser)) {
        if (!rlang::is_missing(start_date)) {
            parser <- readr::guess_parser(start_date)
        } else if (!rlang::is_missing(end_date)) {
            parser <- readr::guess_parser(end_date)
        }
    }


    # APPLY PARSERS ----

    # Apply parser
    if (parser == "datetime") {
        # Sub-daily ----

        if (!rlang::is_missing(start_date)) {
            start_date <- try_parse_date_time(start_date) %>%
                lubridate::as_datetime()
        }

        if (!rlang::is_missing(end_date)) {
            end_date <- try_parse_date_time(end_date, side = "rhs") %>%
                lubridate::as_datetime()
        }

        if (rlang::is_missing(by)) {
            # If length_out is not supplied
            if (condition_count < 3) {
                by <- "sec"
                message("Using by: sec")
            }
        }

        # Handle character length_out
        drop_end_1   <- FALSE
        drop_begin_1 <- FALSE
        if (!rlang::is_missing(length_out)) {
            if (is.character(length_out)) {
                if (!rlang::is_missing(start_date)) {
                    end_date <- start_date %+time% length_out
                    length_out <- NULL
                    drop_end_1 <- TRUE
                    if (is.na(end_date)) rlang::abort("length_out returned an invalid end_date, and may not exist. Try a new length_out.")
                } else if (!rlang::is_missing(end_date)) {
                    start_date   <- end_date %-time% length_out
                    length_out   <- NULL
                    drop_begin_1 <- TRUE
                    if (is.na(start_date)) rlang::abort("length_out returned an invalid start_date, and may not exist. Try a new length_out.")
                } else {
                    rlang::abort("Please specify a start_date or end_date")
                }
            }
        }

        # Use seq.POSIXt to create the sequence
        seq <- seq.POSIXt(
            from = start_date,
            to   = end_date,
            by   = by,
            length.out = length_out
        )

        # Drop last value if length_out is character
        # - This happens because the end_date is 1 period longer than the desired length out
        if (!rlang::is_missing(length_out)) {
            if (is.character(length_out)) {
                seq <- seq[1:(length(seq)-1)]
            }
        }

        # Drop last value if length_out is character
        # - This happens because the end_date is 1 period longer than the desired length out
        if (drop_begin_1 && !include_endpoints) {
            seq <- seq[-1]
        }
        if (drop_end_1 && !include_endpoints) {
            seq <- seq[-length(seq)]
        }

        # Remove skip / insert values
        seq <- add_subtract_sequence(seq, skip_values, insert_values)

    } else {
        # Daily ----

        if (!rlang::is_missing(start_date)) {
            start_date <- try_parse_date_time(start_date)
        }
        if (!rlang::is_missing(end_date)) {
            end_date <- try_parse_date_time(end_date, side = "rhs")
        }
        if (rlang::is_missing(by)) {
            condition_count <- rlang::is_missing(start_date) + rlang::is_missing(end_date) + is.null(length_out)
            if (condition_count < 3) {
                by <- "day"
                message("Using by: day")
            }
        }

        # Handle character length_out
        drop_end_1   <- FALSE
        drop_begin_1 <- FALSE
        if (!rlang::is_missing(length_out)) {
            if (is.character(length_out)) {
                if (!rlang::is_missing(start_date)) {
                    end_date <- start_date %+time% length_out
                    length_out <- NULL
                    drop_end_1 <- TRUE
                    if (is.na(end_date)) rlang::abort("length_out returned an invalid end_date, and may not exist. Try a new length_out.")
                    if (inherits(end_date, "datetime")) {
                        warning("Check your length_out. Expecting only date information. Removing time information (hours, mintues, seconds).")
                        start_date <- lubridate::as_date(end_date)
                    }

                } else if (!rlang::is_missing(end_date)) {
                    start_date <- end_date %-time% length_out
                    length_out <- NULL
                    drop_begin_1 <- TRUE
                    if (is.na(start_date)) rlang::abort("length_out returned an invalid start_date, and may not exist. Try a new length_out.")
                    if (inherits(start_date, "datetime")) {
                        warning("Check your length_out. Expecting only date information. Removing time information (hours, mintues, seconds).")
                        start_date <- lubridate::as_date(start_date)
                    }
                } else {
                    rlang::abort("Please specify a start_date or end_date")
                }
            }
        }

        # Create Sequence
        seq <- seq.Date(
            from = start_date,
            to   = end_date,
            by   = by,
            length.out = length_out
        )

        # Drop last value if length_out is character
        # - This happens because the end_date is 1 period longer than the desired length out
        if (drop_begin_1 && !include_endpoints) {
            seq <- seq[-1]
        }
        if (drop_end_1 && !include_endpoints) {
            seq <- seq[-length(seq)]
        }

        seq <- add_subtract_sequence(seq, skip_values, insert_values)

    }

    return(seq)

}

