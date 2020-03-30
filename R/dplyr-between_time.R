#' Flexible range detection for date or date-time sequences
#'
#' The easiest way to filter time series date or date-time vectors. Returns a
#' logical
#' See [filter_by_time()]
#' for the `data.frame` (`tibble`) implementation.
#'
#' @param .index A date or date-time vector.
#' @param .start_date The starting date
#' @param .end_date The ending date
#'
#' @return A `logical` vector the same length as `.index` indicating whether or not
#' the timestamp value was within the `start_date` and `.end_date` range.
#'
#' @details
#'
#' __Pure Time Series Filtering Flexibilty__
#'
#' The `.start_date`  and `.end_date` parameters are designed with flexibility in mind.
#'
#' Each side of the `time_formula` is specified as the character
#' `'YYYY-MM-DD HH:MM:SS'`, but powerful shorthand is available.
#' Some examples are:
#' * __Year:__ `.start_date = '2013', .end_date = '2015'`
#' * __Month:__ `.start_date = '2013-01', .end_date = '2016-06'`
#' * __Day:__ `.start_date = '2013-01-05', .end_date = '2016-06-04'`
#' * __Second:__ `.start_date = '2013-01-05 10:22:15', .end_date = '2018-06-03 12:14:22'`
#' * __Variations:__ `.start_date = '2013', .end_date = '2016-06'`
#'
#' __Key Words: "start" and "end"__
#'
#' Use the keywords "start" and "end" as shorthand, instead of specifying the
#' actual start and end values. Here are some examples:
#'
#' * __Start of the series to end of 2015:__ `.start_date = 'start', .end_date = '2015'`
#' * __Start of 2014 to end of series:__ `.start_date = '2014', .end_date = 'end'`
#'
#' __Internal Calculations__
#'
#' All shorthand dates are expanded:
#' * The `.start_date` is expanded to be the _first date_ in that period
#' * The `.end_date` side is expanded to be the _last date_ in that period
#'
#' This means that the following examples are equivalent (assuming your
#' index is a POSIXct):
#' * `.start_date = '2015'` is equivalent to `.start_date = '2015-01-01 + 00:00:00' `
#' * `.end_date = '2016'` is equivalent to `2016-12-31 + 23:59:59'`
#'
#' @seealso
#' - [filter_by_time()] - A time-based variant of `dplyr::filter()` that is powered by
#' `between_time()`
#' - `between_time()` is a vectorized function based on `tibbletime::filter_time()`
#'
#'
#' @examples
#' library(tidyverse)
#' library(tidyquant)
#' library(timetk)
#'
#' index_daily <- tk_make_date_sequence("2016-01-01", "2017-01-01", by = "day")
#' index_min   <- tk_make_date_sequence("2016-01-01", "2017-01-01", by = "min")
#'
#' # How it works
#' # - Returns TRUE/FALSE length of .index
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
between_time <- function(.index, .start_date = "start", .end_date = "end") {
    UseMethod("between_time", .index)
}

#' @export
between_time.default <- function(.index, .start_date = "start", .end_date = "end") {
    stop(call. = FALSE, paste0("`between_time(.index)` has no method for class ", class(.index)[[1]]))
}

#' @export
between_time.POSIXct <- function(.index, .start_date = "start", .end_date = "end") {
    between_time_vec(.index, .start_date, .end_date)
}

#' @export
between_time.Date <- function(.index, .start_date = "start", .end_date = "end") {
    between_time_vec(.index, .start_date, .end_date)
}

#' @export
between_time.yearmon <- function(.index, .start_date = "start", .end_date = "end") {
    between_time_vec(.index, .start_date, .end_date)
}

#' @export
between_time.yearqtr <- function(.index, .start_date = "start", .end_date = "end") {
    between_time_vec(.index, .start_date, .end_date)
}

#' @export
between_time.hms <- function(.index, .start_date = "start", .end_date = "end") {
    between_time_vec(.index, .start_date, .end_date)
}

between_time_vec <- function(.index, .start_date = "start", .end_date = "end") {

    tz     <- lubridate::tz(.index)
    .start_date <- as.character(.start_date)
    .end_date   <- as.character(.end_date)

    time_formula <- rlang::new_formula(.start_date, .end_date)

    # Parse the time_formula, don't convert to dates yet
    tf_list <- parse_time_formula(.index, time_formula)

    # Convert to datetime
    from_to <- purrr::map(
        .x = tf_list,
        .f = ~list_to_datetime(.index, .x, tz = tz)
    )

    # Get sequence creation pieces ready
    from <- from_to[[1]]
    to   <- from_to[[2]]

    dplyr::between(.index, from, to)

}


## List to Date Time -------------------------------------------------------------------------
## list_to_datetime()

# Collapse the list of period values into a real datetime class
list_to_datetime <- function(index, tf_side, ...) {
    UseMethod("list_to_datetime")
}

list_to_datetime.POSIXct <- function(index, tf_side, tz, ...) {
    lubridate::make_datetime(tf_side$y, tf_side$m, tf_side$d,
                             tf_side$h, tf_side$M, tf_side$s, tz = tz)
}

list_to_datetime.Date <- function(index, tf_side, ...) {
    lubridate::make_date(tf_side$y, tf_side$m, tf_side$d)
}

list_to_datetime.yearmon <- function(index, tf_side, ...) {
    tf_side$d <- 1
    zoo::as.yearmon(list_to_datetime.Date(index, tf_side))
}

list_to_datetime.yearqtr <- function(index, tf_side, ...) {
    yearqtr_string <- paste0(tf_side$y, "-", tf_side$q)
    zoo::as.yearqtr(yearqtr_string)
}

list_to_datetime.hms <- function(index, tf_side, ...) {
    hms::hms(seconds = tf_side$s, minutes = tf_side$M, hours = tf_side$h)
}

#### parse_time_formula -----

parse_time_formula <- function(index, time_formula) {

    # lhs/rhs list
    tf <- list(
        lhs = rlang::f_lhs(time_formula),
        rhs = rlang::f_rhs(time_formula)
    )

    # Environment to evaluate the sides in
    tf_env <- rlang::f_env(time_formula)

    # Tidy evaluation
    tf <- lapply(tf, function(x) {
        eval(x, envir = tf_env)
    })

    # Double up if 1 sided
    # length = 2 means that it has ~ and 1 side
    if(length(time_formula) == 2) {
        tf$lhs <- tf$rhs
    }

    tf <- lapply(tf, FUN = function(x) keyword_parse(index, x))

    # Split the input
    tf <- lapply(tf, split_to_list)

    # Add default times
    # map2 is a bit slow here
    tf_final      <- list(NA, NA)
    tf_final[[1]] <- add_time_defaults(index, tf[[1]], "lhs")
    tf_final[[2]] <- add_time_defaults(index, tf[[2]], "rhs")

    tf_final
}

# Adds default times to fill out the sides of the time formula
add_time_defaults <- function(index, tf_side, side = "lhs") {

    # Lookup specific index class defaults
    defaults <- lookup_defaults(index, side)

    # Check length
    if(length(tf_side) > length(defaults)) {
        index_class <- class(index)[[1]]
        default_names <- paste(names(defaults), collapse = ", ")
        stop(paste0("For a ", index_class, " index, time_formula can only include ",
                    default_names, " specifications."), call. = FALSE)
    }

    # Overwrite defaults where necessary
    for(i in seq_along(tf_side)) {
        defaults[[i]] <- tf_side[[i]]
    }

    # Handle end of month
    if(!is.null(defaults$d)) { # If this passes it was Date/POSIX
        if(defaults$d == 0) {
            # Fake a date to find the number of days in that month
            fake_date <- lubridate::make_date(defaults$y, defaults$m, 1)
            defaults$d <- lubridate::days_in_month(fake_date)
        }
    }

    defaults
}

## Keyword parsing -----

keyword_parse <- function(index, side) {

    # Dummy index
    if(length(index) == 0) {
        return(side)
    }

    if(as.character(side) == "start") {
        dplyr::first(index)
    } else if (as.character(side) == "end") {
        dplyr::last(index)
    } else {
        side
    }

}

## split_to_list ----
split_to_list <- function(x) {
    UseMethod("split_to_list", x)
}

#' @export
split_to_list.default <- function(x) {
    stop("Unrecognized time formula input")
}

#' @export
split_to_list.Date <- function(x) {
    x_lt <- as.POSIXlt(x, tz = get_default_time_zone())
    list(x_lt$year + 1900, x_lt$mon + 1, x_lt$mday)
}

#' @export
split_to_list.POSIXct <- function(x) {
    x_lt <- as.POSIXlt(x, tz = get_index_col_time_zone(x))
    list(x_lt$year + 1900, x_lt$mon + 1, x_lt$mday,
         x_lt$hour,        x_lt$min, x_lt$sec)
}

#' @export
split_to_list.yearmon <- function(x) {
    x_lt <- as.POSIXlt(x, tz = get_default_time_zone())
    list(x_lt$year + 1900, x_lt$mon + 1)
}

#' @export
split_to_list.yearqtr <- function(x) {
    x_lt <- as.POSIXlt(x, tz = get_default_time_zone())
    list(x_lt$year + 1900, x_lt$mon + 1)
}

#' @export
split_to_list.hms <- function(x) {
    x_lt <- as.POSIXlt(x, tz = get_default_time_zone())
    list(x_lt$hour, x_lt$min, x_lt$sec)
}

#' @export
split_to_list.character <- function(x) {
    # Split on - / , : * + space (notably not .)
    split_str <- unlist(strsplit(x, "-|/|:|[*]|[+]|[,]|[[:space:]]"))

    # Remove the "" that get left
    split_str <- split_str[split_str != ""]

    split_list <- as.list(split_str)

    maybe_to_numeric <- function(x) {
        if(x != ".") {
            x <- suppressWarnings(as.numeric(x))
            if(is.na(x)) {
                stop("Cannot parse time formula specification", call. = FALSE)
            }
        }
        x
    }

    # Attempt to coerce to numeric unless '.'
    split_list <- lapply(
        split_list,
        maybe_to_numeric
    )

    split_list
}

# Lookup Defaults ----
lookup_defaults <- function(index, side = "lhs") {
    UseMethod("lookup_defaults")
}

lookup_defaults.POSIXct <- function(index, side = "lhs") {
    switch(side,
           "lhs" = list(y = 1970, m = 01, d = 01, h = 00, M = 00, s = 00),
           "rhs" = list(y = 1970, m = 12, d = 00, h = 23, M = 59, s = 59))
}

lookup_defaults.Date <- function(index, side = "lhs") {
    switch(side,
           "lhs" = list(y = 1970, m = 01, d = 01),
           "rhs" = list(y = 1970, m = 12, d = 00))
}

lookup_defaults.yearmon <- function(index, side = "lhs") {
    switch(side,
           "lhs" = list(y = 1970, m = 01),
           "rhs" = list(y = 1970, m = 12))
}

lookup_defaults.yearqtr <- function(index, side = "lhs") {
    switch(side,
           "lhs" = list(y = 1970, q = 01),
           "rhs" = list(y = 1970, q = 04))
}

lookup_defaults.hms <- function(index, side = "lhs") {
    switch(side,
           "lhs" = list(h = 00, M = 00, s = 00),
           "rhs" = list(h = 23, M = 59, s = 59))
}

# get_default_time_zone ----
get_default_time_zone <- function() {
    "UTC"
}

# get_index_col_time_zone ----
get_index_col_time_zone <- function(index) {
    if(inherits(index, "POSIXct")) {
        (attr(index, "tzone") %||% Sys.timezone()) %||% get_default_time_zone()
    } else {
        get_default_time_zone()
    }
}
