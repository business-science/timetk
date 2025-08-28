## List to Date Time -------------------------------------------------------------------------
## list_to_datetime()

# Collapse the list of period values into a real datetime class
list_to_datetime <- function(index, tf_side, ...) {
    UseMethod("list_to_datetime")
}

#' @keywords internal
#' @export
#' @method list_to_datetime POSIXct
list_to_datetime.POSIXct <- function(index, tf_side, tz, ...) {
    lubridate::make_datetime(tf_side$y, tf_side$m, tf_side$d,
                             tf_side$h, tf_side$M, tf_side$s, tz = tz)
}

#' @keywords internal
#' @export
#' @method list_to_datetime Date
list_to_datetime.Date <- function(index, tf_side, ...) {
    lubridate::make_date(tf_side$y, tf_side$m, tf_side$d)
}

#' @keywords internal
#' @export
#' @method list_to_datetime yearmon
list_to_datetime.yearmon <- function(index, tf_side, ...) {
    tf_side$d <- 1
    zoo::as.yearmon(list_to_datetime.Date(index, tf_side))
}

#' @keywords internal
#' @export
#' @method list_to_datetime yearqtr
list_to_datetime.yearqtr <- function(index, tf_side, ...) {
    yearqtr_string <- paste0(tf_side$y, "-", tf_side$q)
    zoo::as.yearqtr(yearqtr_string)
}

#' @keywords internal
#' @export
#' @method list_to_datetime hms
list_to_datetime.hms <- function(index, tf_side, ...) {
    hms::hms(seconds = tf_side$s, minutes = tf_side$M, hours = tf_side$h)
}

#### parse_date_time_char ----
try_parse_date_time <- function(index, side = "lhs") {
    tryCatch({
        # Use readr parse_guess
        index <- try_parse_date_time_readr(index, side = side)
    }, warning = function(w) {
        # Try date-time parsing
        index <- try_parse_date_time_char(index, side = side)
    })

    return(index)
}

try_parse_date_time_readr <- function(index, side = "lhs") {
    index <- readr::parse_guess(index)
    if (is.numeric(index) | is.character(index)) {
        index <- try_parse_date_time_char(as.character(index), side = side)
    }
    return(index)
}

try_parse_date_time_char <- function(index, side = "lhs") {
    tryCatch({
        index <- index %>%
            purrr::map(parse_date_time_char, side = side) %>%
            purrr::reduce(c)
    }, error = function(e) {
        rlang::abort("Index could not be parsed. Try entering in 'YYYY-MM-DD' or 'YYYY-MM-DD HH:MM:SS' format.")
    })
    return(index)
}

parse_date_time_char <- function(x, side = "lhs") {

    time_list <- split_to_list(x)

    if (length(time_list) > 3) {
        # Date time
        dt <- lubridate::ymd_hms("1970-01-01 00:00:00")
        l  <- add_time_defaults(dt, time_list, side = side)
        ret <- list_to_datetime(dt, l, tz = "UTC")

    } else {
        # Date
        dt <- lubridate::ymd("1970-01-01")
        l  <- add_time_defaults(dt, time_list, side = side)
        ret <- list_to_datetime(dt, l, tz = "UTC")
    }

    return(ret)
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
#' @keywords internal
#' @export
lookup_defaults <- function(index, side = "lhs") {
    UseMethod("lookup_defaults")
}

#' @keywords internal
#' @export
#' @method lookup_defaults POSIXct
lookup_defaults.POSIXct <- function(index, side = "lhs") {
    switch(side,
           "lhs" = list(y = 1970, m = 01, d = 01, h = 00, M = 00, s = 00),
           "rhs" = list(y = 1970, m = 12, d = 00, h = 23, M = 59, s = 59))
}

#' @keywords internal
#' @export
#' @method lookup_defaults Date
lookup_defaults.Date <- function(index, side = "lhs") {
    switch(side,
           "lhs" = list(y = 1970, m = 01, d = 01),
           "rhs" = list(y = 1970, m = 12, d = 00))
}

#' @keywords internal
#' @export
#' @method lookup_defaults yearmon
lookup_defaults.yearmon <- function(index, side = "lhs") {
    switch(side,
           "lhs" = list(y = 1970, m = 01),
           "rhs" = list(y = 1970, m = 12))
}

#' @keywords internal
#' @export
#' @method lookup_defaults yearqtr
lookup_defaults.yearqtr <- function(index, side = "lhs") {
    switch(side,
           "lhs" = list(y = 1970, q = 01),
           "rhs" = list(y = 1970, q = 04))
}

#' @keywords internal
#' @export
#' @method lookup_defaults hms
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
