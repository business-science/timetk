#' Fast, flexible date and datetime parsing
#'
#' Significantly faster time series parsing than `readr::parse_date`, `readr::parse_datetime`,
#' `lubridate::as_date()`, and `lubridate::as_datetime()`. Uses `anytime` package, which relies on
#' `Boost.Date_Time` C++ library for date/datetime parsing.
#'
#' @param x A character vector
#' @param tz Datetime only. A timezone (see `OlsenNames()`).
#' @param tz_shift Datetime only. If FALSE, forces the datetime into the time zone.
#'  If TRUE, offsets the datetime from UTC to the new time zone.
#' @param ... Additional parameters passed to [anytime()] and [`anydate()`]
#' @param silent If `TRUE`, warns the user of parsing failures.
#'
#' @return
#' Returns a `date` or `datatime` vector from the transformation applied to character timestamp vector.
#'
#' @details
#'
#' __Parsing Formats__
#'
#' - Date Formats: Must follow a Year, Month, Day sequence.
#'  (e.g. `parse_date2("2011 June")` is OK, `parse_date2("June 2011")` is NOT OK).
#' - Date Time Formats: Must follow a YMD HMS sequence.
#'
#' Refer to `lubridate::mdy()` for Month, Day, Year and additional formats.
#'
#' __Time zones (Datetime)__
#'
#' Time zones are handled in a similar way to `lubridate::as_datetime()` in that time zones
#' are forced rather than shifted. This is a key difference between `anytime::anytime()`, which
#' shifts datetimes to the specified timezone by default.
#'
#' @references
#' - This function wraps the `anytime::anytime()` and `anytime::anydate()` functions developed by Dirk Eddelbuettel.
#'
#' @examples
#'
#' # Fast date parsing
#' parse_date2("2011")
#' parse_date2("2011 June 3rd")
#'
#' # Fast datetime parsing
#' parse_datetime2("2011")
#' parse_datetime2("2011 Jan 1 12:35:21")
#'
#' # Time Zones (datetime only)
#' parse_datetime2("2011 Jan 1 12:35:21", tz = "Europe/London")
#'
#' @name parse_date2
#' @export
parse_date2 <- function(x, ..., silent = FALSE) {

    ret <- anytime::anydate(x, ...)
    # lubridate::tz(ret) <- tz

    if (!silent) {
        na_count <- sum(is.na(ret))
        if (na_count > 0) {
            if (na_count == 1) {
                warning(call. = FALSE, stringr::str_glue("parse_date2: {na_count} parsing failure returned as NA."))
            } else {
                warning(call. = FALSE, stringr::str_glue("parse_date2: {na_count} parsing failures returned as NA."))
            }
        }
    }

    return(ret)
}

#' @export
#' @rdname parse_date2
parse_datetime2 <- function(x, tz = "UTC", tz_shift = FALSE, ..., silent = FALSE) {

    ret <- anytime::anytime(x, ...)

    if (!tz_shift) {
        lubridate::tz(ret) <- tz
    } else {
        ret <- lubridate::with_tz(ret, tz = tz)
    }

    if (!silent) {
        na_count <- sum(is.na(ret))
        if (na_count > 0) {
            if (na_count == 1) {
                warning(call. = FALSE, stringr::str_glue("parse_datetime2: {na_count} parsing failure returned as NA."))
            } else {
                warning(call. = FALSE, stringr::str_glue("parse_datetime2: {na_count} parsing failures returned as NA."))
            }
        }
    }

    return(ret)
}
