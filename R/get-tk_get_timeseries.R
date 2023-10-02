#' Get date features from a time-series index
#'
#' @param idx A time-series index that is a vector of dates or datetimes.
#'
#' @return Returns a `tibble` object describing the timeseries.
#'
#' @details
#' `tk_get_timeseries_signature` decomposes the timeseries into commonly
#' needed features such as
#' numeric value, differences,
#' year, month, day, day of week, day of month,
#' day of year, hour, minute, second.
#'
#' `tk_get_timeseries_summary` returns the summary returns the
#' start, end, units, scale, and a "summary" of the timeseries differences
#' in seconds including
#' the minimum, 1st quartile, median, mean, 3rd quartile, and maximum frequency.
#' The timeseries
#' differences give the user a better picture of the index frequency
#' so the user can understand the level of regularity or irregularity.
#' A perfectly regular time series will have equal values in seconds for each metric.
#' However, this is not often the case.
#'
#' __Important Note__: These functions only work with time-based indexes in
#' datetime, date, yearmon, and yearqtr values. Regularized dates cannot be decomposed.
#'
#' @seealso [tk_index()], [tk_augment_timeseries_signature()], [tk_make_future_timeseries()]
#'
#' @examples
#' library(dplyr)
#' library(timetk)
#' library(lubridate)
#' library(zoo)
#'
#' # Works with time-based tibbles
#' FB_tbl <- FANG %>% filter(symbol == "FB")
#' FB_idx <- tk_index(FB_tbl)
#'
#' tk_get_timeseries_signature(FB_idx)
#' tk_get_timeseries_summary(FB_idx)
#'
#'
#' # Works with dates in any periodicity
#' idx_weekly <- seq.Date(from = ymd("2016-01-01"), by = 'week', length.out = 6)
#'
#' tk_get_timeseries_signature(idx_weekly)
#' tk_get_timeseries_summary(idx_weekly)
#'
#'
#' # Works with zoo yearmon and yearqtr classes
#' idx_yearmon <- seq.Date(from       = ymd("2016-01-01"),
#'                         by         = "month",
#'                         length.out = 12) %>%
#'     zoo::as.yearmon()
#'
#' tk_get_timeseries_signature(idx_yearmon)
#' tk_get_timeseries_summary(idx_yearmon)
#'
#' @name tk_get_timeseries
NULL

#' @export
#' @rdname tk_get_timeseries
tk_get_timeseries_signature <- function(idx) {
    UseMethod("tk_get_timeseries_signature", idx)
}

#' @export
tk_get_timeseries_signature.POSIXt <- function(idx) {
    get_timeseries_signature_date(idx)
}

#' @export
tk_get_timeseries_signature.Date <- function(idx) {
    get_timeseries_signature_date(idx)
}

#' @export
tk_get_timeseries_signature.yearmon <- function(idx) {
    get_timeseries_signature_date(idx)
}

#' @export
tk_get_timeseries_signature.yearqtr <- function(idx) {
    get_timeseries_signature_date(idx)
}

#' @export
tk_get_timeseries_signature.numeric <- function(idx) {
    stop("Index must be a non-numeric time-based class.")
}

#' @export
tk_get_timeseries_signature.default <- function(idx) {
    stop(paste0("No method for class ", class(idx)[[1]], "."))
}

get_timeseries_signature_date <- function(idx) {
    ret <- tibble::tibble(index = idx) %>%
        dplyr::mutate(
            index.num = as.numeric(as.POSIXct(index)) %>% as.double(),
            diff      = c(NA, diff(index.num)) %>% as.double(),
            year      = lubridate::year(index) %>% as.integer(),
            year.iso  = lubridate::isoyear(index) %>% as.integer(),
            half      = lubridate::semester(index) %>% as.integer(),
            quarter   = lubridate::quarter(index) %>% as.integer(),
            month     = lubridate::month(index) %>% as.integer(),
            month.xts = as.integer(lubridate::month(index)) - 1L,
            month.lbl = lubridate::month(index, label = TRUE, abbr = FALSE),
            day       = lubridate::day(index) %>% as.integer(),
            hour      = lubridate::hour(index) %>% as.integer(),
            minute    = lubridate::minute(index) %>% as.integer(),
            second    = lubridate::second(index) %>% as.integer(),
            hour12    = stringi::stri_datetime_fields(lubridate::as_datetime(index))$Hour12,
            am.pm     = stringi::stri_datetime_fields(lubridate::as_datetime(index))$AmPm,
            wday      = lubridate::wday(index) %>% as.integer(),
            wday.xts  = as.integer(lubridate::wday(index)) - 1L,
            wday.lbl  = lubridate::wday(index, label = TRUE, abbr = FALSE),
            mday      = lubridate::mday(index) %>% as.integer(),
            qday      = lubridate::qday(lubridate::as_date(index)) %>% as.integer(),
            yday      = lubridate::yday(index) %>% as.integer(),
            mweek     = stringi::stri_datetime_fields(lubridate::as_date(index))$WeekOfMonth %>% as.integer(),
            week      = lubridate::week(index) %>% as.integer(),
            week.iso  = lubridate::isoweek(index) %>% as.integer(),
            week2     = as.integer(week %% 2),
            week3     = as.integer(week %% 3),
            week4     = as.integer(week %% 4),
            mday7     = as.integer(mday %/% 7) + 1L
        )
    return(ret)
}





#' @export
#' @rdname tk_get_timeseries
tk_get_timeseries_summary <- function(idx) {
    UseMethod("tk_get_timeseries_summary", idx)
}

#' @export
tk_get_timeseries_summary.POSIXt <- function(idx) {
    get_timeseries_summary_date(idx)
}

#' @export
tk_get_timeseries_summary.Date <- function(idx) {
    get_timeseries_summary_date(idx)
}

#' @export
tk_get_timeseries_summary.yearmon <- function(idx) {
    get_timeseries_summary_date(idx)
}

#' @export
tk_get_timeseries_summary.yearqtr <- function(idx) {
    get_timeseries_summary_date(idx)
}

#' @export
tk_get_timeseries_summary.numeric <- function(idx) {
    stop("Index must be a non-numeric time-based class.")
}

#' @export
tk_get_timeseries_summary.default <- function(idx) {
    stop(paste0("No method for class ", class(idx)[[1]], "."))
}

get_timeseries_summary_date <- function(idx) {



    idx_numeric <- as.numeric(as.POSIXct(idx))
    idx_diff <- diff(idx_numeric)
    idx_diff_summary <- idx_diff %>%
        summary() %>%
        as.list() %>%
        purrr::set_names(
            c("diff.minimum", "diff.q1", "diff.median", "diff.mean", "diff.q3", "diff.maximum")
        ) %>%
        purrr::map_df(~ .x)

    suppressWarnings(idx_periodicity <- xts::periodicity(idx))

    idx_period_summary <- tibble::tibble(
        start              = idx_periodicity$start,
        end                = idx_periodicity$end,
        units              = idx_periodicity$units,
        scale              = idx_periodicity$label
        # label              = idx_periodicity$label
    )

    idx_nobs_summary <- tibble::tibble(
        n.obs      = length(idx)
    )

    idx_tzone_summary <- tibble::tibble(
        tzone      = lubridate::tz(idx)
    )

    idx_summary <- dplyr::bind_cols(idx_nobs_summary,
                                    idx_period_summary,
                                    idx_tzone_summary,
                                    idx_diff_summary)

    return(idx_summary)
}


