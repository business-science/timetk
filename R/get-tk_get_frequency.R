#' Automatic frequency and trend calculation from a time series index
#'
#' @param idx A date or datetime index.
#' @param period Either "auto", a time-based definition (e.g. "2 weeks"),
#' or a numeric number of observations per frequency (e.g. 10).
#' @param message A boolean. If `message = TRUE`, the frequency or trend is output
#' as a message along with the units in the scale of the data.
#'
#' @return Returns a scalar numeric value indicating the number of observations in the frequency or trend span.
#'
#' @details
#' A _frequency_ is loosely defined as the number of observations that comprise a cycle
#' in a data set. The _trend_ is loosely defined as time span that can
#' be aggregated across to visualize the central tendency of the data.
#' It's often easiest to think of frequency and trend in terms of the time-based units
#' that the data is already in. __This is what `tk_get_frequency()` and `time_trend()`
#' enable: using time-based periods to define the frequency or trend.__
#'
#' __Frequency__:
#'
#' As an example, a weekly cycle is often 5-days (for working
#' days) or 7-days (for calendar days). Rather than specify a frequency of 5 or 7,
#' the user can specify `period = "1 week"`, and
#' `tk_get_frequency()` will detect the scale of the time series and return 5 or 7
#' based on the actual data.
#'
#' The `period` argument has three basic options for returning a frequency.
#' Options include:
#' - `"auto"`: A target frequency is determined using a pre-defined template (see `template` below).
#' - `time-based duration`: (e.g. "1 week" or "2 quarters" per cycle)
#' - `numeric number of observations`: (e.g. 5 for 5 observations per cycle)
#'
#' When `period = "auto"`, the `tk_time_scale_template()` is used to calculate the frequency.
#'
#' __Trend__:
#'
#' As an example, the trend of daily data is often best aggregated by evaluating
#' the moving average over a quarter or a month span. Rather than specify the number
#' of days in a quarter or month, the user can specify "1 quarter" or "1 month",
#' and the `time_trend()` function will return the correct number of observations
#' per trend cycle. In addition, there is an option, `period = "auto"`, to
#' auto-detect an appropriate trend span depending on the data. The `template`
#' is used to define the appropriate trend span.
#'
#' __Time Scale Template__
#'
#' The `tk_time_scale_template()` is a Look-Up Table used by the trend and period to find the
#' appropriate time scale. It contains three features: `time_scale`, `frequency`, and `trend`.
#'
#' The algorithm will inspect
#' the scale of the time series and select the best frequency or trend that matches the scale and
#' number of observations per target frequency. A frequency is then chosen on be the
#' best match.
#'
#' The predefined template is stored in a function `tk_time_scale_template()`.
#' You can modify the template with `set_tk_time_scale_template()`.
#'
#' @seealso
#' - Time Scale Template Modifiers: [get_tk_time_scale_template()], [set_tk_time_scale_template()]
#'
#' @examples
#'
#' library(dplyr)
#' library(timetk)
#'
#' idx_FB <- FANG %>%
#'     filter(symbol == "FB") %>%
#'     pull(date)
#'
#' # Automated Frequency Calculation
#' tk_get_frequency(idx_FB, period = "auto")
#'
#' # Automated Trend Calculation
#' tk_get_trend(idx_FB, period = "auto")
#'
#' # Manually Override Trend
#' tk_get_trend(idx_FB, period = "1 year")
#'
#' @name tk_get_frequency
#' @export
NULL


#' @export
#' @rdname tk_get_frequency
tk_get_frequency <- function(idx, period = "auto", message = TRUE) {

    if (length(idx) <= 1) rlang::abort("Cannot calculate frequency with less than 2 observations.")

    check_weeks(period)

    # Setup inputs
    template <- get_tk_time_scale_template()

    # Get timeseries summary attributes
    ts_summary <- tk_get_timeseries_summary(idx)
    ts_nobs    <- ts_summary$n.obs
    ts_scale   <- ts_summary$scale

    # Frequency Calculation
    if (is.numeric(period)) {
        # 1. Numeric Periods
        freq <- period
        freq_string <- stringr::str_glue("frequency = {freq} observations")

    } else if (tolower(period) != "auto") {
        # 2. Text (e.g. period = "2 Weeks")
        freq <- get_period_statistic(idx, period = period, fn = stats::median)
        freq_string <- frequency_message(freq, period)

    } else {
        # 3. period = "auto"
        periodicity_target <- template %>%
            lookup_time_scale(time_scale = ts_scale, lookup_val = "frequency", index_offset = 0)

        freq <- get_period_statistic(idx, period = periodicity_target, fn = stats::median)
        freq_string <- frequency_message(freq, periodicity_target)



        # Insufficient observations: nobs-to-freq should be at least 3-1
        if (ts_nobs < 3*freq) {
            periodicity_target <- template %>%
                lookup_time_scale(
                    time_scale   = ts_scale,
                    lookup_val   = "frequency",
                    index_offset = 1
                )

            freq <- get_period_statistic(idx, period = periodicity_target, fn = stats::median)
            freq_string <- frequency_message(freq, periodicity_target)

        }

        if (ts_nobs < 3*freq) {
            freq <- 1
            freq_string <- "frequency = 1 (Number of observations insufficient for higher frequencies."
        }
    }

    if (message) {
        # freq_string <- stringr::str_glue("frequency = {freq} {ts_scale}s")
        message(freq_string)
    }

    return(freq)
}

#' @export
#' @rdname tk_get_frequency
tk_get_trend <- function(idx, period = "auto", message = TRUE) {

    if (length(idx) <= 1) rlang::abort("Cannot calculate trend with less than 2 observations.")

    # Setup inputs
    template <- get_tk_time_scale_template()

    # Get timeseries summary attributes
    ts_summary <- tk_get_timeseries_summary(idx)
    ts_start   <- ts_summary$start
    ts_nobs    <- ts_summary$n.obs
    ts_scale   <- ts_summary$scale

    # Trend Calculation
    if (is.numeric(period)) {
        # 1. Numeric Periods
        trend <- period
        trend_string <- stringr::str_glue("trend = {trend} observations")

    } else if (tolower(period) != "auto") {
        # 2. Text (e.g. period = "2 Weeks")
        # trend <- get_period_statistic(idx, period = period, fn = max) - 1

        date_1 <- ts_start                 # Min
        date_2 <- offset_date(date_1, period)
        # date_2 <- add_time(date_1, period) # Max - Don't use until lubridate::period accepts quarter

        trend  <- idx %>% between_time(date_1, date_2) %>% sum()
        trend  <- trend - 1

        trend_string <- frequency_message(trend, period, type = "trend")

    } else {
        # 3. period = "auto"
        periodicity_target <- template %>%
            lookup_time_scale(
                time_scale   = ts_scale,
                lookup_val   = "trend",
                index_offset = 0
            )

        trend <- get_period_statistic(idx, period = periodicity_target, fn = max)
        trend_string <- frequency_message(trend, periodicity_target, type = "trend")
        # trend <- ceiling(trend)

        # Insufficient observations: nobs-to-trend should be at least 2-1
        if (ts_nobs / trend < 2) {
            periodicity_target <- template %>%
                lookup_time_scale(
                    time_scale   = ts_scale,
                    lookup_val   = "trend",
                    index_offset = 1
                )

            trend <- get_period_statistic(idx, period = periodicity_target, fn = max)
            trend <- ceiling(trend)
            trend_string <- frequency_message(trend, periodicity_target, type = "trend")
        }

        if (ts_nobs / trend < 2) {
            trend <- ts_nobs
            trend_string <- stringr::str_glue("trend = {ts_nobs} (Number of observations insufficient for shorter trend cycles.")
        }
    }

    if (message) {
        # trend_string <- stringr::str_glue("trend = {trend} {ts_scale}s")
        message(trend_string)
    }

    return(trend)
}

# Utils -----

offset_date <- function(idx, by) {

    idx %>%
        purrr::map(.f = function(date) {
            tk_make_timeseries(start_date = date, by = by, length_out = 2)[2]
        }) %>%
        purrr::reduce(.f = c)

}


# Helper to get the median observations that fall within a time period
get_period_statistic <- function(idx, period = "1 day", fn = stats::median) {

    if (!is_date_class(idx)) rlang::abort("idx must be date or date-time class.")
    if (!is.character(period)) rlang::abort("period must be character data.")

    data <- tibble::tibble(index = idx)

    ret <- data %>%
        dplyr::mutate(index = lubridate::floor_date(index, unit = period)) %>%
        dplyr::count(index) %>%
        dplyr::pull(n) %>%
        fn(na.rm = T)

    return(ret)

}

# Helper function to get the time decomposition scale
lookup_time_scale <- function(template, time_scale, lookup_val = c("frequency", "trend"), index_offset = 0) {

    lookup_val_expr <-  rlang::sym(lookup_val[[1]])

    idx <- which(template$time_scale == time_scale) - index_offset
    key_value <- template$time_scale[idx]

    template %>%
        dplyr::filter(time_scale == key_value) %>%
        dplyr::pull(!! lookup_val_expr)
}


frequency_message <- function(freq, period, type = "frequency") {

    period_parsed <- parse_period(period)
    freq_string <- stringr::str_glue("{type} = {freq} observations per {period_parsed$freq} {period_parsed$period}")
    if (period_parsed$freq > 1) freq_string <- paste0(freq_string, "s")

    freq_string

}



check_weeks <- function(period) {
    if (tolower(period) %>% stringr::str_detect("week")) {

        period_parsed <- parse_period(period)

        if (period_parsed$freq > 1) rlang::abort("Multiple `weeks` detected. Try using `7 day` increments instead.")


    }
}
