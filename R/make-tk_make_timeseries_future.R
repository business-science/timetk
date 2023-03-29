#' Make future time series from existing
#'
#' @param idx A vector of dates
#' @param length_out Number of future observations. Can be numeric number or a phrase
#'  like "1 year".
#' @param inspect_weekdays Uses a logistic regression algorithm to inspect
#'  whether certain weekdays (e.g. weekends) should be excluded from the future dates.
#'  Default is `FALSE`.
#' @param inspect_months Uses a logistic regression algorithm to inspect
#'  whether certain days of months (e.g. last two weeks of year or seasonal days)
#'  should be excluded from the future dates.
#'  Default is `FALSE`.
#' @param skip_values A vector of same class as `idx` of timeseries
#'  values to skip.
#' @param insert_values A vector of same class as `idx` of timeseries
#'  values to insert.
#' @param n_future (DEPRECATED) Number of future observations. Can be numeric number or a phrase
#'  like "1 year".
#'
#'
#' @details
#'
#' __Future Sequences__
#'
#' `tk_make_future_timeseries` returns a time series based
#' on the input index frequency and attributes.
#'
#' __Specifying Length of Future Observations__
#'
#' The argument `length_out` determines how many future index observations to compute. It can be specified
#' as:
#'
#' - __A numeric value__ - the number of future observations to return.
#'     - The number of observations returned is _always_ equal to the value the user inputs.
#'     - The __end date can vary__ based on the number of timestamps chosen.
#'
#' - __A time-based phrase__ - The duration into the future to include (e.g. "6 months" or "30 minutes").
#'     - The _duration_ defines the _end date_ for observations.
#'     - The __end date will not change__ and those timestamps that fall within the end date will be returned
#'  (e.g. a quarterly time series will return 4 quarters if `length_out = "1 year"`).
#'     - The number of observations will vary to fit within the end date.
#'
#' __Weekday and Month Inspection__
#'
#' The `inspect_weekdays` and `inspect_months` arguments apply to "daily" (scale = "day") data
#' (refer to `tk_get_timeseries_summary()` to get the index scale).
#'
#' - The `inspect_weekdays` argument is useful in determining missing days of the week
#' that occur on a weekly frequency such as every week, every other week, and so on.
#' It's recommended to have at least 60 days to use this option.
#' - The `inspect_months` argument is useful in determining missing days of the month, quarter
#' or year; however, the algorithm can inadvertently select incorrect dates if the pattern
#' is erratic.
#'
#' __Skipping / Inserting Values__
#'
#' The `skip_values` and `insert_values` arguments can be used to remove and add
#' values into the series of future times. The values must be the same format as the `idx` class.
#'
#' - The `skip_values` argument useful for passing holidays or special index values that should
#' be excluded from the future time series.
#' - The `insert_values` argument is useful for adding values back that the algorithm may have
#' excluded.
#'
#'
#'
#' @return A vector containing future index of the same class as the incoming index `idx`
#'
#' @seealso
#' - Making Time Series: [tk_make_timeseries()]
#' - Working with Holidays & Weekends: [tk_make_holiday_sequence()], [tk_make_weekend_sequence()], [tk_make_weekday_sequence()]
#' - Working with Timestamp Index: [tk_index()], [tk_get_timeseries_summary()], [tk_get_timeseries_signature()]
#'
#' @examples
#' library(dplyr)
#' library(timetk)
#'
#' # Basic example - By 3 seconds
#' idx <- tk_make_timeseries("2016-01-01 00:00:00", by = "3 sec", length_out = 3)
#' idx
#'
#' # Make next three timestamps in series
#' idx %>% tk_make_future_timeseries(length_out = 3)
#'
#' # Make next 6 seconds of timestamps from the next timestamp
#' idx %>% tk_make_future_timeseries(length_out = "6 sec")
#'
#'
#' # Basic Example - By 1 Month
#' idx <- tk_make_timeseries("2016-01-01", by = "1 month",
#'                           length_out = "12 months")
#' idx
#'
#' # Make 12 months of timestamps from the next timestamp
#' idx %>% tk_make_future_timeseries(length_out = "12 months")
#'
#'
#'
#' # --- APPLICATION ---
#' # - Combine holiday sequences with future sequences
#'
#' # Create index of days that FB stock will be traded in 2017 based on 2016 + holidays
#' FB_tbl <- FANG %>% filter(symbol == "FB")
#'
#' holidays <- tk_make_holiday_sequence(
#'     start_date = "2017-01-01",
#'     end_date   = "2017-12-31",
#'     calendar   = "NYSE")
#'
#' # Remove holidays with skip_values, and remove weekends with inspect_weekdays = TRUE
#' FB_tbl %>%
#'     tk_index() %>%
#'     tk_make_future_timeseries(length_out       = "1 year",
#'                               inspect_weekdays = TRUE,
#'                               skip_values      = holidays)
#'
#'
#'
#'
#' @name tk_make_future_timeseries
NULL

# FUTURE TIMESERIES ----

#' @export
#' @rdname tk_make_future_timeseries
tk_make_future_timeseries <- function(idx, length_out, inspect_weekdays = FALSE,
                                      inspect_months = FALSE, skip_values = NULL, insert_values = NULL,
                                      n_future = NULL) {

    if (!is.null(n_future)) {
            rlang::warn(stringr::str_glue("ARGUMENT DEPRECATION: `n_future` is deprecated. Please use `length_out` instead.
                                      - `length_out` always returns `length_out` observations
                                      - `n_future` may return fewer than `n_future` when inspect_weekdays/inspect_months and skip/insert values are included."))

    }

    UseMethod("tk_make_future_timeseries", idx)
}

#' @export
tk_make_future_timeseries.POSIXt <- function(idx, length_out, inspect_weekdays = FALSE,
                                             inspect_months = FALSE, skip_values = NULL, insert_values = NULL,
                                             n_future = NULL) {

    # Sort idx
    idx <- sort(idx)

    # Remove once `n_future` is fully deprecated
    if (!is.null(n_future)) {
        if (missing(length_out)) length_out <- n_future
        n_future <- TRUE
    }

    # Checks
    if (missing(length_out)) {
        rlang::abort("Argument `length_out` is missing with no default")
    }

    # Setup
    idx_summary <- tk_get_timeseries_summary(idx)

    # Time zone
    tzone <- lubridate::tz(idx)
    lubridate::tz(idx) <- "UTC"

    # Handle Date formatted as date-time
    if (idx_summary$scale %in% c("day", "week", "month", "quarter", "year")) {
        idx <- lubridate::as_date(idx)
        date_seq <- tk_make_future_timeseries.Date(
            idx               = idx,
            length_out        = length_out,
            inspect_weekdays  = inspect_weekdays,
            inspect_months    = inspect_months,
            skip_values       = skip_values,
            insert_values     = insert_values,
            n_future          = n_future
        )

    } else {
        date_seq <- make_sequential_timeseries_irregular_freq(
            idx               = idx,
            length_out        = length_out,
            skip_values       = skip_values,
            insert_values     = insert_values,
            n_future          = n_future
        )
    }

    # Re-apply time zone
    lubridate::tz(date_seq) <- tzone

    return(date_seq)
}

#' @export
tk_make_future_timeseries.Date <- function(idx, length_out, inspect_weekdays = FALSE,
                                           inspect_months = FALSE, skip_values = NULL, insert_values = NULL,
                                           n_future = NULL) {
    # Sort idx
    idx <- sort(idx)

    # Remove once `n_future` is fully deprecated
    if (!is.null(n_future)) {
        if (missing(length_out)) length_out <- n_future
        n_future <- TRUE
    }

    # Checks
    if (missing(length_out)) {
        rlang::abort("Argument `length_out` is missing with no default")
    }

    # NOTE: Date - Ignores Timezone

    idx_summary <- tk_get_timeseries_summary(idx)

    if (idx_summary$scale == "day" && (inspect_weekdays || inspect_months)) {

        # print("day + inspect_weekdays or inspect_months")

        # Daily scale with weekday and/or month inspection ----
        tryCatch({

            date_seq <- predict_future_timeseries_daily(
                idx               = idx,
                length_out        = length_out,
                inspect_weekdays  = inspect_weekdays,
                inspect_months    = inspect_months,
                skip_values       = skip_values,
                insert_values     = insert_values,
                n_future          = n_future
            )

        }, error = function(e) {

            warning(paste0("Could not perform `glm()`: ", e, "\nMaking sequential timeseries."))
            date_seq <- make_sequential_timeseries_irregular_freq(
                idx               = idx,
                length_out        = length_out,
                skip_values       = skip_values,
                insert_values     = insert_values,
                n_future          = n_future
            )

        })

    } else if (idx_summary$scale == "day") {

        # print("day")

        # Daily scale without weekday inspection -----
        date_seq <- make_sequential_timeseries_irregular_freq(
            idx               = idx,
            length_out        = length_out,
            skip_values       = skip_values,
            insert_values     = insert_values,
            n_future          = n_future
        )

    } else if (idx_summary$scale == "week") {

        # print("week")

        # Weekly scale ----
        date_seq <- make_sequential_timeseries_irregular_freq(
            idx               = idx,
            length_out        = length_out,
            skip_values       = skip_values,
            insert_values     = insert_values,
            n_future          = n_future
        )

    } else if (idx_summary$scale == "month") {

        # print("month")

        # Monthly scale - Switch to yearmon and then back to date ----
        if (!is.null(skip_values)) skip_values <- zoo::as.yearmon(skip_values)
        if (!is.null(insert_values)) insert_values <- zoo::as.yearmon(insert_values)

        date_seq  <- zoo::as.yearmon(idx) %>%
            tk_make_future_timeseries.yearmon(
                length_out        = length_out,
                skip_values       = skip_values,
                insert_values     = insert_values,
                n_future          = n_future
            ) %>%
            lubridate::as_date()

        # Handle day of month that gets dropped in yearmon conversion
        date_seq <- handle_day_of_month_multi(date_seq, idx)

        # Handle end of month
        date_seq <- handle_end_of_month_multi(date_seq, idx)

    } else if (idx_summary$scale == "quarter") {

        # print("quarter")

        # Quarterly scale - Switch to yearqtr and then back to date -----
        if (!is.null(skip_values)) skip_values <- zoo::as.yearqtr(skip_values)
        if (!is.null(insert_values)) insert_values <- zoo::as.yearqtr(insert_values)

        # list(
        #     length_out        = length_out,
        #     skip_values       = skip_values,
        #     insert_values     = insert_values,
        #     include_endpoints = include_endpoints,
        #     n_future          = n_future
        # ) %>% print()

        date_seq  <- zoo::as.yearqtr(idx) %>%
            tk_make_future_timeseries.yearqtr(
                length_out        = length_out,
                skip_values       = skip_values,
                insert_values     = insert_values,
                n_future          = n_future
            ) %>%
            lubridate::as_date()

        # Handle day of month that gets dropped in yearqtr conversion
        date_seq <- handle_day_of_month_multi(date_seq, idx)

        # Handle end of month
        date_seq <- handle_end_of_month_multi(date_seq, idx)

    } else {

        # print("year")

        # Yearly scale - Use yearmon and rely on frequency to dictate yearly scale -----
        if (!is.null(skip_values)) skip_values <- zoo::as.yearmon(skip_values)
        if (!is.null(insert_values)) insert_values <- zoo::as.yearmon(insert_values)

        date_seq  <- zoo::as.yearmon(idx) %>%
            tk_make_future_timeseries.yearmon(
                length_out        = length_out,
                skip_values       = skip_values,
                insert_values     = insert_values,
                n_future          = n_future) %>%
            lubridate::as_date()

        # Handle day of month that gets dropped in yearmon conversion
        day_of_month    <- lubridate::mday(idx)[1]
        date_seq_origin <- date_seq
        date_seq        <- date_seq %+time% stringr::str_glue("{day_of_month - 1} days")

        # Fix endpoints if needed
        day_of_month_is_wrong <- lubridate::mday(date_seq) != day_of_month
        if (any(day_of_month_is_wrong)) {
            date_seq_shifted <- date_seq_origin %>% lubridate::ceiling_date("month")
            date_seq_shifted <- date_seq_shifted %-time% "1 day"
            date_seq[day_of_month_is_wrong] <- date_seq_shifted[day_of_month_is_wrong]
        }
    }

    return(date_seq)

}

#' @export
tk_make_future_timeseries.yearmon <- function(idx, length_out, inspect_weekdays = FALSE,
                                              inspect_months = FALSE, skip_values = NULL, insert_values = NULL,
                                              n_future = NULL) {
    # Sort idx
    idx <- sort(idx)

    # Remove once `n_future` is fully deprecated
    if (!is.null(n_future)) {
        if (missing(length_out)) length_out <- n_future
        n_future <- TRUE
    }

    # Checks
    if (missing(length_out)) {
        rlang::abort("Argument `length_out` is missing with no default")
    }


    ret <- make_sequential_timeseries_regular_freq(
        idx               = idx,
        length_out        = length_out,
        skip_values       = skip_values,
        insert_values     = insert_values,
        n_future          = n_future)

    return(ret)
}

#' @export
tk_make_future_timeseries.yearqtr <- function(idx, length_out, inspect_weekdays = FALSE,
                                              inspect_months = FALSE, skip_values = NULL, insert_values = NULL,
                                              n_future = NULL) {

    # Sort idx
    idx <- sort(idx)

    # Remove once `n_future` is fully deprecated
    if (!is.null(n_future)) {
        if (missing(length_out)) length_out <- n_future
        n_future <- TRUE
    }

    # Checks
    if (missing(length_out)) {
        rlang::abort("Argument `length_out` is missing with no default")
    }

    ret <- make_sequential_timeseries_regular_freq(
        idx               = idx,
        length_out        = length_out,
        skip_values       = skip_values,
        insert_values     = insert_values,
        n_future          = n_future)

    return(ret)
}

#' @export
tk_make_future_timeseries.numeric <- function(idx, length_out, inspect_weekdays = FALSE,
                                              inspect_months = FALSE, skip_values = NULL, insert_values = NULL,
                                              n_future = NULL) {
    # Sort idx
    idx <- sort(idx)

    # Remove once `n_future` is fully deprecated
    if (!is.null(n_future)) {
        if (missing(length_out)) length_out <- n_future
        n_future <- TRUE
    }

    # Checks
    if (missing(length_out)) {
        rlang::abort("Argument `length_out` is missing with no default")
    }

    ret <- make_sequential_timeseries_regular_freq(
        idx               = idx,
        length_out        = length_out,
        skip_values       = skip_values,
        insert_values     = insert_values,
        n_future          = n_future)

    return(ret)
}


# UTILITIY FUNCTIONS -----

predict_future_timeseries_daily <- function(idx, length_out, inspect_weekdays, inspect_months,
                                            skip_values, insert_values, n_future) {

    # n_future will be TRUE/FALSE (and length_out = n_future)
    # - If TRUE, returns the old n_future behavior of number of observations being potentially fewer than n_future
    # - If FALSE, returns new behavior of observations being exactly `length_out`

    # Validation
    if (!is.null(skip_values)) {
        if (class(skip_values)[[1]] != class(idx)[[1]]) {
            warning("Class `skip_values` does not match class `idx`.", call. = FALSE)
            return(NA)
        }
    }

    if (!is.null(insert_values)) {
        if (class(insert_values)[[1]] != class(idx)[[1]]) {
            warning("Class `insert_values` does not match class `idx`.", call. = FALSE)
            return(NA)
        }
    }

    if ((length(idx) < 60) && inspect_weekdays) warning("Less than 60 observations could result in incorrectly predicted weekday frequency due to small sample size.")
    if ((length(idx) < 400) && inspect_months) warning("Less than 400 observations could result in incorrectly predicted month frequency due to small sample size.")

    # Get index attributes
    idx_signature         <- tk_get_timeseries_signature(idx)
    idx_summary           <- tk_get_timeseries_summary(idx)

    # Find start and end
    start <- min(idx)
    end   <- max(idx)

    # Convert character length_out to numeric length_out (if required)
    # print(include_endpoints)
    length_out_is_chr <- is.character(length_out)
    if (length_out_is_chr) {
        period_offset <- length_out
        # print(period_offset)
    }
    length_out <- convert_length_out_chr_to_num(idx, length_out, include_endpoints = TRUE)

    # Format data frame
    suppressMessages({
        train <- tibble::tibble(
            index = idx,
            y     = rep(1, length(idx))) %>%
            padr::pad(start_val = start, end_val = end) %>%
            padr::fill_by_value(y, value = 0) %>%
            tk_augment_timeseries_signature(.date_var = index)
    })


    # fit model based on components
    f <- make_daily_prediction_formula(train, inspect_weekdays, inspect_months)
    fit <- suppressWarnings(
        stats::glm(f, family = stats::binomial(link = 'logit'), data = train)
    )

    # Create new data
    last_numeric_date <- idx_summary$end %>%
        lubridate::as_datetime() %>%
        as.numeric()
    frequency         <- idx_summary$diff.median
    next_numeric_date <- last_numeric_date + frequency

    if (is.null(n_future)) n_future <- FALSE
    numeric_sequence  <- seq(
        from       = next_numeric_date,
        by         = frequency,
        # This will need changed once n_future deprecation is completed
        length.out = ifelse(n_future, length_out, 4*length_out)
    )

    date_sequence <- lubridate::as_datetime(numeric_sequence) %>%
        lubridate::as_date()

    # Create new_data data frame with future obs timeseries signature
    new_data <- date_sequence %>%
        tk_get_timeseries_signature()

    # Predict
    fitted_results <- suppressWarnings(
        stats::predict(fit, newdata = new_data, type = 'response')
    )
    fitted_results <- ifelse(fitted_results > 0.5, 1, 0)

    # Filter on fitted.results
    predictions <- tibble::tibble(
        index = date_sequence,
        yhat  = fitted_results
    )

    predictions <- predictions %>%
        dplyr::filter(yhat == 1)

    # Filter skip_values
    idx_pred <- filter_skip_values(predictions$index, skip_values, length_out)
    idx_pred <- add_insert_values(idx_pred, insert_values)

    # OLD BEHAVIOR
    # - Remove once n_future is deprecated
    if (n_future) {
        return(idx_pred)
    }

    # NEW BEHAVIOR
    # - length_out = "1 year" or 12 periods always returns full observations)
    if (length_out_is_chr) {
        # Character Period: Return values up to the offset
        endpoint <- idx_summary$end %+time% period_offset
        less_than_endpoint <- idx_pred %>% between_time_vec("start", endpoint)
        ret <- idx_pred[less_than_endpoint]
        ret <- ret[!is.na(ret)]
    } else {
        # Numeric: Return exactly length_out values
        ret <- idx_pred[1:length_out]
        ret <- ret[!is.na(ret)]
    }

    return(ret)
}

make_sequential_timeseries_irregular_freq <- function(idx, length_out, skip_values, insert_values, n_future) {

    # n_future will be TRUE/FALSE (and length_out = n_future)
    # - If TRUE, returns the old n_future behavior of number of observations being potentially fewer than n_future
    # - If FALSE, returns new behavior of observations being exactly `length_out`

    # Validation
    if (!is.null(skip_values)) {
        if (class(skip_values)[[1]] != class(idx)[[1]]) {
            rlang::abort("Class `skip_values` does not match class `idx`.")
        }
    }

    if (!is.null(insert_values)) {
        if (class(insert_values)[[1]] != class(idx)[[1]]) {
            rlang::abort("Class `insert_values` does not match class `idx`.")
        }
    }

    # Get index attributes
    idx_signature         <- tk_get_timeseries_signature(idx)
    idx_summary           <- tk_get_timeseries_summary(idx)

    # Convert character length_out to numeric length_out (if required)
    # print(include_endpoints)
    length_out_is_chr <- is.character(length_out)
    if (length_out_is_chr) {
        period_offset <- length_out
    }
    length_out <- convert_length_out_chr_to_num(idx, length_out, include_endpoints = TRUE)

    # Create date sequence based on index.num and median frequency
    last_numeric_date <- dplyr::last(idx_signature$index.num)
    frequency         <- idx_summary$diff.median
    next_numeric_date <- last_numeric_date + frequency

    if (is.null(n_future)) n_future <- FALSE
    numeric_sequence  <- seq(
        from       = next_numeric_date,
        by         = frequency,
        # This will need changed once n_future deprecation is completed
        length.out = ifelse(n_future, length_out, 4*length_out)
    )


    if (inherits(idx, "Date")) {
        # Date
        date_sequence <- lubridate::as_datetime(numeric_sequence) %>%
            lubridate::as_date()
    } else {
        # Datetime
        date_sequence <- lubridate::as_datetime(numeric_sequence)
        lubridate::tz(date_sequence) <- lubridate::tz(idx)
    }

    # Filter skip_values
    date_sequence <- filter_skip_values(date_sequence, skip_values, length_out)
    date_sequence <- add_insert_values(date_sequence, insert_values)

    # OLD BEHAVIOR
    # - Remove once n_future is deprecated
    if (n_future) {
        return(date_sequence)
    }

    # NEW BEHAVIOR
    # - length_out = "1 year" or 12 periods always returns full observations)
    if (length_out_is_chr) {
        # Character Period: Return values up to the offset
        endpoint <- idx_summary$end %+time% period_offset
        if (is.na(endpoint)) {
            # Offset is not available (happens if last day of month)
            start_date <- lubridate::ceiling_date(idx_summary$end, unit = "month")
            endpoint   <- start_date %+time% period_offset

        }
        less_than_endpoint <- date_sequence %>% between_time_vec("start", endpoint)
        ret <- date_sequence[less_than_endpoint]
        ret <- ret[!is.na(ret)]
    } else {
        # Numeric: Return exactly length_out values
        ret <- date_sequence[1:length_out]
        ret <- ret[!is.na(ret)]
    }

    return(ret)
}


make_sequential_timeseries_regular_freq <- function(idx, length_out, skip_values, insert_values, n_future) {

    # n_future will be TRUE/FALSE (and length_out = n_future)
    # - If TRUE, returns the old n_future behavior of number of observations being potentially fewer than n_future
    # - If FALSE, returns new behavior of observations being exactly `length_out`

    # Validation
    if (!is.null(skip_values)) {
        if (class(skip_values)[[1]] != class(idx)[[1]]) {
            warning("Class `skip_values` does not match class `idx`.", call. = FALSE)
            return(NA)
        }
    }

    if (!is.null(insert_values)) {
        if (class(insert_values)[[1]] != class(idx)[[1]]) {
            warning("Class `insert_values` does not match class `idx`.", call. = FALSE)
            return(NA)
        }
    }

    # Get index attributes
    idx_numeric   <- as.numeric(idx)
    idx_diff      <- diff(idx)
    median_diff   <- stats::median(idx_diff)

    # Convert character length_out to numeric length_out (if required)
    # print(include_endpoints)
    length_out_is_chr <- is.character(length_out)
    if (length_out_is_chr) {
        period_offset <- length_out
    }
    length_out <- convert_length_out_chr_to_num(idx, length_out, include_endpoints = TRUE)

    # Create date sequence based on index.num and median frequency
    last_numeric_date <- dplyr::last(idx_numeric)
    frequency         <- median_diff
    next_numeric_date <- last_numeric_date + frequency

    # print(list(
    #     length_out = length_out,
    #     n_future   = n_future
    # ))
    if (is.null(n_future)) n_future <- FALSE
    numeric_sequence  <- seq(
        from       = next_numeric_date,
        by         = frequency,
        # This will need changed once n_future deprecation is completed
        length.out = ifelse(n_future, length_out, 4*length_out)
    )

    if (inherits(idx, "yearmon")) {
        # yearmon
        date_sequence <- zoo::as.yearmon(numeric_sequence)
    } else if (inherits(idx, "yearqtr")) {
        # yearqtr
        date_sequence <- zoo::as.yearqtr(numeric_sequence)
    } else {
        # numeric
        date_sequence <- numeric_sequence
    }

    # Filter skip_values
    date_sequence <- filter_skip_values(date_sequence, skip_values, length_out)
    date_sequence <- add_insert_values(date_sequence, insert_values)

    # OLD BEHAVIOR
    # - Remove once n_future is deprecated
    if (n_future) {
        return(date_sequence)
    }

    # NEW BEHAVIOR
    # - length_out = "1 year" or 12 periods always returns full observations)
    idx_summary <- tk_get_timeseries_summary(idx)
    if (length_out_is_chr) {
        # Character Period: Return values up to the offset
        endpoint <- lubridate::as_date(idx_summary$end) %+time% period_offset
        less_than_endpoint <- lubridate::as_date(date_sequence) %>% between_time_vec("start", endpoint)
        ret <- date_sequence[less_than_endpoint]
        ret <- ret[!is.na(ret)]

        # list(
        #     idx_end = idx_summary$end,
        #     period_offset = period_offset,
        #     endpoint = endpoint,
        #     date_sequence = date_sequence,
        #     between_endpoint = lubridate::as_date(date_sequence) %>% between_time_vec("start", endpoint)
        # ) %>% print()

    } else {
        # Numeric: Return exactly length_out values
        ret <- date_sequence[1:length_out]
        ret <- ret[!is.na(ret)]
    }

    return(ret)
}

filter_skip_values <- function(date_sequence, skip_values, length_out) {
    # Filter skip_values
    if (!is.null(skip_values)) {

        # Check classes
        if (!identical(class(date_sequence)[1], class(skip_values)[1])) {
            rlang::abort("`skip_values` must be same class as `idx`.")
        }

        # Remove duplicates
        skip_values <- unique(skip_values)

        # Inspect skip_values
        skips_not_in_seq <- skip_values[!(skip_values %in% date_sequence[1:length_out])]
        if (length(skips_not_in_seq) > 0)
            message(paste0("The following `skip_values` were not in the future date sequence: ", stringr::str_c(skips_not_in_seq, collapse = ", ")))

        # Filter skip_values
        filter_skip_vals <- !(date_sequence %in% skip_values)
        date_sequence <- date_sequence[filter_skip_vals]
    }

    return(date_sequence)
}

add_insert_values <- function(date_sequence, insert_values) {
    # Add insert values

    ret <- date_sequence

    if (!is.null(insert_values)) {

        # Check classes
        if (!identical(class(date_sequence)[1], class(insert_values)[1])) {
            rlang::abort("`insert_values` must be same class as `idx`.")
        }

        # Remove duplicates
        insert_values <- unique(insert_values)

        # Inspect insert_values
        adds_in_seq <- insert_values[(insert_values %in% date_sequence)]
        if (length(adds_in_seq) > 0)
            message(paste0("The following `insert_values` were already in the future date sequence: ", stringr::str_c(adds_in_seq, collapse = ", ")))

        # Correct timezone
        if (inherits(date_sequence, "Date")) {

            # Deal with time zones
            numeric_sequence <- date_sequence %>%
                lubridate::as_datetime() %>%
                as.numeric()
            numeric_insert_values <- insert_values %>%
                lubridate::as_datetime() %>%
                as.numeric()

            ret <- c(numeric_sequence, numeric_insert_values[!(numeric_insert_values %in% numeric_sequence)]) %>%
                sort() %>%
                lubridate::as_datetime() %>%
                lubridate::as_date()

        } else if (inherits(date_sequence, "POSIXt")) {

            # Deal with time zones
            numeric_sequence <- as.numeric(date_sequence)
            numeric_insert_values <- as.numeric(insert_values)

            ret <- c(numeric_sequence, numeric_insert_values[!(numeric_insert_values %in% numeric_sequence)]) %>%
                sort() %>%
                lubridate::as_datetime()

            lubridate::tz(ret) <- lubridate::tz(date_sequence)

        } else {

            ret <- c(date_sequence, insert_values[!(insert_values %in% date_sequence)]) %>%
                sort()

        }


    }

    return(ret)
}

make_daily_prediction_formula <- function(ts_signature_tbl_train, inspect_weekdays, inspect_months) {

    nm_list <- list()

    # inspect_weekdays
    if (inspect_weekdays) nm_list <- append(nm_list, list("wday.lbl", "week2", "week3", "week4", "wday.lbl:week2", "wday.lbl:week3", "wday.lbl:week4"))

    # inspect_months
    if (inspect_months) {
        # Need all 12 months and time span to be at least across 2 years
        if (length(unique(ts_signature_tbl_train$month)) == 12 &&
            length(unique(ts_signature_tbl_train$year)) >= 2) {
            nm_list <- append(nm_list, list("week", "month.lbl", "month.lbl:week"))
        } else {
            message("Insufficient timespan / months to perform inspect_month prediction.")
        }
    }

    # Build formula
    params <- stringr::str_c(nm_list, collapse = " + ")
    f <- stats::as.formula(paste0("y ~ ", params))

    return(f)
}




convert_length_out_chr_to_num <- function(idx, length_out, include_endpoints) {
    UseMethod("convert_length_out_chr_to_num", idx)
}

# Sub-daily
convert_length_out_chr_to_num.POSIXt <- function(idx, length_out, include_endpoints) {
    convert_length_out_chr_to_num_regular(idx, length_out, include_endpoints)
}

# Daily or Weekly
convert_length_out_chr_to_num.Date <- function(idx, length_out, include_endpoints) {
    convert_length_out_chr_to_num_regular(idx, length_out, include_endpoints)
}

# Month or Year
convert_length_out_chr_to_num.yearmon <- function(idx, length_out, include_endpoints) {
    convert_length_out_chr_to_num_zoo(idx, length_out, include_endpoints)
}

# Quarterly
convert_length_out_chr_to_num.yearqtr <- function(idx, length_out, include_endpoints) {
    convert_length_out_chr_to_num_zoo(idx, length_out, include_endpoints)
}


convert_length_out_chr_to_num_regular <- function(idx, length_out, include_endpoints) {

    if (is.character(length_out)) {

        # Get index attributes
        idx_summary <- tk_get_timeseries_summary(idx)

        # Find start / end
        start_date <- idx_summary$end
        end_date   <- start_date %+time% length_out

        # For months that don't have day values (e.g. Feb 31st doesn't exist),
        #  end_date is returned as NA
        if (is.na(end_date)) {
            start_date <- lubridate::ceiling_date(start_date, unit = "month")
            end_date   <- start_date %+time% length_out
        }

        # Calculate approximate horizon
        idx_horizon <- tk_make_timeseries(start_date, end_date,
                                          by = stringr::str_glue("{idx_summary$diff.median} sec"),
                                          include_endpoints = TRUE)

        if (include_endpoints) {
            length_out    <- length(idx_horizon)
        } else {
            length_out    <- length(idx_horizon) - 1
        }

    }

    return(length_out)
}


convert_length_out_chr_to_num_zoo <- function(idx, length_out, include_endpoints) {

    if (is.character(length_out)) {

        # Get index attributes
        idx_summary <- tk_get_timeseries_summary(idx)

        diff_numeric <- diff(idx) %>% stats::median()

        # Find start / end
        start_zoo     <- idx_summary$end
        start_date    <- lubridate::as_date(idx_summary$end)
        end_date      <- start_date %+time% length_out

        # Detect class
        if (inherits(idx, "yearmon")) {
            end_zoo   <- zoo::as.yearmon(end_date)
        } else {
            end_zoo   <- zoo::as.yearqtr(end_date)
        }

        # Create numeric sequence
        num_sequence <- seq(as.numeric(start_zoo), as.numeric(end_zoo), by = diff_numeric)

        if (include_endpoints) {
            length_out    <- length(num_sequence)
        } else {
            length_out    <- length(num_sequence) - 1
        }

    }

    return(length_out)
}

handle_day_of_month_multi <- function(seq1, seq0) {

    seq <- seq1

    # Analyze original sequence for irregularity & monthly
    seq_summary     <- tk_get_timeseries_summary(seq0)
    is_month_or_qtr <- any(seq_summary$scale %in% c("month", "quarter"))
    # is_irregular    <- seq_summary$diff.q1 != seq_summary$diff.q3

    # print("Month or Quarter?")
    # print(is_month_or_qtr)
    # print("Irregular")
    # print(is_irregular)
    # print(is_month_or_qtr & is_irregular)

    if (is_month_or_qtr) {

        # print("here")

        # # Apply correct day of month based on seq0
        # day_of_month          <- lubridate::mday(seq0) %>% stats::median()
        # lubridate::mday(seq)  <- day_of_month
        #
        # # Handle day of month that gets miss-applied
        # day_of_month_is_wrong <- lubridate::mday(seq) != day_of_month
        # seq_shifted <- lubridate::floor_date(seq, unit = "month") - lubridate::days(1)
        # if (any(day_of_month_is_wrong)) {
        #     seq[day_of_month_is_wrong] <- seq_shifted[day_of_month_is_wrong]
        # }

        seq_orig <- seq

        seq <- tryCatch({

            # Apply correct day of month based on seq0
            day_of_month          <- lubridate::mday(seq0) %>% stats::median()
            lubridate::mday(seq)  <- day_of_month

            # Handle day of month that gets miss-applied
            day_of_month_is_wrong <- lubridate::mday(seq) != day_of_month
            seq_shifted <- lubridate::floor_date(seq, unit = "month") - lubridate::days(1)
            if (any(day_of_month_is_wrong)) {
                seq[day_of_month_is_wrong] <- seq_shifted[day_of_month_is_wrong]
            }

            seq

        }, error = function(e) {
            seq_orig
        })

    }

    return(seq)
}

handle_end_of_month_multi <- function(seq1, seq0) {

    seq <- seq1

    # Analyze original sequence for month/quarter scale
    seq_summary     <- tk_get_timeseries_summary(seq0)
    is_month_or_qtr <- any(seq_summary$scale %in% c("month", "quarter"))

    # Test EOM
    if (is_month_or_qtr) {

        eom_detected <- FALSE
        eom <- lubridate::ceiling_date(seq0, unit = "month") - lubridate::days(1)
        if(all(seq0 == eom)) eom_detected <- TRUE

        # Shift sequence to EOM
        if (eom_detected) {
            seq <- lubridate::ceiling_date(seq, unit = "month") - lubridate::days(1)
        }

    }

    return(seq)
}

