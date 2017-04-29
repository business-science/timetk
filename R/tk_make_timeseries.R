#' Make a future time series from an existing time series
#'
#' @param idx A vector of dates
#' @param n_future Number of future observations
#' @param skip_values A vector of same class as `idx` of timeseries
#' values to skip.
#' @param inspect_weekdays Uses a logistic regression algorithm to inspect
#' whether certain weekdays (e.g. weekends) should be excluded from the future dates.
#' Default is `TRUE`.
#'
#' @details
#' `tk_make_future_timeseries` returns a time series based
#' on the input index frequency and attributes.
#'
#' The argument `n_future` determines how many future index observations to compute.
#'
#' The `skip_values` argument can be used to pass a timeseries vector of values to skip.
#' This argument is good for passing holidays or special index values that should
#' be excluded from the future time series.
#' The values must be the same format as the `idx` class.
#'
#' The `inspect_weekdays` argument applies to "daily" (scale = "day") data
#' (refer to `tk_get_timeseries_summary()` to get the index scale).
#' The default is `TRUE`, which runs a logistic regression on the weekdays to
#' determine if certain weekdays should be excluded from the future dates.
#'
#' @return A vector containing future dates
#'
#' @seealso [tk_index()], [tk_get_timeseries_summary()], [tk_get_timeseries_signature()]
#'
#' @examples
#' library(tidyquant)
#' library(timekit)
#'
#' # Basic example
#' idx <- c("2016-01-01 00:00:00",
#'          "2016-01-01 00:00:03",
#'          "2016-01-01 00:00:06") %>%
#'     ymd_hms()
#' # Make next three dates in series
#' idx %>%
#'     tk_make_future_timeseries(n_future = 3)
#'
#'
#' # Create index of days that FB stock will be traded in 2017 based on 2016 + holidays
#' FB_tbl <- FANG %>% filter(symbol == "FB")
#' holidays <- c("2017-01-02", "2017-01-16", "2017-02-20",
#'               "2017-04-14", "2017-05-29", "2017-07-04",
#'               "2017-09-04", "2017-11-23", "2017-12-25") %>%
#'     ymd()
#' # Remove holidays with skip_values, and remove weekends with inspect_weekdays = TRUE
#' FB_tbl %>%
#'     tk_index() %>%
#'     tk_make_future_timeseries(n_future         = 251,
#'                               skip_values      = holidays,
#'                               inspect_weekdays = TRUE)
#'
#' # Works with regularized indexes as well
#' c(2016.00, 2016.25, 2016.50, 2016.75) %>%
#'     tk_make_future_timeseries(n_future = 4)
#'
#' # Works with zoo yearmon and yearqtr too
#' c("2016 Q1", "2016 Q2", "2016 Q3", "2016 Q4") %>%
#'     as.yearqtr() %>%
#'     tk_make_future_timeseries(n_future = 4)
#'
#'
#' @export
tk_make_future_timeseries <- function(idx, n_future, skip_values = NULL, inspect_weekdays = TRUE) {
    UseMethod("tk_make_future_timeseries", idx)
}

#' @export
tk_make_future_timeseries.POSIXt <- function(idx, n_future, skip_values = NULL, inspect_weekdays = TRUE) {
    return(make_sequential_timeseries_irregular_freq(idx = idx, n_future = n_future, skip_values = skip_values))
}

#' @export
tk_make_future_timeseries.Date <- function(idx, n_future, skip_values = NULL, inspect_weekdays = TRUE) {

    if (missing(n_future)) {
        warning("Argument `n_future` is missing with no default")
        return(NA)
    }

    # Daily Periodicity + Inspect Weekdays
    idx_summary   <- tk_get_timeseries_summary(idx)
    if (idx_summary$scale == "day" && inspect_weekdays) {
        tryCatch({
            return(predict_future_timeseries_daily(idx = idx, n_future = n_future, skip_values = skip_values))
        }, error = function(e) {
            warning(paste0("Could not perform `glm()`: ", e, "\nMaking sequential timeseries."))
            return(make_sequential_timeseries_irregular_freq(idx = idx, n_future = n_future, skip_values = skip_values))
        })
    } else {
        return(make_sequential_timeseries_irregular_freq(idx = idx, n_future = n_future, skip_values = skip_values))
    }
}

#' @export
tk_make_future_timeseries.yearmon <- function(idx, n_future, skip_values = NULL, inspect_weekdays = TRUE) {
    return(make_sequential_timeseries_regular_freq(idx = idx, n_future = n_future, skip_values = skip_values))
}

#' @export
tk_make_future_timeseries.yearqtr <- function(idx, n_future, skip_values = NULL, inspect_weekdays = TRUE) {
    return(make_sequential_timeseries_regular_freq(idx = idx, n_future = n_future, skip_values = skip_values))
}

#' @export
tk_make_future_timeseries.numeric <- function(idx, n_future, skip_values = NULL, inspect_weekdays = TRUE) {
    return(make_sequential_timeseries_regular_freq(idx = idx, n_future = n_future, skip_values = skip_values))
}

# UTILITIY FUNCTIONS -----

predict_future_timeseries_daily <- function(idx, n_future, skip_values) {

    # Validation
    if (!is.null(skip_values))
        if (class(skip_values)[[1]] != class(idx)[[1]]) {
            warning("Class `skip_values` does not match class `idx`.", call. = FALSE)
            return(NA)
            }

    # Get index attributes
    idx_signature         <- tk_get_timeseries_signature(idx)
    idx_summary           <- tk_get_timeseries_summary(idx)

    # Format data frame
    train <- tibble::tibble(
        index = idx,
        y     = rep(1, length(idx))) %>%
        padr::pad() %>%
        padr::fill_by_value(y, value = 0) %>%
        tk_augment_timeseries_signature()

    # fit model
    fit <- suppressWarnings(
        stats::glm(y ~ wday.lbl + wday.lbl:week2 + wday.lbl:week3 + wday.lbl:week4,
                      family = stats::binomial(link = 'logit'),
                      data   = train)
        )

    # Create new data
    last_numeric_date <- dplyr::last(train$index.num)
    frequency         <- idx_summary$diff.median
    next_numeric_date <- last_numeric_date + frequency
    numeric_sequence  <- seq(from = next_numeric_date, by = frequency, length.out = 2*n_future + 500)

    date_sequence <- lubridate::as_datetime(numeric_sequence) %>%
        lubridate::as_date()
    lubridate::tz(date_sequence) <- lubridate::tz(idx)

    # Filter skip_values
    date_sequence <- filter_skip_values(date_sequence, skip_values, 1.5 * n_future)

    # Create new_data data frame with future obs timeseries signature
    new_data <- date_sequence %>%
        tk_get_timeseries_signature()

    # Predict
    fitted.results <- stats::predict(fit, newdata = new_data, type = 'response')
    fitted.results <- ifelse(fitted.results > 0.10, 1, 0)

    # Filter on fitted.results
    predictions <- tibble::tibble(
        index = date_sequence,
        yhat  = fitted.results
        )

    predictions <- predictions %>%
        dplyr::filter(yhat == 1) %>%
        dplyr::slice(1:n_future)

    # Return date sequence
    return(predictions$index)
}

make_sequential_timeseries_irregular_freq <- function(idx, n_future, skip_values) {

    # Validation
    if (!is.null(skip_values))
        if (class(skip_values)[[1]] != class(idx)[[1]]) {
            warning("Class `skip_values` does not match class `idx`.", call. = FALSE)
            return(NA)
        }

    # Get index attributes
    idx_signature         <- tk_get_timeseries_signature(idx)
    idx_summary           <- tk_get_timeseries_summary(idx)

    # Create date sequence based on index.num and median frequency
    last_numeric_date <- dplyr::last(idx_signature$index.num)
    frequency         <- idx_summary$diff.median
    next_numeric_date <- last_numeric_date + frequency
    numeric_sequence  <- seq(from = next_numeric_date, by = frequency, length.out = n_future + length(skip_values))

    if (inherits(idx, "Date")) {
        # Date
        date_sequence <- lubridate::as_datetime(numeric_sequence) %>%
            lubridate::as_date()
        lubridate::tz(date_sequence) <- lubridate::tz(idx)
    } else {
        # Datetime
        date_sequence <- lubridate::as_datetime(numeric_sequence)
        lubridate::tz(date_sequence) <- lubridate::tz(idx)
    }

    # Filter skip_values
    date_sequence <- filter_skip_values(date_sequence, skip_values, n_future)

    # Remove any extra obs
    date_sequence <- date_sequence[1:n_future]

    # Return date sequence
    return(date_sequence)
}


make_sequential_timeseries_regular_freq <- function(idx, n_future, skip_values) {

    # Validation
    if (!is.null(skip_values))
        if (class(skip_values)[[1]] != class(idx)[[1]]) {
            warning("Class `skip_values` does not match class `idx`.", call. = FALSE)
            return(NA)
        }

    # Get index attributes
    idx_numeric   <- as.numeric(idx)
    idx_diff      <- diff(idx)
    median_diff   <- stats::median(idx_diff)

    # Create date sequence based on index.num and median frequency
    last_numeric_date <- dplyr::last(idx_numeric)
    frequency         <- median_diff
    next_numeric_date <- last_numeric_date + frequency
    numeric_sequence  <- seq(from = next_numeric_date, by = frequency, length.out = n_future + length(skip_values))

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
    date_sequence <- filter_skip_values(date_sequence, skip_values, n_future)

    # Remove any extra obs
    date_sequence <- date_sequence[1:n_future]

    # Return date sequence
    return(date_sequence)
}

filter_skip_values <- function(date_sequence, skip_values, n_future) {
    # Filter skip_values
    if (!is.null(skip_values)) {
        # Inspect skip_values
        skips_not_in_seq <- skip_values[!(skip_values %in% date_sequence[1:n_future])]
        if (length(skips_not_in_seq) > 0)
            message(paste0("The following `skip_values` were not in the future date sequence: ", stringr::str_c(skips_not_in_seq, collapse = ", ")))

        # Filter skip_values
        filter_skip_vals <- !(date_sequence %in% skip_values)
        date_sequence <- date_sequence[filter_skip_vals]
    }

    return(date_sequence)
}
