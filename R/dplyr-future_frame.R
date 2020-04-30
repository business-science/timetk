#' Make future time series from existing
#'
#' @param .data A data.frame or tibble
#' @param .date_var A date or date-time variable.
#' @param .length_out Number of future observations. Can be numeric number or a phrase
#'  like "1 year".
#' @param .inspect_weekdays Uses a logistic regression algorithm to inspect
#'  whether certain weekdays (e.g. weekends) should be excluded from the future dates.
#'  Default is `FALSE`.
#' @param .inspect_months Uses a logistic regression algorithm to inspect
#'  whether certain days of months (e.g. last two weeks of year or seasonal days)
#'  should be excluded from the future dates.
#'  Default is `FALSE`.
#' @param .skip_values A vector of same class as `idx` of timeseries
#'  values to skip.
#' @param .insert_values A vector of same class as `idx` of timeseries
#'  values to insert.
#'
#'
#' @details
#'
#' This is a wrapper for [tk_make_future_timeseries()] that works on data.frames. It respects `dplyr` groups.
#'
#' __Specifying Length of Future Observations__
#'
#' The argument `.length_out` determines how many future index observations to compute. It can be specified
#' as:
#'
#' - __A numeric value__ - the number of future observations to return.
#'     - The number of observations returned is _always_ equal to the value the user inputs.
#'     - The __end date can vary__ based on the number of timestamps chosen.
#'
#' - __A time-based phrase__ - The duration into the future to include (e.g. "6 months" or "30 minutes").
#'     - The _duration_ defines the _end date_ for observations.
#'     - The __end date will not change__ and those timestamps that fall within the end date will be returned
#'  (e.g. a quarterly time series will return 4 quarters if `.length_out = "1 year"`).
#'     - The number of observations will vary to fit within the end date.
#'
#' __Weekday and Month Inspection__
#'
#' The `.inspect_weekdays` and `.inspect_months` arguments apply to "daily" (scale = "day") data
#' (refer to `tk_get_timeseries_summary()` to get the index scale).
#'
#' - The `.inspect_weekdays` argument is useful in determining missing days of the week
#' that occur on a weekly frequency such as every week, every other week, and so on.
#' It's recommended to have at least 60 days to use this option.
#' - The `.inspect_months` argument is useful in determining missing days of the month, quarter
#' or year; however, the algorithm can inadvertently select incorrect dates if the pattern
#' is erratic.
#'
#' __Skipping / Inserting Values__
#'
#' The `.skip_values` and `.insert_values` arguments can be used to remove and add
#' values into the series of future times. The values must be the same format as the `idx` class.
#'
#' - The `.skip_values` argument useful for passing holidays or special index values that should
#' be excluded from the future time series.
#' - The `.insert_values` argument is useful for adding values back that the algorithm may have
#' excluded.
#'
#'
#'
#' @return A tibble that has been extended with future date, date-time timestamps.
#'
#' @seealso
#' - Making Future Time Series: [tk_make_future_timeseries()] (Underlying function)
#'
#' @examples
#' library(dplyr)
#' library(tidyquant)
#' library(timetk)
#'
#' # 30-min interval data
#' taylor_30_min %>%
#'     future_frame(date, .length_out = "1 week")
#'
#' # Daily Data (Grouped)
#' m4_daily %>%
#'     group_by(id) %>%
#'     future_frame(date, .length_out = "6 weeks")
#'
#' # Specify how many observations to project into the future
#' m4_daily %>%
#'     group_by(id) %>%
#'     future_frame(date, .length_out = 100)
#'
#'
#' # Remove Non-Working Days (Weekends & Holidays)
#' holidays <- tk_make_holiday_sequence(
#'     start_date = "2017-01-01",
#'     end_date   = "2017-12-31",
#'     calendar   = "NYSE")
#'
#' FANG %>%
#'     group_by(symbol) %>%
#'     future_frame(.date_var         = date,
#'                  .length_out       = "1 year",
#'                  .inspect_weekdays = TRUE,
#'                  .skip_values      = holidays)
#'
#'
#'
#' @name future_frame
NULL

#' @export
#' @rdname future_frame
future_frame <- function(.data, .date_var, .length_out,
                         .inspect_weekdays = FALSE, .inspect_months = FALSE,
                         .skip_values = NULL, .insert_values = NULL) {

    # Checks
    if (rlang::is_missing(.data)) rlang::abort("`.data` is missing.")
    if (rlang::quo_is_missing(enquo(.date_var))) rlang::abort(".date_var is missing.")
    if (rlang::is_missing(.length_out)) rlang::abort("`.length_out` is missing.")

    UseMethod("future_frame")


}

#' @export
future_frame.data.frame <- function(.data, .date_var, .length_out,
                                    .inspect_weekdays = FALSE, .inspect_months = FALSE,
                                    .skip_values = NULL, .insert_values = NULL) {

    date_var_expr <- rlang::enquo(.date_var)

    idx      <- .data %>% dplyr::pull(!! date_var_expr)
    idx_name <- rlang::quo_name(date_var_expr)

    idx_future <- tk_make_future_timeseries(
        idx              = idx,
        length_out       = .length_out,
        inspect_weekdays = .inspect_weekdays,
        inspect_months   = .inspect_months,
        skip_values      = .skip_values,
        insert_values    = .insert_values
    )

    tibble::tibble(
        !! idx_name := idx_future
    )

}

#' @export
future_frame.grouped_df <- function(.data, .date_var, .length_out,
                                    .inspect_weekdays = FALSE, .inspect_months = FALSE,
                                    .skip_values = NULL, .insert_values = NULL) {

    # Tidy Eval Setup
    group_names <- dplyr::group_vars(.data)

    .data %>%
        tidyr::nest() %>%
        dplyr::mutate(nested.col = purrr::map(
            .x         = data,
            .f         = function(df) future_frame(
                .data             = df,
                .date_var         = !! enquo(.date_var),
                .length_out       = .length_out,
                .inspect_weekdays = .inspect_weekdays,
                .inspect_months   = .inspect_months,
                .skip_values      = .skip_values,
                .insert_values    = .insert_values
            )
        )) %>%
        dplyr::select(-data) %>%
        tidyr::unnest(cols = nested.col) %>%
        dplyr::group_by_at(.vars = group_names)


}

#' @export
future_frame.default <- function(.data, .date_var, .length_out,
                                 .inspect_weekdays = FALSE, .inspect_months = FALSE,
                                 .skip_values = NULL, .insert_values = NULL) {
    rlang::abort("Object is not of class `data.frame`.")
}
