#' Group-wise Seasonality Data Preparation
#'
#' `tk_seasonal_diagnostics()` is the preprocessor for `plot_seasonal_diagnostics()`.
#' It helps by automating feature collection for time series seasonality analysis.
#'
#' @param .data A `tibble` or `data.frame` with a time-based column
#' @param .date_var A column containing either date or date-time values
#' @param .value A column containing numeric values
#' @param .feature_set One or multiple selections to analyze for seasonality. Choices include:
#'  - "auto" - Automatically selects features based on the time stamps and length of the series.
#'  - "second" - Good for analyzing seasonality by second of each minute.
#'  - "minute" - Good for analyzing seasonality by minute of the hour
#'  - "hour" - Good for analyzing seasonality by hour of the day
#'  - "wday.lbl" - Labeled weekdays. Good for analyzing seasonality by day of the week.
#'  - "week" - Good for analyzing seasonality by week of the year.
#'  - "month.lbl" - Labeled months. Good for analyzing seasonality by month of the year.
#'  - "quarter" - Good for analyzing seasonality by quarter of the year
#'  - "year" - Good for analyzing seasonality over multiple years.
#'
#' @return A `tibble` or `data.frame` with seasonal features
#'
#' @details
#'
#' __Automatic Feature Selection__
#'
#' Internal calculations are performed to detect a sub-range of features to include
#' useing the following logic:
#'
#' - The _minimum_ feature is selected based on the median difference between consecutive
#' timestamps
#' - The _maximum_ feature is selected based on having 2 full periods.
#'
#' Example: Hourly timestamp data that lasts more than 2 weeks will have the following features:
#' "hour", "wday.lbl", and "week".
#'
#' __Scalable with Grouped Data Frames__
#'
#' This function respects grouped `data.frame` and `tibbles` that were made with `dplyr::group_by()`.
#'
#' For grouped data, the automatic feature selection returned is a collection of all
#' features within the sub-groups. This means extra features are returned even though
#' they may be meaningless for some of the groups.
#'
#' __Transformations__
#'
#' The `.value` parameter respects transformations (e.g. `.value = log(sales)`).
#'
#' @examples
#' \donttest{
#' library(dplyr)
#' library(timetk)
#'
#' # ---- GROUPED EXAMPLES ----
#'
#' # Hourly Data
#' m4_hourly %>%
#'     group_by(id) %>%
#'     tk_seasonal_diagnostics(date, value)
#'
#' # Monthly Data
#' m4_monthly %>%
#'     group_by(id) %>%
#'     tk_seasonal_diagnostics(date, value)
#'
#' # ---- TRANSFORMATION ----
#'
#' m4_weekly %>%
#'     group_by(id) %>%
#'     tk_seasonal_diagnostics(date, log(value))
#'
#' # ---- CUSTOM FEATURE SELECTION ----
#'
#' m4_hourly %>%
#'     group_by(id) %>%
#'     tk_seasonal_diagnostics(date, value, .feature_set = c("hour", "week"))
#'
#' }
#'
#' @name tk_seasonal_diagnostics
#' @export
tk_seasonal_diagnostics <- function(.data, .date_var, .value, .feature_set = "auto") {

    # Tidyeval Setup
    date_var_expr <- rlang::enquo(.date_var)
    value_expr    <- rlang::enquo(.value)

    # Checks
    if (!is.data.frame(.data)) {
        stop(call. = FALSE, "tk_seasonal_diagnostics(.data) is not a data-frame or tibble. Please supply a data.frame or tibble.")
    }
    if (rlang::quo_is_missing(date_var_expr)) {
        stop(call. = FALSE, "tk_seasonal_diagnostics(.date_var) is missing. Please supply a date or date-time column.")
    }
    if (rlang::quo_is_missing(value_expr)) {
        stop(call. = FALSE, "tk_seasonal_diagnostics(.value) is missing. Please a numeric column.")
    }
    if (!all(.feature_set %in% acceptable_seasonal_values())) {
        stop(call. = FALSE, "tk_seasonal_diagnostics(.feature_set): Feature values not in acceptable values. Please use one or more of: ",
             stringr::str_c(acceptable_seasonal_values(), collapse = ", "))
    }

    UseMethod("tk_seasonal_diagnostics", .data)

}

#' @export
tk_seasonal_diagnostics.data.frame <- function(.data, .date_var, .value, .feature_set = "auto") {


    # Tidyeval Setup
    date_var_expr <- rlang::enquo(.date_var)
    value_expr    <- rlang::enquo(.value)

    # ---- DATA SETUP ----

    # Apply any Transformations - Evaluate Formula
    data_formatted <- .data %>%
        dplyr::mutate(.value_mod = !! value_expr) %>%
        dplyr::select(!! date_var_expr, .value_mod)

    # Auto Features Summary
    if (.feature_set[1] == "auto") {
        .feature_set <- .data %>%
            dplyr::pull(!! date_var_expr) %>%
            get_seasonal_auto_features()
    }

    # Return the seasonal features
    data_formatted <- data_formatted %>%
        tk_augment_timeseries_signature(.date_var = !! date_var_expr) %>%
        dplyr::select(!! date_var_expr, .value_mod, .feature_set) %>%
        dplyr::mutate_at(.vars = dplyr::vars(-(!! date_var_expr), -.value_mod), factor, ordered = FALSE) %>%
        dplyr::rename(.value = .value_mod)

    return(data_formatted)

}

#' @export
tk_seasonal_diagnostics.grouped_df <- function(.data, .date_var, .value, .feature_set = "auto") {

    # Tidy Eval Setup
    value_expr    <- rlang::enquo(.value)
    date_var_expr <- rlang::enquo(.date_var)
    group_names   <- dplyr::group_vars(.data)

    # If auto, get common features
    if (.feature_set[1] == "auto") {
        .feature_set <- .data %>%
            dplyr::group_split() %>%
            purrr::map(.f = function(df) {
                df %>%
                    dplyr::pull(!! date_var_expr) %>%
                    get_seasonal_auto_features()
            }) %>%
            purrr::flatten_chr() %>%
            unique()
    }


    # Process groups individually
    .data %>%
        tidyr::nest() %>%
        dplyr::mutate(nested.col = purrr::map(
            .x         = data,
            .f         = function(df) tk_seasonal_diagnostics(
                .data         = df,
                .date_var     = !! date_var_expr,
                .value        = !! value_expr,
                .feature_set  = .feature_set
            )
        )) %>%
        dplyr::select(-data) %>%
        tidyr::unnest(cols = nested.col) %>%
        dplyr::group_by_at(.vars = group_names)

}

# UTILITIES ----

acceptable_seasonal_values <- function() {
    c("auto", "second", "minute", "hour", "wday.lbl", "week", "month.lbl", "quarter", "year")
}

get_seasonal_auto_features <- function(.index) {

    summary_tbl <- .index %>%
        tk_get_timeseries_summary()

    max_min_list <- get_max_min_list(summary_tbl)

    features_to_get <- tk_get_timeseries_unit_frequency() %>%
        tidyr::gather() %>%
        dplyr::mutate(check = value %>% dplyr::between(max_min_list$min_period$value, max_min_list$max_period$value)) %>%
        dplyr::filter(check) %>%
        dplyr::left_join(time_series_signature_lookup_tbl(), by = "key") %>%
        dplyr::pull(feature)

    return(features_to_get)
}


get_max_min_list <- function(time_series_summary_tbl) {

    # Min - Check median diff and go next unit up
    min_period <- tk_get_timeseries_unit_frequency() %>%
        tidyr::gather() %>%
        dplyr::mutate(check = 2 * value > time_series_summary_tbl$diff.median) %>%
        dplyr::filter(check) %>%
        dplyr::slice(1)

    # Max - Check at least 2 periods worth of data
    start_numeric <- as.numeric(as.POSIXct(time_series_summary_tbl$start))
    end_numeric   <- as.numeric(as.POSIXct(time_series_summary_tbl$end))
    start_to_end  <- end_numeric - start_numeric
    start_to_end

    max_period <- tk_get_timeseries_unit_frequency() %>%
        tidyr::gather() %>%
        dplyr::mutate(check = 2 * value < start_to_end) %>%
        dplyr::filter(check) %>%
        dplyr::slice(dplyr::n())

    # Max and min
    max_min_list <- list(min_period = min_period, max_period = max_period)
    return(max_min_list)
}


# Features to get
time_series_signature_lookup_tbl <- function() {

    tibble::tibble(
        sec     = "second",
        min     = "minute",
        hour    = "hour",
        day     = "wday.lbl",
        week    = "week",
        month   = "month.lbl",
        quarter = "quarter",
        year    = "year"
    ) %>%
        tidyr::gather(value = "feature")
}



