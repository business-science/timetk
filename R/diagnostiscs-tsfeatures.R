#' Time series feature matrix (Tidy)
#'
#' `tk_tsfeatures()` is a tidyverse compliant wrapper for `tsfeatures::tsfeatures()`.
#' The function computes a matrix of time series features that describes the various time
#' series. It's designed for groupwise analysis using `dplyr` groups.
#'
#' @param .data A `tibble` or `data.frame` with a time-based column
#' @param .date_var A column containing either date or date-time values
#' @param .value A column containing numeric values
#' @param .period The periodicity (frequency) of the time series data. Values can be provided as follows:
#'
#'  - "auto" (default) Calculates using `tk_get_frequency()`.
#'  - "2 weeks": Would calculate the median number of observations in a 2-week window.
#'  - 7 (numeric): Would interpret the `ts` frequency as 7 observations per cycle (common for weekly data)
#'
#' @param .features Passed to `features` in the underlying `tsfeatures()` function.
#'  A vector of function names that represent a feature aggregation function. Examples:
#'
#'  1. Use one of the function names from `tsfeatures` R package e.g.("lumpiness", "stl_features").
#'
#'  2. Use a function name (e.g. "mean" or "median")
#'
#'  3. Create your own function and provide the function name
#'
#' @param .scale If `TRUE`, time series are scaled to mean 0 and sd 1 before features are computed.
#' @param .trim If `TRUE`, time series are trimmed by trim_amount before features are computed.
#'  Values larger than trim_amount in absolute value are set to `NA`.
#' @param .trim_amount Default level of trimming if trim==TRUE. Default: 0.1.
#' @param .parallel If TRUE, multiple cores (or multiple sessions) will be used.
#'  This only speeds things up when there are a large number of time series.
#'
#'  When `.parallel = TRUE`, the `multiprocess = future::multisession`.
#'  This can be adjusted by setting `multiprocess` parameter.
#'  See the `tsfeatures::tsfeatures()` function for mor details.
#'
#' @param .na_action A function to handle missing values. Use na.interp to estimate missing values.
#' @param .prefix A prefix to prefix the feature columns. Default: `"ts_"`.
#' @param .silent Whether or not to show messages and warnings.
#' @param ... Other arguments get passed to the feature functions.
#'
#' @return A `tibble` or `data.frame` with aggregated features that describe each time series.
#'
#' @details
#'
#' The `timetk::tk_tsfeatures()` function implements the `tsfeatures` package
#' for computing aggregated feature matrix for time series that is useful in many types of
#' analysis such as clustering time series.
#'
#' The `timetk` version ports the `tsfeatures::tsfeatures()` function to a `tidyverse`-compliant
#' format that uses a tidy data frame containing grouping columns (optional), a date column, and
#' a value column. Other columns are ignored.
#'
#' It then becomes easy to summarize each time series by group-wise application of `.features`,
#' which are simply functions that evaluate a time series and return single aggregated value.
#' (Example: "mean" would return the mean of the time series (note that values are scaled to mean 1 and sd 0 first))
#'
#' __Function Internals:__
#'
#' Internally, the time series are converted to `ts` class using `tk_ts(.period)` where the
#' period is the frequency of the time series. Values can be provided for `.period`, which will be used
#' prior to convertion to `ts` class.
#'
#' The function then leverages `tsfeatures::tsfeatures()` to compute the feature matrix of summarized
#' feature values.
#'
#'
#'
#' @references
#' 1. Rob Hyndman, Yanfei Kang, Pablo Montero-Manso, Thiyanga Talagala, Earo Wang, Yangzhuoran Yang,
#'   Mitchell O'Hara-Wild: tsfeatures R package
#'
#' @examples
#' library(dplyr)
#' library(timetk)
#'
#' walmart_sales_weekly %>%
#'     group_by(id) %>%
#'     tk_tsfeatures(
#'       .date_var = Date,
#'       .value    = Weekly_Sales,
#'       .period   = 52,
#'       .features = c("frequency", "stl_features", "entropy", "acf_features", "mean"),
#'       .scale    = TRUE,
#'       .prefix   = "ts_"
#'     )
#'
#' @name tk_tsfeatures
#' @export
tk_tsfeatures <- function(
    .data,
    .date_var,
    .value,
    .period = "auto",
    .features = c("frequency", "stl_features", "entropy", "acf_features"),
    .scale = TRUE,
    .trim = FALSE,
    .trim_amount = 0.1,
    .parallel = FALSE,
    .na_action = na.pass,
    .prefix = "ts_",
    .silent = TRUE,
    ...
) {
    # Tidyeval Setup
    date_var_expr <- rlang::enquo(.date_var)
    value_expr    <- rlang::enquo(.value)

    # Checks
    if (!is.data.frame(.data)) {
        stop(call. = FALSE, "`.data` is not a data-frame or tibble. Please supply a data.frame or tibble.")
    }
    if (rlang::quo_is_missing(date_var_expr)) {
        stop(call. = FALSE, "`.date_var` is missing. Please supply a date or date-time column.")
    }
    if (rlang::quo_is_missing(value_expr)) {
        stop(call. = FALSE, "`.value` is missing. Please supply a numeric column.")
    }

    UseMethod("tk_tsfeatures", .data)
}

#' @export
tk_tsfeatures.data.frame <- function(
    .data,
    .date_var,
    .value,
    .period = "auto",
    .features = c("frequency", "stl_features", "entropy", "acf_features"),
    .scale = TRUE,
    .trim = FALSE,
    .trim_amount = 0.1,
    .parallel = FALSE,
    .na_action = na.pass,
    .prefix = "ts_",
    .silent = TRUE,
    ...
) {

    # Check if numeric period is supplied
    if (!is.numeric(.period)) {
        .period <- .data %>%
            pull(!! rlang::enquo(.date_var)) %>%
            tk_get_frequency(
                period = .period,
                message = !.silent
            )
    }

    # Convert to ts class
    data_ts <- .data %>%
        dplyr::select(!! rlang::enquo(.date_var), !! rlang::enquo(.value)) %>%
        tk_ts(frequency = .period, silent = TRUE)

    # Compute TS Features
    ts_features <- tsfeatures::tsfeatures(
        tslist       = list(data_ts),
        features     = .features,
        scale        = .scale,
        trim         = .trim,
        trim_amount  = .trim_amount,
        parallel     = .parallel,
        na.action    = .na_action,
        ...
    ) %>%
        dplyr::rename_with(.fn = ~ stringr::str_c(.prefix, .))

    return(ts_features)

}

#' @export
tk_tsfeatures.grouped_df <- function(
    .data,
    .date_var,
    .value,
    .period = "auto",
    .features = c("frequency", "stl_features", "entropy", "acf_features"),
    .scale = TRUE,
    .trim = FALSE,
    .trim_amount = 0.1,
    .parallel = FALSE,
    .na_action = na.pass,
    .prefix = "ts_",
    .silent = TRUE,
    ...
) {

    # Tidy Eval Setup
    value_expr    <- rlang::enquo(.value)
    date_var_expr <- rlang::enquo(.date_var)
    group_names   <- dplyr::group_vars(.data)
    group_exprs   <- rlang::syms(group_names)

    # Isolate data
    data_tbl <- .data %>%
        dplyr::select(!!! group_exprs, !!date_var_expr, !!value_expr)

    # Convert to TS
    data_nested <- data_tbl %>%
        tidyr::nest() %>%
        dplyr::mutate(data_ts = purrr::map(data, .f = function(df) {

            # Check if numeric period is supplied
            if (!is.numeric(.period)) {
                .period <- df %>%
                    pull(!! date_var_expr) %>%
                    tk_get_frequency(
                        period = .period,
                        message = !.silent
                    )
            }

            # Convert to ts class
            data_ts <- df %>%
                tk_ts(frequency = .period, silent = TRUE)

            return(data_ts)

        }))


    # Make TS Features
    ts_features <- tsfeatures::tsfeatures(
        tslist       = data_nested$data_ts,
        features     = .features,
        scale        = .scale,
        trim         = .trim,
        trim_amount  = .trim_amount,
        parallel     = .parallel,
        na.action    = .na_action,
        ...
    ) %>%
        dplyr::rename_with(.fn = ~ stringr::str_c(.prefix, .))

    ret <- dplyr::bind_cols(
        data_nested %>% select(!!! group_exprs),
        ts_features
    )

    return(ret)

}

