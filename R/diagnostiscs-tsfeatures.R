#' Time series feature matrix (Tidy)
#'
#' `tk_tsfeatures()` is a tidyverse compliant wrapper for `tsfeatures::tsfeatures()`.
#' The function computes a matrix of time series features that describes the various time
#' series. It's designed for groupwise analysis using `dplyr` groups.
#'
#' @param .data A `tibble` or `data.frame` with a time-based column
#' @param .date_var A column containing either date or date-time values
#' @param .value A column containing numeric values
#'
#' @return A `tibble` or `data.frame` with STL Decomposition Features
#'  (observed, season, trend, remainder, seasadj) and
#'  Anomaly Features (remainder_l1, remainder_l2, anomaly, recomposed_l1, and recomposed_l2)
#'
#' @details
#'
#' Todo
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
#'     filter(id %in% c("1_1", "1_3")) %>%
#'     group_by(id) %>%
#'     tk_tsfeatures(Date, Weekly_Sales)
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
        tk_ts(frequency = .period, silent = .silent)

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

    # Process groups individually


}

