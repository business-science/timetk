#' Automatic group-wise Anomaly Detection
#'
#' `anomalize()` is used to detect anomalies in time series data,
#' either for a single time series or for multiple time series grouped by a specific column.
#'
#' @param .data A `tibble` or `data.frame` with a time-based column
#' @param .date_var A column containing either date or date-time values
#' @param .value A column containing numeric values
#' @param .frequency Controls the seasonal adjustment (removal of seasonality).
#'  Input can be either "auto", a time-based definition (e.g. "2 weeks"),
#'  or a numeric number of observations per frequency (e.g. 10).
#'  Refer to [tk_get_frequency()].
#' @param .trend Controls the trend component.
#'  For STL, trend controls the sensitivity of the LOESS smoother, which is used to remove the remainder.
#'  Refer to [tk_get_trend()].
#' @param .method The outlier detection method. Default: "stl". Currently
#'  "stl" is the only method. "twitter" is planned.
#' @param .iqr_alpha Controls the width of the "normal" range. Lower values are more conservative
#'  while higher values are less prone to incorrectly classifying "normal" observations.
#' @param .clean_alpha Controls the threshold for cleaning
#' the outliers. The default is 0.75, which means that the anomalies will be
#' cleaned using the 0.75 * lower or upper bound of the recomposed time series,
#' depending on the direction of the anomaly.
#' @param .max_anomalies The maximum percent of anomalies permitted to be identified.
#' @param .message A boolean. If `TRUE`, will output information related to automatic frequency
#' and trend selection (if applicable).
#'
#' @return
#' A `tibble` or `data.frame` with the following columns:
#'  - observed: original data
#'  - seasonal: seasonal component
#'  - seasadaj: seasonal adjusted
#'  - trend: trend component
#'  - remainder: residual component
#'  - anomaly: Yes/No flag for outlier detection
#'  - anomaly score: distance from centerline
#'  - anomaly direction: -1, 0, 1 inidicator for direction of the anomaly
#'  - recomposed_l1: lower level bound of recomposed time series
#'  - recomposed_l2: upper level bound of recomposed time series
#'  - observed_clean: original data with anomalies interpolated
#'
#' @details
#'
#' The `anomalize()` method for anomaly detection that implements a 2-step process to
#' detect outliers in time series.
#'
#' __Step 1: Detrend & Remove Seasonality using STL Decomposition__
#'
#' The decomposition separates the "season" and "trend" components from the "observed" values
#' leaving the "remainder" for anomaly detection.
#'
#' The user can control two parameters: frequency and trend.
#'
#' 1. `.frequency`: Adjusts the "season" component that is removed from the "observed" values.
#' 2. `.trend`: Adjusts the trend window (t.window parameter from [stats::stl()] that is used.
#'
#' The user may supply both `.frequency` and `.trend` as time-based durations (e.g. "6 weeks") or
#' numeric values (e.g. 180) or "auto", which predetermines the frequency and/or trend based on
#' the scale of the time series using the [tk_time_scale_template()].
#'
#' __Step 2: Anomaly Detection__
#'
#' Once "trend" and "season" (seasonality) is removed, anomaly detection is performed on the "remainder".
#' Anomalies are identified, and boundaries (recomposed_l1 and recomposed_l2) are determined.
#'
#' The Anomaly Detection Method uses an inner quartile range (IQR) of +/-25 the median.
#'
#' _IQR Adjustment, alpha parameter_
#'
#' With the default `alpha = 0.05`, the limits are established by expanding
#' the 25/75 baseline by an IQR Factor of 3 (3X).
#' The _IQR Factor = 0.15 / alpha_ (hence 3X with alpha = 0.05):
#'
#' - To increase the IQR Factor controlling the limits, decrease the alpha,
#' which makes it more difficult to be an outlier.
#' - Increase alpha to make it easier to be an outlier.
#'
#'
#' - The IQR outlier detection method is used in `forecast::tsoutliers()`.
#' - A similar outlier detection method is used by Twitter's `AnomalyDetection` package.
#' - Both Twitter and Forecast tsoutliers methods have been implemented in Business Science's `anomalize`
#'  package.
#'
#'
#' @references
#' 1. CLEVELAND, R. B., CLEVELAND, W. S., MCRAE, J. E., AND TERPENNING, I.
#'  STL: A Seasonal-Trend Decomposition Procedure Based on Loess.
#'  Journal of Official Statistics, Vol. 6, No. 1 (1990), pp. 3-73.
#'
#' 2. Owen S. Vallis, Jordan Hochenbaum and Arun Kejariwal (2014).
#'  A Novel Technique for Long-Term Anomaly Detection in the Cloud. Twitter Inc.
#'
#' @examples
#' library(dplyr)
#'
#' walmart_sales_weekly %>%
#'     filter(id %in% c("1_1", "1_3")) %>%
#'     group_by(id) %>%
#'     anomalize(Date, Weekly_Sales)
#'
#' @name anomalize
#' @export
#'
anomalize <- function(
        .data,
        .date_var,
        .value,

        .frequency = "auto",
        .trend = "auto",

        method = "stl",

        .iqr_alpha = 0.05,
        .clean_alpha = 0.75,
        .max_anomalies = 0.2,

        .message = TRUE
) {

    # Tidyeval Setup
    date_var_expr <- rlang::enquo(.date_var)
    value_expr    <- rlang::enquo(.value)

    # Checks
    if (!is.data.frame(.data)) {
        stop(call. = FALSE, "anomalize(.data) is not a data-frame or tibble. Please supply a data.frame or tibble.")
    }
    if (rlang::quo_is_missing(date_var_expr)) {
        stop(call. = FALSE, "anomalize(.date_var) is missing. Please supply a date or date-time column.")
    }
    if (rlang::quo_is_missing(value_expr)) {
        stop(call. = FALSE, "anomalize(.value) is missing. Please supply a numeric column.")
    }

    if (.method != "stl") {
        stop(call. = FALSE, "anomalize(.method): Only 'stl' is currently implemented.")
    }

    UseMethod("anomalize", .data)

}


#' @export
anomalize.data.frame <- function(
    .data,
    .date_var,
    .value,

    .frequency = "auto",
    .trend = "auto",

    method = "stl",

    .iqr_alpha = 0.05,
    .clean_alpha = 0.75,
    .max_anomalies = 0.2,

    .message = TRUE
) {

    # STL Decomposition (observed, season, trend, remainder, seasadj)
    ret <- .data %>%
        tk_stl_diagnostics(
            .date_var  = !! rlang::enquo(.date_var),
            .value     = !! rlang::enquo(.value),
            .frequency = .frequency,
            .trend     = .trend,
            .message   = .message
        )

    # Detect Anomalies (remainder_l1, remainder_l2, anomaly)
    ret <- ret %>%
        mutate_anomalies_2(
            target    = remainder,
            alpha     = .iqr_alpha,
            max_anoms = .max_anomalies
        )

    # Recomposition
    ret <- ret %>%
        dplyr::mutate(
            recomposed_l1 = season + trend + remainder_l1,
            recomposed_l2 = season + trend + remainder_l2
        ) %>%
        dplyr::select(-remainder_l1, -remainder_l2)


    # Clean
    ret <- ret %>%
        dplyr::mutate(
            observed_cleaned = dplyr::case_when(
                anomaly_direction == -1 ~ .clean_alpha * recomposed_l1,
                anomaly_direction == 1 ~ .clean_alpha * recomposed_l2,
                TRUE ~ observed
            )
        )


    return(ret)

}

#' @export
anomalize.grouped_df <- function(
        .data,
        .date_var,
        .value,

        .frequency = "auto",
        .trend = "auto",

        method = "stl",

        .iqr_alpha = 0.05,
        .clean_alpha = 0.75,
        .max_anomalies = 0.2,

        .message = TRUE
) {

    # Tidy Eval Setup
    value_expr    <- rlang::enquo(.value)
    date_var_expr <- rlang::enquo(.date_var)
    group_names   <- dplyr::group_vars(.data)

    # Process groups individually
    .data %>%
        tidyr::nest() %>%
        dplyr::mutate(nested.col = purrr::map(
            .x         = data,
            .f         = function(df) anomalize(
                .data          = df,
                .date_var      = !! date_var_expr,
                .value         = !! value_expr,
                .frequency     = .frequency,
                .trend         = .trend,
                .method        = .method,
                .iqr_alpha     = .iqr_alpha,
                .clean_alpha   = .clean_alpha,
                .max_anomalies = .max_anomalies,
                .message       = .message
            )
        )) %>%
        dplyr::select(-"data") %>%
        tidyr::unnest(cols = nested.col) %>%
        dplyr::group_by_at(.vars = group_names)

}

# UTILITIES ----

mutate_anomalies_2 <- function(data, target, alpha = 0.05, max_anoms = 0.20) {

    # Checks
    if (missing(target)) stop('Error in anomalize(): argument "target" is missing, with no default', call. = FALSE)

    # Setup
    target_expr <- rlang::enquo(target)
    x           <- data %>% dplyr::pull(!! target_expr)

    # Explicitly call functions
    outlier_tbl <- iqr_2(x = x, alpha = alpha, max_anoms = max_anoms, verbose = TRUE)

    ret <- data %>% dplyr::bind_cols(outlier_tbl)

    return(ret)

}


# This is anomalize::iqr()
iqr_2 <- function(x, alpha = 0.05, max_anoms = 0.2, verbose = FALSE) {

    quantile_x <- stats::quantile(x, prob = c(0.25, 0.75), na.rm = TRUE)
    iq_range   <- quantile_x[[2]] - quantile_x[[1]]
    limits     <- quantile_x + (0.15 / alpha) * iq_range * c(-1, 1)

    outlier_idx      <- ((x < limits[1]) | (x > limits[2]))

    centerline <- sum(limits) / 2
    score <- abs(x - centerline)

    outlier_reported <- dplyr::case_when(
        x < limits[1] ~ "Yes",
        x > limits[2] ~ "Yes",
        TRUE ~ "No"
    )

    direction <- dplyr::case_when(
        x < limits[1] ~ -1,
        x > limits[2] ~ 1,
        TRUE ~ 0
    )

    remainder_l1 <- limits[1]
    remainder_l2 <- limits[2]

    return(
        tibble::tibble(
            anomaly = outlier_reported,
            anomaly_direction = direction,
            anomaly_score = score,
            remainder_l1 = remainder_l1,
            remainder_l2 = remainder_l2
        )
    )

}

