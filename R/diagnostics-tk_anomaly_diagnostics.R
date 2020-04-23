#' Automatic group-wise Anomaly Detection by STL Decomposition
#'
#' `tk_anomaly_diagnostics()` is the preprocessor for `plot_anomaly_diagnostics()`.
#' It performs automatic anomaly detection for one or more time series groups.
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
#' @param .alpha Controls the width of the "normal" range. Lower values are more conservative
#'  while higher values are less prone to incorrectly classifying "normal" observations.
#' @param .max_anomalies The maximum percent of anomalies permitted to be identified.
#' @param .message A boolean. If `TRUE`, will output information related to automatic frequency
#' and trend selection (if applicable).
#'
#' @return A `tibble` or `data.frame` with STL Decomposition Features
#'  (observed, season, trend, remainder, seasadj) and
#'  Anomaly Features (remainder_l1, remainder_l2, anomaly, recomposed_l1, and recomposed_l2)
#'
#' @details
#'
#' The `tk_anomaly_diagnostics()` method for anomaly detection that implements a 2-step process to
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
#' @seealso
#' - [plot_anomaly_diagnostics()]: Visual anomaly detection
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
#' library(timetk)
#'
#' walmart_sales_weekly %>%
#'     filter(id %in% c("1_1", "1_3")) %>%
#'     group_by(id) %>%
#'     tk_anomaly_diagnostics(Date, Weekly_Sales)
#'
#' @name tk_anomaly_diagnostics
#' @export
tk_anomaly_diagnostics <- function(.data, .date_var, .value,
                                   .frequency = "auto", .trend = "auto",
                                   .alpha = 0.05, .max_anomalies = 0.2,
                                   .message = TRUE) {

    # Tidyeval Setup
    date_var_expr <- rlang::enquo(.date_var)
    value_expr    <- rlang::enquo(.value)

    # Checks
    if (!is.data.frame(.data)) {
        stop(call. = FALSE, "tk_anomaly_diagnostics(.data) is not a data-frame or tibble. Please supply a data.frame or tibble.")
    }
    if (rlang::quo_is_missing(date_var_expr)) {
        stop(call. = FALSE, "tk_anomaly_diagnostics(.date_var) is missing. Please supply a date or date-time column.")
    }
    if (rlang::quo_is_missing(value_expr)) {
        stop(call. = FALSE, "tk_anomaly_diagnostics(.value) is missing. Please a numeric column.")
    }

    UseMethod("tk_anomaly_diagnostics", .data)



}



#' @export
#' @rdname tk_anomaly_diagnostics
tk_anomaly_diagnostics.data.frame <- function(.data, .date_var, .value,
                                              .frequency = "auto", .trend = "auto",
                                              .alpha = 0.05, .max_anomalies = 0.2,
                                              .message = TRUE) {

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
        mutate_anomalies(
            target    = remainder,
            alpha     = .alpha,
            max_anoms = .max_anomalies
        )

    # Recomposition
    ret <- ret %>%
        dplyr::mutate(
            recomposed_l1 = season + trend + remainder_l1,
            recomposed_l2 = season + trend + remainder_l2
        )

    return(ret)

}

#' @export
tk_anomaly_diagnostics.grouped_df <- function(.data, .date_var, .value,
                                              .frequency = "auto", .trend = "auto",
                                              .alpha = 0.05, .max_anomalies = 0.2,
                                              .message = TRUE) {


    # Tidy Eval Setup
    value_expr    <- rlang::enquo(.value)
    date_var_expr <- rlang::enquo(.date_var)
    group_names   <- dplyr::group_vars(.data)

    # Process groups individually
    .data %>%
        tidyr::nest() %>%
        dplyr::mutate(nested.col = purrr::map(
            .x         = data,
            .f         = function(df) tk_anomaly_diagnostics(
                .data          = df,
                .date_var      = !! date_var_expr,
                .value         = !! value_expr,
                .frequency     = .frequency,
                .trend         = .trend,
                .alpha         = .alpha,
                .max_anomalies = .max_anomalies,
                .message       = .message
            )
        )) %>%
        dplyr::select(-data) %>%
        tidyr::unnest(cols = nested.col) %>%
        dplyr::group_by_at(.vars = group_names)

}


# UTILS ----

# This is anomalize::anomalize()
mutate_anomalies <- function(data, target, alpha = 0.05, max_anoms = 0.20) {

    # Checks
    if (missing(target)) stop('Error in anomalize(): argument "target" is missing, with no default', call. = FALSE)

    # Setup
    target_expr <- rlang::enquo(target)
    x           <- data %>% dplyr::pull(!! target_expr)

    # Explicitly call functions
    outlier_list <- iqr_vec(x = x, alpha = alpha, max_anoms = max_anoms, verbose = TRUE)
    outlier      <- outlier_list$outlier
    limit_lower  <- outlier_list$critical_limits[[1]]
    limit_upper  <- outlier_list$critical_limits[[2]]

    # Returns
    ret <- data %>%
        dplyr::mutate(!! paste0(dplyr::quo_name(target_expr), "_l1") := limit_lower,
                      !! paste0(dplyr::quo_name(target_expr), "_l2") := limit_upper) %>%
        tibble::add_column(anomaly = outlier)

    return(ret)

}

# This is anomalize::iqr()
iqr_vec <- function(x, alpha = 0.05, max_anoms = 0.2, verbose = FALSE) {

    quantile_x <- stats::quantile(x, prob = c(0.25, 0.75), na.rm = TRUE)
    iq_range   <- quantile_x[[2]] - quantile_x[[1]]
    limits     <- quantile_x + (0.15 / alpha) * iq_range * c(-1, 1)

    outlier_idx      <- ((x < limits[1]) | (x > limits[2]))
    outlier_vals     <- x[outlier_idx]
    outlier_response <- ifelse(outlier_idx == TRUE, "Yes", "No")

    vals_tbl <- tibble::tibble(value = x) %>%
        tibble::rownames_to_column(var = "index") %>%
        # Establish limits and assess if outside of limits
        dplyr::mutate(
            limit_lower = limits[1],
            limit_upper = limits[2],
            abs_diff_lower = ifelse(value <= limit_lower, abs(value - limit_lower), 0),
            abs_diff_upper = ifelse(value >= limit_upper, abs(value - limit_upper), 0),
            max_abs_diff = ifelse(abs_diff_lower > abs_diff_upper, abs_diff_lower, abs_diff_upper)
        ) %>%
        dplyr::select(index, dplyr::everything()) %>%
        dplyr::select(-c(abs_diff_lower, abs_diff_upper)) %>%
        # Sort by absolute distance from centerline of limits
        dplyr::mutate(
            centerline = (limit_upper + limit_lower) / 2,
            sorting = abs(value - centerline)
        ) %>%
        dplyr::arrange(dplyr::desc(sorting)) %>%
        dplyr::select(-c(centerline, sorting)) %>%
        tibble::rownames_to_column(var = "rank") %>%
        dplyr::mutate(
            rank = as.numeric(rank),
            index = as.numeric(index)
        ) %>%
        # Identify outliers
        dplyr::arrange(dplyr::desc(max_abs_diff)) %>%
        dplyr::mutate(
            outlier = ifelse(max_abs_diff > 0, "Yes", "No"),
            below_max_anoms = ifelse(dplyr::row_number() / dplyr::n() > max_anoms,
                                     "No", "Yes"
            ),
            outlier_reported = ifelse(outlier == "Yes" & below_max_anoms == "Yes",
                                      "Yes", "No"
            ),
            direction = dplyr::case_when(
                (outlier_reported == "Yes") & (value > limit_upper) ~ "Up",
                (outlier_reported == "Yes") & (value < limit_lower) ~ "Down",
                TRUE ~ "NA"
            ),
            direction = ifelse(direction == "NA", NA, direction)
        )

    vals_tbl_filtered <- vals_tbl %>%
        dplyr::filter(below_max_anoms == "Yes") %>%
        dplyr::select(-c(max_abs_diff:below_max_anoms)) %>%
        dplyr::rename(outlier = outlier_reported)

    # Critical Limits
    if (any(vals_tbl$outlier == "No")) {
        # Non outliers identified, pick first limit
        limit_tbl <- vals_tbl %>%
            dplyr::filter(outlier == "No") %>%
            dplyr::slice(1)
        limits_vec <- c(
            limit_lower = limit_tbl$limit_lower,
            limit_upper = limit_tbl$limit_upper
        )
    } else {
        # All outliers, pick last limits
        limit_tbl <- vals_tbl %>%
            dplyr::slice(n())
        limits_vec <- c(
            limit_lower = limit_tbl$limit_lower,
            limit_upper = limit_tbl$limit_upper
        )
    }

    # Return results
    if (verbose) {
        outlier_list <- list(
            outlier = vals_tbl %>% dplyr::arrange(index) %>% dplyr::pull(outlier_reported),
            outlier_idx = vals_tbl %>% dplyr::filter(outlier_reported == "Yes") %>% dplyr::pull(index),
            outlier_vals = vals_tbl %>% dplyr::filter(outlier_reported == "Yes") %>% dplyr::pull(value),
            outlier_direction = vals_tbl %>% dplyr::filter(outlier_reported == "Yes") %>% dplyr::pull(direction),
            critical_limits = limits_vec,
            outlier_report = vals_tbl_filtered
        )
        return(outlier_list)
    } else {
        return(vals_tbl %>% dplyr::arrange(index) %>% dplyr::pull(outlier_reported))
    }
}
