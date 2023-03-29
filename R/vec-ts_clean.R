#' Replace Outliers & Missing Values in a Time Series
#'
#' This is mainly a wrapper for the outlier cleaning function,
#' `tsclean()`, from the `forecast` R package.
#' The `ts_clean_vec()` function includes arguments for applying
#' seasonality to numeric vector (non-`ts`) via the `period` argument.
#'
#' @param x A numeric vector.
#' @param period A seasonal period to use during the transformation. If `period = 1`,
#'  seasonality is not included and `supsmu()` is used to fit a trend.
#'  If `period > 1`, a robust STL decomposition is
#'  first performed and a linear interpolation is applied to the seasonally adjusted data.
#' @param lambda A box cox transformation parameter. If set to `"auto"`, performs
#'  automated lambda selection.
#'
#' @return
#' A `numeric` vector with the missing values and/or anomalies transformed to imputed values.
#'
#' @details
#'
#' __Cleaning Outliers__
#'
#' 1. Non-Seasonal (`period = 1`): Uses `stats::supsmu()`
#' 2. Seasonal (`period > 1`): Uses `forecast::mstl()` with `robust = TRUE` (robust STL decomposition)
#'  for seasonal series.
#'
#' To estimate missing values and outlier replacements, linear interpolation is used on the
#' (possibly seasonally adjusted) series. See `forecast::tsoutliers()` for the outlier detection method.
#'
#'
#' __Box Cox Transformation__
#'
#' In many circumstances, a Box Cox transformation can help. Especially if the series is multiplicative
#' meaning the variance grows exponentially. A Box Cox transformation can be automated by setting `lambda = "auto"`
#' or can be specified by setting `lambda = numeric value`.
#'
#' @seealso
#'   - Box Cox Transformation: [box_cox_vec()]
#'   - Lag Transformation: [lag_vec()]
#'   - Differencing Transformation: [diff_vec()]
#'   - Rolling Window Transformation: [slidify_vec()]
#'   - Loess Smoothing Transformation: [smooth_vec()]
#'   - Fourier Series: [fourier_vec()]
#'   - Missing Value Imputation for Time Series: [ts_impute_vec()]
#'   - Outlier Cleaning for Time Series: [ts_clean_vec()]
#'
#' @references
#' - [Forecast R Package](https://github.com/robjhyndman/forecast)
#' - [Forecasting Principles & Practices: Dealing with missing values and outliers](https://otexts.com/fpp2/missing-outliers.html)
#'
#' @examples
#' library(dplyr)
#' library(timetk)
#'
#'
#' # --- VECTOR ----
#'
#' values <- c(1,2,3, 4*2, 5,6,7, NA, 9,10,11, 12*2)
#' values
#'
#' # Linear interpolation + Outlier Cleansing
#' ts_clean_vec(values, period = 1, lambda = NULL)
#'
#' # Seasonal Interpolation: set period = 4
#' ts_clean_vec(values, period = 4, lambda = NULL)
#'
#' # Seasonal Interpolation with Box Cox Transformation (internal)
#' ts_clean_vec(values, period = 4, lambda = "auto")
#'
#'
#' @name ts_clean_vec
#' @export
NULL

#' @rdname ts_clean_vec
#' @export
ts_clean_vec <- function(x, period = 1, lambda = NULL) {

    if (!is.numeric(x)) rlang::abort("Non-numeric data detected. 'x' must be numeric.")

    x_ts        <- tk_ts(x, frequency = period)

    # Treat NA as NULL (Needed for step_ts_clean)
    if (!is.null(lambda)) {
        if (is.na(lambda)) lambda <- NULL
    }

    x_ts_clean <- forecast::tsclean(x = x_ts, lambda = lambda, replace.missing = TRUE)

    as.numeric(x_ts_clean)
}
