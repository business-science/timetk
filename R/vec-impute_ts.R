#' Missing Value Imputation for Time Series
#'
#' This is mainly a wrapper for the Seasonally Adjusted Missing Value Interpolation function,
#' `na.interp()`, from the `forecast` R package. This function includes arguments for applying
#' seasonality to numeric vector (non-`ts`) via the `period` argument.
#'
#' @param x A numeric vector.
#' @param period A seasonal period to use during the transformation. If `period = 1`,
#'  linear interpolation is performed. If `period > 1`, a robust STL decomposition is
#'  first performed and a linear interpolation is applied to the seasonally adjusted data.
#' @param lambda A box cox transformation parameter. If set to `"auto"`, performs
#'  automated lambda selection.
#'
#' @details
#'
#' __Linear Interpolation__
#'
#' With `period = 1`, the algorithm uses linear interpolation.
#'
#' __Seasonal Interpolation__
#'
#' For seasonal series with `period > 1`, a robust Seasonal Trend Loess (STL) decomposition is first computed.
#' Then a linear interpolation is applied to the seasonally adjusted data, and
#' the seasonal component is added back.
#'
#' @seealso
#'   - Box Cox Transformation: [box_cox_vec()]
#'   - Lag Transformation: [lag_vec()]
#'   - Differencing Transformation: [diff_vec()]
#'   - Rolling Window Transformation: [roll_apply_vec()]
#'   - Loess Smoothing Transformation: [smooth_vec()]
#'   - Fourier Series: [fourier_vec()]
#'   - Missing Value Imputation for Time Series: [impute_ts_vec()]
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
#' # Linear interpolation
#' impute_ts_vec(values, period = 1, lambda = NULL)
#'
#' # Seasonal Interpolation: set period = 4
#' impute_ts_vec(values, period = 4, lambda = NULL)
#'
#' # Seasonal Interpolation with Box Cox Transformation (internal)
#' impute_ts_vec(values, period = 4, lambda = "auto")
#'
#'
#' @name impute_ts_vec
#' @export
NULL

#' @rdname impute_ts_vec
#' @export
impute_ts_vec <- function(x, period = 1, lambda = NULL) {
    x_ts        <- tk_ts(x, frequency = period)
    linear      <- (stats::frequency(x_ts) <= 1 | sum(!is.na(x_ts)) <= 2 * stats::frequency(x))
    x_interp_ts <- forecast::na.interp(x = x_ts, lambda = lambda, linear = linear)

    as.numeric(x_interp_ts)
}
