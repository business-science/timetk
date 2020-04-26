#' Missing Value Imputation for Time Series
#'
#' This is mainly a wrapper for the Seasonally Adjusted Missing Value using Linear Interpolation function,
#' `na.interp()`, from the `forecast` R package. The `ts_impute_vec()` function includes arguments for applying
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
#' __Imputation using Linear Interpolation__
#'
#' Three circumstances cause strictly linear interpolation:
#'
#'   1. __Period is 1:__ With `period = 1`, a seasonality cannot be interpreted and therefore linear is used.
#'   2. __Number of Non-Missing Values is less than 2-Periods__: Insufficient values exist to detect seasonality.
#'   3. __Number of Total Values is less than 3-Periods__: Insufficient values exist to detect seasonality.
#'
#' __Seasonal Imputation using Linear Interpolation__
#'
#' For seasonal series with `period > 1`, a robust Seasonal Trend Loess (STL) decomposition is first computed.
#' Then a linear interpolation is applied to the seasonally adjusted data, and
#' the seasonal component is added back.
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
#' ts_impute_vec(values, period = 1, lambda = NULL)
#'
#' # Seasonal Interpolation: set period = 4
#' ts_impute_vec(values, period = 4, lambda = NULL)
#'
#' # Seasonal Interpolation with Box Cox Transformation (internal)
#' ts_impute_vec(values, period = 4, lambda = "auto")
#'
#'
#' @name ts_impute_vec
#' @export
NULL

#' @rdname ts_impute_vec
#' @export
ts_impute_vec <- function(x, period = 1, lambda = NULL) {

    x_ts        <- tk_ts(x, frequency = period)

    # Treat NA as NULL (Needed for step_ts_impute)
    if (!is.null(lambda)) {
        if (is.na(lambda)) lambda <- NULL
    }

    # Use strictly linear interpolation when any of the following conditions exist:
    # 1. Period is 1
    # 2. Number of available values is less than or equal to twice the period
    # 3. Series contains less than two periods
    linear <- FALSE
    if ({
        period <= 1 |
        sum(!is.na(x_ts)) <= 2 * period |
        length(x) / period <= 3
        }) {

        linear <- TRUE

    }

    x_interp_ts <- forecast::na.interp(x = x_ts, lambda = lambda, linear = linear)

    as.numeric(x_interp_ts)
}
