#' Box Cox Transformation
#'
#' This is mainly a wrapper for the BoxCox transformation from the `forecast`
#' R package. The `box_cox_vec()` function performs the transformation.
#' `box_cox_inv_vec()` inverts the transformation.
#' `auto_lambda()` helps in selecting the optimal `lambda` value.
#'
#' @param x A numeric vector.
#' @param lambda The box cox transformation parameter.
#'  If set to "auto", performs automated lambda selection using `auto_lambda()`.
#' @param silent Whether or not to report the automated `lambda` selection as a message.
#' @param method The method used for automatic `lambda` selection.
#'  Either "guerrero" or "loglik".
#' @param lambda_lower A lower limit for automatic `lambda` selection
#' @param lambda_upper An upper limit for automatic `lambda` selection
#'
#' @return Returns a `numeric` vector that has been transformed.
#'
#' @details
#'
#' The Box Cox transformation is a power transformation that is commonly
#' used to reduce variance of a time series.
#'
#' __Automatic Lambda Selection__
#'
#' If desired, the `lambda` argument can be selected using `auto_lambda()`,
#' a wrapper for the Forecast R Package's `forecast::BoxCox.lambda()` function.
#' Use either of 2 methods:
#'
#' 1. "guerrero" - Minimizes the non-seasonal variance
#' 2. "loglik" - Maximizes the log-likelihood of a linear model fit to `x`
#'
#' @seealso
#'   - Box Cox Transformation: [box_cox_vec()]
#'   - Lag Transformation: [lag_vec()]
#'   - Differencing Transformation: [diff_vec()]
#'   - Rolling Window Transformation: [slidify_vec()]
#'   - Loess Smoothing Transformation: [smooth_vec()]
#'   - Fourier Series: [fourier_vec()]
#'   - Missing Value Imputation for Time Series: [ts_impute_vec()], [ts_clean_vec()]
#'
#' Other common transformations to reduce variance: `log()`, `log1p()` and `sqrt()`
#'
#' @references
#' - [Forecast R Package](https://github.com/robjhyndman/forecast)
#' - [Forecasting: Principles & Practices: Transformations & Adjustments](https://otexts.com/fpp2/transformations.html)
#' - Guerrero, V.M. (1993) Time-series analysis supported by power transformations. _Journal of Forecasting_, 12,  37--48.
#'
#' @examples
#' library(dplyr)
#' d10_daily <- m4_daily %>% dplyr::filter(id == "D10")
#'
#' # --- VECTOR ----
#'
#' value_bc <- box_cox_vec(d10_daily$value)
#' value    <- box_cox_inv_vec(value_bc, lambda = 1.25119350454964)
#'
#' # --- MUTATE ----
#'
#' m4_daily %>%
#'     dplyr::group_by(id) %>%
#'     dplyr::mutate(value_bc = box_cox_vec(value))
#'
#' @name box_cox_vec
NULL

#' @rdname box_cox_vec
#' @export
box_cox_vec <- function(x, lambda = "auto", silent = FALSE) {

    if (is.null(lambda) | lambda[1] == "auto") {
        lambda <- auto_lambda(x)
        if (!silent) message("box_cox_vec(): Using value for lambda: ", lambda)
    }

    if (lambda < 0) {
        x[x < 0] <- NA
    }
    if (lambda == 0) {
        log(x)
    } else {
        (sign(x) * abs(x) ^ lambda - 1) / lambda
    }

}

#' @rdname box_cox_vec
#' @export
box_cox_inv_vec <- function(x, lambda) {

    if (rlang::is_missing(lambda)) {
        rlang::abort("box_cox_inv_vec(lambda): Is missing. Please provide a value for lambda.")
    }

    if (lambda < 0) {
        x[x > -1 / lambda] <- NA
    }
    if (lambda == 0) {
        exp(x)
    } else {
        x <- x * lambda + 1
        sign(x) * abs(x) ^ (1 / lambda)
    }
}

#' @rdname box_cox_vec
#' @export
auto_lambda <- function(x, method = c("guerrero", "loglik"), lambda_lower = -1, lambda_upper = 2) {
    forecast::BoxCox.lambda(x, method = method[1], lower = lambda_lower, upper = lambda_upper)
}


