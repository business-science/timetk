#' Tidying methods for StructTS (Error, Trend, Seasonal) / exponential smoothing
#' modeling of time series
#'
#' These methods tidy the coefficients of StructTS models of univariate time
#' series.
#'
#' @param x An object of class "StructTS"
#' @param data Used with `sw_augment` only.
#' `NULL` by default which simply returns augmented columns only.
#' User can supply the original data, which returns the data + augmented columns.
#' @param index_rename Used with `sw_augment` only.
#' A string representing the name of the index generated.
#'
#'
#' @seealso [StructTS()]
#'
#' @examples
#' library(forecast)
#' library(sweep)
#'
#' fit_StructTS <- WWWusage %>%
#'     StructTS()
#'
#' sw_tidy(fit_StructTS)
#' sw_glance(fit_StructTS)
#' sw_augment(fit_StructTS)
#'
#' @name tidiers_StructTS
NULL


#' @rdname tidiers_StructTS
#'
#' @param ... Additional parameters (not used)
#'
#' @return
#' __`sw_tidy()`__ returns one row for each model parameter,
#' with two columns:
#'   * `term`: The model parameters
#'   * `estimate`: The estimated parameter value
#'
#'
#' @export
sw_tidy.StructTS <- function(x, ...) {

    coefs <- x$coef

    ret <- tibble::tibble(term      = names(coefs),
                          estimate  = coefs)

    return(ret)
}


#' @rdname tidiers_StructTS
#'
#' @return
#' __`sw_glance()`__ returns one row with the columns
#' * `model.desc`: A description of the model including the
#'   three integer components (p, d, q) are the AR order,
#'   the degree of differencing, and the MA order.
#' * `sigma`: The square root of the estimated residual variance
#' * `logLik`: The data's log-likelihood under the model
#' * `AIC`: The Akaike Information Criterion
#' * `BIC`: The Bayesian Information Criterion
#' * `ME`: Mean error
#' * `RMSE`: Root mean squared error
#' * `MAE`: Mean absolute error
#' * `MPE`: Mean percentage error
#' * `MAPE`: Mean absolute percentage error
#' * `MASE`: Mean absolute scaled error
#' * `ACF1`: Autocorrelation of errors at lag 1
#'
#' @export
sw_glance.StructTS <- function(x, ...) {

    x <- forecast::forecast(x)

    # Model description
    ret_1 <- tibble::tibble(model.desc = x$method)

    # Summary statistics
    ret_2 <- tibble::tibble(sigma  = sqrt(mean((x$model$residuals)^2, na.rm = TRUE)),
                            logLik = x$model$loglik,
                            AIC    = -2*x$model$loglik + 2*2,
                            BIC    = -2*x$model$loglik + log(length(x$model$data))*2)

    # forecast accuracy
    ret_3 <- tibble::as_tibble(forecast::accuracy(x))

    ret <- dplyr::bind_cols(ret_1, ret_2, ret_3)

    return(ret)

}


#' @rdname tidiers_StructTS
#'
#' @return
#' __`sw_augment()`__ returns a tibble with the following time series attributes:
#'   * `index`: An index is either attempted to be extracted from the model or
#'   a sequential index is created for plotting purposes
#'   * `.actual`: The original time series
#'   * `.fitted`: The fitted values from the model
#'   * `.resid`: The residual values from the model
#'
#' @export
sw_augment.StructTS <- function(x, data = NULL, index_rename = "index", ...) {

    x <- forecast(x)

    ret <- suppressWarnings(
        sw_tbl(cbind(.actual = x$x, .fitted = x$fitted, .resid = x$residuals),
               index_rename = index_rename)
    )

    ret <- sw_augment_columns(ret, data, index_rename)

    return(ret)

}
