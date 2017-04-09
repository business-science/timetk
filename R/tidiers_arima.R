#' Tidying methods for ARIMA modeling of time series
#'
#' These methods tidy the coefficients of ARIMA models of univariate time
#' series.
#'
#' @param x An object of class "Arima"
#' @param data Used with `sw_augment` only.
#' `NULL` by default which simply returns augmented columns only.
#' User can supply the original data, which returns the data + augmented columns.
#' @param index_rename Used with `sw_augment` only.
#' A string representing the name of the index generated.
#'
#'
#' @seealso [arima()], [Arima()]
#'
#' @examples
#' library(forecast)
#' library(sweep)
#'
#' fit_arima <- WWWusage %>%
#'     auto.arima()
#'
#' sw_tidy(fit_arima)
#' sw_glance(fit_arima)
#' sw_augment(fit_arima)
#'
#'
#' @name tidiers_arima
NULL


#' @rdname tidiers_arima
#'
#' @param conf_level confidence level of the interval passed to `stats::confint`
#' (`sw_tidy()` only)
#' @param ... Additional parameters passed to `stats::confint` (`sw_tidy()` only)
#'
#' @return
#' __`sw_tidy()`__ returns one row for each coefficient in the model,
#' with five columns:
#'   * `term`: The term in the nonlinear model being estimated and tested
#'   * `estimate`: The estimated coefficient
#'   * `std.error`: The standard error from the linear model
#'   * `conf.low`: Low end of confidence interval
#'   * `conf.high`: High end of confidence interval
#'
#' @export
sw_tidy.Arima <- function(x, conf_level = 0.95, ...) {

    coefs <- stats::coef(x)

    # standard errors are computed as in stats:::print.Arima
    ses <- rep.int(0, length(coefs))
    ses[x$mask] <- sqrt(diag(x$var.coef))

    ret <- tibble::tibble(term      = names(coefs),
                          estimate  = coefs,
                          std.error = ses)

    # Confidence intervals are computed using stats::confint()
    ret <- cbind(ret, broom::confint_tidy(x, conf.level = conf_level, ...)) %>%
        tibble::as_tibble()

    return(ret)
}


#' @rdname tidiers_arima
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
sw_glance.Arima <- function(x, ...) {

    # Model description
    ret_1 <- tibble::tibble(model.desc = arima_string(x))

    # Summary statistics
    ret_2 <- tibble::tibble(sigma = sqrt(x$sigma2))
    ret_2 <- broom::finish_glance(ret_2, x) %>%
        tibble::as_tibble()

    # forecast accuracy
    ret_3 <- tibble::as_tibble(forecast::accuracy(x))

    ret <- dplyr::bind_cols(ret_1, ret_2, ret_3)

    return(ret)

}


#' @rdname tidiers_arima
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
sw_augment.Arima <- function(x, data = NULL, index_rename = "index", ...) {

    if ("fitted" %in% names(x)) {
        # forecast::Arima
        ret <- suppressWarnings(
            sw_tbl(cbind(.actual = x$x, .fitted = x$fitted, .resid = x$residuals),
                   index_rename = index_rename)
            )
    } else {
        # stats::Arima
        warning("No `.actual` or `.fitted` within stats::arima() models. Use forecast::Arima() if more information is needed.")
        ret <- suppressWarnings(
            sw_tbl(x$residuals, index_rename = index_rename) %>%
                dplyr::rename(.resid = value)
        )
    }

    ret <- sw_augment_columns(ret, data, index_rename)

    return(ret)

}
