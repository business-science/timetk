#' Tidying methods for Nural Network Time Series models
#'
#' These methods tidy the coefficients of NNETAR models of univariate time
#' series.
#'
#' @param x An object of class "nnetar"
#' @param data Used with `sw_augment` only.
#' `NULL` by default which simply returns augmented columns only.
#' User can supply the original data, which returns the data + augmented columns.
#' @param index_rename Used with `sw_augment` only.
#' A string representing the name of the index generated.
#'
#'
#' @seealso [nnetar()]
#'
#' @examples
#' library(forecast)
#' library(sweep)
#'
#' fit_nnetar <- lynx %>%
#'     nnetar()
#'
#' sw_tidy(fit_nnetar)
#' sw_glance(fit_nnetar)
#' sw_augment(fit_nnetar)
#'
#' @name tidiers_nnetar
NULL


#' @rdname tidiers_nnetar
#'
#' @param ... Additional parameters (not used)
#'
#' @return
#' __`sw_tidy()`__ returns one row for each model parameter,
#' with two columns:
#'   * `term`: The smoothing parameters (alpha, gamma) and the initial states
#'   (l, s0 through s10)
#'   * `estimate`: The estimated parameter value
#'
#'
#' @export
sw_tidy.nnetar <- function(x, ...) {

    terms     <- c("m", "p", "P", "size")
    estimates <- c(x$m, x$p, x$P, x$size)

    ret <- tibble::tibble(term     = terms,
                          estimate = estimates)

    return(ret)
}


#' @rdname tidiers_nnetar
#'
#' @return
#' __`sw_glance()`__ returns one row with the columns
#' * `model.desc`: A description of the model including the
#'   three integer components (p, d, q) are the AR order,
#'   the degree of differencing, and the MA order.
#' * `sigma`: The square root of the estimated residual variance
#' * `logLik`: The data's log-likelihood under the model (`NA`)
#' * `AIC`: The Akaike Information Criterion (`NA`)
#' * `BIC`: The Bayesian Information Criterion (`NA`)
#' * `ME`: Mean error
#' * `RMSE`: Root mean squared error
#' * `MAE`: Mean absolute error
#' * `MPE`: Mean percentage error
#' * `MAPE`: Mean absolute percentage error
#' * `MASE`: Mean absolute scaled error
#' * `ACF1`: Autocorrelation of errors at lag 1
#'
#' @export
sw_glance.nnetar <- function(x, ...) {

    # Model description
    ret_1 <- tibble::tibble(model.desc = x$method)

    # Summary statistics
    ret_2 <- tibble::tibble(sigma  = sqrt(mean((x$residuals)^2, na.rm = TRUE)),
                            logLik = NA,
                            AIC    = NA,
                            BIC    = NA)

    # forecast accuracy
    ret_3 <- tibble::as_tibble(forecast::accuracy(x))

    ret <- dplyr::bind_cols(ret_1, ret_2, ret_3)

    return(ret)

}


#' @rdname tidiers_nnetar
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
sw_augment.nnetar <- function(x, data = NULL, index_rename = "index", ...) {

    ret <- suppressWarnings(
        sw_tbl(cbind(.actual = x$x, .fitted = x$fitted, .resid = x$residuals),
               index_rename = index_rename)
    )

    ret <- sw_augment_columns(ret, data, index_rename)

    return(ret)

}
