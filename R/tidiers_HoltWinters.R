#' Tidying methods for HoltWinters modeling of time series
#'
#' These methods tidy `HoltWinters` models of univariate time
#' series.
#'
#' @param x An object of class "HoltWinters"
#' @param data Used with `sw_augment` only.
#' `NULL` by default which simply returns augmented columns only.
#' User can supply the original data, which returns the data + augmented columns.
#' @param index_rename Used with `sw_augment` only.
#' A string representing the name of the index generated.
#'
#'
#' @seealso [HoltWinters()]
#'
#' @examples
#' library(forecast)
#' library(sweep)
#'
#' fit_hw <- USAccDeaths %>%
#'     stats::HoltWinters()
#'
#' sw_tidy(fit_hw)
#' sw_glance(fit_hw)
#' sw_augment(fit_hw)
#'
#' @name tidiers_HoltWinters
NULL


#' @rdname tidiers_HoltWinters
#'
#' @param ... Additional parameters (not used)
#'
#' @return
#' __`sw_tidy()`__ returns one row for each model parameter,
#' with two columns:
#'   * `term`: The various parameters (alpha, beta, gamma, and coefficients)
#'   * `estimate`: The estimated parameter value
#'
#'
#' @export
sw_tidy.HoltWinters <- function(x, ...) {

    terms     <- c("alpha", "beta", "gamma", names(stats::coef(x)))
    estimates <- c(x$alpha, x$beta, x$gamma, stats::coef(x))

    ret <- tibble::tibble(term     = terms,
                          estimate = estimates)

    return(ret)
}


#' @rdname tidiers_HoltWinters
#'
#' @return
#' __`sw_glance()`__ returns one row with the columns
#' * `model.desc`: A description of the model including the
#'   three integer components (p, d, q) are the AR order,
#'   the degree of differencing, and the MA order.
#' * `sigma`: The square root of the estimated residual variance
#' * `logLik`: The data's log-likelihood under the model
#' * `AIC`: The Akaike Information Criterion
#' * `BIC`: The Bayesian Information Criterion (`NA` for bats / tbats)
#' * `ME`: Mean error
#' * `RMSE`: Root mean squared error
#' * `MAE`: Mean absolute error
#' * `MPE`: Mean percentage error
#' * `MAPE`: Mean absolute percentage error
#' * `MASE`: Mean absolute scaled error
#' * `ACF1`: Autocorrelation of errors at lag 1
#'
#' @export
sw_glance.HoltWinters <- function(x, ...) {

    # Model description
    ret_1 <- tibble::tibble(model.desc = "HoltWinters")

    # Summary statistics
    ret_2 <- tibble::tibble(sigma  = sqrt(x$SSE),
                            logLik = NA,
                            AIC    = NA,
                            BIC    = NA)

    # forecast accuracy
    ret_3 <- tibble::as_tibble(forecast::accuracy(
        forecast::forecast(x)
    ))

    ret <- dplyr::bind_cols(ret_1, ret_2, ret_3)

    return(ret)

}


#' @rdname tidiers_HoltWinters
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
sw_augment.HoltWinters <- function(x, data = NULL, index_rename = "index", ...) {

    ret <- suppressWarnings(
        sw_tbl(cbind(.actual = x$x, .fitted = x$fitted[,1]),
               index_rename = index_rename)
        )

    ret <- ret %>%
        dplyr::mutate(.resid = .actual - .fitted)

    ret <- sw_augment_columns(ret, data, index_rename)

    return(ret)

}
