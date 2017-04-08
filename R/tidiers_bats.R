#' Tidying methods for BATS and TBATS modeling of time series
#'
#' These methods tidy the coefficients of BATS models of univariate time
#' series.
#'
#' @param x An object of class "bats" or "tbats"
#'
#'
#' @seealso [bats()], [tbats()]
#'
#' @examples
#' library(forecast)
#' library(sweep)
#'
#' fit_bats <- WWWusage %>%
#'     bats()
#'
#' sw_tidy(fit_bats)
#' sw_glance(fit_bats)
#' sw_augment(fit_bats)
#'
#' @name tidiers_bats
NULL


#' @rdname tidiers_bats
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
sw_tidy.bats <- function(x, ...) {

    n <- which(stringr::str_detect(names(x), "likelihood")) - 1

    # Collect terms up to but not including likelihood
    x_subset <- x[1:n]

    ret_1 <- tibble::tibble(term = names(x_subset))
    ret_2 <- data.frame(estimate = unlist(x_subset)) %>%
        tibble::rownames_to_column(var = "term")

    ret <- dplyr::left_join(ret_1, ret_2, by = "term")

    return(ret)
}


#' @rdname tidiers_bats
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
sw_glance.bats <- function(x, ...) {

    # Model description
    # ret_1 <- tibble::tibble(model.desc = x$method)
    if (inherits(x, "tbats")) {
        ret_1 <- tibble::tibble(model.desc = "TBATS")
    } else {
        ret_1 <- tibble::tibble(model.desc = "BATS")
    }

    # Summary statistics
    ret_2 <- tibble::tibble(sigma  = sqrt(x$variance),
                            logLik = x$likelihood,
                            AIC    = x$AIC,
                            BIC    = NA)

    # forecast accuracy
    ret_3 <- tibble::as_tibble(forecast::accuracy(x))

    ret <- dplyr::bind_cols(ret_1, ret_2, ret_3)

    return(ret)

}


#' @rdname tidiers_bats
#'
#' @return
#' __`sw_augment()`__ returns a tibble with the following time series attributes:
#'   * `x`: The original time series
#'   * `.fitted`: The fitted values from the model
#'   * `.resid`: The residual values from the model
#'
#' @export
sw_augment.bats <- function(x, ...) {

    ret <- sw_tbl(cbind(.actual = x$y, .fitted = x$fitted.values, .resid = x$errors))

    return(ret)

}
