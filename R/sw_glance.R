#' Construct a single row summary "glance" of a model, fit, or other
#' object
#'
#'
#' @param x model or other R object to convert to single-row data frame
#' @param ... other arguments passed to methods
#'
#' @details
#' `sw_glance` methods always return either a one-row tibble or `NULL`.
#' The single row includes summary statistics relevent to the model accuracy,
#' which can be used to assess model fit and quality. The benefit of `sw_glance`
#' is that it has methods for various time-series model classes such as
#' `HoltWinters`, `ets`, `Arima`, etc.
#'
#' For non-time series, `sw_glance()` defaults to `broom::glance()`.
#' The only difference is that the return is a tibble.
#'
#' @return single-row tibble with model summary information.
#'
#' @seealso [glance()]
#'
#' @export
sw_glance <- function(x, ...) UseMethod("sw_glance")


#' @export
sw_glance.default <- function(x, ...) {
    broom::glance(x, ...) %>%
        tibble::as_tibble()
}
