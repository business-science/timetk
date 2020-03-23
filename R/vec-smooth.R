#' Smoothing Transformation using Loess (Local Polynomial Regressing Fitting)
#'
#' `smooth_vec()` applies a LOESS transformation to a numeric vector.
#'
#'
#' @param x A numeric vector to have a smoothing transformation applied.
#' @param period The number of periods to include in the local smoothing.
#'  Similar to window size for a moving average.
#'  See details for an explanation `period` vs `span` specification.
#' @param span The span is a percentage of data to be included
#'  in the smoothing window. Period is preferred for shorter windows
#'  to fix the window size.
#'  See details for an explanation `period` vs `span` specification.
#' @param degree The degree of the polynomials to be used.
#'  Set to 2 by default for 2nd order polynomial.
#'
#' @return A numeric vector
#'
#' @details
#'
#' __Benefits:__
#'
#' - When using `period`, the effect is
#'  __similar to a moving average without creating missing values.__
#' - When using `span`, the effect is to detect the trend in a series
#'
#' __smooth_vec Algorithm__
#' This function is a simplified wrapper for the `stats::loess()`
#' with a modification to set a fixed `period` rather than a percentage of
#' data points via a `span`.
#'
#' __Why Period vs Span?__
#' The `period` is fixed whereas the `span` changes as the number of observations change.
#'
#' __When to use Period?__
#' The effect of using a `period` is similar to a Moving Average where the Window Size
#' is the ___Fixed Period___. This helps when you are trying to smooth local trends.
#' If you want a 30-day moving average, specify `period = 30`.
#'
#'  __When to use Span?__
#'  Span is easier to specify when you want a ___Long-Term Trendline___ where the
#'  window size is unknown. You can specify `span = 0.75` to locally regress
#'  using a window of 75% of the data.
#'
#'
#' @examples
#' library(tidyverse)
#' library(tidyquant)
#' library(timetk)
#'
#' # Training Data
#' FB_tbl <- FANG %>%
#'     filter(symbol == "FB") %>%
#'     select(symbol, date, adjusted)
#'
#' # ---- PERIOD ----
#'
#' FB_tbl %>%
#'     mutate(adjusted_30 = smooth_vec(adjusted, period = 30)) %>%
#'     ggplot(aes(date, adjusted)) +
#'     geom_line() +
#'     geom_line(aes(y = adjusted_30), color = "red")
#'
#' # ---- SPAN ----
#'
#' FB_tbl %>%
#'     mutate(adjusted_30 = smooth_vec(adjusted, span = 0.75)) %>%
#'     ggplot(aes(date, adjusted)) +
#'     geom_line() +
#'     geom_line(aes(y = adjusted_30), color = "red")
#'
#'
#' @seealso
#'   - [slider::slide_vec] for rolling window calculations
#'   - [zoo::rollapply()] for rolling window calculations
#'   - [stats::loess()] for the loess function used
#'
#'
#' @export
smooth_vec <- function(x, period = 30, span = NULL, degree = 2) {
    UseMethod("smooth_vec", x)
}

#' @export
smooth_vec.default <- function(x, period = 30, span = NULL, degree = 2) {
    stop(paste0("smooth_vec: No method for class ", class(x)[[1]], "."), call. = FALSE)
}

#' @export
smooth_vec.double <- function(x, period = 30, span = NULL, degree = 2) {
    loess_smooth(x, period, span, degree)
}

#' @export
smooth_vec.integer <- function(x, period = 30, span = NULL, degree = 2) {
    loess_smooth(x, period, span, degree)
}

loess_smooth <- function(x, period, span, degree) {

    # Span Calc
    if (is.null(span)) {
        span <- period / length(x)
    }

    # Model
    model_loess <- stats::loess(x ~ seq_along(x), span = span, degree = degree)

    # Predict
    stats::predict(model_loess, 1:length(x))
}
