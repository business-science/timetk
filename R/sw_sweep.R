#' Turn forecast objects into tibbles.
#'
#' @param forecast A time-series forecast of class `forecast`.
#' @param index_rename Enables the index column to be renamed.
#' @param ... Additional parameters passed to the [tibble::as_tibble()] function.
#'
#' @return Returns a `tibble` object.
#'
#' @details `sw_sweep` is designed
#' to coerce `forecast` objects from Rob Hyndman's excellent `forecast` package
#' to `tibble` objects. The returned object contains both the actual values
#' and the forecasted values including the point forecast and upper and lower
#' confidence intervals.
#' A regularized time index is always constructed. If no time index is
#' detected, a sequential index is returned as a default.
#' The index column name can be changed using the `index_rename` argument.
#'
#' @examples
#' library(forecast)
#' library(sweep)
#'
#' # ETS forecasts
#' USAccDeaths %>%
#'     ets() %>%
#'     forecast(level = c(80, 95, 99)) %>%
#'     sw_sweep()
#'
#'
#' @export
sw_sweep <- function(forecast, index_rename = "index", ...) {
    UseMethod("sw_sweep", forecast)
}

#' @export
sw_sweep.forecast <- function(forecast, index_rename = "index", ...) {

    # Get tibbles from forecast model
    ret_x     <- sw_tbl(forecast$x, preserve_index = TRUE, index_rename, ...)
    ret_mean  <- sw_tbl(forecast$mean, preserve_index = TRUE, index_rename, ...)
    ret_upper <- sw_tbl(forecast$upper, preserve_index = FALSE, ...)
    ret_lower <- sw_tbl(forecast$lower, preserve_index = FALSE, ...)

    # Add key column
    ret_x <- ret_x %>%
        tibble::add_column(key = rep("Actual", nrow(.)))
    ret_mean <- ret_mean %>%
        tibble::add_column(key = rep("Forecast", nrow(.)))

    # Fix colnames
    colnames(ret_upper) <- stringr::str_c("hi.", colnames(ret_upper)) %>%
        stringr::str_replace(pattern = "%", replacement = "")
    colnames(ret_lower) <- stringr::str_c("lo.", colnames(ret_lower)) %>%
        stringr::str_replace(pattern = "%", replacement = "")

    # Combine into forecast
    ret_fcast <- dplyr::bind_cols(ret_mean, ret_lower, ret_upper)

    # Validate indexes
    ret_x_has_index <- index_rename %in% colnames(ret_x)
    ret_fcast_has_index <- index_rename %in% colnames(ret_fcast)

    # If no index, drop index columns and auto.index
    if (!ret_x_has_index || ! ret_fcast_has_index) {
        if (ret_x_has_index) ret_x <- dplyr::select(ret_x, -1)
        if (ret_fcast_has_index) ret_fcast <- dplyr::select(ret_fcast, -1)
        ret_x_auto_index <- 1:nrow(ret_x)
        ret_fcast_auto_index <- seq(from = nrow(ret_x) + 1, length.out = nrow(ret_fcast))
        ret_x <- ret_x%>%
            tibble::add_column(index = ret_x_auto_index)
        ret_fcast <- ret_fcast %>%
            tibble::add_column(index = ret_fcast_auto_index)
    }

    # Make column names containing values same
    colnames(ret_fcast)[[2]] = colnames(ret_x)[[2]]

    # Bind Rows
    ret <- dplyr::bind_rows(ret_x, ret_fcast) %>%
        dplyr::select_(index_rename, "key", "dplyr::everything()")

    return(ret)
}

#' @export
sw_sweep.default <- function(forecast, index_rename = "index", ...) {
    warning(paste0("`sw_sweep` function does not support class ", class(forecast)[[1]],
                   ". Object must inherit forecast class."))
    return(forecast)
}
