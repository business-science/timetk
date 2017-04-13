#' Turn forecast objects into tibbles.
#'
#' @param x A time-series forecast of class `forecast`.
#' @param .fitted Whether or not to return the fitted value in the results
#' @param index_rename Enables the index column to be renamed.
#' @param ... Additional parameters passed to the [tibble::as_tibble()] function.
#'
#' @return Returns a `tibble` object.
#'
#' @details `sw_sweep` is designed
#' to coerce `forecast` objects from the `forecast` package
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
sw_sweep <- function(x, .fitted = TRUE, index_rename = "index", ...) {
    UseMethod("sw_sweep", x)
}

#' @export
sw_sweep.forecast <- function(x, .fitted = TRUE, index_rename = "index", ...) {

    # Get tibbles from forecast model
    ret_x     <- suppressWarnings(sw_tbl(x$x, preserve_index = TRUE, index_rename, ...))
    if (.fitted) ret_fit   <- suppressWarnings(sw_tbl(x$fitted, preserve_index = TRUE, index_rename, ...))
    ret_mean  <- suppressWarnings(sw_tbl(x$mean, preserve_index = TRUE, index_rename, ...))

    # Add key column
    ret_x <- ret_x %>%
        tibble::add_column(key = rep("Actual", nrow(.)))
    if (.fitted) {
        ret_fit <- ret_fit %>%
            tibble::add_column(key = rep("Forecast", nrow(.)))
    }
    ret_mean <- ret_mean %>%
        tibble::add_column(key = rep("Forecast", nrow(.)))

    ret_fcast <- ret_mean
    if (!is.null(x$level)) {
        # If levels, add columns to forecast
        ret_upper <- suppressWarnings(sw_tbl(x$upper, preserve_index = FALSE, ...))
        ret_lower <- suppressWarnings(sw_tbl(x$lower, preserve_index = FALSE, ...))
        # Fix colnames
        colnames(ret_upper) <- stringr::str_c("hi.", x$level)
        colnames(ret_lower) <- stringr::str_c("lo.", x$level)
        # Combine into forecast
        ret_fcast <- dplyr::bind_cols(ret_mean, ret_lower, ret_upper)
    }

    # Validate indexes
    ret_x_has_index <- index_rename %in% colnames(ret_x)
    if (.fitted) {
        ret_fit_has_index <- index_rename %in% colnames(ret_fit)
    } else {
        ret_fit_has_index <- TRUE
    }
    ret_fcast_has_index <- index_rename %in% colnames(ret_fcast)

    # If no index, drop index columns and auto.index
    if (!ret_x_has_index || !ret_fcast_has_index || !ret_fit_has_index) {

        if (ret_x_has_index) ret_x <- dplyr::select(ret_x, -1)
        if (.fitted) {
            if (ret_fit_has_index) ret_fit <- dplyr::select(ret_fcast, -1)
        }
        if (ret_fcast_has_index) ret_fcast <- dplyr::select(ret_fcast, -1)

        ret_x_auto_index <- 1:nrow(ret_x)
        if (.fitted)ret_fit_auto_index <- 1:nrow(ret_fit)
        ret_fcast_auto_index <- seq(from = nrow(ret_x) + 1, length.out = nrow(ret_fcast))

        ret_x <- ret_x %>%
            tibble::add_column(index = ret_x_auto_index)
        if (.fitted) {
            ret_fit <- ret_fit %>%
                tibble::add_column(index = ret_fit_auto_index)
        }
        ret_fcast <- ret_fcast %>%
            tibble::add_column(index = ret_fcast_auto_index)
    }

    # Make column names containing values same
    if (.fitted) colnames(ret_fit)[[2]]   = colnames(ret_x)[[2]]
    colnames(ret_fcast)[[2]] = colnames(ret_x)[[2]]

    # Bind Rows
    ret <- ret_x
    if (.fitted) {
        ret <- dplyr::bind_rows(ret_x, ret_fit) %>%
            dplyr::select_(index_rename, "key", "dplyr::everything()")
    }
    ret <- dplyr::bind_rows(ret, ret_fcast) %>%
        dplyr::select_(index_rename, "key", "dplyr::everything()")

    return(ret)
}

#' @export
sw_sweep.default <- function(x, .fitted = TRUE, index_rename = "index", ...) {
    warning(paste0("`sw_sweep` function does not support class ", class(x)[[1]],
                   ". Object must inherit forecast class."))
    return(x)
}
