#' Decomposes time-series objects / models into tibbles.
#'
#' @param model A time-series model of class `ets`, `stl`.
#' @param index_rename Enables the index column to be renamed.
#' @param ... Additional parameters passed to the [tibble::as_tibble()] function.
#'
#' @return Returns a `tibble` object.
#'
#' @details `sw_decompose` is designed
#' to decompose various time-series objects from Rob Hyndman's excellent `forecast` package
#' to `tibble` objects.
#' A regularized time index is always constructed. If no time index is
#' detected, a sequential index is returned as a default.
#' The index column name can be changed using the `index_rename` argument.
#'
#' @examples
#' library(forecast)
#' library(sweep)
#'
#' # Decompose ETS model
#' USAccDeaths %>%
#'     ets() %>%
#'     sw_decompose()
#'
#'
#' @export
sw_decompose <- function(model, index_rename = "index", ...) {
    UseMethod("sw_decompose", model)
}

#' @export
sw_decompose.ets <- function(model, index_rename = "index", ...) {

    # Get tibbles from ets model
    # ref: plot.ets
    # https://github.com/robjhyndman/forecast/blob/master/R/ets.R
    if(!is.null(model$lambda))
        y <- forecast::BoxCox(model$x, model$lambda)
    else
        y <- model$x
    if(model$components[3]=="N" & model$components[2]=="N")
    {
        ret <- cbind(observed=y, level=model$states[,1])
    }
    else if(model$components[3]=="N")
    {
        ret <- cbind(observed=y, level=model$states[,1], slope=model$states[,"b"])
    }
    else if(model$components[2]=="N")
    {
        ret <- cbind(observed=y, level=model$states[,1], season=model$states[,"s1"])
    }
    else
    {
        ret <- cbind(observed=y, level=model$states[,1], slope=model$states[,"b"],
                   season=model$states[,"s1"])
    }

    # Coerce to tibble
    ret <- suppressMessages(suppressWarnings(
        sw_tbl(ret, preserve_index = TRUE, index_rename, ...)
    ))

    # Validate indexes
    ret_has_index <- index_rename %in% colnames(ret)

    # If no index, drop index columns and auto.index
    if (!ret_has_index) {
        ret_auto_index <- 1:nrow(ret)
        ret <- ret %>%
            tibble::add_column(index = ret_auto_index)
    }

    # Rearrange index
    ret <- ret %>%
        dplyr::select_(index_rename, "dplyr::everything()")

    return(ret)
}

#' @export
sw_decompose.default <- function(model, index_rename = "index", ...) {
    warning(paste0("`sw_decompose` function does not support class ", class(model)[[1]],
                   "."))
    return(model)
}
