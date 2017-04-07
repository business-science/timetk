#' Coerce time-series objects to tibble.
#'
#' @param data A time-series object.
#' @param preserve_index Attempts to preserve a time series index. Default is `TRUE`.
#' @param index_rename Enables the index column to be renamed.
#' @param ... Additional parameters passed to the [tibble::as_tibble()] function.
#'
#' @return Returns a `tibble` object.
#'
#' @details `sw_tbl` is designed
#' to coerce time series objects (e.g. `xts`, `zoo`, `ts`, `timeSeries`, etc)
#' to `tibble` objects. The main advantage is that the function attempts to keep the
#' date / date-time information from the underlying time-series object.
#' When `preserve_index = TRUE` is specified, a new column,
#' `index`, is created during object coercion, and the function attempts to preserve
#' the date or date-time information. The date / date-time column name
#' can be changed using the `index_rename` argument.
#'
#' @seealso [sw_xts()], [sw_zoo()], [sw_zooreg()], [sw_ts()]
#'
#' @examples
#' ### ts to tibble: Comparison between as.data.frame() and sw_tbl()
#' data_ts <- ts(1:10, start = c(2015,1), freq = 4)
#'
#' # No index
#' as.data.frame(data_ts)
#'
#' # Regularized numeric index starting in 2015
#' sw_tbl(data_ts)
#'
#'
#' ### zooreg to tibble: Comparison between as.data.frame() and sw_tbl()
#' data_zooreg <- zoo::zooreg(1:8, start = zoo::yearqtr(2000), frequency = 4)
#'
#' # Dates are character class stored in row names
#' as.data.frame(data_zooreg)
#'
#' # Dates are appropriate zoo yearqtr class within the data frame
#' sw_tbl(data_zooreg)
#'
#'
#' ### zoo to tibble: Comparison between as.data.frame() and sw_tbl()
#' data_zoo <- zoo::zoo(1:12, zoo::yearmon(2016 + seq(0, 11)/12))
#'
#' # Dates are character class stored in row names
#' as.data.frame(data_zoo)
#'
#' # Dates are appropriate zoo yearmon class within the data frame
#' sw_tbl(data_zoo)
#'
#'
#' ### xts to tibble: Comparison between as.data.frame() and sw_tbl()
#' data_xts <- xts::as.xts(
#'     x        = matrix(c(1:5 * 10, 5:1 * rnorm(1)), ncol = 2),
#'     order.by = seq.Date(from       = as.Date("2010-01-01"),
#'                         by         = 1,
#'                         length.out = 5)
#'     )
#' colnames(data_xts) <- c("x", "y")
#'
#' # Dates are character class stored in row names
#' as.data.frame(data_xts)
#'
#' # Dates are appropriate date class and within the data frame
#' sw_tbl(data_xts)
#'
#'
#' @export
sw_tbl <- function(data, preserve_index = TRUE, index_rename = "index", ...) {
    UseMethod("sw_tbl", data)
}

#' @export
sw_tbl.data.frame <- function(data, preserve_index = TRUE, index_rename = "index", ...) {

    if (preserve_index == TRUE) {

        idx <- rownames(data)

        # Detect if row.names exist beyond sequential 1:nrow(x) or null value
        if (!is.null(idx) &&
            !identical(as.character(idx), as.character(1:nrow(as.data.frame(data))))
            ) {

            ret <- data %>%
                as.data.frame() %>%
                tibble::rownames_to_column(var = index_rename) %>%
                tibble::as_tibble()

            ret <- suppressMessages(readr::type_convert(ret))

        } else {

            warning("Warning: No index to preserve. Object otherwise converted to tibble successfully.")
            ret <- tibble::as_tibble(data, ...)
        }

    } else {

        ret <- tibble::as_tibble(data, ...)

    }

    return(ret)

}

#' @export
sw_tbl.xts <- function(data, preserve_index = TRUE, index_rename = "index", ...) {

    # Coerce to zoo, then to tbl
    ret <- sw_tbl(zoo::as.zoo(data), preserve_index, index_rename, ...)
    return(ret)
}

#' @export
sw_tbl.matrix <- function(data, preserve_index = TRUE, index_rename = "index", ...) {

    # Coerce to data frame, then to tbl
    ret <- sw_tbl(as.data.frame(data), preserve_index, index_rename, ...)
    return(ret)

}

#' @export
sw_tbl.zoo <- function(data, preserve_index = TRUE, index_rename = "index", ...) {

    if (preserve_index == TRUE) {

        if (!is.null(zoo::index(data))) {
            idx <- zoo::index(data)
        } else {
            idx = NULL
        }

        # Detect if row.names exist beyond sequential 1:nrow(x) or null value
        if (!is.null(idx) &&
            !identical(as.character(idx), as.character(1:nrow(as.data.frame(data))))
            ) {

            ret <- tibble::as_tibble(data, ...) %>%
                tibble::add_column(idx, .before = 1)

            if (!is.null(index_rename)) colnames(ret)[[1]] <- index_rename

            ret <- suppressMessages(readr::type_convert(ret))


        } else {

            warning(paste0("Warning: No index to preserve. ",
                           "Object otherwise converted to tibble successfully."))
            ret <- tibble::as_tibble(data, ...)
        }

    } else {

        ret <- tibble::as_tibble(data, ...)

    }

    return(ret)
}

# zooreg handled via zoo

#' @export
sw_tbl.ts <- function(data, preserve_index = TRUE, index_rename = "index", ...) {

    # Coerce to zoo then convert to tibble
    ret <- sw_tbl(zoo::as.zoo(data), preserve_index, index_rename, ...)
    return(ret)
}

# mts handled via ts class


#' @export
sw_tbl.msts <- function(data, preserve_index = TRUE, index_rename = "index", ...) {

    # Coerce to zoo then convert to tibble
    ret <- sw_tbl(zoo::as.zoo(data), preserve_index, index_rename, ...)
    return(ret)
}

#' @export
sw_tbl.timeSeries <- function(data, preserve_index = TRUE, index_rename = "index", ...) {

    # Coerce to data frame, then to tbl (No index to coerce to zoo)
    ret <- sw_tbl(as.data.frame(data), preserve_index, index_rename, ...)
    return(ret)
}

#' @export
sw_tbl.irts <- function(data, preserve_index = TRUE, index_rename = "index", ...) {

    # Coerce to zoo then convert to tibble
    ret <- sw_tbl(zoo::as.zoo(data), preserve_index, index_rename, ...)
    return(ret)
}


#' @export
sw_tbl.default <- function(data, preserve_index = TRUE, index_rename = "index", ...) {

    tryCatch({
        # Attempt coercion to data.frame, then to tbl
        ret <- sw_tbl(as.data.frame(data), preserve_index, index_rename, ...)
        return(ret)
    }, error = function(e) {
        warning(paste0("Coercion unsuccessful. Function does not support class ", class(data)[[1]], ". ", e))
        return(data)
    })

}
