#' Coerce time-series objects to tibble.
#'
#' @param data A time-series object.
#' @param preserve_index Attempts to preserve a time series index. Default is `TRUE`.
#' @param rename_index Enables the index column to be renamed.
#' @param timekit_idx Used to return a date / datetime index for
#' regularized objects that contain a timekit "index" attribute.
#' Refer to [tk_index()] for more information on returning index information
#' from regularized timeseries objects (i.e. `ts`).
#' @param silent Used to toggle printing of messages and warnings.
#' @param ... Additional parameters passed to the [tibble::as_tibble()] function.
#'
#' @return Returns a `tibble` object.
#'
#' @details `tk_tbl` is designed
#' to coerce time series objects (e.g. `xts`, `zoo`, `ts`, `timeSeries`, etc)
#' to `tibble` objects. The main advantage is that the function keeps the
#' date / date-time information from the underlying time-series object.
#'
#' When `preserve_index = TRUE` is specified, a new column,
#' `index`, is created during object coercion, and the function attempts to preserve
#' the date or date-time information. The date / date-time column name
#' can be changed using the `rename_index` argument.
#'
#' The `timekit_idx` argument is applicable when coercing `ts` objects that were
#' created using `tk_ts()` from an object that had a time base
#' (e.g. `tbl`, `xts`, `zoo`).
#' Setting `timekit_idx = TRUE` enables returning the timekit "index" attribute if present,
#' which is the original (non-regularized) time-based index.
#'
#' @seealso [tk_xts()], [tk_zoo()], [tk_zooreg()], [tk_ts()]
#'
#' @examples
#' library(tidyverse)
#' library(timekit)
#'
#' data_tbl <- tibble(
#'     date = seq.Date(from = as.Date("2010-01-01"), by = 1, length.out = 5),
#'     x    = seq(100, 120, by = 5)
#' )
#'
#'
#' ### ts to tibble: Comparison between as.data.frame() and tk_tbl()
#' data_ts <- tk_ts(data_tbl, start = c(2010,1), freq = 365)
#'
#' # No index
#' as.data.frame(data_ts)
#'
#' # Defualt index returned is regularized numeric index
#' tk_tbl(data_ts)
#'
#' # Original date index returned (Only possible if original data has time-based index)
#' tk_tbl(data_ts, timekit_idx = TRUE)
#'
#'
#' ### xts to tibble: Comparison between as.data.frame() and tk_tbl()
#' data_xts <- tk_xts(data_tbl)
#'
#' # Dates are character class stored in row names
#' as.data.frame(data_xts)
#'
#' # Dates are appropriate date class and within the data frame
#' tk_tbl(data_xts)
#'
#'
#' ### zooreg to tibble: Comparison between as.data.frame() and tk_tbl()
#' data_zooreg <- tk_zooreg(1:8, start = zoo::yearqtr(2000), frequency = 4)
#'
#' # Dates are character class stored in row names
#' as.data.frame(data_zooreg)
#'
#' # Dates are appropriate zoo yearqtr class within the data frame
#' tk_tbl(data_zooreg)
#'
#'
#' ### zoo to tibble: Comparison between as.data.frame() and tk_tbl()
#' data_zoo <- zoo::zoo(1:12, zoo::yearmon(2016 + seq(0, 11)/12))
#'
#' # Dates are character class stored in row names
#' as.data.frame(data_zoo)
#'
#' # Dates are appropriate zoo yearmon class within the data frame
#' tk_tbl(data_zoo)
#'
#'
#'
#'
#' @export
tk_tbl <- function(data, preserve_index = TRUE, rename_index = "index", timekit_idx = FALSE, silent = FALSE, ...) {
    UseMethod("tk_tbl", data)
}

#' @export
tk_tbl.data.frame <- function(data, preserve_index = TRUE, rename_index = "index", timekit_idx = FALSE, silent = FALSE, ...) {

    if (preserve_index == TRUE) {

        idx <- rownames(data)

        # Detect if row.names exist beyond sequential 1:nrow(x) or null value
        if (!is.null(idx) &&
            !identical(as.character(idx), as.character(1:nrow(as.data.frame(data))))
            ) {

            ret <- data %>%
                as.data.frame() %>%
                tibble::rownames_to_column(var = rename_index) %>%
                tibble::as_tibble(...)

            ret <- suppressMessages(readr::type_convert(ret))

        } else {

            if (!silent) warning("Warning: No index to preserve. Object otherwise converted to tibble successfully.")
            ret <- tibble::as_tibble(data, ...)
        }

    } else {

        ret <- tibble::as_tibble(data, ...)

    }

    return(ret)

}

#' @export
tk_tbl.xts <- function(data, preserve_index = TRUE, rename_index = "index", timekit_idx = FALSE, silent = FALSE, ...) {

    # Coerce to zoo, then to tbl
    ret <- tk_tbl(zoo::as.zoo(data), preserve_index, rename_index, timekit_idx, silent, ...)
    return(ret)
}

#' @export
tk_tbl.matrix <- function(data, preserve_index = TRUE, rename_index = "index", timekit_idx = FALSE, silent = FALSE, ...) {

    # Coerce to data frame, then to tbl
    ret <- tk_tbl(as.data.frame(data), preserve_index, rename_index, timekit_idx, silent, ...)
    return(ret)

}

#' @export
tk_tbl.zoo <- function(data, preserve_index = TRUE, rename_index = "index", timekit_idx = FALSE, silent = FALSE, ...) {

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

            if (!is.null(rename_index)) colnames(ret)[[1]] <- rename_index

            ret <- suppressMessages(readr::type_convert(ret))


        } else {

            if (!silent) warning(paste0("Warning: No index to preserve. ",
                           "Object otherwise converted to tibble successfully."))
            ret <- tibble::as_tibble(data, ...)
        }

    } else {

        ret <- tibble::as_tibble(data, ...)

    }

    return(ret)
}

#' @export
tk_tbl.zooreg <- function(data, preserve_index = TRUE, rename_index = "index", timekit_idx = FALSE, silent = FALSE, ...) {

    # Handle timekit index
    if (timekit_idx && preserve_index) {

        # Index attribute found?
        first_val <- rownames(data)[[1]]
        if (!is.null(first_val)) {
            # Coerce to xts then to tbl
            index <- tk_index(data, timekit_idx = TRUE)
            ret <- tk_xts(data, order.by = index) %>%
                tk_tbl(preserve_index = preserve_index,
                       rename_index   = rename_index,
                       timekit_idx     = timekit_idx,
                       silent         = silent,
                       ...            = ...)
        } else {
            if (!silent) warning("No `timekit index` attribute found. Using regularized index.")
            ret <- tk_tbl(zoo::as.zoo(data), preserve_index, rename_index, timekit_idx, silent, ...)
        }

    } else {
        # Coerce to zoo then convert to tibble
        ret <- tk_tbl(zoo::as.zoo(data), preserve_index, rename_index, timekit_idx, silent, ...)
    }

    return(ret)
}

#' @export
tk_tbl.ts <- function(data, preserve_index = TRUE, rename_index = "index", timekit_idx = FALSE, silent = FALSE, ...) {

    # Handle timekit index
    if (timekit_idx && preserve_index) {

        # Index attribute found?
        index_attr <- attr(data, "index")
        if (!is.null(index_attr)) {
            # Coerce to xts then to tbl
            index <- tk_index(data, timekit_idx = TRUE)
            ret <- tk_xts(data, order.by = index) %>%
                tk_tbl(preserve_index = preserve_index,
                       rename_index   = rename_index,
                       timekit_idx     = timekit_idx,
                       silent         = silent,
                       ...            = ...)
        } else {
            # if (!silent) warning("No timekit `index` attribute found. Using regularized index.")
            ret <- tk_tbl(zoo::as.zoo(data), preserve_index, rename_index, timekit_idx, silent, ...)
        }

    } else {
        # Coerce to zoo then convert to tibble
        ret <- tk_tbl(zoo::as.zoo(data), preserve_index, rename_index, timekit_idx, silent, ...)
    }

    return(ret)
}

# mts handled via ts class


#' @export
tk_tbl.msts <- function(data, preserve_index = TRUE, rename_index = "index", timekit_idx = FALSE, silent = FALSE, ...) {

    # Coerce to zoo then convert to tibble
    ret <- tk_tbl(zoo::as.zoo(data), preserve_index, rename_index, timekit_idx, silent, ...)
    return(ret)
}

#' @export
tk_tbl.timeSeries <- function(data, preserve_index = TRUE, rename_index = "index", timekit_idx = FALSE, silent = FALSE, ...) {

    # Coerce to data frame, then to tbl (No index to coerce to zoo)
    ret <- tk_tbl(as.data.frame(data), preserve_index, rename_index, timekit_idx, silent, ...)
    return(ret)
}

#' @export
tk_tbl.irts <- function(data, preserve_index = TRUE, rename_index = "index", timekit_idx = FALSE, silent = FALSE, ...) {

    # Coerce to zoo then convert to tibble
    ret <- tk_tbl(zoo::as.zoo(data), preserve_index, rename_index, timekit_idx, silent, ...)
    return(ret)
}


#' @export
tk_tbl.default <- function(data, preserve_index = TRUE, rename_index = "index", timekit_idx = FALSE, silent = FALSE, ...) {

    ret <- tk_tbl(as.data.frame(data), preserve_index, rename_index, timekit_idx, silent, ...)
    return(ret)

}
