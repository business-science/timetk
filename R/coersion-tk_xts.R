#' Coerce time series objects and tibbles with date/date-time columns to xts.
#'
#'
#' @param data A time-based tibble or time-series object.
#' @param select __Applicable to tibbles and data frames only__.
#' The column or set of columns to be coerced to `ts` class.
#' @param date_var __Applicable to tibbles and data frames only__.
#' Column name to be used to `order.by`.
#' `NULL` by default. If `NULL`, function will find the date or date-time column.
#' @param silent Used to toggle printing of messages and warnings.
#' @param ... Additional parameters to be passed to `xts::xts()`. Refer to `xts::xts()`.
#'
#' @return Returns a `xts` object.
#'
#' @details `tk_xts` is a wrapper for `xts::xts()` that is designed
#' to coerce `tibble` objects that have a "time-base" (meaning the values vary with time)
#' to `xts` class objects. There are three main advantages:
#'
#' 1. Non-numeric columns that are not removed via `select` are dropped and the user is warned.
#' This prevents an error or coercion issue from occurring.
#' 2. The date column is auto-detected if not specified by `date_var`. This takes
#' the effort off the user to assign a date vector during coercion.
#' 3. `ts` objects are automatically coerced if a "timetk index" is present. Refer to [tk_ts()].
#'
#' The `select` argument can be used to select subsets
#' of columns from the incoming data.frame.
#' Only columns containing numeric data are coerced.
#' The `date_var` can be used to specify the column with the date index.
#' If `date_var = NULL`, the date / date-time column is interpreted.
#' Optionally, the `order.by` argument from the underlying `xts::xts()` function can be used.
#' The user must pass a vector of dates or date-times if `order.by` is used.
#'
#' For non-data.frame object classes (e.g. `xts`, `zoo`, `timeSeries`, etc) the objects are coerced
#' using `xts::xts()`.
#'
#' `tk_xts_` is a nonstandard evaluation method.
#'
#' @seealso [tk_tbl()], [tk_zoo()], [tk_zooreg()], [tk_ts()]
#'
#' @examples
#' library(tidyverse)
#' library(timetk)
#'
#' ### tibble to xts: Comparison between tk_xts() and xts::xts()
#' data_tbl <- tibble::tibble(
#'     date = seq.Date(as.Date("2016-01-01"), by = 1, length.out = 5),
#'     x    = rep("chr values", 5),
#'     y    = cumsum(1:5),
#'     z    = cumsum(11:15) * rnorm(1))
#'
#' # xts: Character columns cause coercion issues; order.by must be passed a vector of dates
#' xts::xts(data_tbl[,-1], order.by = data_tbl$date)
#'
#' # tk_xts: Non-numeric columns automatically dropped; No need to specify date column
#' tk_xts(data_tbl)
#'
#' # ts can be coerced back to xts
#' data_tbl %>%
#'     tk_ts(start = 2016, freq = 365) %>%
#'     tk_xts()
#'
#' ### Using select and date_var
#' tk_xts(data_tbl, select = y, date_var = date)
#'
#'
#' ### NSE: Enables programming
#' date_var <- "date"
#' select   <- "y"
#' tk_xts_(data_tbl, select = select, date_var = date_var)
#'
#' @name tk_xts
#' @export
tk_xts <- function(data, select = NULL, date_var = NULL, silent = FALSE, ...) {
    select   <- rlang::quo_name(rlang::enquo(select))
    date_var <- rlang::quo_name(rlang::enquo(date_var))

    ret <- tk_xts_(data = data, select = select, date_var = date_var, silent = silent, ...)
    return(ret)
}

#' @export
#' @rdname tk_xts
tk_xts_ <- function(data, select = NULL, date_var = NULL, silent = FALSE, ...) {
    UseMethod("tk_xts_", data)
}


#' @export
tk_xts_.data.frame <- function(data, select = NULL, date_var = NULL, silent = FALSE, ...) {

    # Implement select
    if (!(select == "NULL" || is.null(select))) {
        # ret <- dplyr::select_(data, select)
        col_nms   <- names(tidyselect::eval_select(rlang::parse_expr(select), data))
        col_exprs <- rlang::syms(col_nms)
        ret       <- dplyr::select(data, !!! col_exprs)
    } else {
        ret <- data
    }

    # Names to check if got dropped
    names_to_check <- colnames(ret)

    # Numeric columns only
    ret <- dplyr::select_if(ret, is.numeric)

    # Provide warning if columns are dropped
    names_dropped <- names_to_check[!(names_to_check %in% colnames(ret))]
    if (length(names_dropped) > 0)
        if (!silent) warning(call. = FALSE, paste0("Non-numeric columns being dropped: ", stringr::str_c(names_dropped, collapse = ", ")))

    # Collect xts args
    xts_args <- list(x = ret)
    xts_args <- append(xts_args, list(...))

    # Interpret the order.by if not specified
    if (!("order.by" %in% names(xts_args))) {

        # Get date column
        if (!(is.null(date_var) || date_var == "NULL")) {

            # User specifies date_var
            date_col <- dplyr::select(data, dplyr::matches(date_var))
            xts_args$order.by <- date_col[[1]]

        } else {

            # Auto detect date if date_var not specified
            date_var <- tk_get_timeseries_variables(data)
            date_found <- !purrr::is_empty(date_var)
            if (date_found) {
                date_col <- dplyr::select(data, dplyr::matches(date_var))
                xts_args$order.by <- date_col[[1]]
                if (!silent) message(paste0("Using column `", date_var, "` for date_var."))
            } else {
                stop("No date or date-time column found. Object must contain an unambigous date or date-time column.")
            }
        }
    }

    # Coerce to xts
    ret <- do.call("xts", xts_args)

    return(ret)

}

#' @export
tk_xts_.ts <- function(data, select = NULL, date_var = NULL, silent = FALSE, ...) {

    # Validate select
    if (!(is.null(select) || select == "NULL"))
        if (!silent) warning("`select` is only applicable to data.frame and tibble objects.")

    # Validate date_var
    if (!(is.null(date_var) || date_var == "NULL"))
        if (!silent) warning("`date_var` is only applicable to data.frame and tibble objects.")

    # Remove tsp attribute
    attr(data, "tsp") <- NULL

    # Collect xts args
    ret <- data
    xts_args <- list(x = ret)
    xts_args <- append(xts_args, list(...))

    # Interpret the order.by
    if (!("order.by" %in% names(xts_args))) {
        if (has_timetk_idx(data)) {
            xts_args$order.by <- tk_index(data, timetk_idx = TRUE)
        } else {
            stop("No date or date-time index found. Object must contain an unambigous date or date-time column.")
        }
    }

    # Coerce to xts
    ret <- do.call("xts", xts_args)

    return(ret)

}

#' @export
tk_xts_.zooreg <- function(data, select = NULL, date_var = NULL, silent = FALSE, ...) {

    # Validate select
    if (!(is.null(select) || select == "NULL"))
        if (!silent) warning("`select` is only applicable to data.frame and tibble objects.")

    # Validate date_var
    if (!(is.null(date_var) || date_var == "NULL"))
        if (!silent) warning("`date_var` is only applicable to data.frame and tibble objects.")

    # Collect xts args
    ret <- data
    xts_args <- list(x = ret)
    xts_args <- append(xts_args, list(...))

    # Interpret the order.by
    if (!("order.by" %in% names(xts_args))) {
        if (has_timetk_idx(data)) {
            xts_args$order.by <- tk_index(data, timetk_idx = TRUE)
        } else {
            stop("No date or date-time index found. Object must contain an unambigous date or date-time column.")
        }
    }

    # Coerce to xts
    ret <- do.call("xts", xts_args)

    return(ret)

}

#' @export
tk_xts_.default <- function(data, select = NULL, date_var = NULL, silent = FALSE, ...) {

    # Validate select
    if (!(is.null(select) || select == "NULL"))
        if (!silent) warning("`select` is only applicable to data.frame and tibble objects.")

    # Validate date_var
    if (!(is.null(date_var) || date_var == "NULL"))
        if (!silent) warning("`date_var` is only applicable to data.frame and tibble objects.")

    # Coerce to xts
    ret <- xts::xts(data, ...)
    return(ret)
}


