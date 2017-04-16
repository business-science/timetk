#' Coerce time series objects and tibbles with date/date-time columns to xts.
#'
#' @name sw_xts
#'
#' @param data A time-based tibble or time-series object.
#' @param select __Applicable to tibbles and data frames only__.
#' The column or set of columns to be coerced to `ts` class.
#' @param date_var __Applicable to tibbles and data frames only__.
#' Column name to be used to `order.by`.
#' `NULL` by default. If `NULL`, function will find the date or date-time column.
#' @param ... Additional parameters to be passed to `xts::xts()`. Refer to `xts::xts()`.
#'
#' @return Returns a `xts` object.
#'
#' @details `sw_xts` is a wrapper for `xts::xts()` that is designed
#' to coerce `tibble` objects that have a "time-base" (meaning the values vary with time)
#' to `xts` class objects. There are three main advantages:
#'
#' 1. Non-numeric columns that are not removed via `select` are dropped and the user is warned.
#' This prevents an error or coercion issue from occurring.
#' 2. The date column is auto-detected if not specified by `date_var`. This takes
#' the effort off the user to assign a date vector during coercion.
#' 3. `ts` objects are automatically coerced if a "sweep index" is present. Refer to [sw_ts()].
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
#' `sw_xts_` is a nonstandard evaluation method.
#'
#' @seealso [sw_tbl()], [sw_zoo()], [sw_zooreg()], [sw_ts()]
#'
#' @examples
#' library(tidyverse)
#' library(sweep)
#'
#' ### tibble to xts: Comparison between sw_xts() and xts::xts()
#' data_tbl <- tibble::tibble(
#'     date = seq.Date(as.Date("2016-01-01"), by = 1, length.out = 5),
#'     x    = rep("chr values", 5),
#'     y    = cumsum(1:5),
#'     z    = cumsum(11:15) * rnorm(1))
#'
#' # xts: Character columns cause coercion issues; order.by must be passed a vector of dates
#' xts::xts(data_tbl[,-1], order.by = data_tbl$date)
#'
#' # sw_xts: Non-numeric columns automatically dropped; No need to specify date column
#' sw_xts(data_tbl)
#'
#' # ts can be coerced back to xts
#' data_tbl %>% sw_ts() %>% sw_xts()
#'
#' ### Using select and date_var
#' sw_xts(data_tbl, select = y, date_var = date)
#'
#'
#' ### NSE: Enables programming
#' date_var <- "date"
#' select   <- "y"
#' sw_xts_(data_tbl, select = select, date_var = date_var)
#'
#' @name sw_xts
#' @export
sw_xts <- function(data, select = NULL, date_var = NULL, ...) {
    select   <- lazyeval::expr_text(select)
    date_var <- lazyeval::expr_text(date_var)

    ret <- sweep::sw_xts_(data = data, select = select, date_var = date_var, ...)
    return(ret)
}

#' @export
#' @rdname sw_xts
sw_xts_ <- function(data, select = NULL, date_var = NULL, ...) {
    UseMethod("sw_xts_", data)
}


#' @export
sw_xts_.data.frame <- function(data, select = NULL, date_var = NULL, ...) {

    # Implement select
    if (!(select == "NULL" || is.null(select))) {
        ret <- dplyr::select_(data, select)
    } else {
        ret <- data
    }

    # Names to check if got dropped
    names_to_check <- colnames(ret)

    # Numeric columns only
    ret <- dplyr::select_if(ret, is.numeric)

    # Provide warning if columns are dropped
    names_dropped <- names_to_check[!(names_to_check %in% colnames(ret))]
    if (length(names_dropped) > 0) warning(paste0("Non-numeric columns being dropped: ",
                                                  stringr::str_c(names_dropped, collapse = ", ")))

    # Collect xts args
    xts_args <- list(x = ret)
    xts_args <- append(xts_args, list(...))

    # Interpret the order.by
    if (!("order.by" %in% names(xts_args))) {

        # Get date column
        if (!(is.null(date_var) || date_var == "NULL")) {

            date_col <- dplyr::select_(data, date_var)
            xts_args$order.by <- date_col[[1]]

        } else {

            date_var <- get_date_variables(data)
            date_found <- !purrr::is_empty(date_var)
            if (date_found) {
                date_col <- dplyr::select_(data, date_var)
                xts_args$order.by <- date_col[[1]]
                message(paste0("Using column `", date_var, "` for date_var."))
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
sw_xts_.ts <- function(data, select = NULL, date_var = NULL, ...) {

    # Validate select
    if (!(is.null(select) || select == "NULL")) warning("`select` is only applicable to data.frame and tibble objects.")

    # Validate date_var
    if (!(is.null(date_var) || date_var == "NULL")) warning("`date_var` is only applicable to data.frame and tibble objects.")

    # Collect xts args
    ret <- data
    xts_args <- list(x = ret)
    xts_args <- append(xts_args, list(...))

    # Interpret the order.by
    if (!("order.by" %in% names(xts_args))) {

        # Check if .sweep_idx present
        sw_index <- attr(data, "index")
        sw_index_present <- !is.null(sw_index)

        if (sw_index_present) {
            xts_args$order.by <- sw_index(data, .sweep_idx = TRUE)
        } else {
            stop("No date or date-time index found. Object must contain an unambigous date or date-time column.")
        }
    }

    # Coerce to xts
    ret <- do.call("xts", xts_args)

    return(ret)

}

#' @export
sw_xts_.zooreg <- function(data, select = NULL, date_var = NULL, ...) {

    # Validate select
    if (!(is.null(select) || select == "NULL")) warning("`select` is only applicable to data.frame and tibble objects.")

    # Validate date_var
    if (!(is.null(date_var) || date_var == "NULL")) warning("`date_var` is only applicable to data.frame and tibble objects.")

    # Collect xts args
    ret <- data
    xts_args <- list(x = ret)
    xts_args <- append(xts_args, list(...))

    # Interpret the order.by
    if (!("order.by" %in% names(xts_args))) {

        # Check if .sweep_idx present
        sw_index <- rownames(data)
        sw_index_present <- !is.null(sw_index)

        if (sw_index_present) {
            xts_args$order.by <- sw_index(data, .sweep_idx = TRUE)
        } else {
            stop("No date or date-time index found. Object must contain an unambigous date or date-time column.")
        }
    }

    # Coerce to xts
    ret <- do.call("xts", xts_args)

    return(ret)

}

#' @export
sw_xts_.default <- function(data, select = NULL, date_var = NULL, ...) {

    # Validate select
    if (!(is.null(select) || select == "NULL")) warning("`select` is only applicable to data.frame and tibble objects.")

    # Validate date_var
    if (!(is.null(date_var) || date_var == "NULL")) warning("`date_var` is only applicable to data.frame and tibble objects.")

    # Coerce to xts
    ret <- xts::xts(data, ...)
    return(ret)
}


