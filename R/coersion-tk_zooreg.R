#' Coerce time series objects and tibbles with date/date-time columns to ts.
#'
#' @name tk_zooreg
#'
#' @param data A time-based tibble or time-series object.
#' @param select __Applicable to tibbles and data frames only__.
#' The column or set of columns to be coerced to `zooreg` class.
#' @param date_var __Applicable to tibbles and data frames only__.
#' Column name to be used to `order.by`.
#' `NULL` by default. If `NULL`, function will find the date or date-time column.
#' @param silent Used to toggle printing of messages and warnings.
#' @inheritParams zoo::zooreg
#'
#' @return Returns a `zooreg` object.
#'
#' @details `tk_zooreg()` is a wrapper for `zoo::zooreg()` that is designed
#' to coerce `tibble` objects that have a "time-base" (meaning the values vary with time)
#' to `zooreg` class objects. There are two main advantages:
#'
#' 1. Non-numeric columns get removed instead causing coercion issues.
#' 2. If an index is present, the returned `zooreg` object retains an index retrievable using [tk_index()].
#'
#' The `select` argument is used to select subsets
#' of columns from the incoming data.frame.
#' The `date_var` can be used to specify the column with the date index.
#' If `date_var = NULL`, the date / date-time column is interpreted.
#' Optionally, the `order.by` argument from the underlying `xts::xts()` function can be used.
#' The user must pass a vector of dates or date-times if `order.by` is used.
#' Only columns containing numeric data are coerced.
#' _At a minimum, a `frequency` and a `start` should be specified._
#'
#' For non-data.frame object classes (e.g. `xts`, `zoo`, `timeSeries`, etc) the objects are coerced
#' using `zoo::zooreg()`.
#'
#' `tk_zooreg_` is a nonstandard evaluation method.
#'
#' @seealso [tk_tbl()], [tk_xts()], [tk_zoo()], [tk_ts()]
#'
#' @examples
#' ### tibble to zooreg: Comparison between tk_zooreg() and zoo::zooreg()
#' data_tbl <- tibble::tibble(
#'     date = seq.Date(as.Date("2016-01-01"), by = 1, length.out = 5),
#'     x    = rep("chr values", 5),
#'     y    = cumsum(1:5),
#'     z    = cumsum(11:15) * rnorm(1))
#'
#' # zoo::zooreg: Values coerced to character; Result does not retain index
#' data_zooreg <- zoo::zooreg(data_tbl[,-1], start = 2016, freq = 365)
#' data_zooreg                # Numeric values coerced to character
#' rownames(data_zooreg)      # NULL, no dates retained
#'
#' # tk_zooreg: Only numeric columns get coerced; Result retains index as rownames
#' data_tk_zooreg <- tk_zooreg(data_tbl, start = 2016, freq = 365)
#' data_tk_zooreg             # No inadvertent coercion to character class
#'
#' # timetk index
#' tk_index(data_tk_zooreg, timetk_idx = FALSE)   # Regularized index returned
#' tk_index(data_tk_zooreg, timetk_idx = TRUE)    # Original date index returned
#'
#' ### Using select and date_var
#' tk_zooreg(data_tbl, select = y, date_var = date, start = 2016, freq = 365)
#'
#'
#' ### NSE: Enables programming
#' select   <- "y"
#' date_var <- "date"
#' tk_zooreg_(data_tbl, select = select, date_var = date_var, start = 2016, freq = 365)
#'
#' @rdname tk_zooreg
#' @export
tk_zooreg <- function(data, select = NULL, date_var = NULL, start = 1, end = numeric(), frequency = 1,
                      deltat = 1, ts.eps = getOption("ts.eps"), order.by = NULL, silent = FALSE) {

    # ts validation
    if (is.matrix(data) || is.data.frame(data))  {
        ndata <- nrow(data)
    } else {
        ndata <- length(data)
    }
    if (ndata == 0) stop("'ts' object must have one or more observations")

    if (missing(frequency))
        frequency <- 1/deltat
    if (missing(deltat))
        deltat <- 1/frequency
    if (frequency > 1 && abs(frequency - round(frequency)) < ts.eps)
        frequency <- round(frequency)
    if (length(start) > 1L) {
        start <- start[1L] + (start[2L] - 1)/frequency
    }
    if (length(end) > 1L) {
        end <- end[1L] + (end[2L] - 1)/frequency
    }
    if (missing(end))
        end <- start + (ndata - 1)/frequency
    else if (missing(start))
        start <- end - (ndata - 1)/frequency
    if (start > end)
        stop("'start' cannot be after 'end'")

    # Format for NSE
    select   <- rlang::quo_name(rlang::enquo(select))
    date_var <- rlang::quo_name(rlang::enquo(date_var))

    # Method dispatch
    ret <- tk_zooreg_dispatch_(data      = data,
                               select    = select,
                               date_var  = date_var,
                               start     = start,
                               end       = end,
                               frequency = frequency,
                               deltat    = deltat,
                               ts.eps    = ts.eps,
                               order.by  = order.by,
                               silent    = silent)
    return(ret)

}

#' @rdname tk_zooreg
#' @export
tk_zooreg_ <- function(data, select = NULL, date_var = NULL, start = 1, end = numeric(), frequency = 1,
                       deltat = 1, ts.eps = getOption("ts.eps"), order.by = NULL, silent = FALSE) {

    # ts validation
    if (is.matrix(data) || is.data.frame(data))  {
        ndata <- nrow(data)
    } else {
        ndata <- length(data)
    }
    if (ndata == 0) stop("'ts' object must have one or more observations")

    if (missing(frequency))
        frequency <- 1/deltat
    if (missing(deltat))
        deltat <- 1/frequency
    if (frequency > 1 && abs(frequency - round(frequency)) < ts.eps)
        frequency <- round(frequency)
    if (length(start) > 1L) {
        start <- start[1L] + (start[2L] - 1)/frequency
    }
    if (length(end) > 1L) {
        end <- end[1L] + (end[2L] - 1)/frequency
    }
    if (missing(end))
        end <- start + (ndata - 1)/frequency
    else if (missing(start))
        start <- end - (ndata - 1)/frequency
    if (start > end)
        stop("'start' cannot be after 'end'")

    # Method dispatch
    ret <- tk_zooreg_dispatch_(data      = data,
                               select    = select,
                               date_var  = date_var,
                               start     = start,
                               end       = end,
                               frequency = frequency,
                               deltat    = deltat,
                               ts.eps    = ts.eps,
                               order.by  = order.by,
                               silent    = silent)
    return(ret)

}


tk_zooreg_dispatch_ <- function(data, select, date_var, start, end, frequency, deltat, ts.eps, order.by, silent) {
    UseMethod("tk_zooreg_", data)
}



#' @rdname timetk_internal
#' @export
tk_zooreg_.data.frame <- function(data, select, date_var, start, end, frequency, deltat, ts.eps, order.by, silent) {

    ret <- data

    # Coerce to xts, which retains index, timezone, etc
    ret <- suppressMessages(tk_xts_(ret, select = select, date_var = date_var, silent = silent))

    # Coerce to ts
    ret <- zoo::zooreg(ret, start = start, end = end, frequency = frequency, deltat = deltat, ts.eps = ts.eps, order.by = order.by)

    return(ret)

}

#' @rdname timetk_internal
#' @export
tk_zooreg_.default <- function(data, select, date_var, start, end, frequency, deltat, ts.eps, order.by, silent) {

    # Validate select
    if (!(is.null(select) || select == "NULL"))
        if (!silent) warning("`select` is only applicable to data.frame and tibble objects.")

    # Validate select
    if (!(is.null(date_var) || date_var == "NULL"))
        if (!silent) warning("`date_var` is only applicable to data.frame and tibble objects.")


    # Coerce
    ret <- zoo::zooreg(data, start = start, end = end, frequency = frequency, deltat = deltat, ts.eps = ts.eps, order.by = order.by)
    return(ret)
}


