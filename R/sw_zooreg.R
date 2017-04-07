#' Coerce time series objects and tibbles with date/date-time columns to ts.
#'
#' @name sw_zooreg
#'
#' @param data A time-based tibble or time-series object.
#' @param select __Applicable to tibbles and data frames only__.
#' The column or set of columns to be coerced to `zooreg` class.
#' @param date_var __Applicable to tibbles and data frames only__.
#' Column name to be used to `order.by`.
#' `NULL` by default. If `NULL`, function will find the date or date-time column.
#' @inheritParams zoo::zooreg
#'
#' @return Returns a `zooreg` object.
#'
#' @details `sw_zooreg()` is a wrapper for `zoo::zooreg()` that is designed
#' to coerce `tibble` objects that have a "time-base" (meaning the values vary with time)
#' to `zooreg` class objects. There are two main advantages:
#'
#' 1. Non-numeric columns get removed instead causing coercion issues.
#' 2. If an index is present, the returned `zooreg` object retains an index retrievable using `rownames()`.
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
#' `sw_zooreg_` is a nonstandard evaluation method.
#'
#' @seealso [sw_tbl()], [sw_xts()], [sw_zoo()], [sw_ts()]
#'
#' @examples
#' ### tibble to zooreg: Comparison between sw_zooreg() and zoo::zooreg()
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
#' # sw_zooreg: Only numeric columns get coerced; Result retains index as rownames
#' data_sw_zooreg <- sw_zooreg(data_tbl, start = 2016, freq = 365)
#' data_sw_zooreg             # No inadvertent coercion to character class
#' rownames(data_sw_zooreg)   # Original date index retained
#' zoo::index(data_sw_zooreg) # New regularized index retrieveable
#'
#' ### Using select and date_var
#' sw_zooreg(data_tbl, select = y, date_var = date, start = 2016, freq = 365)
#'
#'
#' ### NSE: Enables programming
#' select   <- "y"
#' date_var <- "date"
#' sw_zooreg_(data_tbl, select = select, date_var = date_var, start = 2016, freq = 365)
#'
#' @rdname sw_zooreg
#' @export
sw_zooreg <- function(data, select = NULL, date_var = NULL, start = 1, end = numeric(), frequency = 1,
                      deltat = 1, ts.eps = getOption("ts.eps"), order.by = NULL) {

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
    select   <- lazyeval::expr_text(select)
    date_var <- lazyeval::expr_text(date_var)

    # Method dispatch
    ret <- sw_zooreg_dispatch_(data      = data,
                               select    = select,
                               date_var  = date_var,
                               start     = start,
                               end       = end,
                               frequency = frequency,
                               deltat    = deltat,
                               ts.eps    = ts.eps,
                               order.by  = order.by)
    return(ret)

}

#' @rdname sw_zooreg
#' @export
sw_zooreg_ <- function(data, select = NULL, date_var = NULL, start = 1, end = numeric(), frequency = 1,
                       deltat = 1, ts.eps = getOption("ts.eps"), order.by = NULL) {

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
    ret <- sw_zooreg_dispatch_(data      = data,
                               select    = select,
                               date_var  = date_var,
                               start     = start,
                               end       = end,
                               frequency = frequency,
                               deltat    = deltat,
                               ts.eps    = ts.eps,
                               order.by  = order.by)
    return(ret)

}


sw_zooreg_dispatch_ <- function(data, select, date_var, start, end, frequency, deltat, ts.eps, order.by) {
    UseMethod("sw_zooreg_", data)
}



#' @rdname sweep_internal
#' @export
sw_zooreg_.data.frame <- function(data, select, date_var, start, end, frequency, deltat, ts.eps, order.by) {

    ret <- data

    # Coerce to xts, which retains index, timezone, etc
    ret <- suppressMessages(sweep::sw_xts_(ret, select = select, date_var = date_var))

    # Coerce to ts
    ret <- zoo::zooreg(ret, start = start, end = end, frequency = frequency, deltat = deltat, ts.eps = ts.eps, order.by = order.by)

    return(ret)

}

#' @rdname sweep_internal
#' @export
sw_zooreg_.default <- function(data, select, date_var, start, end, frequency, deltat, ts.eps, order.by) {

    # Validate select
    if (!(is.null(select) || select == "NULL")) warning("`select` is only applicable to data.frame and tibble objects.")

    # Validate select
    if (!(is.null(date_var) || date_var == "NULL")) warning("`date_var` is only applicable to data.frame and tibble objects.")


    # Coerce
    ret <- zoo::zooreg(data, start = start, end = end, frequency = frequency, deltat = deltat, ts.eps = ts.eps, order.by = order.by)
    return(ret)
}


