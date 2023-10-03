#' Coerce time series objects and tibbles with date/date-time columns to ts.
#'
#' @name tk_ts
#'
#' @param data A time-based tibble or time-series object.
#' @param select __Applicable to tibbles and data frames only__.
#' The column or set of columns to be coerced to `ts` class.
#' @param silent Used to toggle printing of messages and warnings.
#' @inheritParams stats::ts
#'
#' @return Returns a `ts` object.
#'
#' @details `tk_ts()` is a wrapper for `stats::ts()` that is designed
#' to coerce `tibble` objects that have a "time-base" (meaning the values vary with time)
#' to `ts` class objects. There are two main advantages:
#'
#' 1. Non-numeric columns get removed instead of being populated by NA's.
#' 2. The returned `ts` object retains a "timetk index" (and various other attributes) if detected.
#' The "timetk index" can be used to coerce between `tbl`, `xts`, `zoo`, and `ts` data types.
#'
#' The `select` argument is used to select subsets
#' of columns from the incoming data.frame.
#' Only columns containing numeric data are coerced. _At a minimum, a `frequency`
#' and a `start` should be specified._
#'
#' For non-data.frame object classes (e.g. `xts`, `zoo`, `timeSeries`, etc) the objects are coerced
#' using `stats::ts()`.
#'
#' `tk_ts_` is a nonstandard evaluation method.
#'
#' @seealso [tk_index()], [tk_tbl()], [tk_xts()], [tk_zoo()], [tk_zooreg()]
#'
#' @examples
#' library(dplyr)
#'
#' ### tibble to ts: Comparison between tk_ts() and stats::ts()
#' data_tbl <- tibble::tibble(
#'     date = seq.Date(as.Date("2016-01-01"), by = 1, length.out = 5),
#'     x    = rep("chr values", 5),
#'     y    = cumsum(1:5),
#'     z    = cumsum(11:15) * rnorm(1))
#'
#' # as.ts: Character columns introduce NA's; Result does not retain index
#' stats::ts(data_tbl[,-1], start = 2016)
#'
#' # tk_ts: Only numeric columns get coerced; Result retains index in numeric format
#' data_ts <- tk_ts(data_tbl, start = 2016)
#' data_ts
#'
#' # timetk index
#' tk_index(data_ts, timetk_idx = FALSE)   # Regularized index returned
#' tk_index(data_ts, timetk_idx = TRUE)    # Original date index returned
#'
#' # Coerce back to tibble
#' data_ts %>% tk_tbl(timetk_idx = TRUE)
#'
#'
#' ### Using select
#' tk_ts(data_tbl, select = y)
#'
#'
#' ### NSE: Enables programming
#' select   <- "y"
#' tk_ts_(data_tbl, select = select)
#'
#' @rdname tk_ts
#' @export
tk_ts <- function(data, select = NULL, start = 1, end = numeric(), frequency = 1,
                  deltat = 1, ts.eps = getOption("ts.eps"), silent = FALSE) {

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

    # Method dispatch
    ret <- tk_ts_dispatch_(data = data, select = select, start = start, end = end,
                           frequency = frequency, deltat = deltat, ts.eps = ts.eps,
                           silent = silent)
    return(ret)

}

#' @rdname tk_ts
#' @export
tk_ts_ <- function(data, select = NULL, start = 1, end = numeric(), frequency = 1,
                   deltat = 1, ts.eps = getOption("ts.eps"), silent = FALSE) {

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
    ret <- tk_ts_dispatch_(data = data, select = select, start = start, end = end,
                           frequency = frequency, deltat = deltat, ts.eps = ts.eps,
                           silent = silent)
    return(ret)

}

#' S3 methods for ts method dispatch
#'
#' Method dispatch for ts
#' @inheritParams tk_ts
#' @return A character vector
#' @name tk_ts_dispatch_
#' @keywords internal
#' @export
tk_ts_dispatch_ <- function(data, select, start, end, frequency, deltat, ts.eps, silent) {
    UseMethod("tk_ts_", data)
}



#' @rdname tk_ts_dispatch_
#' @export
tk_ts_.data.frame <- function(data, select, start, end, frequency, deltat, ts.eps, silent) {

    ret <- data

    # Coerce to xts, which retains index, timezone, etc
    ret <- suppressMessages(tk_xts_(ret, select = select, silent = silent))

    # Coerce to ts
    ret <- stats::ts(ret, start = start, end = end, frequency = frequency, deltat = deltat, ts.eps = ts.eps)

    return(ret)

}

#' @rdname tk_ts_dispatch_
#' @export
tk_ts_.default <- function(data, select, start, end, frequency, deltat, ts.eps, silent) {

    # Validate select
    if (!(is.null(select) || select == "NULL"))
        if (!silent) warning("`select` is only applicable to data.frame and tibble objects.")

    # Coerce
    ret <- stats::ts(data, start = start, end = end, frequency = frequency, deltat = deltat, ts.eps = ts.eps)
    return(ret)
}


