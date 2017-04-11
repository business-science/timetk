#' Extract an index of date or datetime from time series
#'
#' @name sw_index
#'
#' @param data A time-based tibble or time-series object.
#'
#' @return Returns a vector of date or date times
#'
#' @details
#' `sw_index` is used to extract the date or datetime index from various
#' time series data types.
#'
#' @seealso [sw_tbl()], [sw_xts()], [sw_zoo()], [sw_zooreg()], [sw_ts()]
#'
#' @examples
#'
#' data_tbl <- tibble::tibble(
#'     date = seq.Date(from = as.Date("2000-01-01"), by = 1, length.out = 5),
#'     x    = rnorm(5) * 10,
#'     y    = 5:1
#' )
#' sw_index(data_tbl)
#'
#' data_ts <- sw_ts(data_tbl)
#' sw_index(data_ts)
#'
#' @rdname sw_index
#' @export
sw_index <- function(data) {
    UseMethod("sw_index", data)
}


#' @export
sw_index.data.frame <- function(data) {

    date_var <- get_date_variables(data)

    if (length(date_var) == 0) stop("No date or date-time identified.")

    date_var <- date_var[[1]]

    ret <- data[[date_var]] %>%
        lubridate::as_datetime()

    return(ret)

}

#' @export
sw_index.ts <- function(data) {

    if (is.null(attr(data, "index"))) {
        stop("Attribute `index` not found.")
    }

    ret <- attr(data, "index") %>%
        lubridate::as_datetime()

    return(ret)

}

#' @export
sw_index.zoo <- function(data) {

    if (is.null(attr(data, "index"))) {
        stop("Attribute `index` not found.")
    }

    ret <- attr(data, "index") %>%
        lubridate::as_datetime()

    return(ret)

}

#' @export
sw_index.xts <- function(data) {

    if (is.null(attr(data, "index"))) {
        stop("Attribute `index` not found.")
    }

    ret <- attr(data, "index") %>%
        lubridate::as_datetime()

    return(ret)

}

#' @export
sw_index.default <- function(data) {

    warning(paste0("`sw_index` is not designed to work with objects of class", class(data), "."))

    return(ret)
}


