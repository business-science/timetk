# UTILILITY FUNCTIONS ----------------------------------------------------------



#' Get date or datetime variables (column names)
#'
#' @param data An object of class `data.frame`
#'
#' @return
#' `tk_get_timeseries_variables` returns a vector containing column names of date-like classes.
#'
#' @details
#' `tk_get_timeseries_variables` returns the column names of date or datetime variables
#' in a data frame.
#' Classes that meet criteria for return include those that inherit
#' `POSIXt`, `Date`, `zoo::yearmon`, `zoo::yearqtr`. Function was adapted from `padr:::get_date_variables()`.
#' See [padr helpers.R](https://github.com/EdwinTh/padr/blob/master/R/helpers.R)
#'
#' @examples
#' library(tidyquant)
#' library(timekit)
#'
#' FANG %>%
#'     tk_get_timeseries_variables()
#'
#' @export
tk_get_timeseries_variables <- function(data){
    if (!is.data.frame(data)) {
        stop('`data` should be a data.frame', call. = FALSE)
    }
    classes <- lapply(data, class)
    date_classes <- (sapply(classes, function(x) 'POSIXt' %in% x) |
                         sapply(classes, function(x) 'Date' %in% x) |
                         sapply(classes, function(x) 'yearmon' %in% x) |
                         sapply(classes, function(x) 'yearqtr' %in% x))
    return(names(which(date_classes)))
}



#' Get the timeseries unit frequency for the primary time scales
#'
#'
#' @return
#' `tk_get_timeseries_unit_frequency` returns a tibble containing
#' the timeseries frequencies in seconds for the primary time scales including
#' "sec", "min", "hour", "day", "week", "month", "quarter", and "year".
#'
#'
#' @examples
#' tk_get_timeseries_unit_frequency()
#'
#'
#' @export
tk_get_timeseries_unit_frequency <- function() {
    # Setup units
    unit <- c("sec", "min", "hour", "day", "week", "month", "quarter", "year")
    freq <- c(0, 60, 3600, 86400, 604800, 2678400, 7948800, 31795200)

    units <- tibble::tibble(unit, freq) %>%
        dplyr::mutate(unit = forcats::as_factor(unit)) %>%
        tidyr::spread(key = unit, value = freq)
    return(units)
}

