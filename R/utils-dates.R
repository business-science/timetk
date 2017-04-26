# UTILILITY FUNCTIONS ----------------------------------------------------------




tk_timeseries_unit_frequency <- function() {
    # Setup units
    unit <- c("sec", "min", "hour", "day", "week", "month", "quarter", "year")
    freq <- c(0, 60, 3600, 86400, 604800, 2678400, 7948800, 31795200)

    units <- tibble::tibble(unit, freq) %>%
        dplyr::mutate(unit = forcats::as_factor(unit)) %>%
        tidyr::spread(key = unit, value = freq)
    return(units)
}

