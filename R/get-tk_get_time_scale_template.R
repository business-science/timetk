#' Get and modify the Time Scale Template
#'
#' @param .data A `tibble` with a "time_scale", "frequency", and "trend" columns.
#'
#' @returns
#' * `get_tk_time_scale_template()`: Returns `tibble` containing the time scale template information.
#' * `set_tk_time_scale_template()`: Returns nothing.
#'
#' @details
#'
#' Used to get and set the time scale template, which is used by `tk_get_frequency()`
#' and `tk_get_trend()` when `period = "auto"`.
#'
#' The predefined template is stored in a function `tk_time_scale_template()`.
#' This is the default used by `timetk`.
#'
#' __Changing the Default Template__
#' - You can access the current template with `get_tk_time_scale_template()`.
#' - You can modify the current template with `set_tk_time_scale_template()`.
#'
#' @seealso
#'  - Automated Frequency and Trend Calculation: [tk_get_frequency()], [tk_get_trend()]
#'
#' @examples
#'
#' get_tk_time_scale_template()
#'
#' set_tk_time_scale_template(tk_time_scale_template())
#'



#' @export
#' @rdname tk_time_scale_template
set_tk_time_scale_template <- function(.data) {
    if (!missing(.data)) {
        options(tk_time_scale_template = .data)
    }
    #getOption('tk_time_scale_template')
}

#' @export
#' @rdname tk_time_scale_template
get_tk_time_scale_template <- function() {
    getOption('tk_time_scale_template')
}

#' @export
#' @rdname tk_time_scale_template
tk_time_scale_template <- function() {

    tibble::tribble(
        ~ "time_scale",   ~ "frequency",        ~ "trend",
        "second",         "1 hour",             "12 hours",
        "minute",         "1 day",              "14 days",
        "hour",           "1 day",              "1 month",
        "day",            "1 week",             "3 months",
        "week",           "1 quarter",          "1 year",
        "month",          "1 year",             "5 years",
        "quarter",        "1 year",             "10 years",
        "year",           "5 years",            "30 years"
    )

}
