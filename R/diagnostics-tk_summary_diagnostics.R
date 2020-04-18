#' Group-wise Time Series Summary
#'
#' `tk_summary_diagnostics()` returns the time series summary from
#' one or more timeseries groups in a tibble.
#'
#' @param .data A `tibble` or `data.frame` with a time-based column
#' @param .date_var A column containing either date or date-time values
#'
#' @return A `tibble` or `data.frame` with timeseries summary features
#'
#' @details
#'
#' Applies [tk_get_timeseries_summary()] group-wise returning the summary of one or more
#' time series groups.
#'
#' - Respects `dplyr` groups
#' - Returns the time series summary from a time-based feature.
#'
#' @examples
#' library(dplyr)
#' library(timetk)
#'
#' # ---- GROUPED EXAMPLES ----
#'
#' # Hourly Data
#' m4_hourly %>%
#'     group_by(id) %>%
#'     tk_summary_diagnostics(date)
#'
#'
#' @name tk_summary_diagnostics
#' @export
tk_summary_diagnostics <- function(.data, .date_var) {

    # Tidyeval Setup
    date_var_expr <- rlang::enquo(.date_var)

    # Checks
    if (!is.data.frame(.data)) {
        stop(call. = FALSE, "tk_summary_diagnostics(.data) is not a data-frame or tibble. Please supply a data.frame or tibble.")
    }
    if (rlang::quo_is_missing(date_var_expr)) {
        stop(call. = FALSE, "tk_summary_diagnostics(.date_var) is missing. Please supply a date or date-time column.")
    }

    UseMethod("tk_summary_diagnostics", .data)

}

#' @export
tk_summary_diagnostics.data.frame <- function(.data, .date_var) {


    # Tidyeval Setup
    date_var_expr <- rlang::enquo(.date_var)

    # ---- DATA SETUP ----

    # Apply any Transformations - Evaluate Formula
    data_formatted <- .data %>%
        dplyr::pull(!! date_var_expr) %>%
        tk_get_timeseries_summary()

    return(data_formatted)

}

#' @export
tk_summary_diagnostics.grouped_df <- function(.data, .date_var) {

    # Tidy Eval Setup
    date_var_expr <- rlang::enquo(.date_var)
    group_names   <- dplyr::group_vars(.data)

    # Process groups individually
    .data %>%
        tidyr::nest() %>%
        dplyr::mutate(nested.col = purrr::map(
            .x         = data,
            .f         = function(df) tk_summary_diagnostics(
                .data         = df,
                .date_var     = !! date_var_expr
            )
        )) %>%
        dplyr::select(-data) %>%
        tidyr::unnest(cols = nested.col) %>%
        dplyr::group_by_at(.vars = group_names)

}
