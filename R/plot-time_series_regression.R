#' Visualize a Time Series Linear Regression Formula
#'
#' A wrapper for [stats::lm()] that overlays a
#' linear regression fitted model over a time series, which can help
#' show the effect of feature engineering
#'
#' @param .data A `tibble` or `data.frame` with a time-based column
#' @param .date_var A column containing either date or date-time values
#' @param .formula A linear regression formula. The left-hand side of the formula is used
#' as the y-axis value. The right-hand side of the formula is used to develop the linear regression model.
#' See [stats::lm()] for details.
#' @param .show_summary If `TRUE`, prints the `summary.lm()`.
#' @param ... Additional arguments passed to [plot_time_series()]
#'
#' @return A static `ggplot2` plot or an interactive `plotly` plot
#'
#' @details
#'
#' `plot_time_series_regression()` is a scalable function that works with both _ungrouped_ and _grouped_
#' `data.frame` objects (and `tibbles`!).
#'
#' __Time Series Formula__
#'
#' The `.formula` uses [stats::lm()] to apply a linear regression, which is used to visualize
#' the effect of feature engineering on a time series.
#'
#' - The left-hand side of the formula is used as the y-axis value.
#' - The right-hand side of the formula is used to develop the linear regression model.
#'
#' __Interactive by Default__
#'
#' `plot_time_series_regression()` is built for exploration using:
#'
#'  - __Interactive Plots:__ `plotly` (default) - Great for exploring!
#'  - __Static Plots:__ `ggplot2` (set `.interactive = FALSE`) - Great for PDF Reports
#'
#' By default, an interactive `plotly` visualization is returned.
#'
#' __Scalable with Facets & Dplyr Groups__
#'
#' `plot_time_series_regression()` returns multiple time series plots using `ggplot2` facets:
#'
#'  - `group_by()` - If groups are detected, multiple facets are returned
#'  - `plot_time_series_regression(.facet_vars)` - You can manually supply facets as well.
#'
#' @examples
#' library(dplyr)
#' library(lubridate)
#'
#' # ---- SINGLE SERIES ----
#' m4_monthly %>%
#'     filter(id == "M750") %>%
#'     plot_time_series_regression(
#'         .date_var     = date,
#'         .formula      = log(value) ~ as.numeric(date) + month(date, label = TRUE),
#'         .show_summary = TRUE,
#'         .facet_ncol   = 2,
#'         .interactive  = FALSE
#'     )
#'
#'
#' # ---- GROUPED SERIES ----
#' m4_monthly %>%
#'     group_by(id) %>%
#'     plot_time_series_regression(
#'         .date_var    = date,
#'         .formula     = log(value) ~ as.numeric(date) + month(date, label = TRUE),
#'         .facet_ncol  = 2,
#'         .interactive = FALSE
#'     )
#'
#'
#' @export
plot_time_series_regression <- function(.data, .date_var, .formula, .show_summary = FALSE, ...) {

    # Tidyeval Setup
    date_var_expr  <- rlang::enquo(.date_var)

    # Checks
    if (!is.data.frame(.data)) {
        stop(call. = FALSE, "plot_time_series_regression(.data) is not a data-frame or tibble. Please supply a data.frame or tibble.")
    }
    if (rlang::quo_is_missing(date_var_expr)) {
        stop(call. = FALSE, "plot_time_series_regression(.date_var) is missing. Please supply a date or date-time column.")
    }
    if (rlang::is_missing(.formula)) {
        stop(call. = FALSE, "plot_time_series_regression(.formula) is missing. Please a formula to pas to stats::lm().")
    }
    if (!rlang::is_formula(.formula)) {
        stop(call. = FALSE, "plot_time_series_regression(.formula) is not a formula. Please make sure to provide a formula in a format suitable for stats::lm().")
    }

    UseMethod("plot_time_series_regression", .data)
}

#' @export
plot_time_series_regression.data.frame <- function(.data, .date_var, .formula, .show_summary = FALSE, ...) {

    date_var_expr  <- rlang::enquo(.date_var)
    value_expr     <- rlang::f_lhs(.formula)

    df <- .data

    # Linear Regression
    model_lm <- stats::lm(.formula, data = df)

    if (.show_summary) {
        print(stats::summary.lm(model_lm))
    }

    # Pad Fitted Values

    fitted_vec <- stats::predict(model_lm, data = df)

    rowid_tbl <- tibble::tibble(row_id = seq(1, nrow(df)))

    predictions_tbl <- tibble::tibble(
        row_id = names(fitted_vec) %>% as.integer(),
        values = as.numeric(fitted_vec)
    )

    predictions_padded_tbl <- dplyr::left_join(rowid_tbl, predictions_tbl, by = "row_id")

    # Data Formatted
    data_formatted <- tibble::as_tibble(df) %>%
        dplyr::mutate(!! rlang::quo_name(value_expr) := !! value_expr) %>%
        dplyr::select(!! date_var_expr, rlang::quo_name(value_expr)) %>%
        dplyr::mutate(fitted = predictions_padded_tbl$values) %>%
        tidyr::pivot_longer(-(!! date_var_expr))

    # Plot
    data_formatted %>%
        plot_time_series(!! date_var_expr, value, .color_var = name, .smooth = FALSE, ...)

}

#' @export
plot_time_series_regression.grouped_df <- function(.data, .date_var, .formula, .show_summary = FALSE, ...) {

    group_names    <- dplyr::group_vars(.data)
    date_var_expr  <- rlang::enquo(.date_var)
    value_expr     <- rlang::f_lhs(.formula)

    # if (.show_summary) message("'.show_summary = TRUE' is only available for ungrouped time series data.")

    # Linear Regression
    data_modeled <- .data %>%
        tidyr::nest() %>%
        dplyr::mutate(grp_names = stringr::str_c(!!! rlang::syms(group_names), collapse = ", ")) %>%
        dplyr::mutate(model = purrr::map2(data, grp_names, .f = function(df, grp_names) {

            mod <- stats::lm(.formula, data = df)

            if (.show_summary) {
                cat("\n")
                cat(stringr::str_glue("Summary for Group: {grp_names}"))
                cat("---")
                print(stats::summary.lm(mod))
                cat('----\n')
            }

            ret <- tibble::tibble(fitted = stats::predict(mod, df))

            return(ret)

        })) %>%
        dplyr::select(-grp_names) %>%
        tidyr::unnest(cols = c(data, model))


    # Data Formatted
    data_formatted <- data_modeled %>%
        dplyr::mutate(!! rlang::quo_name(value_expr) := !! value_expr) %>%
        dplyr::select(!!! rlang::enquos(group_names), !! date_var_expr, !! rlang::quo_name(value_expr), fitted) %>%
        tidyr::pivot_longer(cols = c(rlang::quo_name(value_expr), fitted), names_to = "..nm", values_to = "..val")

    # Plot
    data_formatted %>%
        plot_time_series(!! date_var_expr, ..val, .color_var = ..nm, .smooth = FALSE, ...)

}
