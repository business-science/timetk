#' Visualize the ACF, PACF, and CCFs for One or More Time Series
#'
#' Returns the __ACF and PACF of a target__ and
#' optionally __CCF's of one or more lagged predictors__ in interactive `plotly` plots. Scales
#' to multiple time series with `group_by()`.
#'
#'
#' @param .data A data frame or tibble with numeric features (values) in descending
#'  chronological order
#' @param .value A numeric column with a value to have ACF and PACF calculations
#'  performed.
#' @param ... Additional features to perform Lag Cross Correlations (CCFs)
#' versus the `.value`. Useful for evaluating external lagged regressors.
#' @param .lags A seqence of one or more lags to evaluate.
#' @param .facet_ncol Facets: Number of facet columns. Has no effect if using `grouped_df`.
#' @param .facet_scales Facets: Options include "fixed", "free", "free_y", "free_x"
#' @param .line_color Line color. Use keyword: "scale_color" to change the color by the facet.
#' @param .line_size Line size
#' @param .point_color Point color. Use keyword: "scale_color" to change the color by the facet.
#' @param .point_size Point size
#' @param .hline_color Color for the y-intercept = 0 line.
#' @param .title Title for the plot
#' @param .x_lab X-axis label for the plot
#' @param .y_lab Y-axis label for the plot
#' @param .interactive Returns either a static (`ggplot2`) visualization or an interactive (`plotly`) visualization
#' @param .plotly_slider If TRUE, returns a plotly x-axis range slider.
#'
#' @return A static `ggplot2` plot or an interactive `plotly` plot
#'
#' @details
#'
#' __Simplified ACF, PACF, & CCF__
#'
#' We are often interested in all 3 of these functions. Why not get all 3 at once?
#' Now you can.
#'
#' - __ACF__ - Autocorrelation between a target variable and lagged versions of itself
#'
#' - __PACF__ - Partial Autocorrelation removes the dependence of lags on
#'  other lags highlighting key seasonalities.
#'
#' - __CCF__ - Shows how lagged predictors can be used for prediction of a target
#'  variable.
#'
#' __Scales to many time series with Grouped Data Frames__
#'
#' The `plot_acf_diagnostics()` works with `grouped_df`'s, meaning you can
#' group your time series by one or more categorical columns with `group_by()`
#' and then user `plot_acf_diagnostics()` to return facetted visualizations.
#'
#' @seealso
#' - [tk_acf_diagnostics()]: Function that returns ACF, PACF, & CCF data
#' - [plot_time_series()]: A useful interactive plotting function
#'
#' @examples
#' library(tidyverse)
#' library(tidyquant)
#' library(timetk)
#'
#'
#' # Plotly - Interactive Visualization (Good for Exploration)
#' FANG %>%
#'     group_by(symbol) %>%
#'     plot_acf_diagnostics(
#'         .value  = adjusted,  # ACF & PACF
#'         volume, close,       # CCF
#'         .lags   = 0:180
#'     )
#'
#' # ggplot2 - static visualization (Good for PDF Reports)
#' FANG %>%
#'     group_by(symbol) %>%
#'     plot_acf_diagnostics(
#'         .value  = adjusted,  # ACF & PACF
#'         volume, close,       # CCF
#'         .lags   = 0:180,
#'         .interactive = FALSE
#'     )
#'
#'
#' @export
plot_acf_diagnostics <- function(.data, .value, ..., .lags = 0:20,
                                 .facet_ncol = 1, .facet_scales = "fixed",
                                 .line_color = "#2c3e50", .line_size = 0.5,
                                 .point_color = "#2c3e50", .point_size = 1,
                                 .hline_color = "#2c3e50",
                                 .title = "Lag Diagnostics",
                                 .x_lab = "Lag", .y_lab = "Correlation",
                                 .interactive = TRUE, .plotly_slider = FALSE) {

    # Checks
    value_expr <- enquo(.value)
    if (rlang::quo_is_missing(value_expr)) stop(call. = FALSE, "tk_acf_diagnostics(.value), Please provide a .value.")
    if (!is.data.frame(.data)) {
        stop(call. = FALSE, "plot_diagnostics(.data) is not a data-frame or tibble. Please supply a data.frame or tibble.")
    }

    UseMethod("plot_acf_diagnostics", .data)
}

#' @export
plot_acf_diagnostics.data.frame <- function(.data, .value, ..., .lags = 0:20,
                                            .facet_ncol = 1, .facet_scales = "fixed",
                                            .line_color = "#2c3e50", .line_size = 0.5,
                                            .point_color = "#2c3e50", .point_size = 1,
                                            .hline_color = "#2c3e50",
                                            .title = "Lag Diagnostics",
                                            .x_lab = "Lag", .y_lab = "Correlation",
                                            .interactive = TRUE, .plotly_slider = FALSE) {

    # Tidy Eval Setup
    value_expr    <- rlang::enquo(.value)

    # ---- DATA PREPARATION ----

    data_formatted <- tk_acf_diagnostics(
        .data   = .data,
        .value  = !! value_expr,
        ...     = ...,
        .lags   = .lags
    )

    data_formatted <- data_formatted %>%
        tidyr::pivot_longer(cols = -lag, values_to = "value", names_to = "name") %>%
        dplyr::mutate(name = forcats::as_factor(name))

    # ---- VISUALIZATION ----

    g <- data_formatted %>%
        ggplot2::ggplot(ggplot2::aes(lag, value, color = name)) +
        ggplot2::geom_hline(yintercept = 0, color = .hline_color) +
        ggplot2::facet_wrap(~ name, ncol = .facet_ncol, scales = .facet_scales) +
        ggplot2::expand_limits(y = 0) +
        ggplot2::labs(x = .x_lab, y = .y_lab, title = .title)

    # Add line
    if (.line_color == "scale_color") {
        g <- g +
            ggplot2::geom_line(ggplot2::aes(color = name),
                               size = .line_size) +
            scale_color_tq()
    } else {
        g <- g +
            ggplot2::geom_line(color = .line_color, size = .line_size)
    }

    # Add points
    if (.point_color == "scale_color") {
        g <- g +
            ggplot2::geom_point(ggplot2::aes(color = name),
                                size = .point_size) +
            scale_color_tq()
    } else {
        g <- g +
            ggplot2::geom_point(color = .point_color, size = .point_size)
    }

    # Add theme
    g <- g + theme_tq()

    if (.interactive) {

        p <- plotly::ggplotly(g, dynamicTicks = TRUE)

        if (.plotly_slider) {
            p <- p %>%
                plotly::layout(
                    xaxis = list(
                        rangeslider = list(autorange = TRUE)
                    )
                )
        }
        return(p)
    } else {
        return(g)
    }
}

#' @export
plot_acf_diagnostics.grouped_df <- function(.data, .value, ..., .lags = 0:20,
                                            .facet_ncol = 1, .facet_scales = "fixed",
                                            .line_color = "#2c3e50", .line_size = 0.5,
                                            .point_color = "#2c3e50", .point_size = 1,
                                            .hline_color = "#2c3e50",
                                            .title = "ACF Diagnostics",
                                            .x_lab = "Lag", .y_lab = "Correlation",
                                            .interactive = TRUE, .plotly_slider = FALSE) {

    # Tidy Eval Setup
    group_names   <- dplyr::group_vars(.data)
    value_expr    <- rlang::enquo(.value)

    # ---- DATA PREPARATION ----

    data_formatted <- tk_acf_diagnostics(
        .data   = .data,
        .value  = !! value_expr,
        ...     = ...,
        .lags   = .lags
    )

    # dont_pivot_these <- c(group_names, "lag")
    data_formatted <- data_formatted %>%
        dplyr::ungroup() %>%
        dplyr::mutate(.groups_consolidated = stringr::str_c(!!! rlang::syms(group_names), sep = "_")) %>%
        dplyr::select(-(!!! rlang::syms(group_names))) %>%
        dplyr::select(.groups_consolidated, lag, dplyr::everything()) %>%
        tidyr::pivot_longer(cols      = -c(.groups_consolidated, lag),
                            values_to = "value",
                            names_to  = "name") %>%
        dplyr::mutate(name = forcats::as_factor(name))

    # data_formatted

    g <- data_formatted %>%
        ggplot2::ggplot(ggplot2::aes(lag, value, color = .groups_consolidated)) +
        ggplot2::geom_hline(yintercept = 0, color = .hline_color) +
        ggplot2::facet_grid(rows   = ggplot2::vars(name),
                            cols   = ggplot2::vars(.groups_consolidated),
                            scales = .facet_scales) +
        ggplot2::expand_limits(y = 0) +
        ggplot2::labs(x = .x_lab, y = .y_lab, title = .title)

    # Add line
    if (.line_color == "scale_color") {
        g <- g +
            ggplot2::geom_line(ggplot2::aes(color = .groups_consolidated),
                               size = .line_size) +
            scale_color_tq()
    } else {
        g <- g +
            ggplot2::geom_line(color = .line_color, size = .line_size)
    }

    # Add points
    if (.point_color == "scale_color") {
        g <- g +
            ggplot2::geom_point(ggplot2::aes(color = .groups_consolidated),
                                size = .point_size) +
            scale_color_tq()
    } else {
        g <- g +
            ggplot2::geom_point(color = .point_color, size = .point_size)
    }

    # Add theme
    g <- g + theme_tq()

    if (.interactive) {
        return(plotly::ggplotly(g))
    } else {
        return(g)
    }

}

