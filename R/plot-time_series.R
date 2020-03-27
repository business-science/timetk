#' Plot a Time Series
#'
#' `plot_time_series()` returns a time series visualization in either static (`ggplot2`) or
#' interactive (`plotly`) formats.
#'
#'
#' @param .data A `tibble` or `data.frame`
#' @param .date_var A column containing either date or date-time values
#' @param .value A column containing numeric values
#' @param .facets One or more grouping columns that broken out into `ggplot2` facets.
#'  These can be selected using `tidyselect()` helpers (e.g `contains()`).
#' @param .facet_ncol Facets: Number of facet columns.
#' @param .facet_scales Facets: Options include "fixed", "free", "free_y", "free_x"
#' @param .line_color Line color. Use keyword: "scale_color" to change the color by the facet.
#' @param .line_size Line size
#' @param .y_intercept Value for a y-intercept on the plot
#' @param .y_intercept_color Color for the y-intercept
#' @param .smooth Logical - Whether or not to include a trendline smoother.
#'  Uses See [smooth_vec()] to apply a LOESS smoother.
#' @param .smooth_period Number of observations to include in the Loess Smoother.
#'  See [smooth_vec()].
#' @param .smooth_degree Flexibility of Loess Polynomial.
#'  Either 0, 1, 2 (0 = lest flexible, 2 = more flexible).
#' @param .smooth_color Smoother line color
#' @param .smooth_size Smoother line size
#' @param .title Title for the plot
#' @param .x_lab X-axis label for the plot
#' @param .y_lab Y-axis label for the plot
#' @param .interactive Returns either a static (`ggplot2`) visualization or an interactive (`plotly`) visualization
#'
#' @return A static `ggplot2` plot or an interactive `plotly` plot
#'
#' @examples
#' library(tidyverse)
#' library(tidyquant)
#' library(timetk)
#'
#'
#' # Plotly - Interactive Visualization (Good for Exploration)
#' FANG %>%
#'     plot_time_series(date, adjusted, .facets = contains("symbol"))
#'
#' # ggplot2 - static visualization (Good for PDF Reports)
#' FANG %>%
#'     plot_time_series(
#'          date, adjusted,
#'         .facets        = symbol,
#'         .facet_ncol    = 2,
#'         .line_color    = "scale_color",
#'         .smooth_period = 180,
#'         .interactive   = FALSE) +
#'    theme_tq_dark() +
#'    scale_color_viridis_d() +
#'    theme(legend.position = "bottom")
#'
#'
#' @export
plot_time_series <- function(.data, .date_var, .value, .facets = NULL,
                             .facet_ncol = 1, .facet_scales = "free_y",
                             .line_color = "#2c3e50", .line_size = 0.5,
                             .y_intercept = NULL, .y_intercept_color = "#2c3e50",
                             .smooth = TRUE, .smooth_period = NULL, .smooth_degree = 2,
                             .smooth_color = "#3366FF", .smooth_size = 1,
                             .title = "Time Series Plot", .x_lab = "", .y_lab = "",
                             .interactive = TRUE) {


    # Tidyeval Setup
    date_var_expr <- rlang::enquo(.date_var)
    value_expr    <- rlang::enquo(.value)
    facets_expr   <- rlang::enquos(.facets)


    # Checks
    if (!is.data.frame(.data)) {
        stop(call. = FALSE, "plot_time_series(.data) is not a data-frame or tibble. Please supply a data.frame or tibble.")
    }
    if (rlang::quo_is_missing(date_var_expr)) {
        stop(call. = FALSE, "plot_time_series(.date_var) is missing. Please supply a date or date-time column.")
    }
    if (rlang::quo_is_missing(value_expr)) {
        stop(call. = FALSE, "plot_time_series(.value) is missing. Please a numeric column.")
    }

    # ---- DATA SETUP ----

    data_formatted <- .data %>%
        dplyr::ungroup() %>%
        dplyr::select(!! date_var_expr, !! value_expr, !!! facets_expr)

    facet_names <- data_formatted %>% dplyr::select(!!! facets_expr) %>% colnames()

    if (.smooth) {
        if (!rlang::quo_is_null(facets_expr[[1]])) {
            data_formatted <- data_formatted %>%
                dplyr::group_by(!!! rlang::syms(facet_names))
        }

        .smooth_span <- NULL
        if (is.null(.smooth_period)) {
            .smooth_span <- 0.75
        }

        data_formatted <- data_formatted %>%
            dplyr::mutate(value_smooth = smooth_vec(
                !! value_expr,
                .period = .smooth_period,
                .span   = .smooth_span,
                .degree = .smooth_degree)
            ) %>%
            dplyr::ungroup()
    }


    # ---- PLOT SETUP ----

    g <- data_formatted %>%
        ggplot2::ggplot(ggplot2::aes(!! date_var_expr, !! value_expr)) +
        theme_tq() +
        ggplot2::labs(x = .x_lab, y = .y_lab, title = .title)

    # Add line
    if (.line_color == "scale_color" && length(facet_names) > 0) {
        if (length(facet_names) > 1) message("plot_time_series(facets > 1 & .line_color = 'scale_color'): Using the first facet only:", facet_names[1])
        g <- g +
            ggplot2::geom_line(
                aes_string(color = facet_names[1]),
                size = .line_size) +
            scale_color_tq()
    } else {
        if (.line_color == "scale_color") message("plot_time_series(.line_color = 'scale_color'): Cannot scale color without a faceting column.")
        g <- g +
            ggplot2::geom_line(color = .line_color, size = .line_size)
    }

    # Add facets
    if (length(facet_names) > 0) {
        g <- g +
            ggplot2::facet_wrap(
                ggplot2::vars(!!! rlang::syms(facet_names)),
                ncol   = .facet_ncol,
                scales = .facet_scales
            )
    }

    # Add a smoother
    if (.smooth) {
        g <- g +
            ggplot2::geom_line(
                ggplot2::aes(y = value_smooth),
                color = .smooth_color,
                size  = .smooth_size)
    }

    # Add a Y-Intercept if desired
    if (!is.null(.y_intercept)) {
        g <- g +
            ggplot2::geom_hline(yintercept = .y_intercept, color = .y_intercept_color)
    }

    if (.interactive) {
        return(plotly::ggplotly(g))
    } else {
        return(g)
    }
}
