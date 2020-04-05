#' Interactive Plotting for One or More Time Series
#'
#' A workhorse time-series plotting function that generates interactive `plotly` plots,
#' consolidates 20+ lines of `ggplot2` code, and scales well to many time series.
#'
#'
#' @param .data A `tibble` or `data.frame` with a time-based column
#' @param .date_var A column containing either date or date-time values
#' @param .value A column containing numeric values
#' @param ... One or more grouping columns that broken out into `ggplot2` facets.
#'  These can be selected using `tidyselect()` helpers (e.g `contains()`).
#' @param .color_var A categorical column that can be used to change the
#'  line color
#' @param .facet_ncol Number of facet columns.
#' @param .facet_scales Control facet x & y-axis ranges.
#'  Options include "fixed", "free", "free_y", "free_x"
#' @param .facet_collapse Multiple facets included on one facet strip instead of
#'  multiple facet strips.
#' @param .facet_collapse_sep The separator used for collapsing facets.
#' @param .line_color Line color. Overrided if `.color_var` is specified.
#' @param .line_size Line size.
#' @param .line_type Line type.
#' @param .line_alpha Line alpha (opacity). Range: (0, 1).
#' @param .y_intercept Value for a y-intercept on the plot
#' @param .y_intercept_color Color for the y-intercept
#' @param .smooth Logical - Whether or not to include a trendline smoother.
#'  Uses See [smooth_vec()] to apply a LOESS smoother.
#' @param .smooth_period Number of observations to include in the Loess Smoother.
#'  You can use either period or span. See [smooth_vec()].
#' @param .smooth_span Percentage of observations to include in the Loess Smoother.
#'  You can use either period or span. See [smooth_vec()].
#' @param .smooth_degree Flexibility of Loess Polynomial.
#'  Either 0, 1, 2 (0 = lest flexible, 2 = more flexible).
#' @param .smooth_color Smoother line color
#' @param .smooth_size Smoother line size
#' @param .smooth_alpha Smoother alpha (opacity). Range: (0, 1).
#' @param .title Title for the plot
#' @param .x_lab X-axis label for the plot
#' @param .y_lab Y-axis label for the plot
#' @param .interactive Returns either a static (`ggplot2`) visualization or an interactive (`plotly`) visualization
#' @param .plotly_slider If TRUE, returns a plotly date range slider.
#'
#' @return A static `ggplot2` plot or an interactive `plotly` plot
#'
#' @details
#'
#' `plot_time_series()` is a scalable function that works with both _ungrouped_ and _grouped_
#' `data.frame` objects (and `tibbles`!).
#'
#' __Interactive by Default__
#'
#' `plot_time_series()` is built for exploration using:
#'
#'  - __Interactive Plots:__ `plotly` (default) - Great for exploring!
#'  - __Static Plots:__ `ggplot2` (set `.interactive = FALSE`) - Great for PDF Reports
#'
#' By default, an interactive `plotly` visualization is returned.
#'
#' __Scalable with Facets & Dplyr Groups__
#'
#' `plot_time_series()` returns multiple time series plots using `ggplot2` facets:
#'
#'  - `group_by()` - If groups are detected, multiple facets are returned
#'  - `plot_time_series(.facets)` - You can manually supply facets as well.
#'
#' __Can Transform Values just like ggplot__
#'
#' The `.values` argument accepts transformations just like `ggplot2`.
#' For example, if you want to take the log of sales you can use
#' a call like `plot_time_series(date, log(sales))` and the log transformation
#' will be applied.
#'
#'
#' @examples
#' library(tidyverse)
#' library(tidyquant)
#' library(lubridate)
#' library(timetk)
#'
#' # Works with individual time series
#' FANG %>%
#'     filter(symbol == "FB") %>%
#'     plot_time_series(date, adjusted, .interactive = FALSE)
#'
#' # Works with groups
#' FANG %>%
#'     group_by(symbol) %>%
#'     plot_time_series(date, adjusted,
#'                      .facet_ncol  = 2,     # 2-column layout
#'                      .interactive = FALSE)
#'
#' # Can also group inside
#' FANG %>%
#'     mutate(year = year(date)) %>%
#'     plot_time_series(date, adjusted,
#'                      symbol, year,         # add groups
#'                      .color_var    = year, # color by year
#'                      .facet_ncol   = 4,
#'                      .facet_scales = "free",
#'                      .interactive  = FALSE)
#'
#' # Can apply .value mutations
#' FANG %>%
#'     plot_time_series(date, log(adjusted), symbol,
#'                      .facet_ncol   = 4,
#'                      .facet_scales = "free",
#'                      .interactive  = FALSE)
#'
#' # Plotly - Interactive Visualization By Default (Great for Exploration)
#' FANG %>%
#'     plot_time_series(date, adjusted, symbol, .smooth_alpha = 0.5, .plotly_slider = TRUE)
#'
#' # ggplot2 - static visualization (Great for PDF Reports)
#' FANG %>%
#'     plot_time_series(
#'         date, adjusted, symbol,
#'         .color_var     = symbol,
#'         .facet_ncol    = 2,
#'         .smooth_period = 180,
#'         .interactive   = FALSE) +
#'    theme_tq_dark() +
#'    scale_color_viridis_d()
#'
#'
#' @export
plot_time_series <- function(.data, .date_var, .value, ..., .color_var = NULL,
                             .facet_ncol = 1, .facet_scales = "free_y",
                             .facet_collapse = TRUE, .facet_collapse_sep = " ",
                             .line_color = "#2c3e50", .line_size = 0.5,
                             .line_type = 1, .line_alpha = 1,
                             .y_intercept = NULL, .y_intercept_color = "#2c3e50",
                             .smooth = TRUE, .smooth_period = NULL,
                             .smooth_span = 0.75, .smooth_degree = 2,
                             .smooth_color = "#3366FF", .smooth_size = 1, .smooth_alpha = 1,
                             .title = "Time Series Plot", .x_lab = "", .y_lab = "",
                             .interactive = TRUE, .plotly_slider = FALSE) {

    # Tidyeval Setup
    date_var_expr  <- rlang::enquo(.date_var)
    value_expr     <- rlang::enquo(.value)
    color_var_expr <- rlang::enquo(.color_var)

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


    UseMethod("plot_time_series", .data)
}

#' @export
plot_time_series.data.frame <- function(.data, .date_var, .value, ..., .color_var = NULL,
                             .facet_ncol = 1, .facet_scales = "free_y",
                             .facet_collapse = TRUE, .facet_collapse_sep = " ",
                             .line_color = "#2c3e50", .line_size = 0.5,
                             .line_type = 1, .line_alpha = 1,
                             .y_intercept = NULL, .y_intercept_color = "#2c3e50",
                             .smooth = TRUE, .smooth_period = NULL,
                             .smooth_span = 0.75, .smooth_degree = 2,
                             .smooth_color = "#3366FF", .smooth_size = 1, .smooth_alpha = 1,
                             .title = "Time Series Plot", .x_lab = "", .y_lab = "",
                             .interactive = TRUE, .plotly_slider = FALSE) {


    # Tidyeval Setup
    date_var_expr  <- rlang::enquo(.date_var)
    value_expr     <- rlang::enquo(.value)
    facets_expr    <- rlang::enquos(...)
    color_var_expr <- rlang::enquo(.color_var)

    # ---- DATA SETUP ----

    # Evaluate Formula
    data_formatted <- .data %>%
        dplyr::group_by(!!! facets_expr) %>%
        dplyr::mutate(.value_mod = !! value_expr) %>%
        dplyr::select(!! date_var_expr, .value_mod, !!! facets_expr, !! color_var_expr) %>%
        dplyr::ungroup()

    # Color setup
    if (rlang::quo_is_missing(color_var_expr)) color_var_expr <- enquo(NULL)

    if (!rlang::quo_is_null(color_var_expr)) {
        color_class <- data_formatted %>% dplyr::pull(!! color_var_expr)
        if (inherits(color_class, c("integer", "double", "logical", "numeric"))) {
            message("plot_time_series(.color_var): variable, ", rlang::quo_name(color_var_expr), ", is not categorical. Converting to factor with forcats::as_factor().")
            data_formatted <- data_formatted %>%
                # dplyr::mutate_at(.vars = dplyr::vars(!! color_var_expr), as.character) %>%
                dplyr::mutate_at(.vars = dplyr::vars(!! color_var_expr), forcats::as_factor)
        }
    }

    # Facet setup
    facet_names <- data_formatted %>% dplyr::select(!!! facets_expr) %>% colnames()

    if (length(facet_names) > 0) {
        if (.facet_collapse) {

            data_formatted <- data_formatted %>%
                dplyr::ungroup() %>%
                dplyr::mutate(.facets_collapsed = stringr::str_c(!!! rlang::syms(facet_names),
                                                                sep = .facet_collapse_sep)) %>%
                dplyr::mutate(.facets_collapsed = forcats::as_factor(.facets_collapsed)) %>%
                # dplyr::select(-(!!! rlang::syms(facet_names))) %>%
                dplyr::group_by(.facets_collapsed)

            facet_names <- ".facets_collapsed"

        } else {
            data_formatted <- data_formatted %>%
                dplyr::group_by(!!! rlang::syms(facet_names))
        }
    }

    # Smooth calculation
    if (.smooth) {

        if (!is.null(.smooth_period)) {
            .smooth_span <- NULL
        }

        data_formatted <- data_formatted %>%
            dplyr::mutate(.value_smooth = smooth_vec(
                .value_mod,
                period = .smooth_period,
                span   = .smooth_span,
                degree = .smooth_degree)
            ) %>%
            dplyr::ungroup()
    }


    # ---- PLOT SETUP ----

    g <- data_formatted %>%
        dplyr::rename(.value = .value_mod) %>%
        ggplot2::ggplot(ggplot2::aes(!! date_var_expr, .value))

    # Add line
    if (rlang::quo_is_null(color_var_expr)) {
        g <- g +
            ggplot2::geom_line(
                color    = .line_color,
                size     = .line_size,
                linetype = .line_type,
                alpha    = .line_alpha
            )

    } else {
        g <- g +
            ggplot2::geom_line(
                ggplot2::aes(color = !! color_var_expr),
                size     = .line_size,
                linetype = .line_type,
                alpha    = .line_alpha
            ) +
            scale_color_tq()
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
                ggplot2::aes(y = .value_smooth),
                color = .smooth_color,
                size  = .smooth_size,
                alpha = .smooth_alpha)
    }

    # Add a Y-Intercept if desired
    if (!is.null(.y_intercept)) {
        g <- g +
            ggplot2::geom_hline(yintercept = .y_intercept, color = .y_intercept_color)
    }

    # Add theme & labs
    g <- g +
        theme_tq() +
        ggplot2::labs(x = .x_lab, y = .y_lab, title = .title)

    if (.interactive) {

        p <- plotly::ggplotly(g, dynamicTicks = TRUE)

        if (.plotly_slider) {
            p <- p %>%
                plotly::layout(
                    xaxis = list(
                        rangeslider = list(type = "date")
                    )
                )
        }

        return(p)
    } else {
        return(g)
    }
}

#' @export
plot_time_series.grouped_df <- function(.data, .date_var, .value, ..., .color_var = NULL,
                                        .facet_ncol = 1, .facet_scales = "free_y",
                                        .facet_collapse = TRUE, .facet_collapse_sep = " ",
                                        .line_color = "#2c3e50", .line_size = 0.5,
                                        .line_type = 1, .line_alpha = 1,
                                        .y_intercept = NULL, .y_intercept_color = "#2c3e50",
                                        .smooth = TRUE, .smooth_period = NULL,
                                        .smooth_span = 0.75, .smooth_degree = 2, .smooth_alpha = 1,
                                        .smooth_color = "#3366FF", .smooth_size = 1,
                                        .title = "Time Series Plot", .x_lab = "", .y_lab = "",
                                        .interactive = TRUE, .plotly_slider = FALSE) {

    # Tidy Eval Setup
    group_names   <- dplyr::group_vars(.data)
    value_expr    <- rlang::enquo(.value)
    facets_expr   <- rlang::enquos(...)

    # Checks
    facet_names <- .data %>% dplyr::ungroup() %>% dplyr::select(!!! facets_expr) %>% colnames()
    if (length(facet_names) > 0) message("plot_time_series(...): Groups are previously detected. Grouping by: ",
                                          stringr::str_c(group_names, collapse = ", "))

    # ---- DATA SETUP ----

    # Ungroup Data
    data_formatted <- .data %>% dplyr::ungroup()

    # ---- PLOT SETUP ----

    plot_time_series(
        .data              = data_formatted,
        .date_var          = !! rlang::enquo(.date_var),
        .value             = !! rlang::enquo(.value),
        .color_var         = !! rlang::enquo(.color_var),

        # ...
        !!! syms(group_names),

        .facet_ncol         = .facet_ncol,
        .facet_scales       = .facet_scales,
        .facet_collapse     = .facet_collapse,
        .facet_collapse_sep = .facet_collapse_sep,
        .line_color         = .line_color,
        .line_size          = .line_size,
        .line_type          = .line_type,
        .line_alpha         = .line_alpha,
        .y_intercept        = .y_intercept,
        .y_intercept_color  = .y_intercept_color,
        .smooth             = .smooth,
        .smooth_period      = .smooth_period,
        .smooth_span        = .smooth_span,
        .smooth_degree      = .smooth_degree,
        .smooth_color       = .smooth_color,
        .smooth_size        = .smooth_size,
        .smooth_alpha       = .smooth_alpha,
        .title              = .title,
        .x_lab              = .x_lab,
        .y_lab              = .y_lab,
        .interactive        = .interactive,
        .plotly_slider      = .plotly_slider
    )


}
