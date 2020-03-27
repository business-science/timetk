

plot_lag_diagnostics <- function(
    .data, .value, ..., .lags = 0:20,
    .facet_ncol = 1, .facet_scales = "fixed",
    .line_color = "#2c3e50", .line_size = 0.5,
    .point_color = "#2c3e50", .point_size = 1,
    .hline_color = "#2c3e50",

    .title = "Lag Diagnostics",
    .x_lab = "Lag", .y_lab = "Correlation",
    .interactive = TRUE) {

    # Checks
    value_expr <- enquo(.value)
    if (rlang::quo_is_missing(value_expr)) stop(call. = FALSE, "tk_lag_diagnostics(.value), Please provide a .value.")
    if (!is.data.frame(.data)) {
        stop(call. = FALSE, "plot_diagnostics(.data) is not a data-frame or tibble. Please supply a data.frame or tibble.")
    }
    # if (is_grouped_df(.data)) {
    #     stop(call. = FALSE, "plot_diagnostics(.data) does not currently support grouped data frames.")
    # }
    UseMethod("plot_lag_diagnostics", .data)
}

#' @export
plot_lag_diagnostics.data.frame <- function(
    .data, .value, ..., .lags = 0:20,
    .facet_ncol = 1, .facet_scales = "fixed",
    .line_color = "#2c3e50", .line_size = 0.5,
    .point_color = "#2c3e50", .point_size = 1,
    .hline_color = "#2c3e50",

    .title = "Lag Diagnostics",
    .x_lab = "Lag", .y_lab = "Correlation",
    .interactive = TRUE) {

    # Tidy Eval Setup
    value_expr    <- rlang::enquo(.value)

    # ---- DATA PREPARATION ----

    data_formatted <- tk_lag_diagnostics(
        .data   = .data,
        .value  = !! value_expr,
        ...     = ...
    )

    data_formatted <- data_formatted %>%
        pivot_longer(cols = -lag, values_to = "value", names_to = "name") %>%
        mutate(name = as_factor(name))

    # ---- VISUALIZATION ----

    g <- data_formatted %>%
        ggplot2::ggplot(ggplot2::aes(lag, value, color = name)) +
        geom_hline(yintercept = 0, color = .hline_color) +
        facet_wrap(~ name, ncol = .facet_ncol, scales = .facet_scales) +
        expand_limits(y = 0) +
        ggplot2::labs(x = .x_lab, y = .y_lab, title = .title)

    # Add line
    if (.line_color == "scale_color") {
        g <- g +
            ggplot2::geom_line(color = .line_color, size = .line_size) +
            scale_color_tq()
    } else {
        g <- g +
            ggplot2::geom_line(color = .line_color, size = .line_size)
    }

    # Add points
    if (.point_color == "scale_color") {
        g <- g +
            ggplot2::geom_point(color = .point_color, size = .point_size) +
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

