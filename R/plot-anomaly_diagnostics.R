#' Visualize STL Decomposition Features for One or More Time Series
#'
#' An interactive and scalable function for visualizing time series STL Decomposition.
#' Plots are available in interactive `plotly` (default) and static `ggplot2` format.
#'
#' @inheritParams tk_anomaly_diagnostics
#' @param .data A `tibble` or `data.frame` with a time-based column
#' @param .date_var A column containing either date or date-time values
#' @param .value A column containing numeric values
#' @param ... One or more grouping columns that broken out into `ggplot2` facets.
#'  These can be selected using `tidyselect()` helpers (e.g `contains()`).
#' @param .feature_set The STL decompositions to visualize.
#'  Select one or more of "observed", "season", "trend", "remainder", "seasadj".
#' @param .facet_scales Control facet x & y-axis ranges. Options include "fixed", "free", "free_y", "free_x"
#' @param .line_color Line color.
#' @param .line_size Line size.
#' @param .line_type Line type.
#' @param .line_alpha Line alpha (opacity). Range: (0, 1).
#' @param .title Plot title.
#' @param .x_lab Plot x-axis label
#' @param .y_lab Plot y-axis label
#' @param .interactive If TRUE, returns a `plotly` interactive plot.
#'  If FALSE, returns a static `ggplot2` plot.
#'
#'
#' @return A `plotly` or `ggplot2` visualization
#'
#' @details
#' The `plot_anomaly_diagnostics()` function generates a Seasonal-Trend-Loess decomposition.
#' The function is "tidy" in the sense that it works
#' on data frames and is designed to work with `dplyr` groups.
#'
#' __STL method__:
#'
#' The STL method implements time series decomposition using
#' the underlying [stats::stl()]. The decomposition separates the
#' "season" and "trend" components from
#' the "observed" values leaving the "remainder".
#'
#' __Frequency & Trend Selection__
#'
#' The user can control two parameters: `.frequency` and `.trend`.
#'
#' 1. The `.frequency` parameter adjusts the "season" component that is removed
#' from the "observed" values.
#' 2. The `.trend` parameter adjusts the
#' trend window (`t.window` parameter from `stl()`) that is used.
#'
#' The user may supply both `.frequency`
#' and `.trend` as time-based durations (e.g. "6 weeks") or numeric values
#' (e.g. 180) or "auto", which automatically selects the frequency and/or trend
#' based on the scale of the time series.
#'
#' @examples
#' library(tidyverse)
#' library(timetk)
#'
#' # ---- SINGLE TIME SERIES DECOMPOSITION ----
#' m4_hourly %>%
#'     filter(id == "H10") %>%
#'     plot_anomaly_diagnostics(
#'         date, value,
#'         # Set features to return, desired frequency and trend
#'         .feature_set = c("observed", "season", "trend", "remainder"),
#'         .frequency   = "24 hours",
#'         .trend       = "1 week",
#'         .interactive = FALSE)
#'
#'
#' # ---- GROUPS ----
#' m4_hourly %>%
#'     group_by(id) %>%
#'     plot_anomaly_diagnostics(
#'         date, value,
#'         .feature_set = c("observed", "season", "trend"),
#'         .interactive = FALSE)
#'
#'
#'
#' @name plot_anomaly_diagnostics
#' @export
plot_anomaly_diagnostics <- function(.data, .date_var, .value, ...,

                                     .frequency = "auto", .trend = "auto",
                                     .alpha = 0.05, .max_anomalies = 0.2,
                                     .message = TRUE,

                                     .facet_ncol = 1, .facet_scales = "free",

                                     .line_color = "#2c3e50", .line_size = 0.5,
                                     .line_type = 1, .line_alpha = 1,

                                     .color_no = "#2c3e50", .color_yes = "#e31a1c",
                                     .fill_ribbon = "grey70", .alpha_ribbon = 1,
                                     .alpha_dots = 1, .size_dots = 1.5,

                                     .title = "Anomaly Diagnostics",
                                     .x_lab = "", .y_lab = "",
                                     .color_lab = "Anomaly",

                                     .interactive = TRUE) {

    # Checks
    value_expr <- rlang::enquo(.value)
    date_var_expr <- rlang::enquo(.date_var)

    if (!is.data.frame(.data)) {
        rlang::abort(".data is not a data-frame or tibble. Please supply a data.frame or tibble.")
    }
    if (rlang::quo_is_missing(date_var_expr)) {
        rlang::abort(".date_var is missing. Please supply a date or date-time column.")
    }
    if (rlang::quo_is_missing(value_expr)) {
        rlang::abort(".value is missing. Please supply a numeric column.")
    }


    UseMethod("plot_anomaly_diagnostics", .data)
}

#' @export
plot_anomaly_diagnostics.data.frame <- function(.data, .date_var, .value, ...,

                                                .frequency = "auto", .trend = "auto",
                                                .alpha = 0.05, .max_anomalies = 0.2,
                                                .message = TRUE,

                                                .facet_ncol = 1, .facet_scales = "free",

                                                .line_color = "#2c3e50", .line_size = 0.5,
                                                .line_type = 1, .line_alpha = 1,

                                                .color_no = "#2c3e50", .color_yes = "#e31a1c",
                                                .fill_ribbon = "grey70", .alpha_ribbon = 1,
                                                .alpha_dots = 1, .size_dots = 1.5,

                                                .title = "Anomaly Diagnostics",
                                                .x_lab = "", .y_lab = "",
                                                .color_lab = "Anomaly",

                                                .interactive = TRUE) {

    # Tidy Eval Setup
    value_expr    <- rlang::enquo(.value)
    date_var_expr <- rlang::enquo(.date_var)
    facets_expr   <- rlang::enquos(...)

    data_formatted      <- .data
    .facet_collapse     <- TRUE
    .facet_collapse_sep <- " "

    # FACET SETUP ----
    facet_names <- data_formatted %>% dplyr::select(!!! facets_expr) %>% colnames()

    if (length(facet_names) > 0) {
        # Handle facets
        data_formatted <- data_formatted %>%
            dplyr::ungroup() %>%
            dplyr::mutate(.facets_collapsed = stringr::str_c(!!! rlang::syms(facet_names),
                                                             sep = .facet_collapse_sep)) %>%
            dplyr::mutate(.facets_collapsed = forcats::as_factor(.facets_collapsed)) %>%
            dplyr::select(-(!!! rlang::syms(facet_names))) %>%
            dplyr::group_by(.facets_collapsed)

        facet_names <- ".facets_collapsed"
    }

    # data_formatted

    # ---- DATA PREPARATION ----

    # Seasonal Diagnostics
    data_formatted <- data_formatted %>%
        tk_anomaly_diagnostics(
            .date_var      = !! date_var_expr,
            .value         = !! value_expr,
            .frequency     = .frequency,
            .trend         = .trend,
            .alpha         = .alpha,
            .max_anomalies = .max_anomalies,
            .message       = .message
        )

    data_formatted

    # ---- VISUALIZATION ----

    g <- data_formatted %>%
        ggplot2::ggplot(ggplot2::aes(!! date_var_expr, observed)) +
        ggplot2::labs(x = .x_lab, y = .y_lab, title = .title, color = .color_lab) +
        theme_tq()

    # Add facets
    if (length(facet_names) > 0) {
        g <- g +
            ggplot2::facet_wrap(
                ggplot2::vars(!!! rlang::syms(facet_names)),
                ncol   = .facet_ncol,
                scales = .facet_scales
            )
    }

    # Add Ribbon
    g <- g +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = recomposed_l1, ymax = recomposed_l2),
                             fill = .fill_ribbon, alpha = .alpha_ribbon)


    # Add line
    g <- g +
        ggplot2::geom_line(
            color    = .line_color,
            size     = .line_size,
            linetype = .line_type,
            alpha    = .line_alpha
        )

    # Add Outliers
    g <- g +
        ggplot2::geom_point(ggplot2::aes_string(color = "anomaly"), size = .size_dots, alpha = .alpha_dots,
                            data = . %>% dplyr::filter(anomaly == "Yes")) +
        ggplot2::scale_color_manual(values = c("No" = .color_no, "Yes" = .color_yes))


    # Convert to interactive if selected
    if (.interactive) {
        p <- plotly::ggplotly(g, dynamicTicks = TRUE)
        return(p)
    } else {
        return(g)
    }
}

#' @export
plot_anomaly_diagnostics.grouped_df <- function(.data, .date_var, .value, ...,

                                                .frequency = "auto", .trend = "auto",
                                                .alpha = 0.05, .max_anomalies = 0.2,
                                                .message = TRUE,

                                                .facet_ncol = 1, .facet_scales = "free",

                                                .line_color = "#2c3e50", .line_size = 0.5,
                                                .line_type = 1, .line_alpha = 1,

                                                .color_no = "#2c3e50", .color_yes = "#e31a1c",
                                                .fill_ribbon = "grey70", .alpha_ribbon = 1,
                                                .alpha_dots = 1, .size_dots = 1.5,

                                                .title = "Anomaly Diagnostics",
                                                .x_lab = "", .y_lab = "",
                                                .color_lab = "Anomaly",

                                                .interactive = TRUE) {


    # Tidy Eval Setup
    group_names   <- dplyr::group_vars(.data)
    facets_expr   <- rlang::enquos(...)

    # Checks
    facet_names <- .data %>% dplyr::ungroup() %>% dplyr::select(!!! facets_expr) %>% colnames()
    if (length(facet_names) > 0) message("plot_anomaly_diagnostics(...): Groups are previously detected. Grouping by: ",
                                         stringr::str_c(group_names, collapse = ", "))

    # ---- DATA SETUP ----

    # Ungroup Data
    data_formatted <- .data %>% dplyr::ungroup()

    # ---- PLOT SETUP ----
    plot_anomaly_diagnostics.data.frame(
        .data               = data_formatted,
        .date_var           = !! rlang::enquo(.date_var),
        .value              = !! rlang::enquo(.value),

        # ...
        !!! rlang::syms(group_names),

        .frequency          = .frequency,
        .trend              = .trend,
        .alpha              = .alpha,
        .max_anomalies      = .max_anomalies,
        .message            = .message,

        .facet_ncol         = .facet_ncol,
        .facet_scales       = .facet_scales,

        .line_color         = .line_color,
        .line_size          = .line_size,
        .line_type          = .line_type,
        .line_alpha         = .line_alpha,

        .color_yes          = .color_yes,
        .fill_ribbon        = .fill_ribbon,
        .alpha_ribbon       = .alpha_ribbon,
        .alpha_dots         = .alpha_dots,
        .size_dots          = .size_dots,

        .title              = .title,
        .x_lab              = .x_lab,
        .y_lab              = .y_lab,
        .color_lab          = .color_lab,

        .interactive        = .interactive
    )

}
