#' Visualize STL Decomposition Features for One or More Time Series
#'
#' An interactive and scalable function for visualizing time series STL Decomposition.
#' Plots are available in interactive `plotly` (default) and static `ggplot2` format.
#'
#' @inheritParams tk_stl_diagnostics
#' @param .data A `tibble` or `data.frame` with a time-based column
#' @param .date_var A column containing either date or date-time values
#' @param .value A column containing numeric values
#' @param .facet_vars One or more grouping columns that broken out into `ggplot2` facets.
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
#' The `plot_stl_diagnostics()` function generates a Seasonal-Trend-Loess decomposition.
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
#' library(dplyr)
#' library(timetk)
#'
#' # ---- SINGLE TIME SERIES DECOMPOSITION ----
#' m4_hourly %>%
#'     filter(id == "H10") %>%
#'     plot_stl_diagnostics(
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
#'     plot_stl_diagnostics(
#'         date, value,
#'         .feature_set = c("observed", "season", "trend"),
#'         .interactive = FALSE)
#'
#'
#'
#' @name plot_stl_diagnostics
#' @export
plot_stl_diagnostics <- function(.data, .date_var, .value, .facet_vars = NULL,
                                 .feature_set = c("observed", "season", "trend", "remainder", "seasadj"),
                                 .frequency = "auto", .trend = "auto", .message = TRUE,

                                 .facet_scales = "free",
                                 .line_color = "#2c3e50", .line_size = 0.5,
                                 .line_type = 1, .line_alpha = 1,

                                 .title = "STL Diagnostics",
                                 .x_lab = "", .y_lab = "",
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
    if (!all(.feature_set %in% c("observed", "season", "trend", "remainder", "seasadj"))) {
        rlang::abort(".featue_set contains values that are not allowed. Select one or more of: 'observed', 'season', 'trend', 'remainder', and/or 'seasadj'.")
    }

    UseMethod("plot_stl_diagnostics", .data)
}

#' @export
plot_stl_diagnostics.data.frame <- function(.data, .date_var, .value, .facet_vars = NULL,
                                            .feature_set = c("observed", "season", "trend", "remainder", "seasadj"),
                                            .frequency = "auto", .trend = "auto", .message = TRUE,

                                            .facet_scales = "free",
                                            .line_color = "#2c3e50", .line_size = 0.5,
                                            .line_type = 1, .line_alpha = 1,

                                            .title = "STL Diagnostics",
                                            .x_lab = "", .y_lab = "",
                                            .interactive = TRUE) {

    # Tidy Eval Setup
    value_expr    <- rlang::enquo(.value)
    date_var_expr <- rlang::enquo(.date_var)
    facets_expr   <- rlang::enquo(.facet_vars)

    data_formatted      <- tibble::as_tibble(.data)
    .facet_collapse     <- TRUE
    .facet_collapse_sep <- " "

    # Facet Names
    facets_expr <- rlang::syms(names(tidyselect::eval_select(facets_expr, .data)))

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
        tk_stl_diagnostics(
            .date_var    = !! date_var_expr,
            .value       = !! value_expr,
            .frequency   = .frequency,
            .trend       = .trend,
            .message     = .message
        )

    # Post process
    data_formatted <- data_formatted %>%
        dplyr::ungroup() %>%
        tidyr::pivot_longer(cols = c(!!! rlang::syms(.feature_set)),
                            names_to = ".group", values_to = ".group_value") %>%
        dplyr::mutate(.group = factor(.group, levels = .feature_set))

    # data_formatted

    # ---- VISUALIZATION ----

    g <- data_formatted %>%
        ggplot2::ggplot(ggplot2::aes(!! date_var_expr, .group_value)) +
        ggplot2::labs(x = .x_lab, y = .y_lab, title = .title)

    # Add line
    g <- g +
        ggplot2::geom_line(
            color    = .line_color,
            size     = .line_size,
            linetype = .line_type,
            alpha    = .line_alpha
        )

    # Add facets
    if (length(facet_names) == 0) {
        facet_ncol <- 1
    } else {
        facet_ncol <- data_formatted %>%
            dplyr::select(facet_names) %>%
            dplyr::distinct() %>%
            nrow()
    }

    facet_groups <- stringr::str_c(facet_names, collapse = " + ")
    if (facet_groups == "") facet_groups <- "."

    facet_formula <- stats::as.formula(paste0(".group ~ ", facet_groups))

    g <- g + ggplot2::facet_wrap(facet_formula, ncol = facet_ncol, scales = .facet_scales)

    # Add theme
    g <- g + theme_tq()

    # Convert to interactive if selected
    if (.interactive) {
        p <- plotly::ggplotly(g)
        return(p)
    } else {
        return(g)
    }
}

#' @export
plot_stl_diagnostics.grouped_df <- function(.data, .date_var, .value, .facet_vars = NULL,
                                            .feature_set = c("observed", "season", "trend", "remainder", "seasadj"),
                                            .frequency = "auto", .trend = "auto", .message = TRUE,

                                            .facet_scales = "free",
                                            .line_color = "#2c3e50", .line_size = 0.5,
                                            .line_type = 1, .line_alpha = 1,

                                            .title = "STL Diagnostics",
                                            .x_lab = "", .y_lab = "",
                                            .interactive = TRUE) {


    # Tidy Eval Setup
    group_names   <- dplyr::group_vars(.data)
    facets_expr   <- rlang::enquos(.facet_vars)

    # Checks
    facet_names <- .data %>% dplyr::ungroup() %>% dplyr::select(!!! facets_expr) %>% colnames()
    if (length(facet_names) > 0) message("plot_stl_diagnostics(...): Groups are previously detected. Grouping by: ",
                                         stringr::str_c(group_names, collapse = ", "))

    # ---- DATA SETUP ----

    # Ungroup Data
    data_formatted <- .data %>% dplyr::ungroup()

    # ---- PLOT SETUP ----
    plot_stl_diagnostics.data.frame(
        .data               = data_formatted,
        .date_var           = !! rlang::enquo(.date_var),
        .value              = !! rlang::enquo(.value),

        # ...
        # !!! rlang::syms(group_names),
        .facet_vars         = !! enquo(group_names),

        .frequency          = .frequency,
        .trend              = .trend,
        .message            = .message,
        .feature_set        = .feature_set,

        .facet_scales       = .facet_scales,
        .line_color         = .line_color,
        .line_size          = .line_size,
        .line_type          = .line_type,
        .line_alpha         = .line_alpha,

        .title              = .title,
        .x_lab              = .x_lab,
        .y_lab              = .y_lab,
        .interactive        = .interactive
    )

}
