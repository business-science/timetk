# 1.0 PLOT ANOMALIES ----

#' Visualize Anomalies for One or More Time Series
#'
#' `plot_anomalies()` is an interactive and scalable function for visualizing anomalies in time series data.
#' Plots are available in interactive `plotly` (default) and static `ggplot2` format.
#'
#' @param .data A `tibble` or `data.frame` that has been anomalized by `anomalize()`
#' @param .date_var A column containing either date or date-time values
#' @param .facet_vars One or more grouping columns that broken out into `ggplot2` facets.
#'  These can be selected using `tidyselect()` helpers (e.g `contains()`).
#' @param .facet_ncol Number of facet columns.
#' @param .facet_nrow Number of facet rows (only used for `.trelliscope = TRUE`)
#' @param .facet_scales Control facet x & y-axis ranges. Options include "fixed", "free", "free_y", "free_x"
#' @param .facet_dir The direction of faceting ("h" for horizontal, "v" for vertical). Default is "h".
#' @param .facet_collapse Multiple facets included on one facet strip instead of
#'  multiple facet strips.
#' @param .facet_collapse_sep The separator used for collapsing facets.
#' @param .facet_strip_remove Whether or not to remove the strip and text label for each facet.
#' @param .line_color Line color.
#' @param .line_size Line size.
#' @param .line_type Line type.
#' @param .line_alpha Line alpha (opacity). Range: (0, 1).
#' @param .anom_color Color for the anomaly dots
#' @param .anom_alpha Opacity for the anomaly dots. Range: (0, 1).
#' @param .anom_size Size for the anomaly dots
#' @param .ribbon_fill Fill color for the acceptable range
#' @param .ribbon_alpha Fill opacity for the acceptable range. Range: (0, 1).
#' @param .legend_show Toggles on/off the Legend
#' @param .title Plot title.
#' @param .x_lab Plot x-axis label
#' @param .y_lab Plot y-axis label
#' @param .color_lab Plot label for the color legend
#' @param .interactive If TRUE, returns a `plotly` interactive plot.
#'  If FALSE, returns a static `ggplot2` plot.
#' @param .trelliscope Returns either a normal plot or a trelliscopejs plot (great for many time series)
#'  Must have `trelliscopejs` installed.
#' @param .trelliscope_params Pass parameters to the `trelliscopejs::facet_trelliscope()` function as a `list()`.
#'  The only parameters that cannot be passed are:
#'  - `ncol`: use `.facet_ncol`
#'  - `nrow`: use `.facet_nrow`
#'  - `scales`: use `facet_scales`
#'  - `as_plotly`: use `.interactive`
#'
#'
#' @return A `plotly` or `ggplot2` visualization
#'
#'
#' @examples
#' # Plot Anomalies
#' library(dplyr)
#'
#' walmart_sales_weekly %>%
#'     filter(id %in% c("1_1", "1_3")) %>%
#'     group_by(id) %>%
#'     anomalize(Date, Weekly_Sales) %>%
#'     plot_anomalies(Date, .facet_ncol = 2, .ribbon_alpha = 0.25, .interactive = FALSE)
#'
#' @name plot_anomalies
#' @export
plot_anomalies <- function(
        .data,
        .date_var,

        .facet_vars = NULL,

        .facet_ncol = 1,
        .facet_nrow = 1,
        .facet_scales = "free",
        .facet_dir = "h",
        .facet_collapse = FALSE,
        .facet_collapse_sep = " ",
        .facet_strip_remove = FALSE,

        .line_color = "#2c3e50",
        .line_size = 0.5,
        .line_type = 1,
        .line_alpha = 1,

        .anom_color = "#e31a1c",
        .anom_alpha = 1,
        .anom_size = 1.5,

        .ribbon_fill = "grey20",
        .ribbon_alpha = 0.20,

        .legend_show = TRUE,

        .title = "Anomaly Plot",
        .x_lab = "",
        .y_lab = "",
        .color_lab = "Anomaly",

        .interactive = TRUE,
        .trelliscope = FALSE,
        .trelliscope_params = list()
) {

    # Checks
    date_var_expr <- rlang::enquo(.date_var)

    if (!is.data.frame(.data)) {
        rlang::abort("`.data` must be a a data-frame or tibble. Please supply a data.frame or tibble.")
    }
    if (rlang::quo_is_missing(date_var_expr)) {
        rlang::abort(".date_var is missing. Please supply a date or date-time column.")
    }
    column_names <- names(.data)
    check_names  <- c("observed", "anomaly") %in% column_names
    if (!all(check_names)) stop('Error in plot_anomalies(): column names are missing. Run `anomalize()` and make sure: observed, remainder, anomaly, recomposed_l1, and recomposed_l2 are present', call. = FALSE)

    UseMethod("plot_anomalies", .data)
}

#' @export
plot_anomalies.data.frame <- function(
        .data,
        .date_var,

        .facet_vars = NULL,

        .facet_ncol = 1,
        .facet_nrow = 1,
        .facet_scales = "free",
        .facet_dir = "h",
        .facet_collapse = FALSE,
        .facet_collapse_sep = " ",
        .facet_strip_remove = FALSE,

        .line_color = "#2c3e50",
        .line_size = 0.5,
        .line_type = 1,
        .line_alpha = 1,

        .anom_color = "#e31a1c",
        .anom_alpha = 1,
        .anom_size = 1.5,

        .ribbon_fill = "grey20",
        .ribbon_alpha = 0.20,

        .legend_show = TRUE,

        .title = "Anomaly Plot",
        .x_lab = "",
        .y_lab = "",
        .color_lab = "Anomaly",

        .interactive = TRUE,
        .trelliscope = FALSE,
        .trelliscope_params = list()
) {

    # Tidy Eval Setup
    date_var_expr <- rlang::enquo(.date_var)
    facets_expr   <- rlang::enquo(.facet_vars)

    # Facet Names
    facets_expr <- rlang::syms(names(tidyselect::eval_select(facets_expr, .data)))

    data_formatted      <- tibble::as_tibble(.data)

    # FACET SETUP ----
    facet_names <- data_formatted %>% dplyr::select(!!! facets_expr) %>% colnames()

    if (length(facet_names) > 0) {
        if (.facet_collapse) {

            data_formatted <- data_formatted %>%
                dplyr::ungroup() %>%
                dplyr::mutate(.facets_collapsed = stringr::str_c(!!! rlang::syms(facet_names),
                                                                 sep = .facet_collapse_sep)) %>%
                dplyr::mutate(.facets_collapsed = forcats::as_factor(.facets_collapsed)) %>%
                dplyr::group_by(.facets_collapsed)

            facet_names <- ".facets_collapsed"

        } else {
            data_formatted <- data_formatted %>%
                dplyr::group_by(!!! rlang::syms(facet_names))
        }
    }


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
                scales = .facet_scales,
                dir    = .facet_dir
            )
    }

    # Add Ribbon
    g <- g +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = recomposed_l1, ymax = recomposed_l2),
                             fill = .ribbon_fill, alpha = .ribbon_alpha)


    # Add line
    g <- g +
        ggplot2::geom_line(
            color     = .line_color,
            linewidth = .line_size,
            linetype  = .line_type,
            alpha     = .line_alpha
        )

    # Add Outliers
    g <- g +
        ggplot2::geom_point(ggplot2::aes_string(color = "anomaly"), size = .anom_size, alpha = .anom_alpha,
                            data = . %>% dplyr::filter(anomaly == "Yes")) +
        ggplot2::scale_color_manual(values = c("Yes" = .anom_color))

    # Show Legend?
    if (!.legend_show) {
        g <- g +
            ggplot2::theme(legend.position = "none")
    }

    # Remove the facet strip?
    if (.facet_strip_remove) {
        g <- g +
            ggplot2::theme(
                strip.background = ggplot2::element_blank(),
                strip.text.x     = ggplot2::element_blank()
            )
    }

    # Convert to trelliscope and/or plotly?
    if (!.trelliscope) {

        if (.interactive) {

            g <- plotly::ggplotly(g)

        }

    } else {

        trell <- do.call(trelliscopejs::facet_trelliscope, c(
            list(
                facets    = ggplot2::vars(!!! rlang::syms(facet_names)),
                ncol      = .facet_ncol,
                nrow      = .facet_nrow,
                scales    = .facet_scales,
                as_plotly = .interactive
            ),
            .trelliscope_params
        ))

        g <- g + trell

    }

    return(g)
}

#' @export
plot_anomalies.grouped_df <- function(
        .data,
        .date_var,

        .facet_vars = NULL,

        .facet_ncol = 1,
        .facet_nrow = 1,
        .facet_scales = "free",
        .facet_dir = "h",
        .facet_collapse = FALSE,
        .facet_collapse_sep = " ",
        .facet_strip_remove = FALSE,

        .line_color = "#2c3e50",
        .line_size = 0.5,
        .line_type = 1,
        .line_alpha = 1,

        .anom_color = "#e31a1c",
        .anom_alpha = 1,
        .anom_size = 1.5,

        .ribbon_fill = "grey20",
        .ribbon_alpha = 0.20,

        .legend_show = TRUE,

        .title = "Anomaly Plot",
        .x_lab = "",
        .y_lab = "",
        .color_lab = "Anomaly",

        .interactive = TRUE,
        .trelliscope = FALSE,
        .trelliscope_params = list()
) {


    # Tidy Eval Setup
    group_names   <- dplyr::group_vars(.data)
    facets_expr   <- rlang::enquos(.facet_vars)

    # Checks
    facet_names <- .data %>% dplyr::ungroup() %>% dplyr::select(!!! facets_expr) %>% colnames()
    if (length(facet_names) > 0) message("plot_anomalies(...): Groups are previously detected. Grouping by: ",
                                         stringr::str_c(group_names, collapse = ", "))

    # ---- DATA SETUP ----

    # Ungroup Data
    data_formatted <- .data %>% dplyr::ungroup()

    # ---- PLOT SETUP ----
    plot_anomalies.data.frame(
        .data               = data_formatted,
        .date_var           = !! rlang::enquo(.date_var),

        .facet_vars         = !! enquo(group_names),

        .facet_ncol         = .facet_ncol,
        .facet_nrow         = .facet_nrow,
        .facet_scales       = .facet_scales,
        .facet_dir          = .facet_dir,
        .facet_strip_remove = .facet_strip_remove,

        .line_color         = .line_color,
        .line_size          = .line_size,
        .line_type          = .line_type,
        .line_alpha         = .line_alpha,

        .anom_color         = .anom_color,
        .anom_alpha         = .anom_alpha,
        .anom_size          = .anom_size,

        .ribbon_fill        = .ribbon_fill,
        .ribbon_alpha       = .ribbon_alpha,

        .legend_show        = .legend_show,

        .title              = .title,
        .x_lab              = .x_lab,
        .y_lab              = .y_lab,
        .color_lab          = .color_lab,

        .interactive        = .interactive,
        .trelliscope        = .trelliscope,
        .trelliscope_params = .trelliscope_params
    )

}

# 2.0 PLOT ANOMALIES DECOMP ----
#' Visualize Anomaly Decomposition
#'
#' `plot_anomalies_decomp()`: Takes in data from the `anomalize()`
#' function, and returns a plot of the anomaly decomposition. Useful for interpeting
#' how the `anomalize()` function is determining outliers from "remainder".
#'
#' @param .data A `tibble` or `data.frame` that has been anomalized by `anomalize()`
#' @param .date_var A column containing either date or date-time values
#' @param .facet_vars One or more grouping columns that broken out into `ggplot2` facets.
#'  These can be selected using `tidyselect()` helpers (e.g `contains()`).
#' @param .facet_ncol Number of facet columns.
#' @param .facet_nrow Number of facet rows (only used for `.trelliscope = TRUE`)
#' @param .facet_scales Control facet x & y-axis ranges. Options include "fixed", "free", "free_y", "free_x"
#' @param .facet_dir The direction of faceting ("h" for horizontal, "v" for vertical). Default is "h".
#' @param .facet_collapse Multiple facets included on one facet strip instead of
#'  multiple facet strips.
#' @param .facet_collapse_sep The separator used for collapsing facets.
#' @param .facet_strip_remove Whether or not to remove the strip and text label for each facet.
#' @param .line_color Line color.
#' @param .line_size Line size.
#' @param .line_type Line type.
#' @param .line_alpha Line alpha (opacity). Range: (0, 1).
#' @param .anom_color Color for the anomaly dots
#' @param .anom_alpha Opacity for the anomaly dots. Range: (0, 1).
#' @param .anom_size Size for the anomaly dots
#' @param .ribbon_fill Fill color for the acceptable range
#' @param .ribbon_alpha Fill opacity for the acceptable range. Range: (0, 1).
#' @param .legend_show Toggles on/off the Legend
#' @param .title Plot title.
#' @param .x_lab Plot x-axis label
#' @param .y_lab Plot y-axis label
#' @param .color_lab Plot label for the color legend
#' @param .interactive If TRUE, returns a `plotly` interactive plot.
#'  If FALSE, returns a static `ggplot2` plot.
#' @param .trelliscope Returns either a normal plot or a trelliscopejs plot (great for many time series)
#'  Must have `trelliscopejs` installed.
#' @param .trelliscope_params Pass parameters to the `trelliscopejs::facet_trelliscope()` function as a `list()`.
#'  The only parameters that cannot be passed are:
#'  - `ncol`: use `.facet_ncol`
#'  - `nrow`: use `.facet_nrow`
#'  - `scales`: use `facet_scales`
#'  - `as_plotly`: use `.interactive`
#'
#' @examples
#' # Plot Anomalies Decomposition
#' library(dplyr)
#'
#' walmart_sales_weekly %>%
#'     filter(id %in% c("1_1", "1_3")) %>%
#'     group_by(id) %>%
#'     anomalize(Date, Weekly_Sales, .message = FALSE) %>%
#'     plot_anomalies_decomp(Date, .interactive = FALSE)
#'
#' @name plot_anomalies
#' @export
plot_anomalies_decomp <- function(
        .data,
        .date_var,

        .facet_vars = NULL,

        .facet_scales = "free",

        .line_color = "#2c3e50",
        .line_size = 0.5,
        .line_type = 1,
        .line_alpha = 1,

        .title = "Anomaly Decomposition Plot",
        .x_lab = "",
        .y_lab = "",
        .interactive = TRUE
) {

    date_var_expr <- rlang::enquo(.date_var)

    if (!is.data.frame(.data)) {
        rlang::abort(".data is not a data-frame or tibble. Please supply a data.frame or tibble.")
    }
    if (rlang::quo_is_missing(date_var_expr)) {
        rlang::abort(".date_var is missing. Please supply a date or date-time column.")
    }

    column_names <- names(.data)
    check_names  <- c("observed", "season", "trend", "remainder") %in% column_names
    if (!all(check_names)) stop('Error in plot_anomalies_decomp(): column names are missing. Run `anomalize()` and make sure: observed, remainder, anomaly, recomposed_l1, and recomposed_l2 are present', call. = FALSE)

    UseMethod("plot_anomalies_decomp", .data)

}

#' @export
plot_anomalies_decomp.data.frame <- function(
    .data,
    .date_var,

    .facet_vars = NULL,

    .facet_scales = "free",

    .line_color = "#2c3e50",
    .line_size = 0.5,
    .line_type = 1,
    .line_alpha = 1,

    .title = "Anomaly Decomposition Plot",
    .x_lab = "",
    .y_lab = "",
    .interactive = TRUE
) {

    # ---- FORMAT DATA ----

    date_var_expr <- rlang::enquo(.date_var)

    data_formatted <- .data
    feature_set <- c("observed", "season", "trend", "remainder")

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

    data_formatted <- data_formatted %>%
        dplyr::ungroup() %>%
        tidyr::pivot_longer(cols = c(!!! rlang::syms(feature_set)),
                            names_to = ".group", values_to = ".group_value") %>%
        dplyr::mutate(.group = factor(.group, levels = feature_set))

    # data_formatted

    # ---- VISUALIZATION ----

    g <- data_formatted %>%
        ggplot2::ggplot(ggplot2::aes(!! date_var_expr, .group_value)) +
        ggplot2::labs(x = .x_lab, y = .y_lab, title = .title)

    # Add line
    g <- g +
        ggplot2::geom_line(
            color     = .line_color,
            linewidth = .line_size,
            linetype  = .line_type,
            alpha     = .line_alpha
        )

    # Add facets
    if (length(facet_names) == 0) {
        facet_ncol <- 1
    } else {
        facet_ncol <- data_formatted %>%
            dplyr::distinct(dplyr::pick(dplyr::all_of(facet_names))) %>%
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
plot_anomalies_decomp.grouped_df <- function(
    .data,
    .date_var,

    .facet_vars = NULL,

    .facet_scales = "free",

    .line_color = "#2c3e50",
    .line_size = 0.5,
    .line_type = 1,
    .line_alpha = 1,

    .title = "Anomaly Decomposition Plot",
    .x_lab = "",
    .y_lab = "",
    .interactive = TRUE
) {

    # Tidy Eval Setup
    group_names   <- dplyr::group_vars(.data)
    facets_expr   <- rlang::enquos(.facet_vars)

    # Checks
    facet_names <- .data %>% dplyr::ungroup() %>% dplyr::select(!!! facets_expr) %>% colnames()
    if (length(facet_names) > 0) message("plot_anomalies_decomp(...): Groups are previously detected. Grouping by: ",
                                         stringr::str_c(group_names, collapse = ", "))

    # ---- DATA SETUP ----

    # Ungroup Data
    data_formatted <- .data %>% dplyr::ungroup()

    # ---- PLOT SETUP ----
    g <- plot_anomalies_decomp.data.frame(
        .data               = data_formatted,
        .date_var           = !! rlang::enquo(.date_var),

        .facet_vars         = !! enquo(group_names),

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

    return(g)

}

# 3.0 PLOT ANOMALIES CLEANED -----

#' Visualize Anomalies for One or More Time Series
#'
#' `plot_anomalies_cleaned()` helps users visualize the before/after of
#' cleaning anomalies.
#'
#' @param .data A `tibble` or `data.frame` that has been anomalized by `anomalize()`
#' @param .date_var A column containing either date or date-time values
#' @param .facet_vars One or more grouping columns that broken out into `ggplot2` facets.
#'  These can be selected using `tidyselect()` helpers (e.g `contains()`).
#' @param .facet_ncol Number of facet columns.
#' @param .facet_nrow Number of facet rows (only used for `.trelliscope = TRUE`)
#' @param .facet_scales Control facet x & y-axis ranges. Options include "fixed", "free", "free_y", "free_x"
#' @param .facet_dir The direction of faceting ("h" for horizontal, "v" for vertical). Default is "h".
#' @param .facet_collapse Multiple facets included on one facet strip instead of
#'  multiple facet strips.
#' @param .facet_collapse_sep The separator used for collapsing facets.
#' @param .facet_strip_remove Whether or not to remove the strip and text label for each facet.
#' @param .line_color Line color.
#' @param .line_size Line size.
#' @param .line_type Line type.
#' @param .line_alpha Line alpha (opacity). Range: (0, 1).
#' @param .cleaned_line_color Line color.
#' @param .cleaned_line_size Line size.
#' @param .cleaned_line_type Line type.
#' @param .cleaned_line_alpha Line alpha (opacity). Range: (0, 1).
#' @param .legend_show Toggles on/off the Legend
#' @param .title Plot title.
#' @param .x_lab Plot x-axis label
#' @param .y_lab Plot y-axis label
#' @param .color_lab Plot label for the color legend
#' @param .interactive If TRUE, returns a `plotly` interactive plot.
#'  If FALSE, returns a static `ggplot2` plot.
#' @param .trelliscope Returns either a normal plot or a trelliscopejs plot (great for many time series)
#'  Must have `trelliscopejs` installed.
#' @param .trelliscope_params Pass parameters to the `trelliscopejs::facet_trelliscope()` function as a `list()`.
#'  The only parameters that cannot be passed are:
#'  - `ncol`: use `.facet_ncol`
#'  - `nrow`: use `.facet_nrow`
#'  - `scales`: use `facet_scales`
#'  - `as_plotly`: use `.interactive`
#'
#'
#' @examples
#' # Plot Anomalies Cleaned
#' library(dplyr)
#'
#' walmart_sales_weekly %>%
#'     filter(id %in% c("1_1", "1_3")) %>%
#'     group_by(id) %>%
#'     anomalize(Date, Weekly_Sales, .message = FALSE) %>%
#'     plot_anomalies_cleaned(Date, .facet_ncol = 2, .interactive = FALSE)
#'
#' @name plot_anomalies
#' @export
plot_anomalies_cleaned <- function(
    .data,
    .date_var,

    .facet_vars = NULL,

    .facet_ncol = 1,
    .facet_nrow = 1,
    .facet_scales = "free",
    .facet_dir = "h",
    .facet_collapse = FALSE,
    .facet_collapse_sep = " ",
    .facet_strip_remove = FALSE,

    .line_color = "#2c3e50",
    .line_size = 0.5,
    .line_type = 1,
    .line_alpha = 1,

    .cleaned_line_color = "#e31a1c",
    .cleaned_line_size = 0.5,
    .cleaned_line_type = 1,
    .cleaned_line_alpha = 1,

    .legend_show = TRUE,

    .title = "Anomalies Cleaned Plot",
    .x_lab = "",
    .y_lab = "",
    .color_lab = "Legend",

    .interactive = TRUE,
    .trelliscope = FALSE,
    .trelliscope_params = list()
) {

    date_var_expr <- rlang::enquo(.date_var)

    if (!is.data.frame(.data)) {
        rlang::abort(".data is not a data-frame or tibble. Please supply a data.frame or tibble.")
    }
    if (rlang::quo_is_missing(date_var_expr)) {
        rlang::abort(".date_var is missing. Please supply a date or date-time column.")
    }

    column_names <- names(.data)
    check_names  <- c("observed", "observed_clean") %in% column_names
    if (!all(check_names)) stop('Error in plot_anomalies_decomp(): column names are missing. Run `anomalize()` and make sure: observed, remainder, anomaly, recomposed_l1, and recomposed_l2 are present', call. = FALSE)

    UseMethod("plot_anomalies_cleaned", .data)

}

#' @export
plot_anomalies_cleaned.data.frame <- function(
    .data,
    .date_var,

    .facet_vars = NULL,

    .facet_ncol = 1,
    .facet_nrow = 1,
    .facet_scales = "free",
    .facet_dir = "h",
    .facet_collapse = FALSE,
    .facet_collapse_sep = " ",
    .facet_strip_remove = FALSE,

    .line_color = "#2c3e50",
    .line_size = 0.5,
    .line_type = 1,
    .line_alpha = 1,

    .cleaned_line_color = "#e31a1c",
    .cleaned_line_size = 0.5,
    .cleaned_line_type = 1,
    .cleaned_line_alpha = 1,

    .legend_show = TRUE,

    .title = "Anomalies Cleaned Plot",
    .x_lab = "",
    .y_lab = "",
    .color_lab = "Legend",

    .interactive = TRUE,
    .trelliscope = FALSE,
    .trelliscope_params = list()
) {

    # Tidy Eval Setup
    date_var_expr <- rlang::enquo(.date_var)
    facets_expr   <- rlang::enquo(.facet_vars)

    # Facet Names
    facets_expr <- rlang::syms(names(tidyselect::eval_select(facets_expr, .data)))

    data_formatted      <- tibble::as_tibble(.data)

    # FACET SETUP ----
    facet_names <- data_formatted %>% dplyr::select(!!! facets_expr) %>% colnames()

    if (length(facet_names) > 0) {
        if (.facet_collapse) {

            data_formatted <- data_formatted %>%
                dplyr::ungroup() %>%
                dplyr::mutate(.facets_collapsed = stringr::str_c(!!! rlang::syms(facet_names),
                                                                 sep = .facet_collapse_sep)) %>%
                dplyr::mutate(.facets_collapsed = forcats::as_factor(.facets_collapsed)) %>%
                dplyr::group_by(.facets_collapsed)

            facet_names <- ".facets_collapsed"

        } else {
            data_formatted <- data_formatted %>%
                dplyr::group_by(!!! rlang::syms(facet_names))
        }
    }


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
                scales = .facet_scales,
                dir    = .facet_dir
            )
    }


    # Add line - observed
    g <- g +
        ggplot2::geom_line(
            ggplot2::aes(color = "Observed"),
            # color     = .line_color,
            linewidth = .line_size,
            linetype  = .line_type,
            alpha     = .line_alpha
        )

    # Add color scale
    g <- g +
        ggplot2::scale_color_manual(values = c(.line_color, .cleaned_line_color))

    # Add line - observed_clean
    g <- g +
        ggplot2::geom_line(
            ggplot2::aes(y = observed_clean, color = "Observed Cleaned"),
            # color     = .cleaned_line_color,
            linewidth = .cleaned_line_size,
            linetype  = .cleaned_line_type,
            alpha     = .cleaned_line_alpha
        )

    # Show Legend?
    if (!.legend_show) {
        g <- g +
            ggplot2::theme(legend.position = "none")
    }

    # Remove the facet strip?
    if (.facet_strip_remove) {
        g <- g +
            ggplot2::theme(
                strip.background = ggplot2::element_blank(),
                strip.text.x     = ggplot2::element_blank()
            )
    }

    # Convert to trelliscope and/or plotly?
    if (!.trelliscope) {

        if (.interactive) {

            g <- plotly::ggplotly(g)

        }

    } else {

        trell <- do.call(trelliscopejs::facet_trelliscope, c(
            list(
                facets    = ggplot2::vars(!!! rlang::syms(facet_names)),
                ncol      = .facet_ncol,
                nrow      = .facet_nrow,
                scales    = .facet_scales,
                as_plotly = .interactive
            ),
            .trelliscope_params
        ))

        g <- g + trell

    }

    return(g)

}

#' @export
plot_anomalies_cleaned.grouped_df <- function(
    .data,
    .date_var,

    .facet_vars = NULL,

    .facet_ncol = 1,
    .facet_nrow = 1,
    .facet_scales = "free",
    .facet_dir = "h",
    .facet_collapse = FALSE,
    .facet_collapse_sep = " ",
    .facet_strip_remove = FALSE,

    .line_color = "#2c3e50",
    .line_size = 0.5,
    .line_type = 1,
    .line_alpha = 1,

    .cleaned_line_color = "#e31a1c",
    .cleaned_line_size = 0.5,
    .cleaned_line_type = 1,
    .cleaned_line_alpha = 1,

    .legend_show = TRUE,

    .title = "Anomalies Cleaned Plot",
    .x_lab = "",
    .y_lab = "",
    .color_lab = "Legend",

    .interactive = TRUE,
    .trelliscope = FALSE,
    .trelliscope_params = list()
) {


    # Tidy Eval Setup
    group_names   <- dplyr::group_vars(.data)
    facets_expr   <- rlang::enquos(.facet_vars)

    # Checks
    facet_names <- .data %>% dplyr::ungroup() %>% dplyr::select(!!! facets_expr) %>% colnames()
    if (length(facet_names) > 0) message("plot_anomalies_cleaned(...): Groups are previously detected. Grouping by: ",
                                         stringr::str_c(group_names, collapse = ", "))

    # ---- DATA SETUP ----

    # Ungroup Data
    data_formatted <- .data %>% dplyr::ungroup()

    # ---- PLOT SETUP ----
    g <- plot_anomalies_cleaned.data.frame(
        .data               = data_formatted,
        .date_var           = !! rlang::enquo(.date_var),

        .facet_vars         = !! enquo(group_names),

        .facet_ncol         = .facet_ncol,
        .facet_nrow         = .facet_nrow,
        .facet_scales       = .facet_scales,
        .facet_dir          = .facet_dir,
        .facet_strip_remove = .facet_strip_remove,

        .line_color         = .line_color,
        .line_size          = .line_size,
        .line_type          = .line_type,
        .line_alpha         = .line_alpha,

        .cleaned_line_color = .cleaned_line_color,
        .cleaned_line_size  = .cleaned_line_size,
        .cleaned_line_type  = .cleaned_line_type,
        .cleaned_line_alpha = .cleaned_line_alpha,

        .legend_show        = .legend_show,

        .title              = .title,
        .x_lab              = .x_lab,
        .y_lab              = .y_lab,
        .color_lab          = .color_lab,

        .interactive        = .interactive,
        .trelliscope        = .trelliscope,
        .trelliscope_params = .trelliscope_params
    )

    return(g)

}


