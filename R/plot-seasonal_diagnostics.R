#' Visualize Multiple Seasonality Features for One or More Time Series
#'
#' An interactive and scalable function for visualizing time series seasonality.
#' Plots are available in interactive `plotly` (default) and static `ggplot2` format.
#'
#' @param .data A `tibble` or `data.frame` with a time-based column
#' @param .date_var A column containing either date or date-time values
#' @param .value A column containing numeric values
#' @param .facet_vars One or more grouping columns that broken out into `ggplot2` facets.
#'  These can be selected using `tidyselect()` helpers (e.g `contains()`).
#' @param .feature_set One or multiple selections to analyze for seasonality. Choices include:
#'  - "auto" - Automatically selects features based on the time stamps and length of the series.
#'  - "second" - Good for analyzing seasonality by second of each minute.
#'  - "minute" - Good for analyzing seasonality by minute of the hour
#'  - "hour" - Good for analyzing seasonality by hour of the day
#'  - "wday.lbl" - Labeled weekdays. Good for analyzing seasonality by day of the week.
#'  - "week" - Good for analyzing seasonality by week of the year.
#'  - "month.lbl" - Labeled months. Good for analyzing seasonality by month of the year.
#'  - "quarter" - Good for analyzing seasonality by quarter of the year
#'  - "year" - Good for analyzing seasonality over multiple years.
#' @param .geom Either "boxplot" or "violin"
#' @param .geom_color Geometry color. Line color.
#'  Use keyword: "scale_color" to change the color by the facet.
#' @param .geom_outlier_color Color used to highlight outliers.
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
#'
#' __Automatic Feature Selection__
#'
#' Internal calculations are performed to detect a sub-range of features to include
#' useing the following logic:
#'
#' - The _minimum_ feature is selected based on the median difference between consecutive
#' timestamps
#' - The _maximum_ feature is selected based on having 2 full periods.
#'
#' Example: Hourly timestamp data that lasts more than 2 weeks will have the following features:
#' "hour", "wday.lbl", and "week".
#'
#' __Scalable with Grouped Data Frames__
#'
#' This function respects grouped `data.frame` and `tibbles` that were made with `dplyr::group_by()`.
#'
#' For grouped data, the automatic feature selection returned is a collection of all
#' features within the sub-groups. This means extra features are returned even though
#' they may be meaningless for some of the groups.
#'
#' __Transformations__
#'
#' The `.value` parameter respects transformations (e.g. `.value = log(sales)`).
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(timetk)
#'
#' # ---- MULTIPLE FREQUENCY ----
#' # Taylor 30-minute dataset from forecast package
#' taylor_30_min
#'
#' # Visualize series
#' taylor_30_min %>%
#'     plot_time_series(date, value, .interactive = FALSE)
#'
#' # Visualize seasonality
#' taylor_30_min %>%
#'     plot_seasonal_diagnostics(date, value, .interactive = FALSE)
#'
#' # ---- GROUPED EXAMPLES ----
#' # m4 hourly dataset
#' m4_hourly
#'
#' # Visualize series
#' m4_hourly %>%
#'     group_by(id) %>%
#'     plot_time_series(date, value, .facet_scales = "free", .interactive = FALSE)
#'
#' # Visualize seasonality
#' m4_hourly %>%
#'     group_by(id) %>%
#'     plot_seasonal_diagnostics(date, value, .interactive = FALSE)
#'
#' }
#'
#' @name plot_seasonal_diagnostics
#' @export
plot_seasonal_diagnostics <- function(.data, .date_var, .value, .facet_vars = NULL,
                                      .feature_set = "auto",
                                      .geom = c("boxplot", "violin"),
                                      .geom_color = "#2c3e50",
                                      .geom_outlier_color = "#2c3e50",
                                      # .jitter = FALSE,
                                      # .jitter_color = "#2c3e50", .jitter_alpha = 0.5,
                                      # .jitter_width = NULL,
                                      .title = "Seasonal Diagnostics",
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

    UseMethod("plot_seasonal_diagnostics", .data)
}

#' @export
plot_seasonal_diagnostics.data.frame <- function(.data, .date_var, .value, .facet_vars = NULL,
                                                 .feature_set = "auto",
                                                 .geom = c("boxplot", "violin"),
                                                 .geom_color = "#2c3e50",
                                                 .geom_outlier_color = "#2c3e50",
                                                 # .jitter = FALSE,
                                                 # .jitter_color = "#2c3e50", .jitter_alpha = 0.5,
                                                 # .jitter_width = NULL,
                                                 .title = "Seasonal Diagnostics",
                                                 .x_lab = "", .y_lab = "",
                                                 .interactive = TRUE) {

    # Tidy Eval Setup
    value_expr     <- rlang::enquo(.value)
    date_var_expr  <- rlang::enquo(.date_var)
    facets_expr    <- rlang::enquo(.facet_vars)

    data_formatted      <- tibble::as_tibble(.data)
    .facet_collapse     <- TRUE
    .facet_collapse_sep <- " "

    # Facet Names
    facets_expr <- rlang::syms(names(tidyselect::eval_select(facets_expr, .data)))

    # FACET SETUP ----
    facet_names <- data_formatted %>% dplyr::select(!!! facets_expr) %>% colnames()

    if (length(facet_names) > 0) {
        # Handle facets
        if (.facet_collapse) {

            data_formatted <- data_formatted %>%
                dplyr::ungroup() %>%
                dplyr::mutate(.facets_collapsed = stringr::str_c(!!! rlang::syms(facet_names),
                                                                 sep = .facet_collapse_sep)) %>%
                dplyr::mutate(.facets_collapsed = forcats::as_factor(.facets_collapsed)) %>%
                dplyr::select(-(!!! rlang::syms(facet_names))) %>%
                dplyr::group_by(.facets_collapsed)

            facet_names <- ".facets_collapsed"

        } else {
            data_formatted <- data_formatted %>%
                dplyr::group_by(!!! rlang::syms(facet_names))
        }

        # Get feature set
        if (.feature_set[1] == "auto") {
            .feature_set <- data_formatted %>%
                dplyr::group_split() %>%
                purrr::map(.f = function(df) {
                    df %>%
                        dplyr::pull(!! date_var_expr) %>%
                        get_seasonal_auto_features()
                }) %>%
                purrr::flatten_chr() %>%
                unique()
        }
    }

    # If not grouped, .feature_set == "auto" - collect .features
    if (.feature_set[1] == "auto") {
        .feature_set <- data_formatted %>%
            dplyr::pull(!! date_var_expr) %>%
            get_seasonal_auto_features()
    }

    # data_formatted

    # ---- DATA PREPARATION ----

    # Seasonal Diagnostics
    data_formatted <- data_formatted %>%
        tk_seasonal_diagnostics(
            .date_var    = !! date_var_expr,
            .value       = !! value_expr,
            .feature_set = .feature_set
        )

    # Post process
    data_formatted <- data_formatted %>%
        dplyr::select(-(!! date_var_expr)) %>%
        dplyr::ungroup()

    if (length(facet_names) > 0) {
        data_formatted <- data_formatted %>%
            tidyr::pivot_longer(cols = c(-.value, -(!!! rlang::syms(facet_names))),
                                names_to = ".group", values_to = ".group_value")
    } else {
        data_formatted <- data_formatted %>%
            tidyr::pivot_longer(cols = c(-.value),
                                names_to = ".group", values_to = ".group_value")
    }

    data_formatted <- data_formatted %>%
        dplyr::ungroup() %>%
        dplyr::mutate(.group = factor(.group, levels = .feature_set))

    # data_formatted

    # ---- VISUALIZATION ----

    g <- data_formatted %>%
        ggplot2::ggplot(ggplot2::aes(.group_value, .value)) +
        ggplot2::labs(x = .x_lab, y = .y_lab, title = .title)

    # # Add jitter
    # if (.jitter_color == "scale_color" && length(facet_names) > 0) {
    #     if (length(facet_names) > 1) message("plot_seasonal_diagnostics(facets > 1 & .line_color = 'scale_color'): Using the first facet only:", facet_names[1])
    #     g <- g +
    #         geom_jitter(ggplot2::aes_string(color = facet_names[1]), alpha = .jitter_alpha, width = .jitter_width) +
    #         scale_color_tq()
    # } else {
    #     if (.jitter_color == "scale_color") {
    #         message("plot_seasonal_diagnostics(.geom_color = 'scale_color'): Cannot scale color without a faceting column.")
    #         .jitter_color <- "#2c3e50"
    #     }
    #     g <- g +
    #         geom_jitter(color = .jitter_color, alpha = .jitter_alpha, width = .jitter_width)
    # }

    # Add boxplot or violin

    if (.geom[1] == "boxplot") {
        geom_fun <- ggplot2::geom_boxplot
    } else {
        geom_fun <- ggplot2::geom_violin
    }

    if (.geom_color == "scale_color" && length(facet_names) > 0) {
        if (length(facet_names) > 1) message("plot_seasonal_diagnostics(facets > 1 & .line_color = 'scale_color'): Using the first facet only:", facet_names[1])
        g <- g +
            geom_fun(ggplot2::aes_string(color = facet_names[1]), outlier.color = .geom_outlier_color) +
            scale_color_tq()
    } else {
        if (.geom_color == "scale_color") {
            message("plot_seasonal_diagnostics(.geom_color = 'scale_color'): Cannot scale color without a faceting column.")
            .geom_color <- "#2c3e50"
        }
        g <- g +
            geom_fun(color = .geom_color, outlier.color = .geom_outlier_color)
    }


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

    g <- g + ggplot2::facet_wrap(facet_formula, ncol = facet_ncol, scales = "free")

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
plot_seasonal_diagnostics.grouped_df <- function(.data, .date_var, .value, .facet_vars = NULL,
                                                 .feature_set = "auto",
                                                 .geom = c("boxplot", "violin"),
                                                 .geom_color = "#2c3e50",
                                                 .geom_outlier_color = "#2c3e50",
                                                 # .jitter = FALSE,
                                                 # .jitter_color = "#2c3e50", .jitter_alpha = 0.5,
                                                 # .jitter_width = NULL,
                                                 .title = "Seasonal Diagnostics",
                                                 .x_lab = "", .y_lab = "",
                                                 .interactive = TRUE) {


    # Tidy Eval Setup
    group_names   <- dplyr::group_vars(.data)
    facets_expr   <- rlang::enquos(.facet_vars)

    .data <- tibble::as_tibble(.data)

    # Checks
    facet_names <- .data %>% dplyr::ungroup() %>% dplyr::select(!!! facets_expr) %>% colnames()
    if (length(facet_names) > 0) message("plot_seasonal_diagnostics(...): Groups are previously detected. Grouping by: ",
                                         stringr::str_c(group_names, collapse = ", "))

    # ---- DATA SETUP ----

    # Ungroup Data
    data_formatted <- .data %>% dplyr::ungroup()

    # ---- PLOT SETUP ----
    plot_seasonal_diagnostics(
        .data               = data_formatted,
        .date_var           = !! rlang::enquo(.date_var),
        .value              = !! rlang::enquo(.value),

        # ...
        # !!! rlang::syms(group_names),
        .facet_vars         = !! enquo(group_names),

        .feature_set        = .feature_set,
        .geom               = .geom,
        .geom_color         = .geom_color,
        .geom_outlier_color = .geom_outlier_color,
        # .jitter             = .jitter,
        # .jitter_color       = .jitter_color,
        # .jitter_alpha       = .jitter_alpha,
        # .jitter_width       = .jitter_width,
        .title              = .title,
        .x_lab              = .x_lab,
        .y_lab              = .y_lab,
        .interactive        = .interactive
    )

}
