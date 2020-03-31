

#' @export
plot_seasonal_diagnostics <- function(.data, .date_var, .value, ...,
                                      .feature_set = "auto",
                                      .geom = c("boxplot", "violin"),
                                      .geom_color = "#2c3e50",
                                      .geom_outlier_color = "#2c3e50",
                                      # .jitter = FALSE,
                                      # .jitter_color = "#2c3e50", .jitter_alpha = 0.5,
                                      # .jitter_width = NULL,
                                      .title = "Seasonality Diagnostics",
                                      .x_lab = "", .y_lab = "",
                                      .interactive = TRUE) {

    # Checks
    value_expr <- rlang::enquo(.value)

    if (rlang::quo_is_missing(value_expr)) stop(call. = FALSE, "tk_acf_diagnostics(.value), Please provide a .value.")
    if (!is.data.frame(.data)) {
        stop(call. = FALSE, "plot_diagnostics(.data) is not a data-frame or tibble. Please supply a data.frame or tibble.")
    }

    UseMethod("plot_seasonal_diagnostics", .data)
}

#' @export
plot_seasonal_diagnostics.data.frame <- function(.data, .date_var, .value, ...,
                                                 .feature_set = "auto",
                                                 .geom = c("boxplot", "violin"),
                                                 .geom_color = "#2c3e50",
                                                 .geom_outlier_color = "#2c3e50",
                                                 # .jitter = FALSE,
                                                 # .jitter_color = "#2c3e50", .jitter_alpha = 0.5,
                                                 # .jitter_width = NULL,
                                                 .title = "Seasonality Diagnostics",
                                                 .x_lab = "", .y_lab = "",
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

    # data_formatted

    # Post process
    data_formatted <- data_formatted %>%
        dplyr::select(-(!! date_var_expr)) %>%
        dplyr::ungroup() %>%
        tidyr::pivot_longer(cols = c(-.value, -(!!! rlang::syms(facet_names))),
                            names_to = ".group", values_to = ".group_value") %>%
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

    if (facet_names == 0) {
        facet_ncol <- 1
    } else {
        facet_ncol <- data_formatted %>%
            dplyr::select(facet_names) %>%
            dplyr::distinct() %>%
            nrow()
    }

    facet_groups <- stringr::str_c(facet_names, collapse = " + ")
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
plot_seasonal_diagnostics.grouped_df <- function(.data, .date_var, .value, ...,
                                                 .feature_set = "auto",
                                                 .geom = c("boxplot", "violin"),
                                                 .geom_color = "#2c3e50",
                                                 .geom_outlier_color = "#2c3e50",
                                                 # .jitter = FALSE,
                                                 # .jitter_color = "#2c3e50", .jitter_alpha = 0.5,
                                                 # .jitter_width = NULL,
                                                 .title = "Seasonality Diagnostics",
                                                 .x_lab = "", .y_lab = "",
                                                 .interactive = TRUE) {


    # Tidy Eval Setup
    group_names   <- dplyr::group_vars(.data)
    facets_expr   <- rlang::enquos(...)

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
        !!! rlang::syms(group_names),

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
