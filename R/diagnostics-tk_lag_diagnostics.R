

tk_lag_diagnostics <- function(.data, .value, ..., .lags = 0:20) {

    value_expr <- enquo(.value)
    dots_exprs <- enquos(...)

    # ccf_variabls <- .data %>% select(!!! dots_exprs) %>% colnames()

    if (rlang::quo_is_missing(value_expr)) stop(call. = FALSE, "tk_lag_diagnostics(), Please provide a .value.")

    # Calcs
    x <- .data %>% pull(!! value_expr)
    lag_max <- max(.lags)
    lag_min <- min(.lags)

    # ACF
    acf_values <- x %>%
        stats::acf(
            lag.max   = lag_max,
            plot      = FALSE,
            type      = "correlation",
            demean    = TRUE,
            na.action = na.fail
        ) %>%
        .$acf %>%
        .[,,1]

    # PACF
    pacf_values <- x %>%
        stats::pacf(
            lag.max = lag_max,
            plot    = FALSE) %>%
        .$acf %>%
        .[,,1]

    pacf_values <- c(1, pacf_values)

    # pacf_length <- length(pacf_values)
    # acf_length  <- length(acf_values)
    # if (pacf_length < acf_length) {
    #     pacf_values <- c(rep(NA, acf_length - pacf_length), pacf_values)
    # }

    # CCF
    ccf_tbl <- .data %>%
        select(!!! dots_exprs) %>%
        map(.f = function(y) {
            stats::ccf(
                x = x,
                y = y,
                lag.max = tail(.lags, 1),
                type = "correlation",
                plot = FALSE,
                na.action = na.fail
            ) %>%
                .$acf %>%
                .[,,1] %>%
                .[(length(.) - (lag_max - lag_min)):length(.)]
        }) %>%
        bind_cols() %>%
        rename_all(~ str_c("ccf_", .))

    ret <- tibble(
        acf  = acf_values,
        pacf = pacf_values
    ) %>%
        bind_cols(ccf_tbl) %>%
        rowid_to_column(var = "lag") %>%
        mutate(lag = lag - 1) %>%
        filter(lag %in% .lags)

    return(ret)
}

plot_lag_diagnostics <- function(.data, .value, ..., .lags = 0:20,
                                 .ncol = 1, .scales = "fixed",
                                 .interactive = TRUE,
                                 .title = "Lag Diagnostics",
                                 .y_lab = "Correlation", .x_lab = "Lag") {

    data_prepared <- tk_lag_diagnostics(
        .data   = .data,
        .value  = !! enquo(.value),
        ...     = ...
    )

    g <- data_prepared %>%
        pivot_longer(cols = -lag, values_to = "value", names_to = "name") %>%
        mutate(name = as_factor(name)) %>%
        ggplot(aes(lag, value, color = name)) +
        geom_hline(yintercept = 0, color = "#2c3e50") +
        geom_point() +
        geom_line() +
        facet_wrap(~ name, ncol = .ncol, scales = .scales) +
        expand_limits(y = 0) +
        theme_tq() +
        scale_color_tq() +
        labs(x = .x_lab, y = .y_lab, title = .title)

    if (.interactive) {
        return(plotly::ggplotly(g))
    } else {
        return(g)
    }
}


plot_time_series <- function(.data, .date_var, .value, .facets = NULL,
                             .ncol = 1, .scales = "free_y", .color = "#2c3e50",
                             .interactive = TRUE,
                             .yintercept = NULL,
                             .smooth = TRUE, .smooth_period = NULL, .smooth_degree = 2,
                             .smooth_color = "dodgerblue",
                             .title = "Time Series Plot", .x_lab = "", .y_lab = "") {



    date_var_expr <- enquo(.date_var)
    value_expr    <- enquo(.value)
    facets_expr   <- enquos(.facets)

    # Data Setup
    data_formatted <- .data %>%
        ungroup() %>%
        select(!! date_var_expr, !! value_expr, !!! facets_expr)

    facet_names <- data_formatted %>% select(!!! facets_expr) %>% colnames()

    if (.smooth) {
        if (!rlang::quo_is_null(facets_expr[[1]])) {
            data_formatted <- data_formatted %>%
                group_by(!!! syms(facet_names))
        }

        .smooth_span <- NULL
        if (is.null(.smooth_period)) {
            .smooth_span <- 0.75
        }

        data_formatted <- data_formatted %>%
            mutate(value_smooth = smooth_vec(
                !! value_expr,
                .period = .smooth_period,
                .span   = .smooth_span,
                .degree = .smooth_degree)
            ) %>%
            ungroup()
    }

    # data_formatted

    # Plot Setup
    g <- data_formatted %>%
        ggplot(aes(!!date_var_expr, !! value_expr)) +
        geom_line(color = "#2c3e50") +
        theme_tq() +
        labs(x = .x_lab, y = .y_lab, title = .title)

    if (length(facet_names) > 0) {
        g <- g +
            facet_wrap(vars(!!! syms(facet_names)), ncol = .ncol, scales = .scales)
    }

    if (.smooth) {
        g <- g +
            geom_line(aes(y = value_smooth), color = .smooth_color, size = 1)
    }

    if (!is.null(.yintercept)) {
        g <- g +
            geom_hline(yintercept = .yintercept, color = "#2c3e50")
    }

    if (.interactive) {
        return(plotly::ggplotly(g))
    } else {
        return(g)
    }
}
