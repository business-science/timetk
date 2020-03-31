
#' @export
tk_seasonal_diagnostics <- function(.data, .date_var, .value,
                                    .feature_set = "auto") {

    # Tidyeval Setup
    date_var_expr <- rlang::enquo(.date_var)
    value_expr    <- rlang::enquo(.value)

    # Checks
    if (!is.data.frame(.data)) {
        stop(call. = FALSE, "tk_seasonal_diagnostics(.data) is not a data-frame or tibble. Please supply a data.frame or tibble.")
    }
    if (rlang::quo_is_missing(date_var_expr)) {
        stop(call. = FALSE, "tk_seasonal_diagnostics(.date_var) is missing. Please supply a date or date-time column.")
    }
    if (rlang::quo_is_missing(value_expr)) {
        stop(call. = FALSE, "tk_seasonal_diagnostics(.value) is missing. Please a numeric column.")
    }
    if (!all(.feature_set %in% acceptable_seasonal_values())) {
        stop(call. = FALSE, "tk_seasonal_diagnostics(.feature_set): Feature values not in acceptable values. Please use one or more of: ",
             str_c(acceptabl_seasonal_values(), collapse = ", "))
    }

    UseMethod("tk_seasonal_diagnostics", .data)

}

#' @export
tk_seasonal_diagnostics.data.frame <- function(.data, .date_var, .value,
                                               .feature_set = "auto") {


    # Tidyeval Setup
    date_var_expr <- rlang::enquo(.date_var)
    value_expr    <- rlang::enquo(.value)

    # ---- DATA SETUP ----

    # Apply any Transformations - Evaluate Formula
    data_formatted <- .data %>%
        dplyr::mutate(.value_mod = !! value_expr) %>%
        dplyr::select(!! date_var_expr, .value_mod)

    # Auto Features Summary
    if (.feature_set[1] == "auto") {
        .feature_set <- .data %>%
            pull(!! date_var_expr) %>%
            get_seasonal_auto_features()
    }

    # Return the seasonal features
    data_formatted <- data_formatted %>%
        tk_augment_timeseries_signature() %>%
        select(!! date_var_expr, .value_mod, .feature_set) %>%
        mutate_at(.vars = vars(-(!! date_var_expr), -.value_mod), factor, ordered = FALSE) %>%
        rename(.value = .value_mod) %>%

        # mutate(.group = factor(.group, levels = .feature_set))

    return(data_formatted)

}

#' @export
tk_seasonal_diagnostics.grouped_df <- function(.data, .date_var, .value,
                                               .feature_set = "auto") {

    # Tidy Eval Setup
    value_expr    <- rlang::enquo(.value)
    date_var_expr <- rlang::enquo(.date_var)
    group_names   <- dplyr::group_vars(.data)

    # If auto, get common features
    if (.feature_set[1] == "auto") {
        .feature_set <- .data %>%
            group_split() %>%
            map(.f = function(df) {
                df %>%
                    pull(!! date_var_expr) %>%
                    get_seasonal_auto_features()
            }) %>%
            flatten_chr() %>%
            unique()
    }


    # Process groups individually
    .data %>%
        tidyr::nest() %>%
        dplyr::mutate(nested.col = purrr::map(
            .x         = data,
            .f         = function(df) tk_seasonal_diagnostics(
                .data         = df,
                .date_var     = !! date_var_expr,
                .value        = !! value_expr,
                .feature_set  = .feature_set
            )
        )) %>%
        dplyr::select(-data) %>%
        tidyr::unnest(cols = nested.col) %>%
        dplyr::group_by_at(.vars = group_names)

}

# UTILITIES ----

acceptable_seasonal_values <- function() {
    c("auto", "second", "minute", "hour", "wday.lbl", "week", "month.lbl", "quarter", "year")
}

get_seasonal_auto_features <- function(.index) {

    summary_tbl <- .index %>%
        tk_get_timeseries_summary()

    max_min_list <- get_max_min_list(summary_tbl)

    features_to_get <- tk_get_timeseries_unit_frequency() %>%
        gather() %>%
        mutate(check = value %>% between(max_min_list$min_period$value, max_min_list$max_period$value)) %>%
        filter(check) %>%
        left_join(time_series_signature_lookup_tbl(), by = "key") %>%
        pull(feature)

    return(features_to_get)
}


get_max_min_list <- function(time_series_summary_tbl) {

    # Min - Check median diff and go next unit up
    min_period <- tk_get_timeseries_unit_frequency() %>%
        gather() %>%
        mutate(check = 2 * value > time_series_summary_tbl$diff.median) %>%
        filter(check) %>%
        slice(1)

    # Max - Check at least 2 periods worth of data
    start_numeric <- as.numeric(as.POSIXct(time_series_summary_tbl$start))
    end_numeric   <- as.numeric(as.POSIXct(time_series_summary_tbl$end))
    start_to_end  <- end_numeric - start_numeric
    start_to_end

    max_period <- tk_get_timeseries_unit_frequency() %>%
        gather() %>%
        mutate(check = 2 * value < start_to_end) %>%
        filter(check) %>%
        slice(n())

    # Max and min
    max_min_list <- list(min_period = min_period, max_period = max_period)
    return(max_min_list)
}


# Features to get
time_series_signature_lookup_tbl <- function() {

    tibble(
        sec     = "second",
        min     = "minute",
        hour    = "hour",
        day     = "wday.lbl",
        week    = "week",
        month   = "month.lbl",
        quarter = "quarter",
        year    = "year"
    ) %>%
        gather(value = "feature")
}





# .data <- m4_daily %>% group_by(id)
#
# g <- .data %>%
#     tk_seasonal_diagnostics(date, value)  %>%
#     # Post process
#     select(-(!! date_var_expr)) %>%
#     ungroup() %>%
#     pivot_longer(cols = c(-.value), names_to = ".group", values_to = ".group_value")
#     ungroup() %>%
#     # End post process
#     ggplot(aes(.group_value, .value))+
#     geom_boxplot(color = "#2c3e50") +
#     facet_wrap(.group ~ id, scales = "free", ncol = 4) +
#     # facet_grid(.group ~ id, scales = "free_x") +
#     theme_tq()
#
# plotly::ggplotly(g)
