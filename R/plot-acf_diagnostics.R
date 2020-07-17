#' Visualize the ACF, PACF, and CCFs for One or More Time Series
#'
#' Returns the __ACF and PACF of a target__ and
#' optionally __CCF's of one or more lagged predictors__ in interactive `plotly` plots. Scales
#' to multiple time series with `group_by()`.
#'
#'
#' @param .data A data frame or tibble with numeric features (values) in descending
#'  chronological order
#' @param .date_var A column containing either date or date-time values
#' @param .value A numeric column with a value to have ACF and PACF calculations
#'  performed.
#' @param .ccf_vars Additional features to perform Lag Cross Correlations (CCFs)
#' versus the `.value`. Useful for evaluating external lagged regressors.
#' @param .lags A sequence of one or more lags to evaluate.
#' @param .show_ccf_vars_only Hides the ACF and PACF plots so you can focus on only CCFs.
#' @param .white_noise_bars Shows the white noise significance bounds.
#' @param .facet_ncol Facets: Number of facet columns. Has no effect if using `grouped_df`.
#' @param .facet_scales Facets: Options include "fixed", "free", "free_y", "free_x"
#' @param .line_color Line color. Use keyword: "scale_color" to change the color by the facet.
#' @param .line_size Line size
#' @param .line_alpha Line opacity. Adjust the transparency of the line. Range: (0, 1)
#' @param .point_color Point color. Use keyword: "scale_color" to change the color by the facet.
#' @param .point_size Point size
#' @param .point_alpha Opacity. Adjust the transparency of the points. Range: (0, 1)
#' @param .x_intercept Numeric lag. Adds a vertical line.
#' @param .x_intercept_color Color for the x-intercept line.
#' @param .hline_color Color for the y-intercept = 0 line.
#' @param .title Title for the plot
#' @param .x_lab X-axis label for the plot
#' @param .y_lab Y-axis label for the plot
#' @param .interactive Returns either a static (`ggplot2`) visualization or an interactive (`plotly`) visualization
#' @param .plotly_slider If TRUE, returns a plotly x-axis range slider.
#'
#' @return A static `ggplot2` plot or an interactive `plotly` plot
#'
#' @details
#'
#' __Simplified ACF, PACF, & CCF__
#'
#' We are often interested in all 3 of these functions. Why not get all 3+ at once?
#' Now you can.
#'
#' - __ACF__ - Autocorrelation between a target variable and lagged versions of itself
#'
#' - __PACF__ - Partial Autocorrelation removes the dependence of lags on
#'  other lags highlighting key seasonalities.
#'
#' - __CCF__ - Shows how lagged predictors can be used for prediction of a target
#'  variable.
#'
#' __Lag Specification__
#'
#' Lags (`.lags`) can either be specified as:
#'
#' - A time-based phrase indicating a duraction (e.g. `2 months`)
#' - A maximum lag (e.g. `.lags = 28`)
#' - A sequence of lags (e.g. `.lags = 7:28`)
#'
#' __Scales to Multiple Time Series with Groups__
#'
#' The `plot_acf_diagnostics()` works with `grouped_df`'s, meaning you can
#' group your time series by one or more categorical columns with `dplyr::group_by()`
#' and then apply `plot_acf_diagnostics()` to return group-wise lag diagnostics.
#'
#' __Special Note on Groups__
#'
#' Unlike other plotting utilities, the `.facet_vars` arguments is NOT included.
#' Use `dplyr::group_by()` for processing multiple time series groups.
#'
#' __Calculating the White Noise Significance Bars__
#'
#' The formula for the significance bars is ±2/√*T* where *T* is the length of the
#' time series. For a white noise time series, 95% of the data points should fall
#' within this range. Those that don't may be significant autocorrelations.
#'
#' @seealso
#' - __Visualizing ACF, PACF, & CCF:__ [plot_acf_diagnostics()]
#' - __Visualizing Seasonality:__ [plot_seasonal_diagnostics()]
#' - __Visualizing Time Series:__ [plot_time_series()]
#'
#' @examples
#' library(tidyverse)
#' library(timetk)
#'
#'
#' # Apply Transformations
#' # - Differencing transformation to identify ARIMA & SARIMA Orders
#' m4_hourly %>%
#'     group_by(id) %>%
#'     plot_acf_diagnostics(
#'         date, value,               # ACF & PACF
#'         .lags = "7 days",          # 7-Days of hourly lags
#'         .interactive = FALSE
#'     )
#'
#' # Apply Transformations
#' # - Differencing transformation to identify ARIMA & SARIMA Orders
#' m4_hourly %>%
#'     group_by(id) %>%
#'     plot_acf_diagnostics(
#'         date,
#'         diff_vec(value, lag = 1), # Difference the value column
#'         .lags        = 0:(24*7),   # 7-Days of hourly lags
#'         .interactive = FALSE
#'     ) +
#'     ggtitle("ACF Diagnostics",  subtitle = "1st Difference")
#'
#' # CCFs Too!
#' walmart_sales_weekly %>%
#'     select(id, Date, Weekly_Sales, Temperature, Fuel_Price) %>%
#'     group_by(id) %>%
#'     plot_acf_diagnostics(
#'         Date, Weekly_Sales,                        # ACF & PACF
#'         .ccf_vars    = c(Temperature, Fuel_Price), # CCFs
#'         .lags        = "3 months", # 3 months of weekly lags
#'         .interactive = FALSE
#'     )
#'
#' @export
plot_acf_diagnostics <- function(.data, .date_var, .value, .ccf_vars = NULL, .lags = 1000,
                                 .show_ccf_vars_only = FALSE,
                                 .white_noise_bars = FALSE,
                                 .facet_ncol = 1, .facet_scales = "fixed",
                                 .line_color = "#2c3e50", .line_size = 0.5,
                                 .line_alpha = 1,
                                 .point_color = "#2c3e50", .point_size = 1,
                                 .point_alpha = 1,
                                 .x_intercept = NULL,
                                 .x_intercept_color = "#E31A1C",
                                 .hline_color = "#2c3e50",
                                 .title = "Lag Diagnostics",
                                 .x_lab = "Lag", .y_lab = "Correlation",
                                 .interactive = TRUE, .plotly_slider = FALSE) {

    # Checks
    date_var_expr <- enquo(.date_var)
    value_expr    <- enquo(.value)
    if (rlang::quo_is_missing(date_var_expr)) stop(call. = FALSE, "plot_acf_diagnostics(.date_var), Please provide a .date_var column of class date or date-time.")
    if (rlang::quo_is_missing(value_expr)) stop(call. = FALSE, "plot_acf_diagnostics(.value), Please provide a .value.")
    if (!is.data.frame(.data)) {
        stop(call. = FALSE, "plot_diagnostics(.data) is not a data-frame or tibble. Please supply a data.frame or tibble.")
    }

    UseMethod("plot_acf_diagnostics", .data)
}

#' @export
plot_acf_diagnostics.data.frame <- function(.data, .date_var, .value, .ccf_vars = NULL, .lags = 1000,
                                            .show_ccf_vars_only = FALSE,
                                            .white_noise_bars = FALSE,
                                            .facet_ncol = 1, .facet_scales = "fixed",
                                            .line_color = "#2c3e50", .line_size = 0.5,
                                            .line_alpha = 1,
                                            .point_color = "#2c3e50", .point_size = 1,
                                            .point_alpha = 1,
                                            .x_intercept = NULL,
                                            .x_intercept_color = "#E31A1C",
                                            .hline_color = "#2c3e50",
                                            .title = "Lag Diagnostics",
                                            .x_lab = "Lag", .y_lab = "Correlation",
                                            .interactive = TRUE, .plotly_slider = FALSE) {

    # Tidy Eval Setup
    value_expr    <- rlang::enquo(.value)

    # ---- DATA PREPARATION ----

    data_formatted <- tk_acf_diagnostics(
        .data     = tibble::as_tibble(.data),
        .date_var = !! rlang::enquo(.date_var),
        .value    = !! value_expr,
        .ccf_vars = !! rlang::enquo(.ccf_vars),
        # ...       = ...,
        .lags     = .lags
    )

    if (.show_ccf_vars_only) {
        data_formatted <- data_formatted %>%
            dplyr::select(-dplyr::contains("ACF"))
    }

    data_formatted <- data_formatted %>%
        tidyr::pivot_longer(cols = -lag, values_to = "value", names_to = "name") %>%
        dplyr::mutate(name = forcats::as_factor(name))


    time_series_length <- nrow(.data)


    # ---- VISUALIZATION ----

    g <- data_formatted %>%
        ggplot2::ggplot(ggplot2::aes(lag, value, color = name)) +
        ggplot2::geom_hline(yintercept = 0, color = .hline_color) +
        ggplot2::facet_wrap(~ name, ncol = .facet_ncol, scales = .facet_scales) +
        ggplot2::expand_limits(y = 0) +
        ggplot2::labs(x = .x_lab, y = .y_lab, title = .title)

    if (!is.null(.x_intercept)) {
        if (!is.numeric(.x_intercept)) rlang::abort("`.x_intercept` must be a numeric value.")
        g <- g + ggplot2::geom_vline(xintercept = .x_intercept, color = .x_intercept_color)
    }

    # Add line
    if (.line_color == "scale_color") {
        g <- g +
            ggplot2::geom_line(ggplot2::aes(color = name),
                               size = .line_size, alpha = .line_alpha) +
            scale_color_tq()
    } else {
        g <- g +
            ggplot2::geom_line(color = .line_color, size = .line_size, alpha = .line_alpha)
    }

    # Add points
    if (.point_color == "scale_color") {
        g <- g +
            ggplot2::geom_point(ggplot2::aes(color = name),
                                size = .point_size, alpha = .point_alpha) +
            scale_color_tq()
    } else {
        g <- g +
            ggplot2::geom_point(color = .point_color, size = .point_size, alpha = .point_alpha)
    }

    # Add white noise bars
    if (.white_noise_bars) {
        g <- g +
            ggplot2::geom_hline(yintercept = (2 / sqrt(time_series_length)),
                                linetype = 2, color = "#A6CEE3") +
            ggplot2::geom_hline(yintercept = -(2 / sqrt(time_series_length)),
                                linetype = 2, color = "#A6CEE3")
    }

    # Add theme
    g <- g + theme_tq()

    if (.interactive) {

        p <- plotly::ggplotly(g, dynamicTicks = TRUE)

        if (.plotly_slider) {
            p <- p %>%
                plotly::layout(
                    xaxis = list(
                        rangeslider = list(autorange = TRUE)
                    )
                )
        }
        return(p)
    } else {
        return(g)
    }
}

#' @export
plot_acf_diagnostics.grouped_df <- function(.data, .date_var, .value, .ccf_vars = NULL, .lags = 1000,
                                            .show_ccf_vars_only = FALSE,
                                            .white_noise_bars = FALSE,
                                            .facet_ncol = 1, .facet_scales = "fixed",
                                            .line_color = "#2c3e50", .line_size = 0.5,
                                            .line_alpha = 1,
                                            .point_color = "#2c3e50", .point_size = 1,
                                            .point_alpha = 1,
                                            .x_intercept = NULL,
                                            .x_intercept_color = "#E31A1C",
                                            .hline_color = "#2c3e50",
                                            .title = "Lag Diagnostics",
                                            .x_lab = "Lag", .y_lab = "Correlation",
                                            .interactive = TRUE, .plotly_slider = FALSE) {

    # Tidy Eval Setup
    group_names   <- dplyr::group_vars(.data)
    value_expr    <- rlang::enquo(.value)

    # ---- DATA PREPARATION ----

    data_formatted <- tk_acf_diagnostics(
        .data     = .data,
        .date_var = !! rlang::enquo(.date_var),
        .value    = !! value_expr,
        .ccf_vars = !! rlang::enquo(.ccf_vars),
        # ...       = ...,
        .lags     = .lags
    )

    if (.show_ccf_vars_only) {
        data_formatted <- data_formatted %>%
            dplyr::select(-dplyr::contains("ACF"))
    }

    # dont_pivot_these <- c(group_names, "lag")
    data_formatted <- data_formatted %>%
        dplyr::ungroup() %>%
        dplyr::mutate(.groups_consolidated = stringr::str_c(!!! rlang::syms(group_names), sep = "_")) %>%
        dplyr::mutate(.groups_consolidated = forcats::as_factor(.groups_consolidated)) %>%
        dplyr::select(-(!!! rlang::syms(group_names))) %>%
        dplyr::select(.groups_consolidated, lag, dplyr::everything()) %>%
        tidyr::pivot_longer(cols      = -c(.groups_consolidated, lag),
                            values_to = "value",
                            names_to  = "name") %>%
        dplyr::mutate(name = forcats::as_factor(name))

    time_series_length <- .data %>% count() %>% pull(n) %>% mean()

    # data_formatted

    g <- data_formatted %>%
        ggplot2::ggplot(ggplot2::aes(lag, value, color = .groups_consolidated)) +
        ggplot2::geom_hline(yintercept = 0, color = .hline_color) +
        ggplot2::facet_grid(rows   = ggplot2::vars(name),
                            cols   = ggplot2::vars(.groups_consolidated),
                            scales = .facet_scales) +
        ggplot2::expand_limits(y = 0) +
        ggplot2::labs(x = .x_lab, y = .y_lab, title = .title)

    if (!is.null(.x_intercept)) {
        if (!is.numeric(.x_intercept)) rlang::abort("`.x_intercept` must be a numeric value.")
        g <- g + ggplot2::geom_vline(xintercept = .x_intercept, color = .x_intercept_color)
    }

    # Add line
    if (.line_color == "scale_color") {
        g <- g +
            ggplot2::geom_line(ggplot2::aes(color = .groups_consolidated),
                               size = .line_size, alpha = .line_alpha) +
            scale_color_tq()
    } else {
        g <- g +
            ggplot2::geom_line(color = .line_color, size = .line_size, alpha = .line_alpha)
    }

    # Add points
    if (.point_color == "scale_color") {
        g <- g +
            ggplot2::geom_point(ggplot2::aes(color = .groups_consolidated),
                                size = .point_size, alpha = .point_alpha) +
            scale_color_tq()
    } else {
        g <- g +
            ggplot2::geom_point(color = .point_color, size = .point_size, alpha = .point_alpha)
    }

    # Add white noise bars
    if (.white_noise_bars) {
        g <- g +
            ggplot2::geom_hline(yintercept = (2 / sqrt(time_series_length)),
                                linetype = 2, color = "#A6CEE3") +
            ggplot2::geom_hline(yintercept = -(2 / sqrt(time_series_length)),
                                linetype = 2, color = "#A6CEE3")
    }

    # Add theme
    g <- g + theme_tq()


    if (.interactive) {
        return(plotly::ggplotly(g))
    } else {
        return(g)
    }

}

