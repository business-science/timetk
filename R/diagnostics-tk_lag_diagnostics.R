#' Get ACF, PACF, and CCF in 1 Data Frame
#'
#' The `tk_lag_diagnostics()` function provides a simple interface to
#' detect Autocorrelation, Partial ACF, and Cross Correlation of Lagged
#' Predictors in one `tibble`. This function powers the [plot_lag_diagnostics()]
#' visualization.
#'
#' @param .data A data frame or tibble with numeric features (values) in descending
#'  chronological order
#' @param .value A numeric column with a value to have ACF and PACF calculations
#'  performed.
#' @param ... Additional features to perform Lag Cross Correlations (CCFs)
#' versus the `.value`. Useful for evaluating external lagged regressors.
#' @param .lags A seqence of one or more lags to evaluate.
#'
#' @details
#'
#' __Simplified ACF, PACF, & CCF__
#'
#' We are often interested in all 3 of these functions. Why not get all 3 at once?
#' Now you can!
#'
#' - __ACF__ - Autocorrelation between a target variable and lagged versions of itself
#'
#' - __PACF__ - Partial Autocorrelation removes the dependence of lags on
#'  other lags highlighting key seasonalities.
#'
#' - __CCF__ - Shows how lagged predictors can be used for prediction of a target
#'  variable.
#'
#' __Works with Grouped Data Frames__
#'
#' The `tk_lag_diagnostics()` works with `grouped_df`'s, meaning you can
#' group your time series by one or more categorical columns with `group_by()`
#' and then apply `tk_lag_diagnostics()` to return group-wise lag diagnostics.
#'
#' @seealso
#' - __Visualizing ACF, PACF, & CCF:__ [plot_lag_diagnostics()]
#'
#' @examples
#' library(tidyverse)
#' library(tidyquant)
#' library(timetk)
#'
#' # ACF, PACF, & CCF in 1 Data Frame
#' # - Get ACF & PACF for target (adjusted)
#' # - Get CCF between adjusted and volume and close
#' FANG %>%
#'     filter(symbol == "FB") %>%
#'     tk_lag_diagnostics(.value = adjusted, volume, close, .lags = 0:500)
#'
#' # Do the same thing for groups with group_by()
#' FANG %>%
#'     group_by(symbol) %>%
#'     tk_lag_diagnostics(.value = adjusted, volume, close, .lags = 0:500)
#'
#'
#' @export
tk_lag_diagnostics <- function(.data, .value, ..., .lags = 0:60) {
    # Checks
    value_expr <- enquo(.value)
    if (rlang::quo_is_missing(value_expr)) stop(call. = FALSE, "tk_lag_diagnostics(.value), Please provide a .value.")
    if (!is.data.frame(.data)) {
        stop(call. = FALSE, "tk_lag_diagnostics(.data) is not a data-frame or tibble. Please supply a data.frame or tibble.")
    }
    UseMethod("tk_lag_diagnostics", .data)
}

#' @export
tk_lag_diagnostics.data.frame <- function(.data, .value, ..., .lags = 0:60) {

    # Tidyeval Setup
    value_expr <- rlang::enquo(.value)
    dots_exprs <- rlang::enquos(...)

    # Calcs
    .lags   <- sort(.lags)
    x       <- .data %>% dplyr::pull(!! value_expr)
    lag_max <- max(.lags)
    lag_min <- min(.lags)

    # ---- ACF ----

    acf_values <- x %>%
        stats::acf(
            lag.max   = lag_max,
            plot      = FALSE,
            type      = "correlation",
            demean    = TRUE,
            na.action = stats::na.fail
        ) %>%
        .$acf %>%
        .[,,1]

    # ---- PACF ----
    pacf_values <- x %>%
        stats::pacf(
            lag.max  = lag_max,
            plot     = FALSE
        ) %>%
        .$acf %>%
        .[,,1]

    pacf_values <- c(1, pacf_values)

    # ---- CCF ----
    ccf_tbl <- .data %>%
        dplyr::select(!!! dots_exprs) %>%
        purrr::map(.f = function(y) {
            stats::ccf(
                x         = x,
                y         = y,
                lag.max   = lag_max,
                type      = "correlation",
                plot      = FALSE,
                na.action = stats::na.fail
            ) %>%
                .$acf %>%
                .[,,1] %>%
                .[(length(.) - (lag_max - lag_min)):length(.)]
        }) %>%
        dplyr::bind_cols() %>%
        dplyr::rename_all(~ str_c("ccf_", .))

    ret <- tibble::tibble(
        acf  = acf_values,
        pacf = pacf_values
    ) %>%
        dplyr::bind_cols(ccf_tbl) %>%
        tibble::rowid_to_column(var = "lag") %>%
        dplyr::mutate(lag = lag - 1) %>%
        dplyr::filter(lag %in% .lags)

    return(ret)
}


#' @export
tk_lag_diagnostics.grouped_df <- function(.data, .value, ..., .lags = 0:60) {

    # Tidy Eval Setup
    value_expr  <- rlang::enquo(.value)
    group_names <- dplyr::group_vars(.data)

    # Process groups individually
    .data %>%
        tidyr::nest() %>%
        dplyr::mutate(nested.col = purrr::map(
            .x         = data,
            .f         = function(df) tk_lag_diagnostics(
                .data      = df,
                .value     = !! value_expr,
                ...,
                .lags      = .lags
            )
        )) %>%
        dplyr::select(-data) %>%
        tidyr::unnest(cols = nested.col) %>%
        dplyr::group_by_at(.vars = group_names)

}


