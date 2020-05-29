#' Group-wise ACF, PACF, and CCF Data Preparation
#'
#' The `tk_acf_diagnostics()` function provides a simple interface to
#' detect Autocorrelation (ACF), Partial Autocorrelation (PACF), and Cross Correlation (CCF) of Lagged
#' Predictors in one `tibble`. This function powers the [plot_acf_diagnostics()]
#' visualization.
#'
#' @param .data A data frame or tibble with numeric features (values) in descending
#'  chronological order
#' @param .date_var A column containing either date or date-time values
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
#' __Lag Specification__
#'
#' Lags (`.lags`) can either be specified as:
#'
#' - A time-based phrase indicating a duraction (e.g. `2 months`)
#' - A maximum lag (e.g. `.lags = 28`)
#' - A sequence of lags (e.g. `.lags = 7:28`)
#'
#' __Scales to Multiple Time Series with Groupes__
#'
#' The `tk_acf_diagnostics()` works with `grouped_df`'s, meaning you can
#' group your time series by one or more categorical columns with `dplyr::group_by()`
#' and then apply `tk_acf_diagnostics()` to return group-wise lag diagnostics.
#'
#' __Special Note on Dots (...)__
#'
#' Unlike other plotting utilities, the `...` arguments is NOT used for
#' group-wise analysis. Rather, it's used for processing Cross Correlations (CCFs).
#'
#' Use `dplyr::group_by()` for processing multiple time series groups.
#'
#' @seealso
#' - __Visualizing ACF, PACF, & CCF:__ [plot_acf_diagnostics()]
#' - __Visualizing Seasonality:__ [plot_seasonal_diagnostics()]
#' - __Visualizing Time Series:__ [plot_time_series()]
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
#'     tk_acf_diagnostics(date, adjusted, # ACF & PACF
#'                        volume, close,  # CCFs
#'                        .lags = 500)
#'
#' # Scale with groups using group_by()
#' FANG %>%
#'     group_by(symbol) %>%
#'     tk_acf_diagnostics(date, adjusted, volume, close, .lags = "3 months")
#'
#' # Apply Transformations
#' FANG %>%
#'     group_by(symbol) %>%
#'     tk_acf_diagnostics(
#'         date, diff_vec(adjusted),  # Apply differencing transformation
#'         .lags = 0:500
#'     )
#'
#'
#' @export
tk_acf_diagnostics <- function(.data, .date_var, .value, ..., .lags = 1000) {
    # Checks
    date_var_expr <- enquo(.date_var)
    value_expr    <- enquo(.value)
    if (rlang::quo_is_missing(date_var_expr)) stop(call. = FALSE, "tk_acf_diagnostics(.date_var), Please provide a .date_var column of class date or date-time.")
    if (rlang::quo_is_missing(value_expr)) stop(call. = FALSE, "tk_acf_diagnostics(.value), Please provide a .value.")
    if (!is.data.frame(.data)) {
        stop(call. = FALSE, "tk_acf_diagnostics(.data) is not a data-frame or tibble. Please supply a data.frame or tibble.")
    }
    UseMethod("tk_acf_diagnostics", .data)
}

#' @export
tk_acf_diagnostics.data.frame <- function(.data, .date_var, .value, ..., .lags = 1000) {

    # Tidyeval Setup
    date_var_expr <- rlang::enquo(.date_var)
    value_expr    <- rlang::enquo(.value)
    dots_exprs    <- rlang::enquos(...)

    # Apply transformations
    .data <- .data %>% dplyr::mutate(.value_mod = !! value_expr)

    # Convert character lags to numeric
    if (is.character(.lags)) {
        tryCatch({
            idx   <- .data %>% dplyr::pull(!! date_var_expr)
            row_count <- .data %>%
                filter_by_time(!! date_var_expr, "start", idx[1] %+time% .lags) %>%
                nrow()

            .lags <- row_count - 1
        }, error = function(e) {
            rlang::abort("Could not parse `.lags` value.")
        })

    }
    # Generage lag sequence if needed
    if (length(.lags) == 1) {
        .lags <- 0:.lags
    }

    # Calcs
    .lags   <- sort(.lags)
    x       <- .data %>% dplyr::pull(.value_mod)
    lag_max <- max(.lags)
    lag_min <- min(.lags)

    # Check max lag
    max_lag_possible <- nrow(.data) - 1
    if (lag_max > max_lag_possible) {
        message("Max lag exceeds data available. Using max lag: ", max_lag_possible)
        lag_max <- max_lag_possible
    }

    # ---- ACF ----

    acf_values <- x %>%
        stats::acf(
            lag.max   = lag_max,
            plot      = FALSE,
            type      = "correlation",
            demean    = TRUE,
            na.action = stats::na.pass
        ) %>%
        .$acf %>%
        .[,,1]

    # ---- PACF ----
    pacf_values <- x %>%
        stats::acf(
            lag.max   = lag_max,
            plot      = FALSE,
            type      = "partial",
            demean    = TRUE,
            na.action = stats::na.pass
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
                na.action = stats::na.pass
            ) %>%
                .$acf %>%
                .[,,1] %>%
                .[(length(.) - (lag_max - lag_min)):length(.)]
        }) %>%
        dplyr::bind_cols() %>%
        dplyr::rename_all(~ str_c("CCF_", .))

    ret <- tibble::tibble(
        ACF  = acf_values,
        PACF = pacf_values
    )

    if (nrow(ret) == nrow(ccf_tbl)) {
        ret <- ret %>%
            dplyr::bind_cols(ccf_tbl)
    }

    ret <- ret %>%
        tibble::rowid_to_column(var = "lag") %>%
        dplyr::mutate(lag = lag - 1) %>%
        dplyr::filter(lag %in% .lags)

    return(ret)
}


#' @export
tk_acf_diagnostics.grouped_df <- function(.data, .date_var, .value, ..., .lags = 1000) {

    # Tidy Eval Setup
    value_expr  <- rlang::enquo(.value)
    group_names <- dplyr::group_vars(.data)

    # Process groups individually
    .data %>%
        tidyr::nest() %>%
        dplyr::mutate(nested.col = purrr::map(
            .x         = data,
            .f         = function(df) tk_acf_diagnostics(
                .data      = df,
                .date_var  = !! rlang::enquo(.date_var),
                .value     = !! value_expr,
                ...,
                .lags      = .lags
            )
        )) %>%
        dplyr::select(-data) %>%
        tidyr::unnest(cols = nested.col) %>%
        dplyr::group_by_at(.vars = group_names)


}


