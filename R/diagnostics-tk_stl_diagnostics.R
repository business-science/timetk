#' Group-wise STL Decomposition (Season, Trend, Remainder)
#'
#' `tk_stl_diagnostics()` is the preprocessor for `plot_stl_diagnostics()`.
#' It helps by automating frequency and trend selection.
#'
#' @param .data A `tibble` or `data.frame` with a time-based column
#' @param .date_var A column containing either date or date-time values
#' @param .value A column containing numeric values
#' @param .frequency Controls the seasonal adjustment (removal of seasonality).
#'  Input can be either "auto", a time-based definition (e.g. "2 weeks"),
#'  or a numeric number of observations per frequency (e.g. 10).
#'  Refer to [tk_get_frequency()].
#' @param .trend Controls the trend component.
#'  For STL, trend controls the sensitivity of the lowess smoother, which is used to remove the remainder.
#' @param .message A boolean. If `TRUE`, will output information related to automatic frequency
#' and trend selection (if applicable).
#'
#' @return A `tibble` or `data.frame` with Observed, Season, Trend, Remainder,
#'  and Seasonally-Adjusted features
#'
#' @details
#'
#' The `tk_stl_diagnostics()` function generates a Seasonal-Trend-Loess decomposition.
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
#'
#'
#' # ---- GROUPS & TRANSFORMATION ----
#' m4_daily %>%
#'     group_by(id) %>%
#'     tk_stl_diagnostics(date, box_cox_vec(value))
#'
#' # ---- CUSTOM TREND ----
#' m4_weekly %>%
#'     group_by(id) %>%
#'     tk_stl_diagnostics(date, box_cox_vec(value), .trend = "2 quarters")
#'
#' @name tk_stl_diagnostics
#' @export
tk_stl_diagnostics <- function(.data, .date_var, .value,
                               .frequency = "auto", .trend = "auto",
                               .message = TRUE) {

    # Tidyeval Setup
    date_var_expr <- rlang::enquo(.date_var)
    value_expr    <- rlang::enquo(.value)

    # Checks
    if (!is.data.frame(.data)) {
        stop(call. = FALSE, "tk_stl_diagnostics(.data) is not a data-frame or tibble. Please supply a data.frame or tibble.")
    }
    if (rlang::quo_is_missing(date_var_expr)) {
        stop(call. = FALSE, "tk_stl_diagnostics(.date_var) is missing. Please supply a date or date-time column.")
    }
    if (rlang::quo_is_missing(value_expr)) {
        stop(call. = FALSE, "tk_stl_diagnostics(.value) is missing. Please a numeric column.")
    }


    UseMethod("tk_stl_diagnostics", .data)

}



#' @export
tk_stl_diagnostics.data.frame <- function(.data, .date_var, .value,
                                          .frequency = "auto", .trend = "auto",
                                          .message = TRUE) {

    # Setup
    data          <- .data
    value_expr    <- dplyr::enquo(.value)
    date_var_expr <- dplyr::enquo(.date_var)

    # Index
    date_idx <- data %>% dplyr::pull(!! date_var_expr)

    # Frequency & Trend Calculation
    freq <- tk_get_frequency(date_idx, period = .frequency, message = .message)
    trnd <- tk_get_trend(date_idx, period = .trend, message = .message)

    # Data preprocessing
    # Apply any Transformations - Evaluate Formula
    data_formatted <- data %>%
        dplyr::mutate(.value_mod = !! value_expr)

    # STL Calculation
    stl_obj <- data_formatted %>%
        dplyr::pull(".value_mod") %>%
        stats::ts(frequency = freq) %>%
        stats::stl(s.window = "periodic", t.window = trnd, robust = TRUE)

    # Decomposition
    decomp_tbl <- tibble::tibble(
        season    = stl_obj$time.series[,"seasonal"] %>% as.numeric(),
        trend     = stl_obj$time.series[,"trend"] %>% as.numeric(),
        remainder = stl_obj$time.series[,"remainder"] %>% as.numeric()
    ) %>%
        dplyr::mutate(
            observed = season + trend + remainder,
            seasadj  = trend + remainder
        ) %>%
        tibble::add_column(!! rlang::quo_name(date_var_expr) := date_idx, .after = 0) %>%
        dplyr::select(!! date_var_expr, observed, season, trend, remainder, seasadj)

    return(decomp_tbl)

}

#' @export
tk_stl_diagnostics.grouped_df <- function(.data, .date_var, .value,
                                          .frequency = "auto", .trend = "auto",
                                          .message = TRUE) {

    # Tidy Eval Setup
    value_expr    <- rlang::enquo(.value)
    date_var_expr <- rlang::enquo(.date_var)
    group_names   <- dplyr::group_vars(.data)

    # Process groups individually
    .data %>%
        tidyr::nest() %>%
        dplyr::mutate(nested.col = purrr::map(
            .x         = data,
            .f         = function(df) tk_stl_diagnostics(
                .data         = df,
                .date_var     = !! date_var_expr,
                .value        = !! value_expr,
                .frequency    = .frequency,
                .trend        = .trend,
                .message      = .message
            )
        )) %>%
        dplyr::select(-"data") %>%
        tidyr::unnest(cols = nested.col) %>%
        dplyr::group_by_at(.vars = group_names)

}
