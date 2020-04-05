#' Visualize a Time Series Resample Plan
#'
#' The `plot_time_series_cv_plan()` function provides a visualization
#' for a time series resample specification (`rset`) of either `rolling_origin`
#' or `time_series_cv` class.
#'
#' @inheritParams plot_time_series
#' @param .rset A time series resample specification of of either `rolling_origin`
#' or `time_series_cv` class.
#' @param ... Additional parameters passed to [plot_time_series()]
#'
#' @details
#'
#' __Resample Set__
#'
#' A resample set is an output of the `timetk::time_series_cv()` function or the
#' `rsample::rolling_origin()` function.
#'
#' @seealso
#' - [time_series_cv()] and [rsample::rolling_origin()] - Functions used to create
#'   time series resample specfications.
#' - [plot_time_series_cv_plan()] - The plotting function used for visualizing the
#'   time series resample plan.
#'
#' @examples
#' library(tidyverse)
#' library(tidyquant)
#' library(rsample)
#' library(timetk)
#'
#' FB_tbl <- FANG %>%
#'     filter(symbol == "FB") %>%
#'     select(symbol, date, adjusted)
#'
#' resample_spec <- time_series_cv(
#'     FB_tbl,
#'     initial = 150, assess = 50, skip = 50,
#'     cumulative = FALSE,
#'     lag = 30,
#'     slice_limit = 6
#' )
#'
#' resample_spec %>% tk_time_series_cv_plan()
#'
#' resample_spec %>%
#'     plot_time_series_cv_plan(
#'         date, adjusted, # date variable and value variable
#'         # Additional arguments passed to plot_time_series(),
#'         .facet_ncol = 3,
#'         .line_alpha = 0.5,
#'         .interactive = FALSE
#'     )
#'
#' @export
plot_time_series_cv_plan <- function(.rset, .date_var, .value, ...,
                                     .facet_ncol = 3, .smooth = FALSE) {

    UseMethod("plot_time_series_cv_plan", .rset)
}

#' @export
plot_time_series_cv_plan.rolling_origin <- function(.rset, .date_var, .value, ...,
                                                    .facet_ncol = 3, .smooth = FALSE) {

    plot_ts_cv(
        .rset,
        .date_var   = !! rlang::enquo(.date_var),
        .value      = !! rlang::enquo(.value),
        ...,
        .facet_ncol = .facet_ncol,
        .smooth     = .smooth
    )



}

#' @export
plot_time_series_cv_plan.time_series_cv <- function(.rset, .date_var, .value, ...,
                                                    .facet_ncol = 3, .smooth = FALSE) {

    plot_ts_cv(
        .rset,
        .date_var   = !! rlang::enquo(.date_var),
        .value      = !! rlang::enquo(.value),
        ...,
        .facet_ncol = .facet_ncol,
        .smooth     = .smooth
    )


}

#' @export
plot_time_series_cv_plan.default <- function(.rset, .date_var, .value, ...,
                                             .facet_ncol = 3, .line_alpha = 0.5, .smooth = FALSE) {
    rlang::abort("plot_time_series_cv_plan: No method for class, ", class(.rset)[1])
}


plot_ts_cv <- function(.rset, .date_var, .value, ...,
                       .facet_ncol = 3, .smooth = FALSE) {

    date_var_expr <- rlang::enquo(.date_var)
    value_expr    <- rlang::enquo(.value)

    # Format data
    data_formatted <- tk_time_series_cv_plan(.rset)

    data_formatted %>%
        dplyr::group_by(id) %>%
        plot_time_series(
            .date_var   = !! date_var_expr,
            .value      = !! value_expr,
            .color_var  = key,
            ...,
            .facet_ncol = .facet_ncol,
            .smooth     = .smooth)

}

