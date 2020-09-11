#' Time Series Resample Plan Data Preparation
#'
#' The `tk_time_series_cv_plan()` function provides a simple interface to
#' prepare a time series resample specification (`rset`) of either `rolling_origin`
#' or `time_series_cv` class.
#'
#' @param .data A time series resample specification of of either `rolling_origin`
#' or `time_series_cv` class.
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
#'     slice_limit = n())
#'
#' resample_spec %>% tk_time_series_cv_plan()
#'
#' @export
tk_time_series_cv_plan <- function(.data) {
    UseMethod("tk_time_series_cv_plan", .data)
}

#' @export
tk_time_series_cv_plan.rolling_origin <- function(.data) {
    time_series_cv_plan(.data)
}

#' @export
tk_time_series_cv_plan.time_series_cv <- function(.data) {
    time_series_cv_plan(.data)
}

#' @export
tk_time_series_cv_plan.rset <- function(.data) {
    time_series_cv_plan(.data)
}

#' @export
tk_time_series_cv_plan.rsplit <- function(.data) {

    tib_2 <- tibble::tibble(
        training = list(rsample::training(!! rlang::enquo(.data)))
    )

    tib_3 <- tibble::tibble(
        testing = list(rsample::testing(!! rlang::enquo(.data)))
    )

    dplyr::bind_rows(tib_2, tib_3) %>%
        tidyr::gather(training:testing, key = ".key", value = ".value", factor_key = TRUE) %>%
        tidyr::unnest(.value) %>%
        tibble::as_tibble() %>%
        tibble::add_column(.id = "Slice1", .before = 1)
}

#' @export
tk_time_series_cv_plan.default <- function(.data) {
    rlang::abort("tk_time_series_cv_plan: No method for class, ", class(.data)[1])
}

time_series_cv_plan <- function(.data) {

    .data %>%
        # Prevent name collisions
        dplyr::rename(.splits = splits, .id = id) %>%

        dplyr::ungroup() %>%
        dplyr::mutate(
            training = purrr::map(.splits, ~ rsample::training(.x)),
            testing  = purrr::map(.splits, ~ rsample::testing(.x))
        ) %>%
        dplyr::select(-.splits) %>%
        tidyr::gather(-.id, key = ".key", value = ".value", factor_key = TRUE) %>%
        tidyr::unnest(.value) %>%
        tibble::as_tibble()

}



