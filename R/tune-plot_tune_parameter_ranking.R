#' Visualize Hyperparameter Rankings
#'
#' The `plot_parameter_ranking()` function provides a visualization
#' for time series hyperparameter tuning results (`tune_results`), the output of `tune::tune_grid()`.
#'
#' @param .data A `tibble` of class "tune_results" or the output from [tk_parameter_ranking()]
#' @param .metric Select a metric to use for tuning performance investigation.
#' @param .max_failure_rate Range (0,1). Use to `.max_failure_rate` to filter models below an acceptable failure threshold.
#' @param .point_alpha Opacity for points. Set to 0.6 by default.
#' @param .point_color_best Color of point for best model rank
#' @param .point_color_worst Color of point for worst model rank
#' @param .point_size_best Size of point for the best model rank
#' @param .point_size_worst Size of point for the worst model rank
#' @param .title Plot title
#' @param .color_lab Legend label for color (Failure Rate Ranking)
#' @param .size_lab Legend label for the size (Standard Error Ranking)
#' @param .interactive Toggle between interactive plotly chart and static ggplot chart
#'
#' @details
#'
#' __Metric Ranking (Model Accuracy, Y-Axis)__
#'
#' The model with the lowest (best) rank is that with the lowest mean error. Y-Axis is the Metric value.
#'
#' - __Metric Calculation__ - Refer to the appropriate metric from the `yardstick` R package.
#' - __Metric Ranking__ - Sort Best to Worst mean metric. Rank 1 to N.
#'
#' __Failure Rate Ranking (Robustness to New Data, Color)__
#'
#' Models with lower failure rates are more robust to new data. Color is the Failure Rate.
#' Failure rate rank is a score based on the proportion of models that failed during tuning.
#'
#' - __Failure Rate Calculation:__ _Failure Rate = n / No. of Resample Slices_
#' - __Failure Rate Ranking:__ Use Min Ranking (`dplyr::min_rank()`) on the Failure Rate Calculation.
#'
#' __Standard Error Ranking (Model Variability, Size)__
#'
#' Models with lower standard error are more consistent (less variability). Size of the point is
#' the standard error ranking.
#'
#' - __Standard Error Calculation__ - See `base::sd()`.
#' - __Standard Error Ranking__ - Sort Best to Worst standard error. Rank 1 to N.
#'
#' @seealso
#' - [tk_parameter_ranking()] - Underlying parameter ranking function
#' - [tk_parameter_select_by_row()] - Select the best parameters from the `tk_parameter_ranking()`
#'
#' @examples
#' library(dplyr)
#' library(tune)
#' library(timetk)
#'
#' # Output of tune::tune_grid()
#' arima_workflow_tuned
#'
#' # TUNE RESULTS INTERFACE ----
#' arima_workflow_tuned %>%
#'     plot_parameter_ranking(.interactive = FALSE)
#'
#' # RANKED PARAMETERS INTERFACE ----
#' # - Rank using tk_parameter_ranking() first, then plot
#' arima_workflow_tuned %>%
#'     tk_parameter_ranking(.metric = "rsq", .max_failure_rate = 0.6) %>%
#'     plot_parameter_ranking(.interactive = FALSE)
#'
#' @export
plot_parameter_ranking <- function(.data,
                                   .metric,
                                   .max_failure_rate = 1,
                                   .point_alpha = 0.6,
                                   .point_color_best = "#2C3E50",
                                   .point_color_worst = "#E31A1C",
                                   .point_size_best = 4,
                                   .point_size_worst = 2,
                                   .title = "Hyperparameter Ranking & Model Selection",
                                   .color_lab = "Failure Rate Rank",
                                   .size_lab  = "Variability Rank",
                                   .interactive = TRUE) {

    UseMethod("plot_parameter_ranking", .data)
}

#' @export
plot_parameter_ranking.tune_results <- function(.data,
                                                  .metric,
                                                  .max_failure_rate = 1,
                                                  .point_alpha = 0.6,
                                                  .point_color_best = "#2C3E50",
                                                  .point_color_worst = "#E31A1C",
                                                  .point_size_best = 4,
                                                  .point_size_worst = 2,
                                                  .title = "Hyperparameter Ranking & Model Selection",
                                                  .color_lab = "Failure Rate Rank",
                                                  .size_lab  = "Variability Rank",
                                                  .interactive = TRUE) {

    data_formatted <- tk_parameter_ranking(.data, .metric = .metric, .max_failure_rate = .max_failure_rate)

    plot_parameter_ranking.data.frame(
        .data              = data_formatted,
        .point_alpha       = .point_alpha,
        .point_color_best  = .point_color_best,
        .point_color_worst = .point_color_worst,
        .point_size_best   = .point_size_best,
        .point_size_worst  = .point_size_worst,
        .title             = .title,
        .color_lab         = .color_lab,
        .size_lab          = .size_lab,
        .interactive       = .interactive
    )

}

# #' @export
# plot_parameter_ranking.rolling_origin <- function(.data,
#                                                   .metric,
#                                                   .max_failure_rate = 1,
#                                                   .point_alpha = 0.6,
#                                                   .point_color_best = "#2C3E50",
#                                                   .point_color_worst = "#E31A1C",
#                                                   .point_size_best = 4,
#                                                   .point_size_worst = 2,
#                                                   .title = "Hyperparameter Ranking & Model Selection",
#                                                   .color_lab = "Failure Rate Rank",
#                                                   .size_lab  = "Variability Rank",
#                                                   .interactive = TRUE) {
#
#     rlang::abort("Please use 'tk_tune_rank_parameters()' first.")
#
# }

# #' @export
# plot_parameter_ranking.time_series_cv <- function(.data,
#                                                   .metric,
#                                                   .max_failure_rate = 1,
#                                                   .point_alpha = 0.6,
#                                                   .point_color_best = "#2C3E50",
#                                                   .point_color_worst = "#E31A1C",
#                                                   .point_size_best = 4,
#                                                   .point_size_worst = 2,
#                                                   .title = "Hyperparameter Ranking & Model Selection",
#                                                   .color_lab = "Failure Rate Rank",
#                                                   .size_lab  = "Variability Rank",
#                                                   .interactive = TRUE) {
#
#     rlang::abort("Please use 'tk_tune_rank_parameters()' first.")
#
# }

#' @export
plot_parameter_ranking.data.frame <- function(.data,
                                              .metric,
                                              .max_failure_rate = 1,
                                              .point_alpha = 0.6,
                                              .point_color_best = "#2C3E50",
                                              .point_color_worst = "#E31A1C",
                                              .point_size_best = 4,
                                              .point_size_worst = 2,
                                              .title = "Hyperparameter Ranking & Model Selection",
                                              .color_lab = "Failure Rate Rank",
                                              .size_lab  = "Variability Rank",
                                              .interactive = TRUE) {

    # Checks

    data_formatted <- .data
    error_metric   <- .data %>% dplyr::slice(1) %>% dplyr::pull(.metric)

    # PARAMETER TEXT ----

    nms <- colnames(data_formatted)
    remove_names <- (nms %in% ".metric") %>% cumsum() %>% as.logical()
    nms_to_keep  <- nms[!remove_names]

    txt <- nms_to_keep %>%
        purrr::map(function (x) {
            stringr::str_c(x, ": ", "{", x, "}")
        }) %>%
        stringr::str_c(collapse = "\n")


    data_formatted <- data_formatted %>%
        dplyr::mutate(.param_txt = stringr::str_glue(txt)) %>%
        dplyr::mutate(.txt = stringr::str_glue("Parameters:
                             {.param_txt}
                             ---
                             mean_err: {round(mean, 2)}
                             failure_rate: {scales::percent(failure_rate)}
                             std_err: {round(std_err)}
                             ---"))

    # PLOT ----

    g <- data_formatted %>%
        ggplot2::ggplot(ggplot2::aes(.rank_metric, mean,
                        color = .rank_failure_rate,
                        size  = .rank_std_err))

    suppressWarnings({
        # Avoids warning with unknown aesthetic: text
        g <- g +
            ggplot2::geom_point(ggplot2::aes(text = .txt), alpha = .point_alpha)
    })

    g <- g +
        ggplot2::scale_color_gradient(low  = .point_color_best,
                                      high = .point_color_worst) +
        ggplot2::scale_size(range = c(.point_size_best, .point_size_worst)) +
        ggplot2::scale_x_reverse() +
        ggplot2::expand_limits(y = 0) +
        theme_tq() +
        ggplot2::labs(title = .title,
                      x     = "Model Rank",
                      y     = toupper(error_metric),
                      color = .color_lab,
                      size  = .size_lab)

    if (.interactive) {
        p <- plotly::ggplotly(g)
        return(p)
    } else {
        return(g)
    }

}



