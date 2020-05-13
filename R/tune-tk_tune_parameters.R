# PARAMETER RANKING ----

#' Rank Hyperparameter Tuning Results
#'
#' `tk_parameter_ranking()` returns the tuning performance results
#' from functions like `tune::tune_grid()`. If possible, adds parameter ranking
#' results based on metric performance, model failure rates, and standard error (variability).
#'
#' @param .data A `tibble` of class "tune_results"
#' @param .metric Select a metric to use for tuning performance investigation.
#' @param .max_failure_rate Range (0,1). Use to `.max_failure_rate` to filter models below an acceptable failure threshold.
#'
#'
#' @return A `tibble` or `data.frame` with ranked tuning parameters.
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
#' @examples
#' library(dplyr)
#' library(tune)
#' library(timetk)
#'
#' # Output of tune::tune_grid()
#' arima_workflow_tuned
#'
#' # PARAMETER RANKING ----
#' arima_workflow_tuned %>%
#'     tk_parameter_ranking(.max_failure_rate = 1)
#'
#' # PARAMETER SELECTION ----
#' arima_workflow_tuned %>%
#'     tk_parameter_ranking(.max_failure_rate = 1) %>%
#'     tk_parameter_select_by_row(.row_id = 3)
#'
#' @name tk_parameter_ranking
#' @export
tk_parameter_ranking <- function(.data, .metric, .max_failure_rate = 1) {
    UseMethod("tk_parameter_ranking", .data)
}

#' @export
tk_parameter_ranking.default <- function(.data, .metric, .max_failure_rate = 1) {
    rlang::abort("No method for class: ", class(.data)[[1]])
}

#' @export
tk_parameter_ranking.tune_results <- function(.data, .metric, .max_failure_rate = 1) {

    data_formatted <- .data
    slice_count    <- data_formatted %>% dplyr::pull(id) %>% length()

    # If no .metric provided, use first metric found
    if (rlang::is_missing(.metric)) {
        .metric <- tune_results_pull_metric(data_formatted, .n_metric = 1)
        message("Using .metric = '", .metric, "'")
    }

    data_formatted <- data_formatted %>%
        tune::show_best(.metric, n = Inf)

    tryCatch({
        data_formatted <- data_formatted %>%
            # Failure Rate Calc
            dplyr::mutate(failure_rate = 1 - (n / slice_count)) %>%
            dplyr::filter(failure_rate <= .max_failure_rate) %>%

            # Metric Rank -
            dplyr::mutate(.rank_metric = seq(1, dplyr::n())) %>%

            # Failure Rate Rank
            dplyr::arrange(failure_rate, .rank_metric) %>%
            # dplyr::mutate(.rank_failure_rate = seq(1, dplyr::n())) %>%
            dplyr::mutate(.rank_failure_rate = dplyr::min_rank(failure_rate)) %>%

            # std_error Rank
            dplyr::arrange(std_err) %>%
            dplyr::mutate(.rank_std_err = seq(1, dplyr::n())) %>%

            # Arrange by metric
            dplyr::arrange(.rank_metric) %>%
            tibble::rowid_to_column(var = ".row_id")

        return(data_formatted)

    }, error = function(e) {
        warning(call. = FALSE, "Ranking calculations could not be performed. Possible issues are that model metrics do not produce columns 'std_err' or 'n'.")
        return(data_formatted)
    })

}


# PARAMETER SELECTION ----

#' Select Hyperparameters
#'
#' `tk_parameter_select_by_row()` provides a wrapper to `tune::select_best()` that
#' makes it easy to select the n-th model.
#'
#' @param .data A `tibble` of class "tune_results"
#' @param .row_id A numeric Row ID to select from the model ranking
#'
#' @return A `tibble` or `data.frame` with parameter values.
#'
#' @examples
#' library(dplyr)
#' library(tune)
#' library(timetk)
#'
#' # Output of tune::tune_grid()
#' arima_workflow_tuned
#'
#' # PARAMETER RANKING ----
#' arima_workflow_tuned %>%
#'     tk_parameter_ranking(.max_failure_rate = 1)
#'
#' # PARAMETER SELECTION ----
#' arima_workflow_tuned %>%
#'     tk_parameter_ranking(.max_failure_rate = 1) %>%
#'     tk_parameter_select_by_row(.row_id = 3)
#'
#' @name tk_parameter_select_by_row
#' @export
tk_parameter_select_by_row <- function(.data, .row_id = 1) {
    UseMethod("tk_parameter_select_by_row", .data)
}

#' @export
tk_parameter_select_by_row.default <- function(.data, .row_id = 1) {
    rlang::abort("No method for class: ", class(.data)[[1]])
}

#' @export
tk_parameter_select_by_row.tune_results <- function(.data, .row_id = 1) {
    message("Ranking Not Detected: Tune results are expected to be ranked with 'tk_parameter_ranking()' first. Reverting to tune::select_best(). '.row_id' argument not being used.")
    tune::select_best(.data, metric = tune_results_pull_metric(.data, .n_metric = 1))
}

#' @export
tk_parameter_select_by_row.data.frame <- function(.data, .row_id = 1) {

    nms <- names(.data)

    # Locate .metric column and remove everything after
    remove_names    <- (nms %in% ".metric") %>% cumsum() %>% as.logical()
    remove_names[1] <-  TRUE #.row_id
    nms_to_keep     <- nms[!remove_names]

    .data %>%
        dplyr::select(!!! rlang::syms(nms_to_keep)) %>%
        dplyr::filter(.row_id == .row_id)

}

# UTILITIES ----
tune_results_pull_metric <- function(.data, .n_metric = 1) {
    .data %>%
        dplyr::select(.metrics) %>%
        tidyr::unnest(.metrics) %>%
        dplyr::distinct(.metric) %>%
        dplyr::slice(.n_metric) %>%
        dplyr::pull(.metric)

}
