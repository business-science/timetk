# PARAMETER RANKING ----

#' Rank Hyperparameter Tuning Results
#'
#' `tk_tune_rank_parameters()` returns the tuning performance results
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
#' __Metric Ranking (Model Accuracy)__
#'
#' The model with the lowest (best) rank is that with the lowest mean error.
#'
#' __Failure Rate Ranking (Robustness to New Data)__
#'
#' Models with lower failure rates are more robust to new data.
#' Failure rate rank is a score based on the proportion of models that failed during tuning.
#'
#' - Calculation: _Failure Rate = n / No. of Resample Slices_
#' - Models with a non-zero failure rate have a higher likelihood of failing on new data
#' and are therefore less robust.
#'
#' __Standard Error Ranking (Model Variability)__
#'
#' Models with lower standard error are more consistent (less variability).
#'
#' @examples
#' library(dplyr)
#' library(tune)
#' library(timetk)
#'
#' arima_workflow_tuned %>%
#'     tk_tune_rank_parameters(.max_failure_rate = 1)
#'
#' @name tk_tune_rank_parameters
#' @export
tk_tune_rank_parameters <- function(.data, .metric, .max_failure_rate = 1) {
    UseMethod("tk_tune_rank_parameters", .data)
}

#' @export
tk_tune_rank_parameters.default <- function(.data, .metric, .max_failure_rate = 1) {
    rlang::abort("No method for class: ", class(.data)[[1]])
}

#' @export
tk_tune_rank_parameters.tune_results <- function(.data, .metric, .max_failure_rate = 1) {

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
            dplyr::mutate(.rank_failure_rate = seq(1, dplyr::n())) %>%

            # std_error Rank
            dplyr::arrange(std_err) %>%
            dplyr::mutate(.rank_std_err = seq(1, dplyr::n())) %>%

            # Arrange by metric
            dplyr::arrange(.rank_metric)

        return(data_formatted)

    }, error = function(e) {
        warning(call. = FALSE, "Ranking calculations could not be performed. Possible issues are that model metrics do not produce columns 'std_err' or 'n'.")
        return(data_formatted)
    })

}


# PARAMETER SELECTION ----

#' Select Hyperparameters
#'
#' `tk_tune_select_parameters()` provides a wrapper to `tune::select_best()` that
#' makes it easy to select the n-th model.
#'
#' @param .data A `tibble` of class "tune_results"
#' @param .n Row to select from the model ranking
#'
#' @return A `tibble` or `data.frame` with parameter values.
#'
#' @examples
#' library(dplyr)
#' library(tune)
#' library(timetk)
#'
#' arima_workflow_tuned %>%
#'     tk_tune_rank_parameters(.max_failure_rate = 1) %>%
#'     tk_tune_select_parameters(.n = 3)
#'
#' @name tk_tune_select_parameters
#' @export
tk_tune_select_parameters <- function(.data, .n = 1) {
    UseMethod("tk_tune_select_parameters", .data)
}

#' @export
tk_tune_select_parameters.default <- function(.data, .n = 1) {
    rlang::abort("No method for class: ", class(.data)[[1]])
}

#' @export
tk_tune_select_parameters.tune_results <- function(.data, .n = 1) {
    message("Ranking Not Detected: Tune results are expected to be ranked with 'tk_tune_rank_parameters()' first. Reverting to tune::select_best(). '.n' argument not being used.")
    tune::select_best(.data, metric = tune_results_pull_metric(.data, .n_metric = 1))
}

#' @export
tk_tune_select_parameters.data.frame <- function(.data, .n = 1) {

    nms <- names(.data)

    # Locate .metric column and remove everything after
    remove_names <- (nms %in% ".metric") %>% cumsum() %>% as.logical()
    nms_to_keep  <- nms[!remove_names]

    .data %>%
        dplyr::select(!!! rlang::syms(nms_to_keep)) %>%
        dplyr::slice(.n)

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
