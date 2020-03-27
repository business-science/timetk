#' Simple Training/Test Set Splitting for Time Series
#'
#' `initial_time_split_2()` creates training and testing sets for Time Series
#' Validation. This version differs from `rsample::initial_time_split()` in
#' that it has an `overlap` parameter which is useful for prediction with
#' lagged predictors.
#'
#' @inheritParams rsample::vfold_cv
#' @param prop The proportion of data to be retained for modeling/analysis.
#' @param overlap A value to include an overlap between the assessment
#'  and analysis set. This is useful if lagged predictors will be used
#'  during training and testing.
#' @return An `rset` object that can be used with the `training` and `testing`
#'  functions to extract the data in each split.
#'
#' @details
#'
#' __Lagged Predictors__
#'
#' `overlap` enables the test data to overlap with the training data, which
#'  is useful for predictions needing access to prior history such as when
#'  using lagged predictors.
#'
#' @examples
#' library(recipes)
#' library(rsample)
#' library(timetk)
#' library(tidyverse)
#'
#' # 5-years of monthly data
#' drinks_subset <- drinks %>%
#'     tail(5*12) %>%
#'     rename(sales = S4248SM144NCEN) %>%
#'     as_tibble()
#'
#' # Time series split with 12 period overlap
#' drinks_overlap_split <- initial_time_split_2(
#'     drinks_subset,
#'     prop    = 4 / 5,
#'     overlap = 12
#' )
#'
#' # Extract train and test sets
#' train_data <- training(drinks_overlap_split)
#' test_data  <- testing(drinks_overlap_split)
#'
#' # Note 12 period overlap
#' train_data %>% tail(12)
#' test_data
#'
#' # Make recipe
#' rec_lag <- recipe(sales ~ ., training(drinks_overlap_split)) %>%
#'     step_lag(sales, lag = 12) %>%
#'     step_naomit(all_predictors())
#'
#' # Apply recipe to test data - Correctly applies the lag
#' bake(prep(rec_lag), testing(drinks_overlap_split))
#'
#' @export
initial_time_split_2 <- function(data, prop = 3/4, overlap = 0, ...) {

    if (!is.numeric(prop) | prop >= 1 | prop <= 0) {
        stop("`prop` must be a number on (0, 1).", call. = FALSE)
    }

    n_train <- floor(nrow(data) * prop)

    rsplit(data, 1:n_train, (n_train + 1 - overlap):nrow(data))
}

#' @export
print.rsplit <- function(x, ...) {
    out_char <-
        if (all(is.na(x$out_id)))
            paste(length(rsample::complement(x)))
    else
        paste(length(x$out_id))

    cat("<Training/Testing/Total>\n")
    cat("<",
        length(x$in_id), "/",
        out_char, "/",
        nrow(x$data), ">\n",
        sep = "")
}
