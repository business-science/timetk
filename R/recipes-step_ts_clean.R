#' Clean Outliers and Missing Data for Time Series
#'
#' `step_ts_clean` creates a *specification* of a recipe
#'  step that will clean outliers and impute time series data.
#'
#' @inheritParams step_ts_impute
#'
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` (the
#'  selectors or variables selected) and `value` (the
#'  lambda estimate).
#'
#' @details
#'
#' The `step_ts_clean()` function is designed specifically to handle time series
#' using seasonal outlier detection methods implemented in the Forecast R Package.
#'
#' __Cleaning Outliers__
#'
#' #' Outliers are replaced with missing values using the following methods:
#'
#' 1. Non-Seasonal (`period = 1`): Uses `stats::supsmu()`
#' 2. Seasonal (`period > 1`): Uses `forecast::mstl()` with `robust = TRUE` (robust STL decomposition)
#'  for seasonal series.
#'
#'
#' __Imputation using Linear Interpolation__
#'
#' Three circumstances cause strictly linear interpolation:
#'
#'   1. __Period is 1:__ With `period = 1`, a seasonality cannot be interpreted and therefore linear is used.
#'   2. __Number of Non-Missing Values is less than 2-Periods__: Insufficient values exist to detect seasonality.
#'   3. __Number of Total Values is less than 3-Periods__: Insufficient values exist to detect seasonality.
#'
#' __Seasonal Imputation using Linear Interpolation__
#'
#' For seasonal series with `period > 1`, a robust Seasonal Trend Loess (STL) decomposition is first computed.
#' Then a linear interpolation is applied to the seasonally adjusted data, and
#' the seasonal component is added back.
#'
#' __Box Cox Transformation__
#'
#' In many circumstances, a Box Cox transformation can help. Especially if the series is multiplicative
#' meaning the variance grows exponentially. A Box Cox transformation can be automated by setting `lambda = "auto"`
#' or can be specified by setting `lambda = numeric value`.
#'
#' @seealso
#'  Time Series Analysis:
#'  - Engineered Features: [step_timeseries_signature()], [step_holiday_signature()], [step_fourier()]
#'  - Diffs & Lags [step_diff()], `recipes::step_lag()`
#'  - Smoothing: [step_slidify()], [step_smooth()]
#'  - Variance Reduction: [step_box_cox()]
#'  - Imputation: [step_ts_impute()], [step_ts_clean()]
#'  - Padding: [step_ts_pad()]
#'
#' @references
#' - [Forecast R Package](https://github.com/robjhyndman/forecast)
#' - [Forecasting Principles & Practices: Dealing with missing values and outliers](https://otexts.com/fpp2/missing-outliers.html)
#'
#' @examples
#'
#' library(tidyverse)
#' library(tidyquant)
#' library(recipes)
#' library(timetk)
#'
#' # Get missing values
#' FANG_wide <- FANG %>%
#'     select(symbol, date, adjusted) %>%
#'     pivot_wider(names_from = symbol, values_from = adjusted) %>%
#'     pad_by_time()
#'
#' FANG_wide
#'
#' # Apply Imputation
#' recipe_box_cox <- recipe(~ ., data = FANG_wide) %>%
#'     step_ts_clean(FB, AMZN, NFLX, GOOG, period = 252) %>%
#'     prep()
#'
#' recipe_box_cox %>% bake(FANG_wide)
#'
#' # Lambda parameter used during imputation process
#' recipe_box_cox %>% tidy(1)
#'
#'
#'
#' @export
step_ts_clean <-
    function(recipe,
             ...,
             period = 1,
             lambda = "auto",
             role = NA,
             trained = FALSE,
             lambdas_trained = NULL,
             skip = FALSE,
             id = rand_id("ts_clean")) {

        recipes::add_step(
            recipe,
            step_ts_clean_new(
                terms           = recipes::ellipse_check(...),
                role            = role,
                trained         = trained,

                period          = period,
                lambda          = lambda,
                lambdas_trained = lambdas_trained,

                skip            = skip,
                id              = id
            )
        )
    }

step_ts_clean_new <-
    function(terms, role, trained, period, lambda, lambdas_trained, skip, id) {
        recipes::step(
            subclass   = "ts_clean",
            terms      = terms,
            role       = role,
            trained    = trained,

            period          = period,
            lambda          = lambda,
            lambdas_trained = lambdas_trained,

            skip       = skip,
            id         = id
        )
    }

#' @export
prep.step_ts_clean <- function(x, training, info = NULL, ...) {

    col_names <- recipes_eval_select(x$terms, data = training, info = info)
    recipes::check_type(training[, col_names])

    # Lambda Calculation
    if (is.null(x$lambda[1])) {
        lambda_values <- rep(NA, length(col_names))
        names(lambda_values) <- col_names
    } else if (x$lambda[1] == "auto") {
        lambda_values <- training[, col_names] %>%
            purrr::map(auto_lambda)
    } else {
        lambda_values <- rep(x$lambda[1], length(col_names))
        names(lambda_values) <- col_names
    }

    step_ts_clean_new(
        terms           = x$terms,
        role            = x$role,
        trained         = TRUE,

        period          = x$period,
        lambda          = x$lambda,
        lambdas_trained = lambda_values,

        skip            = x$skip,
        id              = x$id
    )
}

#' @export
bake.step_ts_clean <- function(object, new_data, ...) {

    col_names <- names(object$lambdas_trained)

    for (i in seq_along(object$lambdas_trained)) {

        # Handle "non-numeric" naming issue
        val_i <- object$lambdas_trained[i]
        if (!is.na(val_i)) {
            val_i <- as.numeric(val_i)
        }

        new_data[, col_names[i]] <- ts_clean_vec(
            x      = new_data %>% purrr::pluck(col_names[i]),
            period = object$period[1],
            lambda = val_i
        )
    }

    tibble::as_tibble(new_data)
}

#' @export
print.step_ts_clean <- function(x, width = max(20, options()$width - 35), ...) {
    title <- "Time Series Outlier Cleaning on "
    recipes::print_step(names(x$lambdas_trained), x$terms, x$trained, width = width, title = title)
    invisible(x)
}




#' @rdname step_ts_clean
#' @param x A `step_ts_clean` object.
#' @export
tidy.step_ts_clean <- function(x, ...) {
    if (is_trained(x)) {
        res <- tibble::tibble(
            terms  = names(x$lambdas_trained),
            lambda = as.numeric(x$lambdas_trained)
        )
    } else {
        term_names <- recipes::sel2char(x$terms)
        res <- tibble::tibble(
            terms  = term_names,
            lambda = rlang::na_dbl
        )
    }
    res$id <- x$id
    res
}

#' @rdname required_pkgs.timetk
#' @export
required_pkgs.step_ts_clean <- function(x, ...) {
    c("timetk")
}
