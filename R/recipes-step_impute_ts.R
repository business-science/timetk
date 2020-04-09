#' Imputation for Time Series using Forecast Methods
#'
#' `step_impute_ts` creates a *specification* of a recipe
#'  step that will transform data using a Box-Cox
#'  transformation. This function differs from
#'  `recipes::step_BoxCox` by adding multiple methods
#'  including Guerrero lambda optimization and handling for
#'  negative data used in the Forecast R Package.
#'
#' @inheritParams step_box_cox
#' @param period A seasonal period to use during the transformation. If `period = 1`,
#'  linear interpolation is performed. If `period > 1`, a robust STL decomposition is
#'  first performed and a linear interpolation is applied to the seasonally adjusted data.
#' @param lambda A box cox transformation parameter. If set to `"auto"`, performs
#'  automated lambda selection.
#' @param lambdas_trained A named numeric vector of lambdas. This is `NULL` until computed
#'  by [prep.recipe()]. Note that, if the original data are integers, the mean
#'  will be converted to an integer to maintain the same a data type.
#'
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` (the
#'  selectors or variables selected) and `value` (the
#'  lambda estimate).
#'
#' @details
#'
#' The `step_impute_ts()` function is designed specifically to handle time series
#' using seaonal imputation methods implemented in the Forecast R Package.
#'
#' __Imputation using Linear Interpolation__
#'
#' Two circumstances cause strictly linear interpolation:
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
#'  - Diffs & Lags [step_diff()], [recipes::step_lag()]
#'  - Smoothing: [step_roll_apply()], [step_smooth()]
#'  - Variance Reduction: [step_box_cox()]
#'  - Imputation: [step_impute_ts()]
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
#'     step_impute_ts(FB, AMZN, NFLX, GOOG, period = 252) %>%
#'     prep()
#'
#' recipe_box_cox %>% bake(FANG_wide)
#'
#' # Lambda parameter used during imputation process
#' recipe_box_cox %>% tidy(1)
#'
#'
#' @seealso
#'
#'  Time Series Analysis:
#'  - [step_timeseries_signature()], [step_holiday_signature()]
#'  - [step_diff()], [recipes::step_lag()]
#'  - [step_roll_apply()], [step_smooth()]
#'  - [step_box_cox()]
#'  - [step_impute_ts()]
#'
#'
#' Recipe Setup and Application:
#' - Create: [recipes::recipe()]
#' - Prepare: [recipes::prep.recipe()]
#' - Apply: [recipes::bake.recipe()]
#'
#' @export
step_impute_ts <-
    function(recipe,
             ...,
             period = 1,
             lambda = "auto",
             role = NA,
             trained = FALSE,
             lambdas_trained = NULL,
             skip = FALSE,
             id = rand_id("impute_ts")) {

        recipes::add_step(
            recipe,
            step_impute_ts_new(
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

step_impute_ts_new <-
    function(terms, role, trained, period, lambda, lambdas_trained, skip, id) {
        recipes::step(
            subclass   = "impute_ts",
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
prep.step_impute_ts <- function(x, training, info = NULL, ...) {

    col_names <- terms_select(x$terms, info = info)
    recipes::check_type(training[, col_names])

    # Lambda Calculation
    if (x$lambda[1] == "auto") {
        lambda_values <- training[, col_names] %>%
            purrr::map(auto_lambda)
    } else if (is.null(x$lamda[1])) {
        lambda_values <- rep(NULL, length(col_names))
        names(lambda_values) <- col_names
    } else {
        lambda_values <- rep(x$lambda[1], length(col_names))
        names(lambda_values) <- col_names
    }

    step_impute_ts_new(
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
bake.step_impute_ts <- function(object, new_data, ...) {

    col_names <- names(object$lambdas_trained)

    for (i in seq_along(object$lambdas_trained)) {

        # Handle "non-numeric" naming issue
        val_i <- object$lambdas_trained[i]
        if (!is.null(val_i)) {
            val_i <- as.numeric(val_i)
        }

        new_data[, col_names[i]] <- impute_ts_vec(
            x      = new_data %>% purrr::pluck(col_names[i]),
            period = object$period[1],
            lambda = val_i
        )
    }

    tibble::as_tibble(new_data)
}

print.step_impute_ts <-
    function(x, width = max(20, options()$width - 35), ...) {
        cat("Time Series Imputation on ", sep = "")
        printer(names(x$lambdas_trained), x$terms, x$trained, width = width)
        invisible(x)
    }




#' @rdname step_impute_ts
#' @param x A `step_impute_ts` object.
#' @export
tidy.step_impute_ts <- function(x, ...) {
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
