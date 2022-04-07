#' Log Interval Transformation for Constrained Interval Forecasting
#'
#' `step_log_interval` creates a *specification* of a recipe
#'  step that will transform data using a Log-Inerval
#'  transformation. This function provides a `recipes` interface
#'  for the `log_interval_vec()` transformation function.
#'
#' @inheritParams log_interval_vec
#' @param recipe A `recipe` object. The step will be added to the sequence of operations for this recipe.
#' @param ... One or more selector functions to choose which
#'  variables are affected by the step. See [selections()]
#'  for more details. For the `tidy` method, these are not
#'  currently used.
#'
#' @param role Not used by this step since no new variables are
#'  created.
#' @param trained A logical to indicate if the quantities for preprocessing have been estimated.
#' @param limit_lower_trained A numeric vector of transformation values. This
#'  is `NULL` until computed by `prep()`.
#' @param limit_upper_trained A numeric vector of transformation values. This
#'  is `NULL` until computed by `prep()`.
#' @param skip A logical. Should the step be skipped when the recipe
#'  is baked by `bake.recipe()`? While all operations are baked when `prep.recipe()` is run,
#'  some operations may not be able to be conducted on new data (e.g. processing the outcome variable(s)).
#'  Care should be taken when using `skip = TRUE` as it may affect the computations for subsequent operations.
#' @param id A character string that is unique to this step to identify it.
#'
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` (the
#'  selectors or variables selected) and `value` (the
#'  lambda estimate).
#'
#' @details
#'
#' The `step_log_interval()` function is designed specifically to handle time series
#' using methods implemented in the Forecast R Package.
#'
#' __Positive Data__
#'
#' If data includes values of zero, use `offset` to adjust the series to make the values positive.
#'
#' __Implementation__
#'
#' Refer to the [log_interval_vec()] function for the transformation implementation details.
#'
#'
#'
#' @examples
#' library(tidyverse)
#' library(tidyquant)
#' library(recipes)
#' library(timetk)
#'
#' FANG_wide <- FANG %>%
#'     select(symbol, date, adjusted) %>%
#'     pivot_wider(names_from = symbol, values_from = adjusted)
#'
#' recipe_log_interval <- recipe(~ ., data = FANG_wide) %>%
#'     step_log_interval(FB, AMZN, NFLX, GOOG, offset = 1) %>%
#'     prep()
#'
#' recipe_log_interval %>%
#'     bake(FANG_wide) %>%
#'     pivot_longer(-date) %>%
#'     plot_time_series(date, value, name, .smooth = FALSE, .interactive = FALSE)
#'
#' recipe_log_interval %>% tidy(1)
#'
#' @seealso
#'
#'  Time Series Analysis:
#'  - Engineered Features: [step_timeseries_signature()], [step_holiday_signature()], [step_fourier()]
#'  - Diffs & Lags [step_diff()], `recipes::step_lag()`
#'  - Smoothing: [step_slidify()], [step_smooth()]
#'  - Variance Reduction: [step_log_interval()]
#'  - Imputation: [step_ts_impute()], [step_ts_clean()]
#'  - Padding: [step_ts_pad()]
#'
#' Transformations to reduce variance:
#' - `recipes::step_log()` - Log transformation
#' - `recipes::step_sqrt()` - Square-Root Power Transformation
#'
#' Recipe Setup and Application:
#' - `recipes::recipe()`
#' - `recipes::prep()`
#' - `recipes::bake()`
#'
#' @export
step_log_interval <-
    function(recipe,
             ...,

             limit_lower = "auto",
             limit_upper = "auto",
             offset = 0,

             role = NA,
             trained = FALSE,
             limit_lower_trained = NULL,
             limit_upper_trained = NULL,
             skip = FALSE,
             id = rand_id("log_interval")) {

        recipes::add_step(
            recipe,
            step_log_interval_new(
                terms           = recipes::ellipse_check(...),
                role            = role,
                trained         = trained,

                limit_lower_trained  = limit_lower_trained,
                limit_upper_trained  = limit_upper_trained,

                limit_lower     = limit_lower[1],
                limit_upper     = limit_upper[1],
                offset          = offset[1],

                skip            = skip,
                id              = id
            )
        )
    }

step_log_interval_new <-
    function(terms, role, trained, limit_lower_trained, limit_upper_trained,
             limit_lower, limit_upper, offset, method, skip, id) {
        recipes::step(
            subclass           = "log_interval",
            terms              = terms,
            role               = role,
            trained            = trained,

            limit_lower_trained     = limit_lower_trained,
            limit_upper_trained     = limit_upper_trained,

            limit_lower        = limit_lower,
            limit_upper        = limit_upper,
            offset             = offset,

            skip               = skip,
            id                 = id
        )
    }

#' @export
prep.step_log_interval <- function(x, training, info = NULL, ...) {

    col_names <- terms_select(x$terms, info = info)
    recipes::check_type(training[, col_names])

    limit_lower_trained <- training[, col_names] %>%
        purrr::map(.f = function(vals) {

            vals <- vals + x$offset

            max_x   <- max(vals)
            min_x   <- min(vals)
            range_x <- abs(max_x - min_x)

            auto_limit_lower(x$limit_lower, min_x, range_x)
        })

    limit_upper_trained <- training[, col_names] %>%
        purrr::map(.f = function(vals) {

            vals <- vals + x$offset

            max_x   <- max(vals)
            min_x   <- min(vals)
            range_x <- abs(max_x - min_x)

            auto_limit_upper(x$limit_upper, max_x, range_x)
        })

    step_log_interval_new(
        terms           = x$terms,
        role            = x$role,
        trained         = TRUE,

        limit_lower_trained = limit_lower_trained,
        limit_upper_trained = limit_upper_trained,

        limit_lower     = x$limit_lower,
        limit_upper     = x$limit_upper,
        offset          = x$offset,

        skip            = x$skip,
        id              = x$id
    )
}

#' @export
bake.step_log_interval <- function(object, new_data, ...) {

    # Column names to transform
    param <- names(object$limit_lower_trained)

    for (i in seq_along(object$limit_lower_trained)) {

        print(object$limit_lower_trained[i])
        print(object$limit_upper_trained[i])

        new_data[, param[i]] <- log_interval_vec(
            x           = new_data %>% purrr::pluck(param[i]),
            limit_lower = as.numeric(object$limit_lower_trained[i]),
            limit_upper = as.numeric(object$limit_upper_trained[i]),
            offset      = object$offset,
            silent      = TRUE
        )
    }

    tibble::as_tibble(new_data)
}

#' @export
print.step_log_interval <-
    function(x, width = max(20, options()$width - 35), ...) {
        title <- "Log-interval transformation on "
        recipes::print_step(names(x$limit_lower_trained), x$terms, x$trained, width = width, title = title)
        invisible(x)
    }




#' @rdname step_log_interval
#' @param x A `step_log_interval` object.
#' @export
tidy.step_log_interval <- function(x, ...) {
    if (is_trained(x)) {
        res <- tibble::tibble(
            terms       = names(x$limit_lower_trained),
            limit_lower = as.numeric(x$limit_lower_trained),
            limit_upper = as.numeric(x$limit_upper_trained),
            offset      = x$offset
        )
    } else {
        term_names <- recipes::sel2char(x$terms)
        res <- tibble::tibble(
            terms  = term_names,
            limit_lower = x$limit_lower,
            limit_upper = x$limit_upper,
            offset      = x$offset
        )
    }
    res$id <- x$id
    res
}

#' @rdname required_pkgs.timetk
#' @export
required_pkgs.step_log_interval <- function(x, ...) {
    c("timetk")
}
