#' Box-Cox Transformation using Forecast Methods
#'
#' `step_box_cox` creates a *specification* of a recipe
#'  step that will transform data using a Box-Cox
#'  transformation. This function differs from
#'  `recipes::step_BoxCox` by adding multiple methods
#'  including Guerrero lambda optimization and handling for
#'  negative data used in the Forecast R Package.
#'
#' @param recipe A `recipe` object. The step will be added to the sequence of operations for this recipe.
#' @param ... One or more selector functions to choose which
#'  variables are affected by the step. See [recipes::selections()]
#'  for more details. For the `tidy` method, these are not
#'  currently used.
#' @param method One of "guerrero" or "loglik"
#' @param limits A length 2 numeric vector defining the range to
#'  compute the transformation parameter lambda.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param trained A logical to indicate if the quantities for preprocessing have been estimated.
#' @param lambdas_trained A numeric vector of transformation values. This
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
#' The `step_box_cox()` function is designed specifically to handle time series
#' using methods implemented in the Forecast R Package.
#'
#' __Negative Data__
#'
#' This function can be applied to Negative Data.
#'
#' __Lambda Optimization Methods__
#'
#'  This function uses 2 methods for optimizing the lambda selection
#'  from the Forecast R Package:
#'
#'  1. `method = "guerrero"`: Guerrero's (1993) method is used, where lambda minimizes
#'  the coefficient of variation for subseries of x.
#'
#'  2. `method = loglik`: the value of lambda is chosen to maximize the profile
#'   log likelihood of a linear model fitted to x. For non-seasonal data, a
#'   linear time trend is fitted while for seasonal data, a linear time trend
#'   with seasonal dummy variables is used.
#'
#'
#' @references
#' 1. Guerrero, V.M. (1993) Time-series analysis supported by power transformations. _Journal of Forecasting_, __12__, 37–48.
#' 2. Box, G. E. P. and Cox, D. R. (1964) An analysis of transformations. _JRSS_ B __26__ 211–246.
#'
#' @examples
#' library(dplyr)
#' library(recipes)
#'
#' FANG_wide <- FANG %>%
#'     select(symbol, date, adjusted) %>%
#'     tidyr::pivot_wider(names_from = symbol, values_from = adjusted)
#'
#' recipe_box_cox <- recipe(~ ., data = FANG_wide) %>%
#'     step_box_cox(FB, AMZN, NFLX, GOOG) %>%
#'     prep()
#'
#' recipe_box_cox %>% bake(FANG_wide)
#'
#' recipe_box_cox %>% tidy(1)
#'
#' @seealso
#'
#'  Time Series Analysis:
#'  - Engineered Features: [step_timeseries_signature()], [step_holiday_signature()], [step_fourier()]
#'  - Diffs & Lags [step_diff()], [recipes::step_lag()]
#'  - Smoothing: [step_slidify()], [step_smooth()]
#'  - Variance Reduction: [step_box_cox()]
#'  - Imputation: [step_ts_impute()], [step_ts_clean()]
#'  - Padding: [step_ts_pad()]
#'
#' Transformations to reduce variance:
#' - [recipes::step_log()] - Log transformation
#' - [recipes::step_sqrt()] - Square-Root Power Transformation
#'
#' Recipe Setup and Application:
#' - [recipes::recipe()]
#' - [recipes::prep()]
#' - [recipes::bake()]
#'
#' @export
step_box_cox <-
    function(recipe,
             ...,
             method = c("guerrero", "loglik"),
             limits = c(-1, 2),
             role = NA,
             trained = FALSE,
             lambdas_trained = NULL,
             skip = FALSE,
             id = rand_id("box_cox")) {

        recipes::add_step(
            recipe,
            step_box_cox_new(
                terms           = recipes::ellipse_check(...),
                role            = role,
                trained         = trained,

                lambdas_trained = lambdas_trained,
                limits          = sort(limits)[1:2],
                method          = tolower(method)[1],

                skip            = skip,
                id              = id
            )
        )
    }

step_box_cox_new <-
    function(terms, role, trained, lambdas_trained, limits, method, skip, id) {
        recipes::step(
            subclass           = "box_cox",
            terms              = terms,
            role               = role,
            trained            = trained,
            lambdas_trained    = lambdas_trained,
            method             = method,
            limits             = limits,
            skip               = skip,
            id                 = id
        )
    }

#' @export
prep.step_box_cox <- function(x, training, info = NULL, ...) {

    col_names <- recipes_eval_select(x$terms, data = training, info = info)
    recipes::check_type(training[, col_names], types = c("double", "integer"))

    lambda_values <- training[, col_names] %>%
        purrr::map(.f = function(vals) {
            auto_lambda(
                vals,
                method       = x$method,
                lambda_lower = x$limits[1],
                lambda_upper = x$limits[2]
            )
        })

    step_box_cox_new(
        terms           = x$terms,
        role            = x$role,
        trained         = TRUE,

        lambdas_trained = lambda_values,
        limits          = x$limits,
        method          = x$method,

        skip            = x$skip,
        id              = x$id
    )
}

#' @export
bake.step_box_cox <- function(object, new_data, ...) {

    # object$lambdas_trained

    param <- names(object$lambdas_trained)

    for (i in seq_along(object$lambdas_trained)) {
        new_data[, param[i]] <- box_cox_vec(
            x      = new_data %>% purrr::pluck(param[i]),
            lambda = as.numeric(object$lambdas_trained[i])
        )
    }

    tibble::as_tibble(new_data)
}

#' @export
print.step_box_cox <-
    function(x, width = max(20, options()$width - 35), ...) {
        title <- "Box-Cox transformation on "
        recipes::print_step(names(x$lambdas_trained), x$terms, x$trained, width = width, title = title)
        invisible(x)
    }




#' @rdname step_box_cox
#' @param x A `step_box_cox` object.
#' @export
tidy.step_box_cox <- function(x, ...) {
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



#' S3 methods for tracking which additional packages are needed for steps.
#'
#' Recipe-adjacent packages always list themselves as a required package so that
#' the steps can function properly within parallel processing schemes.
#' @param x A recipe step
#' @return A character vector
#' @rdname required_pkgs.timetk
#' @keywords internal
#' @export
required_pkgs.step_box_cox <- function(x, ...) {
    c("timetk")
}

