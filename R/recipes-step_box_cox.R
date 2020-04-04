#' Box-Cox Transformation using Forecast Methods
#'
#' `step_box_cox` creates a *specification* of a recipe
#'  step that will transform data using a Box-Cox
#'  transformation. This function differs from
#'  `recipes::step_BoxCox` by adding multiple methods
#'  including Guerrero lambda optimization and handling for
#'  negative data used in the Forecast R Package.
#'
#' @inheritParams recipes::step_center
#' @param ... One or more selector functions to choose which
#'  variables are affected by the step. See [selections()]
#'  for more details. For the `tidy` method, these are not
#'  currently used.
#' @param method One of "guerrero" or "loglik"
#' @param limits A length 2 numeric vector defining the range to
#'  compute the transformation parameter lambda.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param lambdas A numeric vector of transformation values. This
#'  is `NULL` until computed by [prep.recipe()].
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
#' library(tidyverse)
#' library(tidyquant)
#' library(recipes)
#' library(timetk)
#'
#' FANG_wide <- FANG %>%
#' select(symbol, date, adjusted) %>%
#'     pivot_wider(names_from = symbol, values_from = adjusted)
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
#' Transformations to reduce variance:
#'
#' 1. [recipes::step_YeoJohnson()] - Yeo-Johnson modified Box Cox for Negative Data
#' 2. [recipes::step_BoxCox()] - Implements Box Cox LogLik method
#' 3. [recipes::step_log()] - Log transformation
#' 4. [recipes::step_sqrt()] - Square-Root Power Transformation
#'
#' Recipe Setup and Application:
#' - Create: [recipes::recipe()]
#' - Prepare: [recipes::prep.recipe()]
#' - Apply: [recipes::bake.recipe()]
#'
#' @export
step_box_cox <-
    function(recipe,
             ...,
             method = c("guerrero", "loglik"),
             limits = c(-1, 2),
             role = NA,
             trained = FALSE,
             lambdas = NULL,
             skip = FALSE,
             id = rand_id("box_cox")) {

        recipes::add_step(
            recipe,
            step_box_cox_new(
                terms         = recipes::ellipse_check(...),
                role          = role,
                trained       = trained,

                lambdas       = lambdas,
                limits        = sort(limits)[1:2],
                method        = tolower(method)[1],

                skip          = skip,
                id            = id
            )
        )
    }

step_box_cox_new <-
    function(terms, role, trained, lambdas, limits, method, skip, id) {
        recipes::step(
            subclass   = "box_cox",
            terms      = terms,
            role       = role,
            trained    = trained,
            lambdas    = lambdas,
            method     = method,
            limits     = limits,
            skip       = skip,
            id         = id
        )
    }

#' @export
prep.step_box_cox <- function(x, training, info = NULL, ...) {

    col_names <- terms_select(x$terms, info = info)
    check_type(training[, col_names])

    # values <- vapply(
    #     training[, col_names],
    #     estimate_bc,
    #     c(lambda = 0),
    #     limits = x$limits,
    #     num_unique = x$num_unique
    # )

    # lambda_values <- vapply(
    #     training[, col_names],
    #     auto_lambda,
    #     .method = x$method,
    #     .lambda_lower = x$limits[1],
    #     .lambda_upper = x$limits[2]
    # )

    lambda_values <- training[, col_names] %>%
        purrr::map(.f = function(vals) {
            auto_lambda(
                vals,
                .method       = x$method,
                .lambda_lower = x$limits[1],
                .lambda_upper = x$limits[2]
            )
        })

    step_box_cox_new(
        terms     = x$terms,
        role      = x$role,
        trained   = TRUE,

        lambdas   = lambda_values,
        limits    = x$limits,
        method    = x$method,

        skip      = x$skip,
        id        = x$id
    )
}

#' @export
bake.step_box_cox <- function(object, new_data, ...) {

    # object$lambdas

    param <- names(object$lambdas)

    for (i in seq_along(object$lambdas)) {
        new_data[, param[i]] <- box_cox_vec(
            .x      = new_data %>% purrr::pluck(param[i]),
            .lambda = as.numeric(object$lambdas[i])
        )
    }

    tibble::as_tibble(new_data)
}

print.step_box_cox <-
    function(x, width = max(20, options()$width - 35), ...) {
        cat("Box-Cox transformation on ", sep = "")
        printer(names(x$lambdas), x$terms, x$trained, width = width)
        invisible(x)
    }




#' @rdname step_box_cox
#' @param x A `step_box_cox` object.
#' @export
tidy.step_box_cox <- function(x, ...) {
    if (is_trained(x)) {
        res <- tibble(terms  = names(x$lambdas),
                      lambda = as.numeric(x$lambdas))
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
