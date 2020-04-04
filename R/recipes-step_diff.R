#' Create a differenced predictor
#'
#' `step_diff` creates a *specification* of a recipe step that
#'   will add new columns of differenced data. Differenced data will
#'   include NA values where a difference was induced.
#'   These can be removed with [step_naomit()].
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param ... One or more selector functions to choose which variables are
#'   affected by the step. See [selections()] for more details.
#' @param role Defaults to "predictor"
#' @param trained A logical to indicate if the quantities for preprocessing
#'   have been estimated.
#' @param lag A vector of positive integers identifying which lags (how far back)
#'  to be included in the differencing calculation.
#' @param difference The number of differences to perform.
#' @param log Calculates log differences instead of differences.
#' @param prefix A prefix for generated column names, default to "diff_".
#' @param columns A character string of variable names that will
#'  be populated (eventually) by the `terms` argument.
#' @param id A character string that is unique to this step to identify it.
#' @param skip A logical. Should the step be skipped when the
#'  recipe is baked by [bake.recipe()]? While all operations are baked
#'  when [prep.recipe()] is run, some operations may not be able to be
#'  conducted on new data (e.g. processing the outcome variable(s)).
#'  Care should be taken when using `skip = TRUE` as it may affect
#'  the computations for subsequent operations
#' @return An updated version of `recipe` with the
#'   new step added to the sequence of existing steps (if any).
#' @details The step assumes that the data are already _in the proper sequential
#'  order_ for lagging.
#'
#' @seealso
#' Steps:
#' - Lags: [recipes::step_lag()]
#' - NA Removal: [recipes::step_naomit()]
#'
#' Recipe Setup and Application
#' - Create: [recipes::recipe()]
#' - Prepare: [recipes::prep.recipe()]
#' - Apply: [recipes::bake.recipe()]
#'
#' @export
#' @rdname step_diff
#'
#' @examples
#' library(tidyverse)
#' library(tidyquant)
#' library(recipes)
#' library(timetk)
#'
#'
#' FANG_wide <- FANG %>%
#'     select(symbol, date, adjusted) %>%
#'     pivot_wider(names_from = symbol, values_from = adjusted)
#'
#'
#' # Make and apply recipe ----
#'
#' recipe_diff <- recipe(~ ., data = FANG_wide) %>%
#'   step_diff(FB, AMZN, NFLX, GOOG, lag = 1:3, difference = 1) %>%
#'   prep()
#'
#' recipe_diff %>% bake(FANG_wide)
#'
#'
#' # Get information with tidy ----
#'
#' recipe_diff %>% tidy()
#'
#' recipe_diff %>% tidy(1)
#'
step_diff <-
    function(recipe,
             ...,
             role = "predictor",
             trained = FALSE,
             lag = 1,
             difference = 1,
             log = FALSE,
             prefix = "diff_",
             columns = NULL,
             skip = FALSE,
             id = rand_id("diff")) {

        recipes::add_step(
            recipe,
            step_diff_new(
                terms       = recipes::ellipse_check(...),
                role        = role,
                trained     = trained,
                lag         = lag,
                difference  = difference,
                log         = log,
                prefix      = prefix,
                columns     = columns,
                skip        = skip,
                id          = id
            )
        )
    }

step_diff_new <-
    function(terms, role, trained, lag, difference, log, prefix, columns, skip, id) {
        step(
            subclass       = "diff",
            terms          = terms,
            role           = role,
            trained        = trained,
            lag            = lag,
            difference     = difference,
            log            = log,
            prefix         = prefix,
            columns        = columns,
            skip           = skip,
            id             = id
        )
    }

#' @export
prep.step_diff <- function(x, training, info = NULL, ...) {
    step_diff_new(
        terms       = x$terms,
        role        = x$role,
        trained     = TRUE,
        lag         = x$lag,
        difference  = x$difference,
        log         = x$log,
        prefix      = x$prefix,
        columns     = terms_select(x$terms, info = info),
        skip        = x$skip,
        id          = x$id
    )
}

#' @export
bake.step_diff <- function(object, new_data, ...) {

    if (!all(object$lag == as.integer(object$lag)))
        rlang::abort("step_diff() requires 'lag' argument to be integer valued.")

    if (!all(object$difference == as.integer(object$difference)))
        rlang::abort("step_diff() requires 'difference' argument to be integer valued.")

    make_call <- function(col, lag_val, diff_val) {
        rlang::call2(
            "diff_vec",
            .x          = rlang::sym(col),
            .lag        = lag_val,
            .difference = diff_val,
            .log        = object$log,
            .ns         = "timetk"
        )
    }

    grid <- expand.grid(
        col      = object$columns,
        lag_val  = object$lag,
        diff_val = object$difference,
        stringsAsFactors = FALSE)

    calls   <- purrr::pmap(.l = list(grid$col, grid$lag_val, grid$diff_val), make_call)
    newname <- paste0(object$prefix, grid$lag_val, "_", grid$diff_val, "_", grid$col)
    calls   <- recipes::check_name(calls, new_data, object, newname, TRUE)

    dplyr::as_tibble(dplyr::mutate(new_data, !!!calls))
}

print.step_diff <-
    function(x, width = max(20, options()$width - 30), ...) {
        cat("Differencing ",  sep = "")
        printer(x$columns, x$terms, x$trained, width = width)
        invisible(x)
    }

#' @rdname step_diff
#' @param x A `step_diff` object.
#' @export
tidy.step_diff <- function(x, ...) {

    res <- expand.grid(
        col      = x$columns,
        lag      = x$lag,
        diff     = x$difference,
        log      = x$log,
        stringsAsFactors = FALSE)
    res$id <- x$id
    tibble::as_tibble(res)
}
