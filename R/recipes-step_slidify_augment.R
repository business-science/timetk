#' Slidify Rolling Window Transformation (Augmented Version)
#'
#' `step_slidify_augment` creates a a *specification* of a recipe
#'  step that will "augment" (add multiple new columns) that have had a sliding function applied.
#'
#' @inheritParams step_slidify
#' @param prefix A prefix for generated column names, default to "slidify_".
#' @param columns A character string of variable names that will
#'  be populated (eventually) by the `terms` argument.
#' @param id A character string that is unique to this step to identify it.
#' @param skip A logical. Should the step be skipped when the
#'  recipe is baked by `bake.recipe()`? While all operations are baked
#'  when `prep.recipe()` is run, some operations may not be able to be
#'  conducted on new data (e.g. processing the outcome variable(s)).
#'  Care should be taken when using `skip = TRUE` as it may affect
#'  the computations for subsequent operations
#'
#' @return For `step_slidify_augment`, an updated version of recipe with
#'  the new step added to the sequence of existing steps (if any).
#'  For the `tidy` method, a tibble with columns `terms`
#'  (the selectors or variables selected), `value` (the feature
#'  names).
#'
#' @keywords datagen
#' @concept preprocessing
#' @concept moving_windows
#'
#'
#' @details
#'
#' __Alignment__
#'
#' Rolling functions generate `period - 1` fewer values than the incoming vector.
#' Thus, the vector needs to be aligned. Alignment of the vector follows 3 types:
#'
#'  - __Center:__ `NA` or `partial` values are divided and added to the beginning and
#'    end of the series to "Center" the moving average.
#'    This is common for de-noising operations. See also `[smooth_vec()]` for LOESS without NA values.
#'  - __Left:__ `NA` or `partial` values are added to the end to shift the series to the Left.
#'  - __Right:__ `NA` or `partial` values are added to the beginning to shif the series to the Right. This is common in
#'    Financial Applications such as moving average cross-overs.
#'
#' __Partial Values__
#'
#' - The advantage to using `partial` values vs `NA` padding is that
#' the series can be filled (good for time-series de-noising operations).
#' - The downside to partial values is that the partials can become less stable
#' at the regions where incomplete windows are used.
#'
#' If instability is not desirable for de-noising operations, a suitable alternative
#' is [`step_smooth()`], which implements local polynomial regression.
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
#'  Main Recipe Functions:
#'  - `recipes::recipe()`
#'  - `recipes::prep()`
#'  - `recipes::bake()`
#'
#' @examples
#' # library(tidymodels)
#' library(dplyr)
#' library(recipes)
#' library(parsnip)
#'
#' m750 <- m4_monthly %>%
#'     filter(id == "M750") %>%
#'     mutate(value_2 = value / 2)
#'
#' m750_splits <- time_series_split(m750, assess = "2 years", cumulative = TRUE)
#'
#' # Make a recipe
#' recipe_spec <- recipe(value ~ date + value_2, rsample::training(m750_splits)) %>%
#'     step_slidify_augment(
#'         value, value_2,
#'         period = c(6, 12, 24),
#'         .f = ~ mean(.x),
#'         align = "center",
#'         partial = FALSE
#'     )
#'
#' recipe_spec %>% prep() %>% juice()
#'
#' bake(prep(recipe_spec), rsample::testing(m750_splits))
#'
#'
#' @importFrom recipes rand_id
#' @export
step_slidify_augment <-
    function(recipe,
             ...,
             period,
             .f,
             align = c("center", "left", "right"),
             partial = FALSE,
             prefix = "slidify_",
             role = "predictor",
             trained = FALSE,
             columns = NULL,
             f_name  = NULL,
             skip = FALSE,
             id = rand_id("slidify_augment")) {

        if (rlang::quo(.f) %>% rlang::quo_is_missing()) stop(call. = FALSE, "step_slidify_augment(.f) is missing.")
        if (rlang::is_missing(period)) stop(call. = FALSE, "step_slidify_augment(period) is missing.")

        f_name <- rlang::enquo(.f) %>% rlang::expr_text()

        recipes::add_step(
            recipe,
            step_slidify_augment_new(
                terms      = recipes::ellipse_check(...),
                period     = period,
                .f         = .f,
                align      = align,
                partial    = partial,
                prefix     = prefix,
                trained    = trained,
                role       = role,
                columns    = columns,
                f_name     = f_name,
                skip       = skip,
                id         = id
            )
        )
    }

step_slidify_augment_new <-
    function(terms, role, trained, columns, period, .f, align, partial, prefix, f_name, skip, id) {
        step(
            subclass   = "slidify_augment",
            terms      = terms,
            role       = role,
            prefix     = prefix,
            trained    = trained,
            columns    = columns,
            period     = period,
            .f         = .f,
            align      = align,
            partial    = partial,
            f_name     = f_name,
            skip       = skip,
            id         = id
        )
    }


#' @export
prep.step_slidify_augment <- function(x, training, info = NULL, ...) {

    col_names <- recipes::recipes_eval_select(x$terms, data = training, info = info)

    check_type(training[, col_names], types = c("double", "integer"))

    step_slidify_augment_new(
        terms    = x$terms,
        role     = x$role,
        trained  = TRUE,
        columns  = col_names,
        period   = x$period,
        .f       = x$.f,
        align    = tolower(x$align[1]),
        partial  = x$partial,
        prefix   = x$prefix,
        f_name   = x$f_name,
        skip     = x$skip,
        id       = x$id
    )
}

#' @export
bake.step_slidify_augment <- function(object, new_data, ...) {

    if (!all(object$period == as.integer(object$lag)))
        rlang::abort("step_slidify_augment() requires 'period' argument(s) to be integer valued.")

    make_call <- function(col, period_val) {
        rlang::call2(
            .fn         = "slidify_vec",
            .x          = rlang::sym(col),
            .f          = object$.f,
            .period     = period_val,
            .align      = tolower(object$align[1]),
            .partial    = object$partial[1],
            .ns         = "timetk"
        )
    }

    grid <- expand.grid(
        col         = object$columns,
        period_val  = object$period,
        stringsAsFactors = FALSE
    )

    calls   <- purrr::pmap(.l = list(grid$col, grid$period_val), make_call)
    newname <- paste0(object$prefix, grid$period_val, "_", grid$col)
    calls   <- recipes::check_name(calls, new_data, object, newname, TRUE)

    # print(calls)

    tibble::as_tibble(dplyr::mutate(new_data, !!!calls))
}

#' @export
print.step_slidify_augment <-
    function(x, width = max(20, options()$width - 35), ...) {
        title <- "Sliding Augmentation applied to: "
        recipes::print_step(x$columns, x$terms, x$trained, width = width, title = title)
        invisible(x)
    }

#' @rdname step_slidify_augment
#' @param x A `step_slidify_augment` object.
#' @export
tidy.step_slidify_augment <- function(x, ...) {
    res <- expand.grid(
        terms    = x$columns,
        period   = x$period,
        stringsAsFactors = FALSE)
    res$id <- x$id
    res$terms <- paste0(x$prefix, res$period, "_", res$terms)
    tibble::as_tibble(res)
}

#' @rdname required_pkgs.timetk
#' @export
required_pkgs.step_slidify_augment <- function(x, ...) {
    c("timetk")
}


