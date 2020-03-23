#' Smoother Transformation using Loess (Local Polynomial Regressing Fitting)
#'
#' `step_smooth` creates a a *specification* of a recipe
#'  step that will apply local polynomial regression
#'  to one or more a Numeric column(s). The effect is smoothing the time series
#'  __similar to a moving average without creating missing values or using partial smoothing.__
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more numeric columns to be smoothed.
#'  See [recipes::selections()] for more details.
#'  For the `tidy` method, these are not currently used.
#' @param period The number of periods to include in the local smoothing.
#'  Similar to window size for a moving average.
#'  See details for an explanation `period` vs `span` specification.
#' @param span The span is a percentage of data to be included
#'  in the smoothing window. Period is preferred for shorter windows
#'  to fix the window size.
#'  See details for an explanation `period` vs `span` specification.
#' @param degree The degree of the polynomials to be used.
#'  Set to 2 by default for 2nd order polynomial.
#' @param names An optional character string that is the same
#'  length of the number of terms selected by `terms`. These will be
#'  the names of the __new columns__ created by the step.
#'
#'  - If `NULL`, existing columns are transformed.
#'  - If not `NULL`, new columns will be created.
#' @param trained A logical to indicate if the quantities for
#'  preprocessing have been estimated.
#' @param role For model terms created by this step, what analysis
#'  role should they be assigned?. By default, the function assumes
#'  that the new variable columns created by the original variables
#'  will be used as predictors in a model.
#' @param columns A character string of variables that will be
#'  used as inputs. This field is a placeholder and will be
#'  populated once [recipes::prep.recipe()] is used.
#' @param skip A logical. Should the step be skipped when the recipe is
#'  baked by bake.recipe()? While all operations are baked when prep.recipe()
#'  is run, some operations may not be able to be conducted on new data
#'  (e.g. processing the outcome variable(s)). Care should be taken when
#'  using skip = TRUE as it may affect the computations for subsequent operations.
#' @param id A character string that is unique to this step to identify it.
#'
#' @return For `step_smooth`, an updated version of recipe with
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
#' __Smoother Algorithm__
#' This function is a `recipe` specification that wraps the `stats::loess()`
#' with a modification to set a fixed `period` rather than a percentage of
#' data points via a `span`.
#'
#' __Why Period vs Span?__
#' The `period` is fixed whereas the `span` changes as the number of observations change.
#'
#' __When to use Period?__
#' The effect of using a `period` is similar to a Moving Average where the Window Size
#' is the ___Fixed Period___. This helps when you are trying to smooth local trends.
#' If you want a 30-day moving average, specify `period = 30`.
#'
#'  __When to use Span?__
#'  Span is easier to specify when you want a ___Long-Term Trendline___ where the
#'  window size is unknown. You can specify `span = 0.75` to locally regress
#'  using a window of 75% of the data.
#'
#'  __Warning - Using Span with New Data__
#'  When using span on New Data, the number of observations is likely different than
#'  what you trained with. This means the trendline / smoother can be vastly different
#'  than the smoother you trained with.
#'
#'  __Solution to Span with New Data__
#'  Don't use `span`. Rather, use `period` to fix the window size.
#'  This ensures that new data includes the same number of observations in the local
#'  polynomial regression (loess) as the training data.
#'
#'
#'
#' @examples
#' library(recipes)
#' library(tidyverse)
#' library(tidyquant)
#' library(timetk)
#'
#' # Training Data
#' FB_tbl <- FANG %>%
#'     filter(symbol == "FB") %>%
#'     select(symbol, date, adjusted)
#'
#' # New Data
#' new_data <- FB_tbl %>%
#'     tk_index() %>%
#'     tk_make_future_timeseries(n_future = 90) %>%
#'     tibble(date = .)  %>%
#'     mutate(date = date) %>%
#'     bind_cols(FB_tbl %>% slice((n() - 90 + 1):n()))
#'
#' # ---- PERIOD ----
#'
#' # Create a recipe object with a step_smooth()
#' rec_smooth_period <- recipe(adjusted ~ ., data = FB_tbl) %>%
#'     step_smooth(adjusted, period = 30)
#'
#' # Bake the recipe object - Applies the Loess Transformation
#' training_data_baked <- bake(prep(rec_smooth_period), FB_tbl)
#'
#' # "Period" Effect on New Data
#' new_data_baked <- bake(prep(rec_smooth_period), new_data)
#'
#' # Smoother's fit on new data is very similar because
#' # 30 days are used in the new data regardless of the new data being 90 days
#' training_data_baked %>%
#'     ggplot(aes(date, adjusted)) +
#'     geom_line() +
#'     geom_line(color = "red", data = new_data_baked)
#'
#' # ---- SPAN ----
#'
#' # Create a recipe object with a step_smooth
#' rec_smooth_span <- recipe(adjusted ~ ., data = FB_tbl) %>%
#'     step_smooth(adjusted, span = 0.03)
#'
#' # Bake the recipe object - Applies the Loess Transformation
#' training_data_baked <- bake(prep(rec_smooth_span), FB_tbl)
#'
#' # "Period" Effect on New Data
#' new_data_baked <- bake(prep(rec_smooth_span), new_data)
#'
#' # Smoother's fit is not the same using span because new data is only 90 days
#' # and 0.03 x 90 = 2.7 days
#' training_data_baked %>%
#'     ggplot(aes(date, adjusted)) +
#'     geom_line() +
#'     geom_line(color = "red", data = new_data_baked)
#'
#' # ---- NEW COLUMNS ----
#' # Use the `names` argument to create new columns instead of overwriting existing
#'
#' rec_smooth_names <- recipe(adjusted ~ ., data = FB_tbl) %>%
#'     step_smooth(adjusted, period = 30, names = "adjusted_smooth_30") %>%
#'     step_smooth(adjusted, period = 180, names = "adjusted_smooth_180") %>%
#'     step_smooth(adjusted, span = 0.75, names = "long_term_trend")
#'
#' bake(prep(rec_smooth_names), FB_tbl) %>%
#'     ggplot(aes(date, adjusted)) +
#'     geom_line(alpha = 0.5) +
#'     geom_line(aes(y = adjusted_smooth_30), color = "red", size = 1) +
#'     geom_line(aes(y = adjusted_smooth_180), color = "blue", size = 1) +
#'     geom_line(aes(y = long_term_trend), color = "orange", size = 1)
#'
#' @seealso
#'   [recipes::step_window()] [stats::loess()]
#'   [recipes::recipe()] [recipes::prep.recipe()]
#'   [recipes::bake.recipe()]
#'
#'
#' @importFrom recipes rand_id
#' @export
step_smooth <-
    function(recipe,
             ...,
             period = 30,
             span = NULL,
             degree = 2,
             names = NULL,
             role = "predictor",
             trained = FALSE,
             columns = NULL,
             skip = FALSE,
             id = rand_id("smooth")) {

        recipes::add_step(
            recipe,
            step_smooth_new(
                terms = recipes::ellipse_check(...),
                period = period,
                span = span,
                degree = degree,
                names = names,
                trained = trained,
                role = role,
                columns = columns,
                skip = skip,
                id = id
            )
        )
    }

step_smooth_new <-
    function(terms, role, trained, columns, period, span, degree, names, skip, id) {
        step(
            subclass = "smooth",
            terms = terms,
            role = role,
            names = names,
            trained = trained,
            columns = columns,
            period = period,
            span = span,
            degree = degree,
            skip = skip,
            id = id
        )
    }


#' @export
prep.step_smooth <- function(x, training, info = NULL, ...) {

    col_names <- recipes::terms_select(x$terms, info = info)

    if (any(info$type[info$variable %in% col_names] != "numeric"))
        rlang::abort("The selected variables should be numeric")

    if (!is.null(x$names)) {
        if (length(x$names) != length(col_names))
            rlang::abort(
                paste0("There were ", length(col_names), " term(s) selected but ",
                       length(x$names), " values for the new features ",
                       "were passed to `names`."
                )
            )
    }

    step_smooth_new(
        terms = x$terms,
        role = x$role,
        trained = TRUE,
        columns = col_names,
        period = x$period,
        span = x$span,
        degree = x$degree,
        names = x$names,
        skip = x$skip,
        id = x$id
    )
}

#' @export
bake.step_smooth <- function(object, new_data, ...) {

    # Loop through and create variables
    col_names <- object$columns

    # Span Calc
    if (is.null(object$span)) {
        span <- object$period / nrow(new_data)
    } else {
        span <- object$span
    }

    # Degree
    degree <- object$degree

    if (!is.null(object$names)) {
        # New columns provided
        for (i in seq_along(object$names)) {
            new_data[,object$names[i]] <- new_data %>% dplyr::pull(col_names[i]) %>% smooth_vec(.span = span, .degree = degree)
        }
    } else {
        # No new columns - overwrite existing
        for (i in seq_along(col_names)) {
            new_data[,col_names[i]] <- new_data %>% dplyr::pull(col_names[i]) %>% smooth_vec(.span = span, .degree = degree)
        }
    }

    new_data
}


print.step_smooth <-
    function(x, width = max(20, options()$width - 35), ...) {
        cat("Smoother: Local Polynomial Regression Fitting (Loess) on ")
        printer(x$columns, x$terms, x$trained, width = width)
        invisible(x)
    }

#' @rdname step_smooth
#' @param x A `step_smooth` object.
#' @export
tidy.step_smooth <- function(x, ...) {
    out <- simple_terms(x, ...)
    if (is.null(x$span)) {
        out$period <- x$period
    } else {
        out$span <- x$span
    }
    out$degree <- x$degree
    out$id <- x$id
    out
}

simple_terms <- function(x, ...) {
    if (recipes::is_trained(x)) {
        res <- tibble::tibble(terms = x$columns)
    }
    else {
        term_names <- recipes::sel2char(x$terms)
        res <- tibble::tibble(terms = term_names)
    }
    res
}
