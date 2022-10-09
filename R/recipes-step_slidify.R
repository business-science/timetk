#' Slidify Rolling Window Transformation
#'
#' `step_slidify` creates a a *specification* of a recipe
#'  step that will apply a function
#'  to one or more a Numeric column(s).
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more numeric columns to be smoothed.
#'  See [recipes::selections()] for more details.
#'  For the `tidy` method, these are not currently used.
#' @param period The number of periods to include in the local rolling window.
#'  This is effectively the "window size".
#' @param .f A summary __formula__ in one of the following formats:
#'  - `mean` with no arguments
#'  - `function(x) mean(x, na.rm = TRUE)`
#'  - `~ mean(.x, na.rm = TRUE)`, it is converted to a function.
#'
#' @param align Rolling functions generate `period - 1` fewer values than the incoming vector.
#'  Thus, the vector needs to be aligned. Alignment of the vector follows 3 types:
#'
#'  - __Center:__ `NA` or `.partial` values are divided and added to the beginning and
#'    end of the series to "Center" the moving average.
#'    This is common for de-noising operations. See also `[smooth_vec()]` for LOESS without NA values.
#'  - __Left:__ `NA` or `.partial` values are added to the end to shift the series to the Left.
#'  - __Right:__ `NA` or `.partial` values are added to the beginning to shif the series to the Right. This is common in
#'    Financial Applications such as moving average cross-overs.
#' @param partial Should the moving window be allowed to return partial (incomplete) windows instead of NA values.
#'  Set to FALSE by default, but can be switched to TRUE to remove NA's.
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
#'  populated once `recipes::prep()` is used.
#' @param f_name A character string for the function being applied.
#'  This field is a placeholder and will be populated during the `tidy()` step.
#' @param skip A logical. Should the step be skipped when the recipe is
#'  baked by bake.recipe()? While all operations are baked when prep.recipe()
#'  is run, some operations may not be able to be conducted on new data
#'  (e.g. processing the outcome variable(s)). Care should be taken when
#'  using skip = TRUE as it may affect the computations for subsequent operations.
#' @param id A character string that is unique to this step to identify it.
#'
#' @return For `step_slidify`, an updated version of recipe with
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
#' # New Data - Make some fake new data next 90 time stamps
#' new_data <- FB_tbl %>%
#'     tail(90) %>%
#'     mutate(date = date %>% tk_make_future_timeseries(length_out = 90))
#'
#' # OVERWRITE EXISTING COLUMNS -----
#'
#' # Create a recipe object with a step_slidify
#' rec_ma_50 <- recipe(adjusted ~ ., data = FB_tbl) %>%
#'     step_slidify(adjusted, period = 50, .f = ~ AVERAGE(.x))
#'
#' # Bake the recipe object - Applies the Moving Average Transformation
#' training_data_baked <- bake(prep(rec_ma_50), FB_tbl)
#'
#' # Apply to New Data
#' new_data_baked <- bake(prep(rec_ma_50), new_data)
#'
#' # Visualize effect
#' training_data_baked %>%
#'     ggplot(aes(date, adjusted)) +
#'     geom_line() +
#'     geom_line(color = "red", data = new_data_baked)
#'
#' # ---- NEW COLUMNS ----
#' # Use the `names` argument to create new columns instead of overwriting existing
#'
#' rec_ma_30_names <- recipe(adjusted ~ ., data = FB_tbl) %>%
#'     step_slidify(adjusted, period = 30, .f = AVERAGE, names = "adjusted_ma_30")
#'
#' bake(prep(rec_ma_30_names), FB_tbl) %>%
#'     ggplot(aes(date, adjusted)) +
#'     geom_line(alpha = 0.5) +
#'     geom_line(aes(y = adjusted_ma_30), color = "red", size = 1)
#'
#'
#'
#' @importFrom recipes rand_id
#' @export
step_slidify <-
    function(recipe,
             ...,
             period,
             .f,
             align = c("center", "left", "right"),
             partial = FALSE,
             names = NULL,
             role = "predictor",
             trained = FALSE,
             columns = NULL,
             f_name  = NULL,
             skip = FALSE,
             id = rand_id("slidify")) {

        if (rlang::quo(.f) %>% rlang::quo_is_missing()) stop(call. = FALSE, "step_slidify(.f) is missing.")
        if (rlang::is_missing(period)) stop(call. = FALSE, "step_slidify(period) is missing.")

        f_name <- rlang::enquo(.f) %>% rlang::expr_text()

        if (length(period) > 1) rlang::abort("step_slidify() can only use one 'period'. For multiple periods, use step_slidify_augment().")

        recipes::add_step(
            recipe,
            step_slidify_new(
                terms      = recipes::ellipse_check(...),
                period     = period,
                .f         = .f,
                align      = align,
                partial    = partial,
                names      = names,
                trained    = trained,
                role       = role,
                columns    = columns,
                f_name     = f_name,
                skip       = skip,
                id         = id
            )
        )
    }

step_slidify_new <-
    function(terms, role, trained, columns, period, .f, align, partial, names, f_name, skip, id) {
        step(
            subclass   = "slidify",
            terms      = terms,
            role       = role,
            names      = names,
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
prep.step_slidify <- function(x, training, info = NULL, ...) {

    col_names <- recipes::recipes_eval_select(x$terms, data = training, info = info)

    check_type(training[, col_names])

    if (!is.null(x$names)) {
        if (length(x$names) != length(col_names))
            rlang::abort(
                paste0("There were ", length(col_names), " term(s) selected but ",
                       length(x$names), " values for the new features ",
                       "were passed to `names`."
                )
            )
    }

    step_slidify_new(
        terms    = x$terms,
        role     = x$role,
        trained  = TRUE,
        columns  = col_names,
        period   = x$period,
        .f       = x$.f,
        align    = x$align,
        partial  = x$partial,
        names    = x$names,
        f_name   = x$f_name,
        skip     = x$skip,
        id       = x$id
    )
}

#' @export
bake.step_slidify <- function(object, new_data, ...) {

    col_names <- object$columns

    align   <- object$align[1]
    partial <- object$partial[1]

    if (!is.null(object$names)) {
        # New columns provided
        for (i in seq_along(object$names)) {
            new_data[,object$names[i]] <- new_data %>%
                dplyr::pull(col_names[i]) %>%
                slidify_vec(
                    .period  = object$period,
                    .f       = object$.f,
                    .align   = align,
                    .partial = partial)
        }
    } else {
        # No new columns - overwrite existing
        for (i in seq_along(col_names)) {
            new_data[,col_names[i]] <- new_data %>%
                dplyr::pull(col_names[i]) %>%
                slidify_vec(
                    .period  = object$period,
                    .f       = object$.f,
                    .align   = align,
                    .partial = partial)
        }
    }

    new_data
}

#' @export
print.step_slidify <-
    function(x, width = max(20, options()$width - 35), ...) {
        title <- "Slidify Function Applied on "
        recipes::print_step(x$columns, x$terms, x$trained, width = width, title = title)
        invisible(x)
    }

#' @rdname step_slidify
#' @param x A `step_slidify` object.
#' @export
tidy.step_slidify <- function(x, ...) {
    out         <- simple_terms(x, ...)
    out$period  <- x$period
    out$.f      <- x$f_name
    out$align   <- x$align[1]
    out$partial <- x$partial
    out$id      <- x$id
    out
}

#' @rdname required_pkgs.timetk
#' @export
required_pkgs.step_slidify <- function(x, ...) {
    c("timetk")
}


