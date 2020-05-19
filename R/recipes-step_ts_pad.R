#' Pad: Add rows to fill gaps and go from low to high frequency
#'
#' `step_ts_pad` creates a a *specification* of a recipe
#'  step that will analyze a Date or Date-time column adding rows
#'  at a specified interval.
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... A single column with class `Date` or
#'  `POSIXct`. See [recipes::selections()] for more details.
#'  For the `tidy` method, these are not currently used.
#' @param by Either "auto", a time-based frequency like "year", "month", "day", "hour", etc,
#'  or a time expression like "5 min", or "7 days". See Details.
#' @param pad_value Fills in padded values. Default is `NA`.
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
#' @return For `step_ts_pad`, an updated version of recipe with
#'  the new step added to the sequence of existing steps (if any).
#'  For the `tidy` method, a tibble with columns `terms`
#'  (the selectors or variables selected), `value` (the feature
#'  names).
#'
#' @concept preprocessing
#' @concept model_specification
#' @concept dates
#'
#'
#' @details
#'
#' __Date Variable__
#'
#' - Only one date or date-time variable may be supplied.
#' - `step_ts_pad())` does *not* remove the original date variables.
#'
#' __Interval Specification (by)__
#'
#' Padding can be applied in the following ways:
#'
#' - The eight intervals in are: year, quarter, month, week, day, hour, min, and sec.
#' - Intervals like 30 minutes, 1 hours, 14 days are possible.
#'
#' __Imputing Missing Values__
#'
#' The generic `pad_value` defaults to `NA`, which typically requires imputation.
#' Some common strategies include:
#'
#' - __Numeric data:__ The `step_ts_impute()` preprocessing step can be used to impute
#' numeric time series data with or without seasonality
#' - __Nominal data:__ The `step_mode_impute()` preprocessing step can be used to replace
#'  missing values with the most common value.
#'
#'
#' @examples
#' library(recipes)
#' library(tidyverse)
#' library(tidyquant)
#' library(timetk)
#'
#' FB_tbl <- FANG %>%
#'     filter(symbol == "FB") %>%
#'     select(symbol, date, adjusted)
#'
#'
#' rec_obj <- recipe(adjusted ~ ., data = FB_tbl) %>%
#'     step_ts_pad(date, by = "day", pad_value = NA)
#'
#' # View the recipe object
#' rec_obj
#'
#' # Prepare the recipe object
#' prep(rec_obj)
#'
#' # Bake the recipe object - Adds the padding
#' bake(prep(rec_obj), FB_tbl)
#'
#' # Tidy shows which features have been added during the 1st step
#' #  in this case, step 1 is the step_timeseries_signature step
#' tidy(prep(rec_obj))
#' tidy(prep(rec_obj), number = 1)
#'
#' @seealso
#'
#' Padding & Imputation:
#' - Pad Time Series: [step_ts_pad()]
#' - Impute missing values with these: [step_ts_impute()], [step_ts_clean()]
#'
#'  Time Series Analysis:
#'  - Engineered Features: [step_timeseries_signature()], [step_holiday_signature()], [step_fourier()]
#'  - Diffs & Lags [step_diff()], [recipes::step_lag()]
#'  - Smoothing: [step_slidify()], [step_smooth()]
#'  - Variance Reduction: [step_box_cox()]
#'
#'
#'  Main Recipe Functions:
#'  - [recipes::recipe()]
#'  - [recipes::prep.recipe()]
#'  - [recipes::bake.recipe()]
#'
#'
#' @importFrom recipes rand_id
#' @export
step_ts_pad <-
    function(recipe,
             ...,
             by = "day",
             pad_value = NA,
             role = "predictor",
             trained = FALSE,
             columns = NULL,
             skip = FALSE,
             id = rand_id("ts_padding")
    ) {
        # Checks
        if (by == "auto") {
            stop("by must be explicitly specified.")
        }
        if (is.null(by)) {
            stop("by must be explicitly specified.")
        }
        recipes::add_step(
            recipe,
            step_ts_pad_new(
                terms = recipes::ellipse_check(...),
                by = by,
                pad_value = pad_value,
                role = role,
                trained = trained,
                columns = columns,
                skip = skip,
                id = id
            )
        )
    }

step_ts_pad_new <-
    function(terms, role, by, pad_value, trained, columns, skip, id) {
        step(
            subclass = "ts_pad",
            terms = terms,
            role = role,
            by = by,
            pad_value = pad_value,
            trained = trained,
            columns = columns,
            skip = skip,
            id = id
        )
    }


#' @export
prep.step_ts_pad <- function(x, training, info = NULL, ...) {

    col_names <- recipes::terms_select(x$terms, info = info)

    date_data <- info[info$variable %in% col_names, ]

    if (length(col_names) > 1) {
        rlang::abort("Only one column permitted")
    }

    if (any(date_data$type != "date")) {
        rlang::abort(
            paste0("All variables for `step_ts_pad` should be either `Date` or",
                   " `POSIXct` classes."
            )
        )
    }

    step_ts_pad_new(
        terms = x$terms,
        role = x$role,
        trained = TRUE,
        columns = col_names[1],
        skip = x$skip,
        id = x$id,

        by = x$by,
        pad_value = x$pad_value
    )
}




#' @export
bake.step_ts_pad <- function(object, new_data, ...) {

    date_var_expr <- rlang::sym(object$columns[1])

    # print(object$columns[1])
    new_data %>%
        pad_by_time(
            .date_var   = !! date_var_expr,
            .by         = object$by,
            .pad_value  = object$pad_value
        )

}




#' @export
print.step_ts_pad <- function(x, width = max(20, options()$width - 29), ...) {
    cat("Padded time series features from ")
    recipes::printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
}

#' @rdname step_ts_pad
#' @param x A `step_ts_pad` object.
#' @export
tidy.step_ts_pad <- function(x, ...) {

    res <- tibble::tibble(
        terms      = x$columns,
        by         = x$by,
        pad_value  = x$pad_value,
        id         = x$id
    )

    tibble::as_tibble(res)

}


