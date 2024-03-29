#' Time Series Feature (Signature) Generator
#'
#' `step_timeseries_signature` creates a a *specification* of a recipe
#'  step that will convert date or date-time data into many
#'  features that can aid in machine learning with time-series data
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose which
#'  variables that will be used to create the new variables. The
#'  selected variables should have class `Date` or
#'  `POSIXct`. See [recipes::selections()] for more details.
#'  For the `tidy` method, these are not currently used.
#' @param trained A logical to indicate if the quantities for
#'  preprocessing have been estimated.
#' @param role For model terms created by this step, what analysis
#'  role should they be assigned?. By default, the function assumes
#'  that the new variable columns created by the original variables
#'  will be used as predictors in a model.
#' @param columns A character string of variables that will be
#'  used as inputs. This field is a placeholder and will be
#'  populated once `recipes::prep()` is used.
#' @param skip A logical. Should the step be skipped when the recipe is
#'  baked by bake.recipe()? While all operations are baked when prep.recipe()
#'  is run, some operations may not be able to be conducted on new data
#'  (e.g. processing the outcome variable(s)). Care should be taken when
#'  using skip = TRUE as it may affect the computations for subsequent operations.
#' @param id A character string that is unique to this step to identify it.
#'
#' @return For `step_timeseries_signature`, an updated version of recipe with
#'  the new step added to the sequence of existing steps (if any).
#'  For the `tidy` method, a tibble with columns `terms`
#'  (the selectors or variables selected), `value` (the feature
#'  names).
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
#'
#' @details
#'
#' __Date Variable__
#'  Unlike other steps, `step_timeseries_signature` does *not*
#'  remove the original date variables. [recipes::step_rm()] can be
#'  used for this purpose.
#'
#' __Scaling index.num__
#'  The `index.num` feature created has a large magnitude (number of seconds since 1970-01-01).
#'  It's a good idea to scale and center this feature (e.g. use  [recipes::step_normalize()]).
#'
#' __Removing Unnecessary Features__
#'  By default, many features are created automatically. Unnecessary features can
#'  be removed using [recipes::step_rm()].
#'
#' @examples
#' library(recipes)
#' library(dplyr)
#'
#' FB_tbl <- FANG %>% dplyr::filter(symbol == "FB")
#'
#' # Create a recipe object with a timeseries signature step
#' rec_obj <- recipe(adjusted ~ ., data = FB_tbl) %>%
#'     step_timeseries_signature(date)
#'
#' # View the recipe object
#' rec_obj
#'
#' # Prepare the recipe object
#' prep(rec_obj)
#'
#' # Bake the recipe object - Adds the Time Series Signature
#' bake(prep(rec_obj), FB_tbl)
#'
#' # Tidy shows which features have been added during the 1st step
#' #  in this case, step 1 is the step_timeseries_signature step
#' tidy(rec_obj)
#' tidy(rec_obj, number = 1)
#'
#'
#'
#' @importFrom recipes rand_id
#' @export
step_timeseries_signature <-
    function(recipe,
             ...,
             role = "predictor",
             trained = FALSE,
             columns = NULL,
             skip = FALSE,
             id = rand_id("timeseries_signature")
    ) {

        recipes::add_step(
            recipe,
            step_timeseries_signature_new(
                terms = recipes::ellipse_check(...),
                role = role,
                trained = trained,
                columns = columns,
                skip = skip,
                id = id
            )
        )
    }

step_timeseries_signature_new <-
    function(terms, role, trained, columns, skip, id) {
        step(
            subclass = "timeseries_signature",
            terms = terms,
            role = role,
            trained = trained,
            columns = columns,
            skip = skip,
            id = id
        )
    }


#' @export
prep.step_timeseries_signature <- function(x, training, info = NULL, ...) {

    col_names <- recipes::recipes_eval_select(x$terms, data = training, info = info)

    recipes::check_type(training[, col_names], types = c("date", "datetime"))

    step_timeseries_signature_new(
        terms = x$terms,
        role = x$role,
        trained = TRUE,
        columns = col_names,
        skip = x$skip,
        id = x$id
    )
}




#' @export
bake.step_timeseries_signature <- function(object, new_data, ...) {

    # feature length - subtract index and diff columns (2 columns)
    feat_len <- (tk_get_timeseries_signature(lubridate::ymd("2016-01-01")) %>% ncol()) - 2

    # Build empty placeholder tibble to house the new features
    new_cols <- rep(
        feat_len,
        each = length(object$columns)
    )

    date_values <- matrix(NA, nrow = nrow(new_data), ncol = sum(new_cols))

    # Dummy column names to avoid tibble warning
    colnames(date_values) <- as.character(seq_len(sum(new_cols)))

    date_values <- tibble::as_tibble(date_values)

    new_names <- vector("character", length = ncol(date_values))

    # Loop through each date column adding features, filling the placeholder tibble
    strt <- 1
    for (i in seq_along(object$columns)) {

        cols <- (strt):(strt + new_cols[i] - 1)

        tmp <- getElement(new_data, object$columns[i]) %>%
            tk_get_timeseries_signature() %>%
            dplyr::select(-index, -diff)

        date_values[, cols] <- tmp

        new_names[cols] <- paste(
            object$columns[i],
            names(tmp),
            sep = "_"
        )

        strt <- max(cols) + 1
    }

    names(date_values) <- new_names

    new_data <- dplyr::bind_cols(new_data, date_values)

    if (!tibble::is_tibble(new_data)) {
        new_data <- tibble::as_tibble(new_data)
    }

    new_data
}


#' @export
print.step_timeseries_signature <-
    function(x, width = max(20, options()$width - 29), ...) {
        title <- "Timeseries signature features from "
        recipes::print_step(x$columns, x$terms, x$trained, width = width, title = title)
        invisible(x)
    }

#' @rdname step_timeseries_signature
#' @param x A `step_timeseries_signature` object.
#' @export
tidy.step_timeseries_signature <- function(x, ...) {

    features <- lubridate::ymd("2016-01-01") %>%
        tk_get_timeseries_signature() %>%
        dplyr::select(-index, -diff) %>%
        colnames()

    if (recipes::is_trained(x)) {
        res <- expand.grid(
            terms = x$columns,
            value = features
        )
    } else {
        term_names <- recipes::sel2char(x$terms)
        res <- expand.grid(
            terms = term_names,
            value = features
        )
    }
    res$id <- x$id
    tibble::as_tibble(res)
}

#' @rdname required_pkgs.timetk
#' @export
required_pkgs.step_timeseries_signature <- function(x, ...) {
    c("timetk")
}
