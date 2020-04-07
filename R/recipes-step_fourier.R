#' Fourier Features for Modeling Seasonality
#'
#' `step_fourier` creates a a *specification* of a recipe
#'  step that will convert a Date or Date-time column into a Fourier
#'  series
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... A single column with class `Date` or
#'  `POSIXct`. See [recipes::selections()] for more details.
#'  For the `tidy` method, these are not currently used.
#' @param period The numeric period for the oscillation frequency.
#'  See details for examples of `period` specification.
#' @param K The number of orders to include for each sine/cosine
#'  fourier series. More orders increase the number of fourier terms and
#'  therefore the variance of the fitted
#'  model at the expense of bias. See details for examples of `K` specification.
#' @param trained A logical to indicate if the quantities for
#'  preprocessing have been estimated.
#' @param role For model terms created by this step, what analysis
#'  role should they be assigned?. By default, the function assumes
#'  that the new variable columns created by the original variables
#'  will be used as predictors in a model.
#' @param columns A character string of variables that will be
#'  used as inputs. This field is a placeholder and will be
#'  populated once [recipes::prep.recipe()] is used.
#' @param scale_factor A factor for scaling the numeric index extracted
#'  from the date or date-time feature. This is a placeholder and will be populated
#'  once [recipes::prep.recipe()] is used.
#' @param skip A logical. Should the step be skipped when the recipe is
#'  baked by bake.recipe()? While all operations are baked when prep.recipe()
#'  is run, some operations may not be able to be conducted on new data
#'  (e.g. processing the outcome variable(s)). Care should be taken when
#'  using skip = TRUE as it may affect the computations for subsequent operations.
#' @param id A character string that is unique to this step to identify it.
#'
#' @return For `step_fourier`, an updated version of recipe with
#'  the new step added to the sequence of existing steps (if any).
#'  For the `tidy` method, a tibble with columns `terms`
#'  (the selectors or variables selected), `value` (the feature
#'  names).
#'
#' @keywords datagen
#' @concept preprocessing
#' @concept model_specification
#' @concept variable_encodings
#' @concept dates
#'
#'
#' @details
#'
#' __Date Variable__
#'
#'  Unlike other steps, `step_fourier` does *not*
#'  remove the original date variables. [recipes::step_rm()] can be
#'  used for this purpose.
#'
#' __Period Specification__
#'
#'  The `period` argument is used to generate the distance between peaks
#'  in the fourier sequence. The key is to line up the peaks with unique
#'  seasonalities in the data.
#'
#'  For Daily Data, typical period specifications are:
#'  - Yearly frequency is 365
#'  - Quarterly frequency is 365 / 4 = 91.25
#'  - Monthly frequency is 365 / 12 = 30.42
#'
#'
#' __K Specification__
#'
#'  The `K` argument specifies the maximum number of orders of Fourier terms.
#'  Examples:
#'  - Specifying `period = 365` and `K = 1` will return a `cos365_K1` and `sin365_K1` fourier series
#'  - Specifying `period = 365` and `K = 2` will return a `cos365_K1`, `cos365_K2`, `sin365_K1` and `sin365_K2`
#'  sequence, which tends to increase the models ability to fit vs the `K = 1` specification
#'  (at the expense of possibly overfitting).
#'
#'  __Multiple values of `period` and `K`__
#'
#'  It's possible to specify multiple values of `period` in a single
#'  step such as `step_fourier(period = c(91.25, 365), K = 2`.
#'  This returns 8 Fouriers series:
#'   - `cos91.25_K1`, `sin91.25_K1`, `cos91.25_K2`, `sin91.25_K2`
#'   - `cos365_K1`, `sin365_K1`, `cos365_K2`, `sin365_K2`
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
#' # Create a recipe object with a timeseries signature step
#' # - 252 Trade days per year
#' # - period = c(252/4, 252): Adds quarterly and yearly fourier series
#' # - K = 2: Adds 1st and 2nd fourier orders
#'
#' rec_obj <- recipe(adjusted ~ ., data = FB_tbl) %>%
#'     step_fourier(date, period = c(252/4, 252), K = 2)
#'
#' # View the recipe object
#' rec_obj
#'
#' # Prepare the recipe object
#' prep(rec_obj)
#'
#' # Bake the recipe object - Adds the Fourier Series
#' bake(prep(rec_obj), FB_tbl)
#'
#' # Tidy shows which features have been added during the 1st step
#' #  in this case, step 1 is the step_timeseries_signature step
#' tidy(prep(rec_obj))
#' tidy(prep(rec_obj), number = 1)
#'
#' @seealso
#'
#'  Time Series Analysis:
#'  - [step_timeseries_signature()]
#'  - [step_holiday_signature()]
#'  - [step_diff()]
#'  - [recipes::step_lag()]
#'  - [step_roll_apply()]
#'  - [step_smooth()]
#'  - [step_box_cox()]
#'
#'  Main Recipe Functions:
#'  - [recipes::recipe()]
#'  - [recipes::prep.recipe()]
#'  - [recipes::bake.recipe()]
#'
#'
#' @importFrom recipes rand_id
#' @export
step_fourier <-
    function(recipe,
             ...,
             period,
             K,
             role = "predictor",
             trained = FALSE,
             columns = NULL,
             scale_factor = NULL,
             skip = FALSE,
             id = rand_id("fourier")
    ) {
        # Checks
        # if (length(period) != length(K)) {
        #     stop("Number of periods does not match number of K's (fourier orders)")
        # }
        if (any(2 * K > period)) {
            stop("K must be not be greater than period/2")
        }
        recipes::add_step(
            recipe,
            step_fourier_new(
                terms = recipes::ellipse_check(...),
                period = period,
                K = K,
                role = role,
                trained = trained,
                columns = columns,
                scale_factor = scale_factor,
                skip = skip,
                id = id
            )
        )
    }

step_fourier_new <-
    function(terms, role, period, K, scale_factor, trained, columns, skip, id) {
        step(
            subclass = "fourier",
            terms = terms,
            role = role,
            period = period,
            K = K,
            scale_factor = scale_factor,
            trained = trained,
            columns = columns,
            skip = skip,
            id = id
        )
    }


#' @export
prep.step_fourier <- function(x, training, info = NULL, ...) {

    col_names <- recipes::terms_select(x$terms, info = info)

    date_data <- info[info$variable %in% col_names, ]

    if (length(col_names) > 1) {
        rlang::abort("Only one column permitted")
    }

    if (any(date_data$type != "date"))
        rlang::abort(
            paste0("All variables for `step_fourier` should be either `Date` or",
                   " `POSIXct` classes."
            )
        )

    scale_factor_calculated <- training[,col_names[1]] %>%
        tk_index() %>%
        tk_get_timeseries_summary() %>%
        dplyr::pull(diff.median)

    step_fourier_new(
        terms = x$terms,
        role = x$role,
        trained = TRUE,
        columns = col_names,
        skip = x$skip,
        id = x$id,
        period = x$period,
        K = x$K,
        scale_factor = scale_factor_calculated
    )
}




#' @export
bake.step_fourier <- function(object, new_data, ...) {

    if (!all(object$period == as.numeric(object$period)))
        rlang::abort("step_fourier() requires 'period' argument to be numeric valued.")

    if (!all(object$K == as.integer(object$K)))
        rlang::abort("step_diff() requires 'K' argument to be integer valued.")

    make_call <- function(col, period_val, K_val, type_val) {
        rlang::call2(
            "fourier_vec",
            x          = rlang::sym(col),
            period     = period_val,
            K          = K_val,
            type       = type_val,
            .ns        = "timetk"
        )
    }

    grid <- expand.grid(
        col         = object$columns,

        type_val    = c("sin", "cos"),
        K_val       = 1:max(object$K),
        period_val  = object$period,

        stringsAsFactors = FALSE)

    calls   <- purrr::pmap(.l = list(grid$col, grid$period_val, grid$K_val, grid$type_val), make_call)

    # Column names
    newname <- paste0(grid$col, "_", grid$type_val, round(grid$period_val, 2), "_K", grid$K_val)
    calls   <- recipes::check_name(calls, new_data, object, newname, TRUE)

    tibble::as_tibble(dplyr::mutate(new_data, !!!calls))

}




#' @export
print.step_fourier <-
    function(x, width = max(20, options()$width - 29), ...) {
        cat("Fourier series features from ")
        recipes::printer(x$columns, x$terms, x$trained, width = width)
        invisible(x)
    }

#' @rdname step_fourier
#' @param x A `step_fourier` object.
#' @export
tidy.step_fourier <- function(x, ...) {

    res <- expand.grid(
        terms       = x$columns,
        type        = c("sin", "cos"),
        K           = 1:max(x$K),
        period      = x$period,
        stringsAsFactors = FALSE)

    res$id    <- x$id
    res$terms <- paste0(res$terms, "_", res$type, round(res$period, 2), "_K", res$K)

    tibble::as_tibble(res)

}


