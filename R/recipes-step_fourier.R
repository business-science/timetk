#' Fourier Features for Modeling Seasonality
#'
#' `step_fourier_series` creates a a *specification* of a recipe
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
#'  fourier term. More orders increase the number of fourier terms and
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
#' @return For `step_fourier_series`, an updated version of recipe with
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
#'  Unlike other steps, `step_fourier_series` does *not*
#'  remove the original date variables. [recipes::step_rm()] can be
#'  used for this purpose.
#'
#' __Period Specification__
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
#'  The `K` argument specifies the maximum number of orders of Fourier terms.
#'  Examples:
#'  - Specifying `period = 365` and `K = 1` will return a `cos_1_365` and `sin_1_365` sequence
#'  - Specifying `period = 365` and `K = 2` will return a `cos_1_365`, `cos_2_365`, `sin_1_365` and `sin_2_365`
#'  sequence, which tends to increase the models ability to fit vs the `K = 1` specification
#'  (at the expense of possibly overfitting).
#'
#'  __Multiple values of `period` and `K`__
#'  It's possible to specify multiple values of `period` in a single
#'  step such as `step_fourier_series(period = c(91.25, 365), K = c(1, 1))`.
#'  Note that the number of `K` elements must match the number of `period` elements.
#'
#' @examples
#' library(recipes)
#' library(tidyverse)
#' library(tidyquant)
#' library(timetk)
#'
#' FB_tbl <- FANG %>% filter(symbol == "FB")
#'
#' # Create a recipe object with a timeseries signature step
#' rec_obj <- recipe(adjusted ~ ., data = FB_tbl) %>%
#'     step_fourier_series(date, period = c(90.25, 365), K = c(1, 1))
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
#'  - Date Features: [step_timeseries_signature()], [recipes::step_holiday()], [recipes::step_date()]
#'  - Main Recipe Functions: [recipes::recipe()], [recipes::prep.recipe()], [recipes::bake.recipe()]
#'
#'
#' @importFrom recipes rand_id
#' @export
step_fourier_series <-
    function(recipe,
             ...,
             period,
             K,
             role = "predictor",
             trained = FALSE,
             columns = NULL,
             scale_factor = NULL,
             skip = FALSE,
             id = rand_id("fourier_series")
    ) {
        # Checks
        if (length(period) != length(K)) {
            stop("Number of periods does not match number of K's (fourier orders)")
        }
        if (any(2 * K > period)) {
            stop("K must be not be greater than period/2")
        }
        recipes::add_step(
            recipe,
            step_fourier_series_new(
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

step_fourier_series_new <-
    function(terms, role, period, K, scale_factor, trained, columns, skip, id) {
        step(
            subclass = "fourier_series",
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
prep.step_fourier_series <- function(x, training, info = NULL, ...) {

    col_names <- recipes::terms_select(x$terms, info = info)

    date_data <- info[info$variable %in% col_names, ]

    if (length(col_names) > 1) {
        rlang::abort("Only one column permitted")
    }

    if (any(date_data$type != "date"))
        rlang::abort(
            paste0("All variables for `step_timeseries_signature` should be either `Date` or",
                   "`POSIXct` classes."
            )
        )

    scale_factor_calculated <- training[,col_names[1]] %>%
        tk_index() %>%
        tk_get_timeseries_summary() %>%
        dplyr::pull(diff.median)

    step_fourier_series_new(
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
bake.step_fourier_series <- function(object, new_data, ...) {

    tmp <- new_data[,object$columns[1]] %>%
        tk_index() %>%
        calculate_fourier_series(
            period       = object$period,
            K            = object$K,
            scale_factor = object$scale_factor) %>%
        dplyr::rename_all(~ stringr::str_c(object$columns[1], "_", .))

    new_data <- dplyr::bind_cols(new_data, tmp)

    if (!tibble::is_tibble(new_data)) {
        new_data <- tibble::as_tibble(new_data)
    }

    new_data
}

calculate_fourier_terms <- function(period, K) {

    purrr::map2(period, K, .f = function(period, K) {
        purrr::set_names(seq_len(K) / period, paste0(seq_len(K), "_", round(period)))
    }) %>%
        unlist() %>%
        .[!duplicated(.)]

}

calculate_fourier_series <- function(idx, period, K, scale_factor) {
    # Convert Date to numeric index & scale to 1 unit sequence
    x        <- as.POSIXct(idx) %>% as.numeric() %>% as.integer()
    x_scaled <- x / scale_factor
    period   <- as.numeric(period)

    fourier_terms <- calculate_fourier_terms(period, K)

    # Cosine Series
    cos_series_tbl <- fourier_terms %>%
        purrr::map_dfr(function(term) {
            cos(2 * pi * term * x_scaled)
        }) %>%
        dplyr::rename_all(~ str_c("cos_", .))

    # Sine Series
    sin_series_tbl <- fourier_terms %>%
        purrr::map_dfr(function(term) {
            sin(2 * pi * term * x_scaled)
        }) %>%
        dplyr::rename_all(~ str_c("sin_", .))

    ret <- dplyr::bind_cols(cos_series_tbl, sin_series_tbl)

    return(ret)
}


#' @export
print.step_fourier_series <-
    function(x, width = max(20, options()$width - 29), ...) {
        cat("Fourier series features from ")
        recipes::printer(x$columns, x$terms, x$trained, width = width)
        invisible(x)
    }

#' @rdname step_fourier_series
#' @param x A `step_fourier_series` object.
#' @export
tidy.step_fourier_series <- function(x, ...) {

    if (recipes::is_trained(x)) {

        scale_factor <- x$scale_factor
        names(scale_factor) <- "scale_factor"

        fourier_terms <- calculate_fourier_terms(x$period, x$K)

        res <- tibble::tibble(
            terms = x$columns,
            # terms = "date",
            names = c(names(fourier_terms), names(scale_factor)),
            value = c(fourier_terms, scale_factor)
        )

    } else {
        term_names <- recipes::sel2char(x$terms)
        res <- expand.grid(
            terms = term_names,
            names = NA,
            value = NA
        )
    }
    res$id <- x$id
    tibble::as_tibble(res)
}
