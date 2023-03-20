#' Holiday Feature (Signature) Generator
#'
#' `step_holiday_signature` creates a a *specification* of a recipe
#'  step that will convert date or date-time data into many
#'  holiday features that can aid in machine learning with time-series data.
#'  By default, many features are returned for different _holidays, locales, and stock exchanges_.
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose which
#'  variables that will be used to create the new variables. The
#'  selected variables should have class `Date` or
#'  `POSIXct`. See [recipes::selections()] for more details.
#'  For the `tidy` method, these are not currently used.
#' @param holiday_pattern A regular expression pattern to search the "Holiday Set".
#' @param locale_set Return binary holidays based on locale.
#' One of: "all", "none", "World", "US", "CA", "GB", "FR", "IT", "JP", "CH", "DE".
#' @param exchange_set Return binary holidays based on Stock Exchange Calendars.
#' One of: "all", "none", "NYSE", "LONDON", "NERC", "TSX", "ZURICH".
#' @param trained A logical to indicate if the quantities for
#'  preprocessing have been estimated.
#' @param role For model terms created by this step, what analysis
#'  role should they be assigned?. By default, the function assumes
#'  that the new variable columns created by the original variables
#'  will be used as predictors in a model.
#' @param columns A character string of variables that will be
#'  used as inputs. This field is a placeholder and will be
#'  populated once `recipes::prep()` is used.
#' @param features A character string of features that will be
#'  generated. This field is a placeholder and will be
#'  populated once `recipes::prep()` is used.
#' @param skip A logical. Should the step be skipped when the recipe is
#'  baked by bake.recipe()? While all operations are baked when prep.recipe()
#'  is run, some operations may not be able to be conducted on new data
#'  (e.g. processing the outcome variable(s)). Care should be taken when
#'  using skip = TRUE as it may affect the computations for subsequent operations.
#' @param id A character string that is unique to this step to identify it.
#'
#' @return For `step_holiday_signature`, an updated version of recipe with
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
#' __Use Holiday Pattern and Feature Sets to Pare Down Features__
#'  By default, you're going to get A LOT of Features. This is a good thing because many
#'  machine learning algorithms have regularization built in. But, in many cases you
#'  will still want to reduce the number of _unnecessary features_. Here's how:
#'
#'  - __Holiday Pattern:__ This is a Regular Expression pattern that can be used to filter.
#'   Try `holiday_pattern = "(US_Christ)|(US_Thanks)"` to return just Christmas and Thanksgiving
#'   features.
#'  - __Locale Sets:__ This is a logical as to whether or not the locale has a holiday.
#'   For locales outside of US you may want to combine multiple locales.
#'   For example, `locale_set = c("World", "GB")` returns both World Holidays and Great Britain.
#'  - __Exchange Sets:__ This is a logical as to whether or not the _Business is off_ due
#'   to a holiday. Different Stock Exchanges are used as a proxy for business holiday calendars.
#'   For example, `exchange_set = "NYSE"` returns business holidays for New York Stock Exchange.
#'
#'
#' __Removing Unnecessary Features__
#'  By default, many features are created automatically. Unnecessary features can
#'  be removed using [recipes::step_rm()] and [recipes::selections()] for more details.
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
#' library(timetk)
#' library(tibble)
#' library(dplyr)
#'
#' # Sample Data
#' dates_in_2017_tbl <- tibble(
#'     index = tk_make_timeseries("2017-01-01", "2017-12-31", by = "day")
#' )
#'
#' # Add US holidays and Non-Working Days due to Holidays
#' # - Physical Holidays are added with holiday pattern (individual) and locale_set
#' rec_holiday <- recipe(~ ., dates_in_2017_tbl) %>%
#'     step_holiday_signature(index,
#'                            holiday_pattern = "^US_",
#'                            locale_set      = "US",
#'                            exchange_set    = "NYSE")
#'
#' # Not yet prep'ed - just returns parameters selected
#' rec_holiday %>% tidy(1)
#'
#' # Prep the recipe
#' rec_holiday_prep <- prep(rec_holiday)
#'
#' # Now prep'ed - returns new features that will be created
#' rec_holiday_prep %>% tidy(1)
#'
#' # Apply the recipe to add new holiday features!
#' bake(rec_holiday_prep, dates_in_2017_tbl)
#'
#'
#'
#'
#' @importFrom recipes rand_id
#' @export
step_holiday_signature <-
    function(recipe,
             ...,
             holiday_pattern = ".",
             locale_set = "all",
             exchange_set = "all",
             role = "predictor",
             trained = FALSE,
             columns = NULL,
             features = NULL,
             skip = FALSE,
             id = rand_id("holiday_signature")
    ) {

        recipes::add_step(
            recipe,
            step_holiday_signature_new(
                terms           = recipes::ellipse_check(...),
                holiday_pattern = holiday_pattern,
                locale_set      = locale_set,
                exchange_set    = exchange_set,
                role            = role,
                trained         = trained,
                columns         = columns,
                features        = features,
                skip            = skip,
                id              = id
            )
        )
    }

step_holiday_signature_new <-
    function(terms, holiday_pattern, locale_set, exchange_set, role, trained, columns, features, skip, id) {
        step(
            subclass        = "holiday_signature",
            terms           = terms,
            holiday_pattern = holiday_pattern,
            locale_set      = locale_set,
            exchange_set    = exchange_set,
            role            = role,
            trained         = trained,
            columns         = columns,
            features        = features,
            skip            = skip,
            id              = id
        )
    }


#' @export
prep.step_holiday_signature <- function(x, training, info = NULL, ...) {

    col_names <- recipes::recipes_eval_select(x$terms, data = training, info = info)

    recipes::check_type(training[, col_names], types = c("date", "datetime"))

    # Prep Information
    proxy_signature <- tk_get_holiday_signature(
        lubridate::ymd("2016-01-01"),
        holiday_pattern = x$holiday_pattern,
        locale_set      = x$locale_set,
        exchange_set    = x$exchange_set
    ) %>%
        dplyr::select(-index)

    feature_names <- colnames(proxy_signature)

    step_holiday_signature_new(
        terms           = x$terms,
        role            = x$role,
        trained         = TRUE,

        columns         = col_names,
        features        = feature_names,

        skip            = x$skip,
        id              = x$id,
        holiday_pattern = x$holiday_pattern,
        locale_set      = x$locale_set,
        exchange_set    = x$exchange_set
    )
}




#' @export
bake.step_holiday_signature <- function(object, new_data, ...) {

    # feature length - subtract index and diff columns (2 columns)
    feat_len <- length(object$features)

    # Build empty placeholder tibble to house the new features
    new_cols <- rep(feat_len, each = length(object$columns))

    date_values <- matrix(NA, nrow = nrow(new_data), ncol = sum(new_cols))

    # Dummy column names to avoid tibble warning
    colnames(date_values) <- as.character(seq_len(sum(new_cols)))
    date_values           <- tibble::as_tibble(date_values)
    new_names             <- vector("character", length = ncol(date_values))

    # Loop through each date column adding features, filling the placeholder tibble
    strt <- 1
    for (i in seq_along(object$columns)) {

        cols <- (strt):(strt + new_cols[i] - 1)

        tmp <- getElement(new_data, object$columns[i]) %>%
            tk_get_holiday_signature(
                holiday_pattern = object$holiday_pattern,
                locale_set      = object$locale_set,
                exchange_set    = object$exchange_set
            ) %>%
            dplyr::select(-index)

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
print.step_holiday_signature <-
    function(x, width = max(20, options()$width - 29), ...) {
        title <- "Holiday signature features from "
        recipes::print_step(x$columns, x$terms, x$trained, width = width, title = title)
        invisible(x)
    }

#' @rdname step_holiday_signature
#' @param x A `step_holiday_signature` object.
#' @export
tidy.step_holiday_signature <- function(x, ...) {

    if (recipes::is_trained(x)) {
        res <- expand.grid(
            terms = x$columns,
            value = x$features
        )
    } else {
        term_names <- recipes::sel2char(x$terms)
        res_1 <- expand.grid(
            terms = term_names,
            param = c("holiday_pattern", "locale_set", "exchange_set"),
            stringsAsFactors = FALSE
        )

        res_2 <- tibble::tibble(
            param = c("holiday_pattern", "locale_set", "exchange_set"),
            value = c(x$holiday_pattern, x$locale_set, x$exchange_set)
        )

        res <- dplyr::left_join(res_1, res_2, by = "param")
    }
    res$id <- x$id
    tibble::as_tibble(res)
}

#' @rdname required_pkgs.timetk
#' @export
required_pkgs.step_holiday_signature <- function(x, ...) {
    c("timetk")
}
