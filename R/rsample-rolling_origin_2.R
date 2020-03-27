#' Rolling Origin Forecast Resampling
#'
#' This resampling method is useful when the data set has a strong time
#'  component. The resamples contain data points that are
#'  _consecutive values_. This version differs from `rsample::initial_time_split()` in
#'  that it has an `overlap` parameter which is useful for prediction with
#'  lagged predictors.
#'
#' @details
#' The main options, `initial` and `assess`, control the number of
#'  data points from the original data that are in the analysis and assessment
#'  set, respectively. When `cumulative = TRUE`, the analysis set will grow as
#'  resampling continues while the assessment set size will always remain
#'  static.
#'
#' `skip` enables the function to not use every data point in the resamples.
#'  When `skip = 0`, the resampling data sets will increment by one position.
#'  Suppose that the rows of a data set are consecutive days. Using `skip = 6`
#'  will make the analysis data set operate on *weeks* instead of days. The
#'  assessment set size is not affected by this option.
#'
#'  `overlap` enables the test data to overlap with the training data, which
#'  is useful for predictions needing access to prior history such as when
#'  using lagged predictors.
#'
#' @inheritParams rsample::vfold_cv
#' @param initial The number of samples used for analysis/modeling in the
#'  initial resample.
#' @param assess The number of samples used for each assessment resample.
#' @param cumulative A logical. Should the analysis resample grow beyond the
#'  size specified by `initial` at each resample?.
#' @param skip A integer indicating how many (if any) _additional_ resamples
#'  to skip to thin the total amount of data points in the analysis resample.
#' See the example below.
#' @param overlap A value to include an overlap between the assessment
#'  and analysis set. This is useful if lagged predictors will be used
#'  during training and testing.
#'
#' @return An tibble with classes `rolling_origin`, `rset`, `tbl_df`, `tbl`,
#'  and `data.frame`. The results include a column for the data split objects
#'  and a column called `id` that has a character string with the resample
#'  identifier.
#'
#' @examples
#' library(recipes)
#' library(rsample)
#' library(timetk)
#' library(tidyverse)
#'
#' # 5-years of monthly data
#' drinks_tbl <- drinks %>%
#'     rename(sales = S4248SM144NCEN) %>%
#'     as_tibble()
#'
#' # Resample with a 12-month overlap
#' # - Allows use of a 12-month or longer lag
#' resample_spec <- rolling_origin_2(
#'     drinks_tbl,
#'     initial    = 4 * 12,
#'     assess     = 12,
#'     cumulative = FALSE,
#'     skip       = 2 * 12,
#'     overlap    = 12
#' )
#'
#' resample_spec
#'
#' # Create a lag recipe using one of the training resamples
#' resample_1 <- resample_spec %>% pluck(1, 1)
#'
#' recipe_lag <- recipe(sales ~ ., data = training(resample_1)) %>%
#'     step_lag(sales, lag = 12) %>%
#'     step_naomit(all_predictors())
#'
#' # Apply a lag recipe to one of the testing resamples to show the effect
#' bake(prep(recipe_lag), testing(resample_1))
#'
#'
#' @export
rolling_origin_2 <- function(data, initial = 5, assess = 1,
                             cumulative = TRUE, skip = 0, overlap = 0, ...) {
    n <- nrow(data)

    if (n < initial + assess + overlap)
        stop("There should be at least ",
             initial + assess + overlap,
             " nrows in `data`",
             call. = FALSE)

    # Update assess to account for overlap (added to backend of assess)
    stops <- seq(initial, (n - assess), by = skip + 1)

    # Adjust starts for cumulative vs sliding period
    if (!cumulative) {
        starts <- stops - initial + 1
    } else {
        starts <- rep(1, length(stops))
    }

    # Add overlap for predictions with lags
    # starts_test <- stops - overlap

    in_ind  <- mapply(seq, starts, stops, SIMPLIFY = FALSE)
    out_ind <- mapply(seq, stops + 1 - overlap, stops + assess, SIMPLIFY = FALSE)

    indices <- mapply(merge_lists, in_ind, out_ind, SIMPLIFY = FALSE)

    split_objs <- purrr::map(indices, make_splits, data = data, class = "rof_split")
    split_objs <- list(splits = split_objs,
                       id = names0(length(split_objs), "Slice"))

    roll_att <- list(initial    = initial,
                     assess     = assess,
                     overlap    = overlap,
                     cumulative = cumulative,
                     skip       = skip)

    new_rset(splits = split_objs$splits,
             ids = split_objs$id,
             attrib = roll_att,
             subclass = c("rolling_origin", "rset"))
}

#' @export
print.rolling_origin <- function(x, ...) {
    cat("#", pretty(x), "\n")
    class(x) <- class(x)[!(class(x) %in% c("rolling_origin", "rset"))]
    print(x, ...)
}

merge_lists <- function(a, b) list(analysis = a, assessment = b)

make_splits <- function(ind, data, class = NULL) {
    res <- rsplit(data, ind$analysis,  ind$assessment)
    if (!is.null(class))
        res <- add_class(res, class)
    res
}

new_rset <-  function(splits, ids, attrib = NULL,
                      subclass = character()) {
    stopifnot(is.list(splits))
    if (!tibble::is_tibble(ids)) {
        ids <- tibble::tibble(id = ids)
    } else {
        if (!all(grepl("^id", names(ids))))
            stop("The `ids` tibble column names should start with 'id'",
                 call. = FALSE)
    }
    either_type <- function(x)
        is.character(x) | is.factor(x)
    ch_check <- vapply(ids, either_type, c(logical = TRUE))
    if(!all(ch_check))
        stop("All ID columns should be character or factor ",
             "vectors.", call. = FALSE)

    if (!tibble::is_tibble(splits)) {
        splits <- tibble::tibble(splits = splits)
    } else {
        if(ncol(splits) > 1 | names(splits)[1] != "splits")
            stop("The `splits` tibble should have a single column ",
                 "named `splits`.", call. = FALSE)
    }

    if (nrow(ids) != nrow(splits))
        stop("Split and ID vectors have different lengths.",
             call. = FALSE)

    # Create another element to the splits that is a tibble containing
    # an identifer for each id column so that, in isolation, the resample
    # id can be known just based on the `rsplit` object. This can then be
    # accessed using the `labels` methof for `rsplits`

    splits$splits <- purrr::map2(splits$splits, split(ids, 1:nrow(ids)), add_id)

    res <- dplyr::bind_cols(splits, ids)

    if (!is.null(attrib)) {
        if (any(names(attrib) == ""))
            stop("`attrib` should be a fully named list.",
                 call. = FALSE)
        for (i in names(attrib))
            attr(res, i) <- attrib[[i]]
    }

    if (length(subclass) > 0)
        res <- add_class(res, cls = subclass, at_end = FALSE)

    res
}

rsplit <- function(data, in_id, out_id) {
    if (!is.data.frame(data) & !is.matrix(data))
        stop("`data` must be a data frame.", call. = FALSE)

    if (!is.integer(in_id) | any(in_id < 1))
        stop("`in_id` must be a positive integer vector.", call. = FALSE)

    if(!all(is.na(out_id))) {
        if (!is.integer(out_id) | any(out_id < 1))
            stop("`out_id` must be a positive integer vector.", call. = FALSE)
    }

    if (length(in_id) == 0)
        stop("At least one row should be selected for the analysis set.",
             call. = FALSE)

    structure(
        list(
            data = data,
            in_id = in_id,
            out_id = out_id
        ),
        class = "rsplit"
    )
}

add_class <- function(x, cls, at_end = TRUE) {
    class(x) <- if (at_end)
        c(class(x), cls)
    else
        c(cls, class(x))
    x
}

add_id <- function(split, id) {
    split$id <- id
    split
}
