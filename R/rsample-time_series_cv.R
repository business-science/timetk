#' Time Series Cross Validation
#'
#' Create `rsample` cross validation sets for time series.
#' This function produces a sampling plan starting with the most recent
#' time series observations, rolling backwards. The sampling procedure
#' is similar to `rsample::rolling_origin()`, but places the focus
#' of the cross validation on the most recent time series data.
#'
#' @inheritParams rsample::rolling_origin
#' @param lag A value to include an lag between the assessment
#'  and analysis set. This is useful if lagged predictors will be used
#'  during training and testing.
#' @param slice_limit The number of slices to return. Set to `dplyr::n()`,
#'  which returns the maximum number of slices.
#'
#' @details
#'
#' __Intial (Training Set) and Assess (Testing Set)__
#'
#' The main options, `initial` and `assess`, control the number of
#'  data points from the original data that are in the analysis (training set)
#'  and the assessment (testing set), respectively.
#'
#' __Cumulative vs Sliding Window__
#'
#' When `cumulative = TRUE`, the `initial` parameter is ignored and the
#' analysis (training) set will grow as
#'  resampling continues while the assessment (testing) set size will always remain
#'  static.
#'
#' When `cumulative = FALSE`, the `initial` parameter fixes the analysis (training)
#' set and resampling occurs over a fixed window.
#'
#' __Skip__
#'
#' `skip` enables the function to not use every data point in the resamples.
#'  When `skip = 1`, the resampling data sets will increment by one position.
#'
#'  Example: Suppose that the rows of a data set are consecutive days. Using `skip = 7`
#'  will make the analysis data set operate on *weeks* instead of days. The
#'  assessment set size is not affected by this option.
#'
#' __Lag__
#'
#' The Lag parameter creates an overlap between the Testing set. This is needed
#' when lagged predictors are used.
#'
#' __Slice Limit__
#'
#' This controls the number of slices. If `slice_limit = 5`, only the most recent
#' 5 slices will be returned.
#'
#'
#'
#' @return An tibble with classes `time_series_cv`, `rset`, `tbl_df`, `tbl`,
#'  and `data.frame`. The results include a column for the data split objects
#'  and a column called `id` that has a character string with the resample
#'  identifier.
#'
#' @seealso
#' - [time_series_cv()] and [rsample::rolling_origin()] - Functions used to create
#'   time series resample specfications.
#' - [plot_time_series_cv_plan()] - The plotting function used for visualizing the
#'   time series resample plan.
#'
#' @examples
#' library(tidyverse)
#' library(timetk)
#'
#' # DATA ----
#' m750 <- m4_monthly %>% filter(id == "M750")
#'
#' m750 %>% plot_time_series(date, value)
#'
#' # RESAMPLE SPEC ----
#' resample_spec <- time_series_cv(data = m750,
#'                                 initial     = 12 * 6,
#'                                 assess      = 12 * 2,
#'                                 skip        = 12 * 2,
#'                                 cumulative  = FALSE,
#'                                 slice_limit = 2)
#'
#' resample_spec
#'
#' # VISUALIZE CV PLAN ----
#'
#' # Select date and value columns from the tscv diagnostic tool
#' resample_spec %>% tk_time_series_cv_plan()
#'
#' # Plot the date and value columns to see the CV Plan
#' resample_spec %>% plot_time_series_cv_plan(date, value, .interactive = FALSE)
#'
#' @export
#' @importFrom dplyr n
time_series_cv <- function(data, initial = 5, assess = 1,
                           cumulative = TRUE, skip = 1, lag = 0,
                           slice_limit = n(), ...) {
    n <- nrow(data)

    if (n < initial + assess) {
        stop("There should be at least ",
             initial + assess,
             " nrows in `data`",
             call. = FALSE)
    }

    if (skip < 1) {
        rlang::abort("skip cannot be less than 1.")
    }

    if (!is.numeric(lag) | !(lag%%1==0)) {
        stop("`lag` must be a whole number.", call. = FALSE)
    }

    if (lag > initial) {
        stop("`lag` must be less than or equal to the number of training observations.", call. = FALSE)
    }

    # --- IMPLEMENT REVERSED ROLLING ORIGIN ----

    # Update assess to account for lag (added to backend of assess)
    # stops <- n - seq(initial, (n - assess), by = skip + 1)
    stops <- n - seq(assess, (n - initial), by = skip)

    # Adjust starts for cumulative vs sliding period
    if (!cumulative) {
        starts <- stops - initial + 1
    } else {
        starts <- rep(1, length(stops))
    }

    starts_stops_tbl <- tibble::tibble(
        starts = starts,
        stops  = stops
    ) %>%
        dplyr::filter(starts > 0) %>%
        dplyr::slice(1:slice_limit)

    starts <- starts_stops_tbl$starts
    stops  <- starts_stops_tbl$stops

    # --- END REVERSE ----

    in_ind <- mapply(seq, starts, stops, SIMPLIFY = FALSE)
    out_ind <-
        mapply(seq, stops + 1 - lag, stops + assess, SIMPLIFY = FALSE)
    indices <- mapply(merge_lists, in_ind, out_ind, SIMPLIFY = FALSE)
    split_objs <-
        purrr::map(indices, make_splits, data = data, class = "ts_cv_split")
    split_objs <- list(splits = split_objs,
                       id = names0(length(split_objs), "Slice"))

    roll_att <- list(
        initial     = initial,
        assess      = assess,
        cumulative  = cumulative,
        skip        = skip,
        lag         = lag,
        slice_limit = slice_limit
    )

    new_rset(
        splits   = split_objs$splits,
        ids      = split_objs$id,
        attrib   = roll_att,
        subclass = c("time_series_cv", "rset")
    )
}

#' @export
print.time_series_cv <- function(x, ...) {
    cat("# Time Series Cross Validation Plan", "\n")
    # Drop classes: time_series_cv and rset
    class(x) <- class(x)[!(class(x) %in% c("time_series_cv", "rset"))]
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
    either_type <- function(x) {
        is.character(x) | is.factor(x)
    }

    ch_check <- vapply(ids, either_type, c(logical = TRUE))
    if(!all(ch_check)) {
        stop("All ID columns should be character or factor ",
             "vectors.", call. = FALSE)
    }

    if (!tibble::is_tibble(splits)) {
        splits <- tibble::tibble(splits = splits)
    } else {
        if(ncol(splits) > 1 | names(splits)[1] != "splits")
            stop("The `splits` tibble should have a single column ",
                 "named `splits`.", call. = FALSE)
    }

    if (nrow(ids) != nrow(splits)) {
        stop("Split and ID vectors have different lengths.",
             call. = FALSE)
    }


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

    if (length(subclass) > 0) {
        res <- add_class(res, cls = subclass, at_end = FALSE)
    }

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



