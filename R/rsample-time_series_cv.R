# TSCV ----

#' Time Series Cross Validation
#'
#' Create `rsample` cross validation sets for time series.
#' This function produces a sampling plan starting with the most recent
#' time series observations, rolling backwards. The sampling procedure
#' is similar to `rsample::rolling_origin()`, but places the focus
#' of the cross validation on the most recent time series data.
#'
#' @inheritParams rsample::rolling_origin
#' @param date_var A date or date-time variable.
#' @param lag A value to include an lag between the assessment
#'  and analysis set. This is useful if lagged predictors will be used
#'  during training and testing.
#' @param slice_limit The number of slices to return. Set to `dplyr::n()`,
#'  which returns the maximum number of slices.
#' @param point_forecast Whether or not to have the testing set be a single point forecast or to be a forecast horizon.
#'  The default is to be a forecast horizon. Default: `FALSE`
#'
#' @details
#'
#' __Time-Based Specification__
#'
#'  The `initial`, `assess`, `skip`, and `lag` variables can be specified as:
#'
#' - Numeric: `initial = 24`
#' - Time-Based Phrases: `initial = "2 years"`, if the `data` contains
#'  a `date_var` (date or datetime)
#'
#' __Initial (Training Set) and Assess (Testing Set)__
#'
#' The main options, `initial` and `assess`, control the number of
#'  data points from the original data that are in the analysis (training set)
#'  and the assessment (testing set), respectively.
#'
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
#' __Slice Limit__
#'
#' This controls the number of slices. If `slice_limit = 5`, only the most recent
#' 5 slices will be returned.
#'
#' __Point Forecast__
#'
#' A point forecast is sometimes desired such as if you want to forecast a value
#' "4-weeks" into the future. You can do this by setting the following parameters:
#'
#' - assess = "4 weeks"
#' - point_forecast = TRUE
#'
#' __Panel Data / Time Series Groups / Overlapping Timestamps__
#'
#' Overlapping timestamps occur when your data has more than one
#' time series group. This is sometimes called _Panel Data_ or _Time Series Groups_.
#'
#' When timestamps are duplicated (as in the case of "Panel Data" or "Time Series Groups"),
#' the resample calculation applies a sliding window of
#' fixed length to the dataset. See the example using `walmart_sales_weekly`
#' dataset below.
#'
#'
#' @return An tibble with classes `time_series_cv`, `rset`, `tbl_df`, `tbl`,
#'  and `data.frame`. The results include a column for the data split objects
#'  and a column called `id` that has a character string with the resample
#'  identifier.
#'
#' @seealso
#' - [time_series_cv()] and [rsample::rolling_origin()] - Functions used to create
#'   time series resample specifications.
#' - [plot_time_series_cv_plan()] - The plotting function used for visualizing the
#'   time series resample plan.
#' - [time_series_split()] - A convenience function to return a single time series
#'   split containing a training/testing sample.
#'
#' @examples
#' library(dplyr)
#' library(timetk)
#'
#' # DATA ----
#' m750 <- m4_monthly %>% dplyr::filter(id == "M750")
#'
#'
#' # RESAMPLE SPEC ----
#' resample_spec <- time_series_cv(data = m750,
#'                                 initial     = "6 years",
#'                                 assess      = "24 months",
#'                                 skip        = "24 months",
#'                                 cumulative  = FALSE,
#'                                 slice_limit = 3)
#'
#' resample_spec
#'
#' # VISUALIZE CV PLAN ----
#'
#' # Select date and value columns from the tscv diagnostic tool
#' resample_spec %>% tk_time_series_cv_plan()
#'
#' # Plot the date and value columns to see the CV Plan
#' resample_spec %>%
#'     plot_time_series_cv_plan(date, value, .interactive = FALSE)
#'
#'
#' # PANEL DATA / TIME SERIES GROUPS ----
#' # - Time Series Groups are processed using an *ungrouped* data set
#' # - The data has sliding windows applied starting with the beginning of the series
#' # - The seven groups of weekly time series are
#' #   processed together for <split [358/78]> dimensions
#'
#' walmart_tscv <- walmart_sales_weekly %>%
#'     time_series_cv(
#'         date_var    = Date,
#'         initial     = "12 months",
#'         assess      = "3 months",
#'         skip        = "3 months",
#'         slice_limit = 4
#'     )
#'
#' walmart_tscv
#'
#' walmart_tscv %>%
#'     plot_time_series_cv_plan(Date, Weekly_Sales, .interactive = FALSE)
#'
#' @export
#' @importFrom dplyr n
time_series_cv <- function(data, date_var = NULL, initial = 5, assess = 1,
                           skip = 1, lag = 0, cumulative = FALSE,
                           slice_limit = n(), point_forecast = FALSE, ...) {

    if (!inherits(data, "data.frame")) rlang::abort("'data' must be an object of class `data.frame`.")
    if (inherits(data, "grouped_df")) message("Groups detected. Removing groups.")

    date_var_expr <- rlang::enquo(date_var)

    # Check date_var
    if (rlang::quo_is_null(date_var_expr)) {
        date_var_text <- tk_get_timeseries_variables(data)[1]
        message("Using date_var: ", date_var_text)
        date_var_expr <- rlang::sym(date_var_text)
    }

    # Make sure arranged by date variable
    original_date_vec <- data %>% dplyr::pull(!! date_var_expr)
    data <- data %>%
        dplyr::ungroup() %>%
        dplyr::arrange(!! date_var_expr)
    sorted_date_vec <- data %>% dplyr::pull(!! date_var_expr)

    if (!identical(original_date_vec, sorted_date_vec)) message(stringr::str_glue("Data is not ordered by the 'date_var'. Resamples will be arranged by `{rlang::as_label(date_var_expr)}`."))

    # 1.0 HANDLE DUPLICATED TIMESTAMPS ----
    # - Index table finds unique timestamps in data, creating a lookup table matching
    #   the row ID to the unique timestamp.
    #   This is important in instances with duplicated time stamps
    lookup_table <- data %>%
        tibble::rowid_to_column(".rowid")

    # print(lookup_table)

    index_table_min <- lookup_table %>%
        dplyr::select(.rowid, !! date_var_expr) %>%
        dplyr::group_by(!! date_var_expr) %>%
        dplyr::slice_min(.rowid) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(idx = 1:dplyr::n())

    index_table_max <- lookup_table %>%
        dplyr::select(.rowid, !! date_var_expr) %>%
        dplyr::group_by(!! date_var_expr) %>%
        dplyr::slice_max(.rowid) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(idx = 1:dplyr::n())

    # print(index_table_max)

    timestamps_duplicated <- FALSE
    if (nrow(data) > nrow(index_table_min)) {
        message("Overlapping Timestamps Detected. Processing overlapping time series together using sliding windows.")
        timestamps_duplicated <- TRUE
    }

    # 2.0 CONVERT TIME-BASED PHRASES ----
    character_args <- c(
        is.character(initial),
        is.character(assess),
        is.character(skip),
        is.character(lag)
    )

    if (any(character_args)) {

        # initial
        if (character_args[1]) {
            initial <- period_chr_to_n(index_table_min, !! date_var_expr, period = initial)
        }

        # assess
        if (character_args[2]) {
            assess <- period_chr_to_n(index_table_min, !! date_var_expr, period = assess)
        }

        # skip
        if (character_args[3]) {
            skip <- period_chr_to_n(index_table_min, !! date_var_expr, period = skip)
        }

        # initial
        if (character_args[4]) {
            lag <- period_chr_to_n(index_table_min, !! date_var_expr, period = lag)
        }

    }


    # 3.0 CHECK INPUTS ----
    n <- nrow(index_table_min)

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

    # 4.0 REVERSE ROLLING ORIGIN ----

    # Update assess to account for lag (added to backend of assess)
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

    # 5.0 MAP STARTS/STOPS TO ROW IDs ----
    # - Should only be required when duplicated timestamps
    get_row_ids <- function(idx, type = "min") {

        if (type == "min") {
            index_table <- index_table_min
        } else {
            index_table <- index_table_max
        }

        tibble::tibble(idx = idx) %>%
            dplyr::left_join(index_table, by = "idx") %>%
            dplyr::pull(.rowid)
    }

    starts_conv       <- get_row_ids(starts, type = "min")
    stops_conv        <- get_row_ids(stops, type = "max")
    stops_lag_conv    <- get_row_ids(stops + 1 - lag, type = "min")
    stops_assess_conv <- get_row_ids(stops + assess, type = "max")



    # 6.0 SELECT INDICIES -----
    in_ind  <- mapply(seq, starts_conv, stops_conv, SIMPLIFY = FALSE)

    if (!point_forecast) {
        out_ind <- mapply(seq, stops_lag_conv, stops_assess_conv, SIMPLIFY = FALSE)
    } else {
        stops_assess_conv_min <- get_row_ids(stops + assess, type = "min")
        out_ind <- mapply(seq, stops_assess_conv_min, stops_assess_conv, SIMPLIFY = FALSE)
    }


    # message("in_ind")
    # print(in_ind)
    # message("out_ind")
    # print(out_ind)

    # 7.0 MAKE SPLIT OBJECTS ----
    indices    <- mapply(merge_lists, in_ind, out_ind, SIMPLIFY = FALSE)
    split_objs <- purrr::map(indices, .f = rsample::make_splits, data = data, class = "ts_cv_split")
    split_objs <- list(
        splits = split_objs,
        id     = recipes::names0(length(split_objs), prefix = "Slice")
    )

    # 8.0 Create New Rset ----
    ret <- rsample::new_rset(
        splits   = split_objs$splits,
        ids      = split_objs$id,
        attrib   = list(
            initial     = initial,
            assess      = assess,
            cumulative  = cumulative,
            skip        = skip,
            lag         = lag,
            slice_limit = slice_limit
        ),
        subclass = c("time_series_cv", "rset")
    )

    return(ret)
}

#' @export
print.time_series_cv <- function(x, ...) {
    cat("# Time Series Cross Validation Plan", "\n")
    # Drop classes: time_series_cv and rset
    class(x) <- class(x)[!(class(x) %in% c("time_series_cv", "rset"))]
    print(x, ...)
}

merge_lists <- function(a, b) {
    list(analysis = a, assessment = b)
}

# make_splits <- function(ind, data, class = NULL) {
#     res <- rsplit(data, ind$analysis,  ind$assessment)
#     if (!is.null(class))
#         res <- add_class(res, class)
#     res
# }

# new_rset <-  function(splits, ids, attrib = NULL,
#                       subclass = character()) {
#     stopifnot(is.list(splits))
#     if (!tibble::is_tibble(ids)) {
#         ids <- tibble::tibble(id = ids)
#     } else {
#         if (!all(grepl("^id", names(ids))))
#             stop("The `ids` tibble column names should start with 'id'",
#                  call. = FALSE)
#     }
#     either_type <- function(x) {
#         is.character(x) | is.factor(x)
#     }
#
#     ch_check <- vapply(ids, either_type, c(logical = TRUE))
#     if(!all(ch_check)) {
#         stop("All ID columns should be character or factor ",
#              "vectors.", call. = FALSE)
#     }
#
#     if (!tibble::is_tibble(splits)) {
#         splits <- tibble::tibble(splits = splits)
#     } else {
#         if(ncol(splits) > 1 | names(splits)[1] != "splits")
#             stop("The `splits` tibble should have a single column ",
#                  "named `splits`.", call. = FALSE)
#     }
#
#     if (nrow(ids) != nrow(splits)) {
#         stop("Split and ID vectors have different lengths.",
#              call. = FALSE)
#     }
#
#
#     # Create another element to the splits that is a tibble containing
#     # an identifer for each id column so that, in isolation, the resample
#     # id can be known just based on the `rsplit` object. This can then be
#     # accessed using the `labels` methof for `rsplits`
#
#     splits$splits <- purrr::map2(splits$splits, split(ids, 1:nrow(ids)), add_id)
#
#     res <- dplyr::bind_cols(splits, ids)
#
#     if (!is.null(attrib)) {
#         if (any(names(attrib) == ""))
#             stop("`attrib` should be a fully named list.",
#                  call. = FALSE)
#         for (i in names(attrib))
#             attr(res, i) <- attrib[[i]]
#     }
#
#     if (length(subclass) > 0) {
#         res <- add_class(res, cls = subclass, at_end = FALSE)
#     }
#
#     res
# }

# rsplit <- function(data, in_id, out_id) {
#     if (!is.data.frame(data) & !is.matrix(data))
#         stop("`data` must be a data frame.", call. = FALSE)
#
#     if (!is.integer(in_id) | any(in_id < 1))
#         stop("`in_id` must be a positive integer vector.", call. = FALSE)
#
#     if(!all(is.na(out_id))) {
#         if (!is.integer(out_id) | any(out_id < 1))
#             stop("`out_id` must be a positive integer vector.", call. = FALSE)
#     }
#
#     if (length(in_id) == 0)
#         stop("At least one row should be selected for the analysis set.",
#              call. = FALSE)
#
#     structure(
#         list(
#             data = data,
#             in_id = in_id,
#             out_id = out_id
#         ),
#         class = "rsplit"
#     )
# }
#
# add_class <- function(x, cls, at_end = TRUE) {
#     class(x) <- if (at_end)
#         c(class(x), cls)
#     else
#         c(cls, class(x))
#     x
# }
#
# add_id <- function(split, id) {
#     split$id <- id
#     split
# }


period_chr_to_n <- function(data, date_var, period) {
    idx <- data %>% dplyr::pull(!! rlang::enquo(date_var))
    end <- idx[1] %+time% period
    row_count <- data %>%
        filter_by_time(.date_var = !! rlang::enquo(date_var), .start_date = "start", .end_date = end) %>%
        nrow()

    row_count - 1
}
