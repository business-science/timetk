#' Create a rolling version of any function
#'
#' `rollify` returns a rolling version of the input function, with a
#' rolling `.period` specified by the user.
#'
#' @inheritParams purrr::as_mapper
#' @param .period The period size to roll over
#' @param .align One of "center", "left" or "right.
#' @param .unlist If the function returns a single value each time it is called,
#' use `.unlist = TRUE`. If the function returns more than one value, or a more
#' complicated object (like a linear model), use `.unlist = FALSE` to create
#' a list-column of the rolling results.
#' @param .na_value A default value for the `NA` values at the beginning of the
#' roll.
#'
#' @details
#'
#'
#' __Make a Rolling Function__
#'
#' `rollify()` turns a function into a rolling version
#' of itself for use inside of a call to [dplyr::mutate()], however it works
#' equally as well when called from [purrr::map()].
#'
#' Because of it's intended use with [dplyr::mutate()], `rollify`
#' creates a function that always returns output with the same length of the
#' input
#'
#' __Alignment__
#'
#' Rolling functions generate `.period - 1` fewer values than the incoming vector.
#' Thus, the vector needs to be aligned. Alignment of the vector follows 3 types:
#'
#'  - __center (default):__ `NA` or `.partial` values are divided and added to the beginning and
#'    end of the series to "Center" the moving average. This is common in Time Series applications (e.g. denoising).
#'  - __left:__ `NA` or `.partial` values are added to the end to shift the series to the Left.
#'  - __right:__ `NA` or `.partial` values are added to the beginning to shift the series to the Right. This is common in
#'    Financial Applications (e.g moving average cross-overs).
#'
#' __Functional Form (.f)__
#'
#' The form of the `.f` argument is the same as the form that can be passed
#' to [purrr::map()]. Use `.x` or `.` to refer to the first object to roll over,
#' and `.y` to refer to the second object if required. The examples explain this
#' further.
#'
#' If optional arguments to the function are required, specify them in the
#' call to `rollify`, and not in the call to the rolling version of the
#' function. See the examples for more details.
#'
#'
#' @examples
#' library(tidyverse)
#' library(tidyquant)
#' library(timetk)
#'
#' FB <- FANG %>% filter(symbol == "FB")
#'
#'
#' # Rolling mean --------------------------------------------------------------
#'
#' # Turn the normal mean function into a rolling mean with a 5 row .period
#' mean_roll_5 <- rollify(mean, .period = 5, .align = "right")
#'
#' FB %>%
#'     mutate(rolling_mean_5 = mean_roll_5(adjusted))
#'
#' # There's nothing stopping you from combining multiple rolling functions with
#' # different .period sizes in the same mutate call
#' mean_roll_10 <- rollify(mean, .period = 10, .align = "right")
#'
#' FB %>%
#'     mutate(
#'         rolling_mean_5  = mean_roll_5(adjusted),
#'         rolling_mean_10 = mean_roll_10(adjusted)
#'     )
#'
#' # Functions with multiple args and optional args ----------------------------
#'
#' # With 2 args, use the purrr syntax of ~ and .x, .y
#' # Rolling correlation example
#' cor_roll <- rollify(~cor(.x, .y), .period = 5, .align = "right")
#'
#' FB %>%
#'     mutate(running_cor = cor_roll(adjusted, open))
#'
#' # With >2 args, create an anonymous function with >2 args or use
#' # the purrr convention of ..1, ..2, ..3 to refer to the arguments
#' avg_of_avgs <- rollify(
#'     function(x, y, z) {
#'         (mean(x) + mean(y) + mean(z)) / 3
#'     },
#'     .period = 10,
#'     .align = "right"
#' )
#'
#' # Or
#' avg_of_avgs <- rollify(~(mean(..1) + mean(..2) + mean(..3)) / 3, .period = 10, .align = "right")
#'
#' FB %>%
#'     mutate(avg_of_avgs = avg_of_avgs(open, high, low))
#'
#' # Optional arguments MUST be passed at the creation of the rolling function
#' # Only data arguments that are "rolled over" are allowed when calling the
#' # rolling version of the function
#' FB$adjusted[1] <- NA
#'
#' roll_mean_na_rm <- rollify(~mean(.x, na.rm = TRUE), .period = 5, .align = "right")
#'
#' FB %>%
#'     mutate(roll_mean = roll_mean_na_rm(adjusted))
#'
#'
#' # Returning multiple values -------------------------------------------------
#'
#' FB <- FANG %>% filter(symbol == "FB")
#'
#' # If the function returns >1 value, set the `.unlist = FALSE` argument
#' # Running 5 number summary
#' summary_roll <- rollify(summary, .period = 5, .unlist = FALSE, .align = "right")
#'
#' FB_summarised <- FB %>% mutate(summary_roll = summary_roll(adjusted))
#' FB_summarised$summary_roll[[5]]
#'
#' # dplyr::bind_rows() is often helpful in these cases to get
#' # meaningful output
#'
#' summary_roll <- rollify(~ bind_rows(summary(.)),
#'                         .period = 5, .unlist = FALSE, .align = "right")
#'
#' FB %>%
#'     mutate(summary_roll = summary_roll(adjusted)) %>%
#'     filter(!is.na(summary_roll)) %>%
#'     unnest(summary_roll)
#'
#' # Rolling regressions -------------------------------------------------------
#'
#' # Rolling regressions are easy to implement
#' lm_roll <- rollify(~lm(.x ~ .y), .period = 90, .unlist = FALSE, .align = "right")
#'
#' FB %>%
#'     mutate(numeric_date = as.numeric(date)) %>%
#'     mutate(rolling_lm = lm_roll(adjusted, numeric_date)) %>%
#'     filter(!is.na(rolling_lm))
#'
#' # Rolling with groups -------------------------------------------------------
#'
#' # One of the most powerful things about this is that it works with
#' # groups since `mutate` is being used
#' data(FANG)
#'
#' mean_roll_3 <- rollify(mean, .period = 3, .align = "right")
#'
#' FANG %>%
#'     group_by(symbol) %>%
#'     mutate(mean_roll = mean_roll_3(adjusted)) %>%
#'     slice(1:5)
#'
#' @seealso [purrr::safely], [purrr::possibly]
#'
#' @export
#'
rollify <- function(.f, .period = 1,
                    .align = c("center", "left", "right"),
                    .partial = FALSE,
                    .unlist = TRUE) {

    # Checks
    .align <- tolower(.align[1])

    # Mappify the function
    .f <- purrr::as_mapper(.f)

    # Return function that calls slide
    function(...) {
        # roller_1(..., .f = .f, .period = .period, .align = .align, .unlist = .unlist)

        roller_2(
            ...,
            .slider_fun = slider::pslide,
            .f          = .f,
            .period     = .period,
            .align      = .align,
            .partial    = .partial,
            .unlist     = .unlist
        )
    }
}


# Utils ------------------------------------------------------------------------

roller_2 <- function(..., .slider_fun, .f, .period, .align, .partial, .unlist) {

    # Capture dots as list. These should be the arguments that are rolled
    .dots <- rlang::dots_list(...)

    # Error check the dots
    check_dots(.dots, .period)

    ret_vec <- roll_to_slide(
        .slider_fun = .slider_fun,
        .l          = .dots,
        .f          = .f,
        .period     = .period,
        .align      = .align,
        .partial    = .partial
    )

    # Don't .unlist if requested (when >1 value returned)
    if(.unlist) {
        unlist(ret_vec)
    } else {
        ret_vec
    }

}


roller_1 <- function(..., .f, .period, .align, .unlist = TRUE) {

    # .na_value always NA
    .na_value = NA

    # Capture dots as list. These should be the arguments that are rolled
    .dots <- rlang::dots_list(...)

    # Error check the dots
    check_dots(.dots, .period)

    # Each data element of .dots should be of the same length so use the first
    # as the length of the dataset
    roll_length <- length(.dots[[1]])

    # Initialize `filled` vector
    filled <- rlang::rep_along(1:roll_length, list(.na_value))

    # Roll and fill
    for(i in .period:roll_length) {
        .f_dots   <- lapply(.dots, function(x) {x[(i-.period+1):i]})
        filled[[i]] <- do.call(.f, .f_dots)
    }

    # Apply alignment
    filled_na_aligned <- apply_na_align(filled, .align[1])

    # Don't .unlist if requested (when >1 value returned)
    if(.unlist) {
        unlist(filled_na_aligned)
    } else {
        filled_na_aligned
    }

}

apply_na_align <- function(x, .align) {

    na_len <- sum(is.na(x))
    x_trim <- x[!is.na(x)]

    # Calculate padding
    .align <- .align[1]
    if (.align == "center") {
        split_period <- na_len / 2
        na_before <- rep(NA, floor(split_period))
        na_after  <- rep(NA, ceiling(split_period))
    } else if (.align == "left") {
        na_before <- c()
        na_after  <- rep(NA, na_len)
    } else {
        na_before <- rep(NA, na_len)
        na_after  <- c()
    }

    c(na_before, x_trim, na_after)
}

# Check that dots follow the necessary convention for rolling
check_dots <- function(x, .period) {

    # The user must have passed something to be passed on to .f
    assertthat::assert_that(length(x) > 0,
                            msg = "At least 1 data argument must be supplied to be
                          passed on to the rolling function")


    # The .period must be smaller than the length of the data
    assertthat::assert_that(.period <= length(x[[1]]),
                            msg = "Cannot roll apply with a .period larger than the
                          length of the data")


    # Length of every element of .dots should be the same
    # Only data used in the rolling should be in .dots
    # Optional args should be specified in the rollify call
    for(i in 1:length(x)) {
        assertthat::assert_that(length(x[[i]]) == length(x[[1]]),
                                msg = "Arguments supplied to the rolling version
                            of the function should be data of the same length.
                            Optional arguments should be specified when creating
                            the rolling version with `rollify()`")
    }
}

