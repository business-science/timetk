#' Create a rolling (sliding) version of any function
#'
#' `slidify` returns a rolling (sliding) version of the input function, with a
#' rolling (sliding) `.period` specified by the user.
#'
#' @inheritParams purrr::as_mapper
#' @param .period The period size to roll over
#' @param .align One of "center", "left" or "right".
#' @param .partial Should the moving window be allowed to return partial (incomplete) windows
#' instead of `NA` values. Set to FALSE by default, but can be switched to TRUE to remove `NA`'s.
#' @param .unlist If the function returns a single value each time it is called,
#' use `.unlist = TRUE`. If the function returns more than one value, or a more
#' complicated object (like a linear model), use `.unlist = FALSE` to create
#' a list-column of the rolling results.
#'
#' @details
#' The `slidify()` function is almost identical to `tibbletime::rollify()`
#' with 3 improvements:
#'
#' 1. Alignment ("center", "left", "right")
#' 2. Partial windows are allowed
#' 3. Uses `slider` under the hood, which improves speed and reliability by implementing
#'  code at C++ level
#'
#' __Make any function a Sliding (Rolling) Function__
#'
#' `slidify()` turns a function into a sliding version
#' of itself for use inside of a call to [dplyr::mutate()], however it works
#' equally as well when called from [purrr::map()].
#'
#' Because of it's intended use with [dplyr::mutate()], `slidify`
#' creates a function that always returns output with the same length of the
#' input
#'
#' __Alignment__
#'
#' Rolling / Sliding functions generate `.period - 1` fewer values than the incoming vector.
#' Thus, the vector needs to be aligned. Alignment of the vector follows 3 types:
#'
#'  - __center (default):__ `NA` or `.partial` values are divided and added to the beginning and
#'    end of the series to "Center" the moving average. This is common in Time Series applications (e.g. denoising).
#'  - __left:__ `NA` or `.partial` values are added to the end to shift the series to the Left.
#'  - __right:__ `NA` or `.partial` values are added to the beginning to shift the series to the Right. This is common in
#'    Financial Applications (e.g moving average cross-overs).
#'
#' __Allowing Partial Windows__
#'
#' A key improvement over `tibbletime::slidify()` is that `timetk::slidify()` implements
#' `.partial` rolling windows. Just set `.partial = TRUE`.
#'
#' @references
#'
#' - The [Tibbletime R Package](https://business-science.github.io/tibbletime/index.html)
#'  by Davis Vaughan, which includes the original `slidify()`
#'  Function
#'
#' @seealso
#'
#' Transformation Functions:
#'
#' - [roll_apply_vec()] - A simple vectorized function for applying summary functions
#'  to rolling windows.
#'
#' Augmentation Functions (Add Rolling Multiple Columns):
#'
#' - [tk_augment_roll_apply()] - For easily adding multiple rolling windows to you data
#'
#' Slider R Package:
#'
#' - `slider::pslide()` - The workhorse function that powers `timetk::slidify()`
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
#' # --- ROLLING MEAN (SINGLE ARG EXAMPLE) ---
#'
#' # Turn the normal mean function into a rolling mean with a 5 row .period
#' mean_roll_5 <- slidify(mean, .period = 5, .align = "right")
#'
#' FB %>%
#'     mutate(rolling_mean_5 = mean_roll_5(adjusted))
#'
#' # Use `partial = TRUE` to allow partial windows (those with less than the full .period)
#' mean_roll_5_partial <- slidify(mean, .period = 5, .align = "right", .partial = TRUE)
#'
#' FB %>%
#'     mutate(rolling_mean_5 = mean_roll_5_partial(adjusted))
#'
#' # There's nothing stopping you from combining multiple rolling functions with
#' # different .period sizes in the same mutate call
#'
#' mean_roll_10 <- slidify(mean, .period = 10, .align = "right")
#'
#' FB %>%
#'     select(symbol, date, adjusted) %>%
#'     mutate(
#'         rolling_mean_5  = mean_roll_5(adjusted),
#'         rolling_mean_10 = mean_roll_10(adjusted)
#'     )
#'
#' # For summary operations like rolling means, we can accomplish large-scale
#' # multi-rolls with tk_augment_roll_apply()
#'
#' FB %>%
#'     select(symbol, date, adjusted) %>%
#'     tk_augment_roll_apply(
#'         adjusted, .period = 5:10, .f = mean, .align = "right",
#'         .names = str_c("MA_", 5:10)
#'     )
#'
#' # --- GROUPS AND ROLLING ----
#'
#' # One of the most powerful things about this is that it works with
#' # groups since `mutate` is being used
#' data(FANG)
#'
#' mean_roll_3 <- slidify(mean, .period = 3, .align = "right")
#'
#' FANG %>%
#'     group_by(symbol) %>%
#'     mutate(mean_roll = mean_roll_3(adjusted)) %>%
#'     slice(1:5)
#'
#'
#' # --- ROLLING CORRELATION (MULTIPLE ARG EXAMPLE) ---
#'
#' # With 2 args, use the purrr syntax of ~ and .x, .y
#' # Rolling correlation example
#' cor_roll <- slidify(~cor(.x, .y), .period = 5, .align = "right")
#'
#' FB %>%
#'     mutate(running_cor = cor_roll(adjusted, open))
#'
#' # With >2 args, create an anonymous function with >2 args or use
#' # the purrr convention of ..1, ..2, ..3 to refer to the arguments
#' avg_of_avgs <- slidify(
#'     function(x, y, z) (mean(x) + mean(y) + mean(z)) / 3,
#'     .period = 10,
#'     .align = "right"
#' )
#'
#' # Or
#' avg_of_avgs <- slidify(
#'     ~(mean(..1) + mean(..2) + mean(..3)) / 3,
#'     .period = 10,
#'     .align  = "right"
#' )
#'
#' FB %>%
#'     mutate(avg_of_avgs = avg_of_avgs(open, high, low))
#'
#' # Optional arguments MUST be passed at the creation of the rolling function
#' # Only data arguments that are "rolled over" are allowed when calling the
#' # rolling version of the function
#' FB$adjusted[1] <- NA
#'
#' roll_mean_na_rm <- slidify(~mean(.x, na.rm = TRUE), .period = 5, .align = "right")
#'
#' FB %>%
#'     mutate(roll_mean = roll_mean_na_rm(adjusted))
#'
#'
#' # --- RETURNING MULTIPLE VALUES (SUMMARY) ----
#'
#' FB <- FANG %>% filter(symbol == "FB")
#'
#' # If the function returns >1 value, set the `.unlist = FALSE` argument
#' # Running 5 number summary
#' summary_roll <- slidify(summary,
#'                         .period = 5, .unlist = FALSE, .align = "right")
#'
#' FB_summarised <- FB %>% mutate(summary_roll = summary_roll(adjusted))
#'
#' FB_summarised$summary_roll[[5]]
#'
#' # dplyr::bind_rows() is often helpful in these cases to get
#' # meaningful output
#'
#' summary_roll <- slidify(~ bind_rows(summary(.)),
#'                         .period = 5, .unlist = FALSE, .align = "right")
#'
#' FB %>%
#'     mutate(summary_roll = summary_roll(adjusted)) %>%
#'     filter(!is.na(summary_roll)) %>%
#'     unnest(summary_roll)
#'
#'
#' # --- ROLLING REGRESSIONS ----
#'
#' # Rolling regressions are easy to implement using `.unlist = FALSE`
#' lm_roll <- slidify(~lm(.x ~ .y), .period = 90, .unlist = FALSE, .align = "right")
#'
#' FB %>%
#'     mutate(numeric_date = as.numeric(date)) %>%
#'     mutate(rolling_lm = lm_roll(adjusted, numeric_date)) %>%
#'     filter(!is.na(rolling_lm))
#'
#'
#'
#'
#' @export
#'
slidify <- function(.f, .period = 1,
                    .align = c("center", "left", "right"),
                    .partial = FALSE,
                    .unlist = TRUE) {

    # Checks
    .align <- tolower(.align[1])

    # Mappify the function
    .f <- purrr::as_mapper(.f)

    # Return function that calls slide
    function(...) {
        slider_2(
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

slider_2 <- function(..., .slider_fun, .f, .period, .align, .partial, .unlist) {

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
    # Optional args should be specified in the slidify call
    for(i in 1:length(x)) {
        assertthat::assert_that(length(x[[i]]) == length(x[[1]]),
                                msg = "Arguments supplied to the rolling version
                            of the function should be data of the same length.
                            Optional arguments should be specified when creating
                            the rolling version with `slidify()`")
    }
}

