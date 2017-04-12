#' Tidying methods that work with nested data frames (list-columns)
#'
#'
#' @param data a nested tibble
#' @param x the column name of the column containing the models to
#' be tidied. For tidy, augment, and glance it should be the bare name; for
#' _ methods it should be quoted.
#' @param ... additional arguments to pass on to the respective tidying method
#'
#' @return A `grouped_df`, where the non-list columns of the
#' original are used as grouping columns alongside the tidied outputs.
#'
#' @details
#' These methods are designed to expedite tidying on data frames that are
#' nested meaning the columns contain lists that with contents
#' to be tidied by one of the `sweep` tidying functions.
#'
#'
#' @examples
#' library(tidyverse)
#' library(sweep)
#'
#' regressions <- mtcars %>%
#'     group_by(cyl) %>%
#'     do(mod = lm(mpg ~ wt, .))
#'
#' regressions
#'
#' regressions %>% sw_tidy(mod)
#' regressions %>% sw_glance(mod)
#' regressions %>% sw_augment(mod)
#'
#' @name tidiers_nested_df
NULL

#' @rdname tidiers_nested_df
#' @export
sw_tidy.data.frame <- function(data, x, ...) {
    sw_wrap_nested_tibble(sw_tidy, data, x, ...)
}

#' @rdname tidiers_nested_df
#' @export
sw_glance.data.frame <- function(data, x, ...) {
    sw_wrap_nested_tibble(sw_glance, data, x, ...)
}

#' @rdname tidiers_nested_df
#' @export
sw_augment.data.frame <- function(data, x, ...) {
    sw_wrap_nested_tibble(sw_augment, data, x, ...)
}

#' @rdname tidiers_nested_df
#' @export
sw_tidy_decomp.data.frame <- function(data, x, ...) {
    sw_wrap_nested_tibble(sw_tidy_decomp, data, x, ...)
}

#' @rdname tidiers_nested_df
#' @export
sw_sweep.data.frame <- function(data, x, ...) {
    sw_wrap_nested_tibble(sw_sweep, data, x, ...)
}

#' @export
sw_wrap_nested_tibble <- function(fun, data, x, ...) {

    # Target column
    colname <- lazyeval::expr_text(x)

    # Validations???
    class_target <- dplyr::select_(data, colname) %>%
        lapply(., class) %>%
        unlist()
    if (class_target[[1]] != "list")
        stop(paste0("Can only tidy class ", class(data)[[1]], " using list-columns. Class of column selected is ", class_target[[1]], "."))

    # Detect columns that can be kept
    groupers <- colnames(data)[sapply(data, function(x) class(x)[1]) != "list"]

    # Drop list columns
    data_subset <- data %>%
        dplyr::select_(.dots = list(groupers, colname))

    # Rename x colname to target
    colnames(data_subset)[length(colnames(data_subset))] <- "target"

    # purrr magic: map `fun` to target
    ret <- data_subset %>%
        dplyr::ungroup() %>%
        dplyr::group_by_(groupers) %>%
        dplyr::mutate(..data = purrr::map(.x = target, .f = ~ fun(.x, ...))) %>%
        dplyr::select(-target) %>%
        tidyr::unnest()

    return(ret)
}
