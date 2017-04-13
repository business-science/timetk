#' Map a function rowwise to a list-column
#'
#'
#' @param .d A nested tibble
#' @param .l The column name of the column containing the nested lists.
#' @param .f The function to be applied rowwise
#' @param ... Additional arguments to pass on to the function, `.f`.
#' @param .unnest Whether or not to unnest the return and drop other list columns
#' in the process. `FALSE` by default, which returns nested list-columns.
#' @param .to Used to rename a nested list column that is returned if
#' `.unnest = FALSE`.
#'
#' @return A `grouped_df`, where the non-list columns of the
#' original are used as grouping columns alongside the tidied outputs.
#'
#' @details
#' The `map_list_column` method is designed to expedite applying functions rowise to list-columns
#' that are nested meaning the columns contain lists.
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
#' regressions %>% map_list_column(mod, sw_tidy, .unnest = TRUE)
#'
#' @name map_list_column
NULL

#' @rdname map_list_column
#' @export
map_list_column <- function(.d, .l, .f, ..., .unnest = FALSE, .to = "out") {
    # Target column
    .l <- lazyeval::expr_text(.l)

    map_list_column_(.d           = .d,
                     .l           = .l,
                     .f           = .f,
                     ...          = ...,
                     .unnest      = .unnest,
                     .to          = .to)
}

#' @rdname map_list_column
#' @export
map_list_column_ <- function(.d, .l, .f, ..., .unnest = FALSE, .to = "out") {
    UseMethod("map_list_column_", .d)

}

#' @export
map_list_column_.data.frame <- function(.d, .l, .f, ..., .unnest = FALSE, .to = "out") {

    # Validations
    class_target <- .d %>%
        dplyr::ungroup() %>%
        dplyr::select_(.l) %>%
        lapply(., class) %>%
        unlist()
    if (class_target[[1]] != "list")
        stop(paste0("Can only tidy class ", class(.d)[[1]], " using list-columns. Class of column selected is ", class_target[[1]], "."))

    # Detect columns that can be kept
    groupers <- colnames(.d)[sapply(.d, function(x) class(x)[1]) != "list"]

    # purrr magic: map `.f` to target
    ret <- .d
    colnames(ret)[stringr::str_detect(colnames(ret), .l)] <- "..target"
    ret <- ret %>%
        dplyr::ungroup() %>%
        dplyr::group_by_(groupers) %>%
        dplyr::mutate(..data.map = purrr::map(.x = ..target, .f = ~ .f(.x, ...)))


    if (.unnest == TRUE) {
        # Drop list columns and unnest
        ret <- ret %>%
            dplyr::select_(.dots = list(groupers, "..data.map")) %>%
            tidyr::unnest()
    } else {
        # rename target
        colnames(ret)[stringr::str_detect(colnames(ret), "..target")] <- .l
        colnames(ret)[stringr::str_detect(colnames(ret), "..data.map")] <- .to
    }

    return(ret)
}

#' @export
map_list_column_.default <- function(.d, .l, .f, ..., .unnest = FALSE, .to = "out") {
    stop(paste0("Method does not work with `.d` of class ", class(.d)[[1]], "."))
}
