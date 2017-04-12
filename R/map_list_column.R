#' Map a function rowwise to a list-column
#'
#'
#' @param data A nested tibble
#' @param list_col The column name of the column containing the nested lists.
#' @param fun The function to be applied rowwise
#' @param ... Additional arguments to pass on to the function, `fun`.
#' @param .drop Whether or not to unnest the return and drop other list columns
#' in the process. `FALSE` by default, which returns nested list-columns.
#' @param col_rename Used to rename a nested list column that is returned if
#' `.drop = FALSE`.
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
#' regressions %>% map_list_column(mod, sw_tidy, .drop = TRUE)
#'
#' @name map_list_column
NULL

#' @rdname map_list_column
#' @export
map_list_column <- function(data, list_col, fun, ..., .drop = FALSE, col_rename = "data.map") {
    # Target column
    list_col <- lazyeval::expr_text(list_col)

    map_list_column_(data         = data,
                     list_col     = list_col,
                     fun          = fun,
                     ...          = ...,
                     .drop        = .drop,
                     col_rename   = col_rename)
}

#' @rdname map_list_column
#' @export
map_list_column_ <- function(data, list_col, fun, ..., .drop = FALSE, col_rename = "data.map") {
    UseMethod("map_list_column_", data)

}

#' @export
map_list_column_.data.frame <- function(data, list_col, fun, ..., .drop = FALSE, col_rename = "data.map") {

    # Validations
    class_target <- data %>%
        dplyr::ungroup() %>%
        dplyr::select_(list_col) %>%
        lapply(., class) %>%
        unlist()
    if (class_target[[1]] != "list")
        stop(paste0("Can only tidy class ", class(data)[[1]], " using list-columns. Class of column selected is ", class_target[[1]], "."))

    # Detect columns that can be kept
    groupers <- colnames(data)[sapply(data, function(x) class(x)[1]) != "list"]

    # purrr magic: map `fun` to target
    ret <- data
    colnames(ret)[stringr::str_detect(colnames(ret), list_col)] <- "..target"
    ret <- ret %>%
        dplyr::ungroup() %>%
        dplyr::group_by_(groupers) %>%
        dplyr::mutate(..data.map = purrr::map(.x = ..target, .f = ~ fun(.x, ...)))


    if (.drop == TRUE) {
        # Drop list columns and unnest
        ret <- ret %>%
            dplyr::select_(.dots = list(groupers, "..data.map")) %>%
            tidyr::unnest()
    } else {
        # rename target
        colnames(ret)[stringr::str_detect(colnames(ret), "..target")] <- list_col
        colnames(ret)[stringr::str_detect(colnames(ret), "..data.map")] <- col_rename
    }

    return(ret)
}

#' @export
map_list_column_.default <- function(data, list_col, fun, ..., .drop = FALSE, col_rename = "data.map") {
    stop(paste0("Method does not work with `data` of class ", class(data)[[1]], "."))
}
