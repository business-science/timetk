#' Add many holiday features to the data
#'
#' Quickly add the "holiday signature" - sets of holiday features that correspond
#' to calendar dates. Works with `dplyr` groups too.
#'
#' @param .data A time-based tibble or time-series object.
#' @param .date_var A column containing either date or date-time values.
#'  If `NULL`, the time-based column will interpret from the object (tibble).
#' @param .holiday_pattern A regular expression pattern to search the "Holiday Set".
#' @param .locale_set Return binary holidays based on locale.
#' One of: "all", "none", "World", "US", "CA", "GB", "FR", "IT", "JP", "CH", "DE".
#' @param .exchange_set Return binary holidays based on Stock Exchange Calendars.
#' One of: "all", "none", "NYSE", "LONDON", "NERC", "TSX", "ZURICH".
#'
#' @return Returns a `tibble` object describing the holiday timeseries.
#'
#' @details
#' `tk_augment_holiday_signature` adds the holiday signature
#' features. See [tk_get_holiday_signature()] (powers the augment function)
#' for a full description and examples for how to use.
#'
#' __1. Individual Holidays__
#'
#' These are __single holiday features__ that can be filtered using a pattern.
#' This helps in identifying which holidays are important to a machine learning model.
#' This can be useful for example in __e-commerce initiatives__
#' (e.g. sales during Christmas and Thanskgiving).
#'
#' __2. Locale-Based Summary Sets__
#'
#' Locale-based holdiay sets are useful for __e-commerce initiatives__
#' (e.g. sales during Christmas and Thanskgiving). Filter on a locale to
#' identify all holidays in that locale.
#'
#' __3. Stock Exchange Calendar Summary Sets__
#'
#' Exchange-based holdiay sets are useful for identifying __non-working days.__
#' Filter on an index to identify all holidays that are commonly non-working.
#'
#'
#' @seealso
#'
#' Augment Operations:
#'
#' - [tk_augment_timeseries_signature()] - Group-wise augmentation of timestamp features
#' - [tk_augment_holiday_signature()] - Group-wise augmentation of holiday features
#' - [tk_augment_roll_apply()] - Group-wise augmentation of rolling functions
#' - [tk_augment_lags()] - Group-wise augmentation of lagged data
#' - [tk_augment_differences()] - Group-wise augmentation of differenced data
#' - [tk_augment_fourier()] - Group-wise augmentation of fourier series
#'
#' Underlying Function:
#'
#' - [tk_get_holiday_signature()] - Underlying function that powers holiday feature generation
#'
#' @examples
#' library(dplyr)
#' library(timetk)
#'
#' dates_in_2017_tbl <- tibble(index = tk_make_timeseries("2017-01-01", "2017-12-31", by = "day"))
#'
#' # Non-working days in US due to Holidays using NYSE stock exchange calendar
#' dates_in_2017_tbl %>%
#'     tk_augment_holiday_signature(
#'         index,
#'         .holiday_pattern = "^$",   # Returns nothing on purpose
#'         .locale_set      = "none",
#'         .exchange_set    = "NYSE")
#'
#' # All holidays in US
#' dates_in_2017_tbl %>%
#'     tk_augment_holiday_signature(
#'         index,
#'         .holiday_pattern = "US_",
#'         .locale_set      = "US",
#'         .exchange_set    = "none")
#'
#' # All holidays for World and Italy-specific Holidays
#' # - Note that Italy celebrates specific holidays in addition to many World Holidays
#' dates_in_2017_tbl %>%
#'     tk_augment_holiday_signature(
#'         index,
#'         .holiday_pattern = "(World)|(IT_)",
#'         .locale_set      = c("World", "IT"),
#'         .exchange_set    = "none")
#'
#'
#'
#' @name tk_augment_holiday
NULL

#' @export
#' @rdname tk_augment_holiday
tk_augment_holiday_signature <- function(.data,
                                         .date_var = NULL,
                                         .holiday_pattern = ".",
                                         .locale_set = c("all", "none", "World", "US", "CA", "GB", "FR", "IT", "JP", "CH", "DE"),
                                         .exchange_set = c("all", "none", "NYSE", "LONDON", "NERC", "TSX", "ZURICH")
                                         ) {

    # Checks
    if (is.data.frame(.data)) {
        if (rlang::quo_is_null(rlang::enquo(.date_var))) {
            # .date_var is NULL
            date_var <- tk_get_timeseries_variables(.data)[[1]]
            if (length(date_var) == 0 ) stop(call. = FALSE, "tk_augment_holiday_signature(): No date variable detected.")
            message("tk_augment_holiday_signature(): Using the following .date_var variable: ", date_var)
        }
    }

    UseMethod("tk_augment_holiday_signature", .data)
}

#' @export
tk_augment_holiday_signature.data.frame <- function(.data,
                                                    .date_var = NULL,
                                                    .holiday_pattern = ".",
                                                    .locale_set = c("all", "none", "World", "US", "CA", "GB", "FR", "IT", "JP", "CH", "DE"),
                                                    .exchange_set = c("all", "none", "NYSE", "LONDON", "NERC", "TSX", "ZURICH")
                                                    ) {

    date_var_expr <- rlang::enquo(.date_var)
    group_names   <- dplyr::group_vars(.data)

    # Get date_var
    if (rlang::quo_is_null(date_var_expr)) {
        # .date_var is NULL
        date_var <- tk_get_timeseries_variables(.data)[[1]]
    } else {
        date_var <- .data %>%
            dplyr::ungroup() %>%
            dplyr::select(!! date_var_expr) %>%
            colnames()
    }

    # Arrange by date_var
    # .data <- .data %>% dplyr::arrange(!! sym(date_var))

    # Bind Time Series Signature
    ret_1 <- .data %>% dplyr::ungroup()

    ret_2 <- .data %>%
        dplyr::ungroup() %>%
        dplyr::pull(date_var) %>%
        tk_get_holiday_signature(
            holiday_pattern = .holiday_pattern,
            locale_set      = .locale_set,
            exchange_set    = .exchange_set
        ) %>%
        dplyr::select(-1)

    ret <- dplyr::bind_cols(ret_1, ret_2)

    # Re-apply groups
    if (length(group_names) > 0) {
        ret <- ret %>%
            dplyr::group_by(!!! rlang::syms(group_names))
    }

    return(ret)

}

#' @export
tk_augment_holiday_signature.default <- function(.data,
                                                 .date_var = NULL,
                                                 .holiday_pattern = ".",
                                                 .locale_set = c("all", "none", "World", "US", "CA", "GB", "FR", "IT", "JP", "CH", "DE"),
                                                 .exchange_set = c("all", "none", "NYSE", "LONDON", "NERC", "TSX", "ZURICH")) {
    stop(paste0("`tk_augment_holiday_signature` has no method for class ", class(.data)[[1]]))
}
