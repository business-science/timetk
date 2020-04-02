#' Get holiday features from a time-series index
#'
#' @param idx A time-series index that is a vector of dates or datetimes.
#' @param holiday_pattern A regular expression pattern to search the "Holiday Set".
#' @param locale_set Return binary holidays based on locale.
#' One of: "all", "none", "World", "US", "CA", "GB", "FR", "IT", "JP", "CH", "DE".
#' @param exchange_set Return binary holidays based on Stock Exchange Calendars.
#' One of: "all", "none", "NYSE", "LONDON", "NERC", "TSX", "ZURICH".
#' @param years One or more years to collect holidays for.
#'
#'
#' @return Returns a `tibble` object describing the timeseries holidays.
#'
#' @details
#' Feature engineering holidays can help identify critical patterns for
#' machine learning algorithms. `tk_get_holiday_signature()` helps by providing
#' feature sets for 3 types of features:
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
#' @seealso
#'
#' - [tk_augment_holiday_signature()] - A quick way to add holiday features to a data.frame
#' - [step_holiday_signature()] - Preprocessing and feature engineering steps for use with `recipes`
#'
#' @examples
#' library(tidyverse)
#' library(tidyquant)
#' library(timetk)
#'
#' # Works with time-based tibbles
#' idx <- tk_make_date_sequence("2017-01-01", "2017-12-31", by = "day")
#'
#' # --- BASIC USAGE ----
#'
#' tk_get_holiday_signature(idx)
#'
#' # ---- FILTERING WITH PATTERNS & SETS ----
#'
#' # List available holidays - see patterns
#' tk_get_holidays_by_year(2020) %>%
#'     filter(holiday_name %>% str_detect("US_"))
#'
#' # Filter using holiday patterns
#' # - Get New Years, Christmas and Thanksgiving Features in US
#' tk_get_holiday_signature(
#'     idx,
#'     holiday_pattern = "(US_NewYears)|(US_Christmas)|(US_Thanks)",
#'     locale_set      = "none",
#'     exchange_set    = "none")
#'
#' # Filter with locale sets - Signals all holidays in a locale
#' tk_get_holiday_signature(
#'     idx,
#'     holiday_pattern = "$^", # Matches nothing on purpose
#'     locale_set      = "US",
#'     exchange_set    = "none")
#'
#' # Filter with exchange sets - Signals Common Non-Business Days
#' tk_get_holiday_signature(
#'     idx,
#'     holiday_pattern = "$^", # Matches nothing on purpose
#'     locale_set      = "none",
#'     exchange_set    = "NYSE")
#'
#' @name tk_get_holiday
#' @import timeDate
NULL

#' @export
#' @rdname tk_get_holiday
tk_get_holiday_signature <- function(idx,
                                     holiday_pattern = ".",
                                     locale_set = c("all", "none", "World", "US", "CA", "GB", "FR", "IT", "JP", "CH", "DE"),
                                     exchange_set = c("all", "none", "NYSE", "LONDON", "NERC", "TSX", "ZURICH")
                                     ) {
    UseMethod("tk_get_holiday_signature", idx)
}

#' @export
tk_get_holiday_signature.character <- function(idx,
                                               holiday_pattern = ".",
                                               locale_set = c("all", "none", "World", "US", "CA", "GB", "FR", "IT", "JP", "CH", "DE"),
                                               exchange_set = c("all", "none", "NYSE", "LONDON", "NERC", "TSX", "ZURICH")) {
    get_holiday_signature(lubridate::as_date(idx), holiday_pattern, locale_set, exchange_set)
}

#' @export
tk_get_holiday_signature.POSIXt <- function(idx,
                                            holiday_pattern = ".",
                                            locale_set = c("all", "none", "World", "US", "CA", "GB", "FR", "IT", "JP", "CH", "DE"),
                                            exchange_set = c("all", "none", "NYSE", "LONDON", "NERC", "TSX", "ZURICH")
                                            ) {
    get_holiday_signature(idx, holiday_pattern, locale_set, exchange_set)
}

#' @export
tk_get_holiday_signature.Date <- function(idx,
                                          holiday_pattern = ".",
                                          locale_set = c("all", "none", "World", "US", "CA", "GB", "FR", "IT", "JP", "CH", "DE"),
                                          exchange_set = c("all", "none", "NYSE", "LONDON", "NERC", "TSX", "ZURICH")
                                          ) {
    get_holiday_signature(idx, holiday_pattern, locale_set, exchange_set)
}

#' @export
tk_get_holiday_signature.default <- function(idx,
                                             holiday_pattern = ".",
                                             locale_set = c("all", "none", "World", "US", "CA", "GB", "FR", "IT", "JP", "CH", "DE"),
                                             exchange_set = c("all", "none", "NYSE", "LONDON", "NERC", "TSX", "ZURICH")
                                             ) {
    stop(paste0("No method for class ", class(idx)[[1]], "."))
}


get_holiday_signature <- function(idx,
                                  holiday_pattern = ".",
                                  locale_set = c("all", "none", "World", "US", "CA", "GB", "FR", "IT", "JP", "CH", "DE"),
                                  exchange_set = c("all", "none", "NYSE", "LONDON", "NERC", "TSX", "ZURICH")
                                  ) {

    # Setup
    idx          <- lubridate::as_date(idx)
    years        <- lubridate::year(idx) %>% unique()
    locale_set   <- locale_set %>% tolower()
    exchange_set <- exchange_set %>% tolower()

    if (any("all" %in% locale_set)) locale_set <- "all"
    if (any("all" %in% exchange_set)) exchange_set <- "all"

    if (any("none" %in% locale_set)) locale_set <- "none"
    if (any("none" %in% exchange_set)) exchange_set <- "none"

    initial_index_tbl <- tibble::tibble(index = idx)
    unique_index_tbl  <- initial_index_tbl %>% dplyr::distinct()

    # HOLIDAY & LOCALE FEATURES ----

    # Step 1 - Collect holidays by date
    holiday_table <- tk_get_holidays_by_year(years)

    # Separate into 2 sets
    holiday_table_locale <- holiday_table %>% dplyr::select(date, locale)
    holiday_table_name   <- holiday_table %>% dplyr::select(date, holiday_name)

    # 1. HOLIDAY FEATURES ----

    holiday_table_name <- holiday_table_name %>%
        dplyr::mutate(values = 1) %>%
        dplyr::filter(tolower(holiday_name) %>% stringr::str_detect(pattern = tolower(holiday_pattern))) %>%
        tidyr::pivot_wider(names_from = holiday_name, values_from = values)

    if (nrow(holiday_table_name) == 0) {
        holiday_table_name[,"values"] <- NULL
    }


    # 2. LOCALE FEATURES ----

    # - Filter by locale
    locales_needing_filtered <- c("World", "US", "CA", "GB", "FR", "IT", "JP", "CH", "DE") %>% tolower()
    if (any(tolower(locale_set) %in% tolower(locales_needing_filtered))) {
        # Not all or none - must have a locale selected
        holiday_table_locale <- holiday_table_locale %>%
            dplyr::mutate(value = 1) %>%

            dplyr::filter(tolower(locale) %in% tolower(locale_set)) %>%

            dplyr::mutate(locale = stringr::str_c("locale_", locale)) %>%
            dplyr::group_by(date, locale) %>%
            dplyr::summarize(value = min(value)) %>%
            dplyr::ungroup() %>%
            tidyr::pivot_wider(names_from = locale, values_from = value)
    } else if (tolower(locale_set) == "all") {
        # All selected - Just pivot
        holiday_table_locale <- holiday_table_locale %>%
            dplyr::mutate(value = 1) %>%
            dplyr::mutate(locale = stringr::str_c("locale_", locale)) %>%
            dplyr::group_by(date, locale) %>%
            dplyr::summarize(value = min(value)) %>%
            dplyr::ungroup() %>%
            tidyr::pivot_wider(names_from = locale, values_from = value)
    } else {
        # none selected - drop locale column
        holiday_table_locale <- holiday_table_locale %>%
            dplyr::select(-locale) %>%
            dplyr::mutate(value = 1) %>%
            dplyr::group_by(date) %>%
            dplyr::summarize(value = min(value)) %>%
            dplyr::ungroup()
        holiday_table_locale[,"value"] <- NULL
    }


    # 3. EXCHANGES ----
    nyse_dates   <- timeDate::holidayNYSE(years) %>% lubridate::ymd()
    london_dates <- timeDate::holidayLONDON(years) %>% lubridate::ymd()
    nerc_dates   <- timeDate::holidayNERC(years) %>% lubridate::ymd()
    tsx_dates    <- timeDate::holidayTSX(years) %>% lubridate::ymd()
    zurich_dates <- timeDate::holidayZURICH(years) %>% lubridate::ymd()

    holiday_table_exchange <- unique_index_tbl %>%
        dplyr::mutate(
            exch_NYSE   = ifelse(index %in% nyse_dates, 1, 0),
            exch_LONDON = ifelse(index %in% london_dates, 1, 0),
            exch_NERC   = ifelse(index %in% nerc_dates, 1, 0),
            exch_TSX    = ifelse(index %in% tsx_dates, 1, 0),
            exch_ZURICH = ifelse(index %in% zurich_dates, 1, 0)
        )

    exchanges_needing_filtered <- c("NYSE", "LONDON", "NERC", "TSX", "ZURICH") %>% tolower()
    if (any(tolower(exchange_set) %in% tolower(exchanges_needing_filtered))) {
        # Not all or none - must have a locale selected
        holiday_table_exchange <- holiday_table_exchange %>%
            dplyr::select(index, dplyr::contains(toupper(exchange_set)))
    } else if (tolower(exchange_set) == "all") {
        # All selected - Nothing to do

    } else {
        # none selected - drop locale column
        holiday_table_exchange <- holiday_table_exchange %>% dplyr::select(index)
    }

    # JOIN ALL TABLES

    holidays_joined_tbl <- unique_index_tbl %>%
        dplyr::left_join(holiday_table_exchange, by = "index") %>%
        dplyr::left_join(holiday_table_locale, by = c("index" = "date")) %>%
        dplyr::left_join(holiday_table_name, by = c("index" = "date"))

    # Cleanup
    holidays_joined_tbl[is.na(holidays_joined_tbl)] <- 0
    holidays_joined_tbl[,"values"] <- NULL

    # Join with initial index
    holidays_joined_tbl <- dplyr::left_join(initial_index_tbl, holidays_joined_tbl, by = "index")

    return(holidays_joined_tbl)

}

#' @rdname tk_get_holiday
#' @export
#' @importFrom lubridate year today
tk_get_holidays_by_year <- function(years = year(today())) {

    years <- as.integer(years)

    tibble::tibble(holidays = timeDate::listHolidays()) %>%
        dplyr::mutate(date = purrr::map(holidays, .f = function(holiday) {
            timeDate::holiday(years, Holiday = holiday) %>%
                lubridate::as_date()
            # do.call(holiday, args = list(year = years))
            })
        ) %>%
        tidyr::unnest(date) %>%
        dplyr::arrange(date) %>%
        dplyr::mutate(locale = holidays %>% stringr::str_sub(1,2)) %>%
        dplyr::mutate(locale = ifelse(locale == toupper(locale), locale, "World")) %>%
        dplyr::mutate(holiday_name = holidays %>% stringr::str_replace(pattern = locale, "")) %>%
        dplyr::mutate(holiday_name = ifelse(is.na(holiday_name), holidays, holiday_name)) %>%
        dplyr::mutate(holiday_name = stringr::str_c(locale, "_", holiday_name)) %>%
        dplyr::select(-holidays)
}

