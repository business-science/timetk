library(timekit)
library(tidyquant)
context("Testing tk_make_future_timeseries")

# tk_make_future_timeseries_signature -----

test_datetime <- c("2016-01-01 00:00:00",
                   "2016-01-01 00:00:03",
                   "2016-01-01 00:00:06") %>%
    ymd_hms()

test_that("tk_make_future_timeseries(datetime) test returns correct format.", {
    # No skip values
    test <- tk_make_future_timeseries(test_datetime, n_future = 3)
    expectation <- c("2016-01-01 00:00:09", "2016-01-01 00:00:12", "2016-01-01 00:00:15") %>%
        ymd_hms()
    expect_equal(test, expectation)

    # Skip values
    skip <- ymd_hms("2016-01-01 00:00:15")
    test <- tk_make_future_timeseries(test_datetime, n_future = 3, skip_values = skip)
    expectation <- c("2016-01-01 00:00:09", "2016-01-01 00:00:12", "2016-01-01 00:00:18") %>%
        ymd_hms()
    expect_equal(test, expectation)

    # Skip values not within sequence
    skip <- ymd_hms("2016-01-01 00:00:10")
    expect_message(test <- tk_make_future_timeseries(test_datetime, n_future = 3, skip_values = skip))
    expectation <- c("2016-01-01 00:00:09", "2016-01-01 00:00:12", "2016-01-01 00:00:15") %>%
        ymd_hms()
    expect_equal(test, expectation)

    # Inspect validation of skip_values
    expect_warning(test <- tk_make_future_timeseries(test_datetime, n_future = 10, skip_values = 1))
    expect_equal(test, NA)

})

test_date <- FANG %>%
    filter(symbol == "FB") %>%
    tk_index()

test_that("tk_make_future_timeseries(date) test returns correct format.", {
    # No skip values, inspect_weekdays = FALSE
    test <- tk_make_future_timeseries(test_date, n_future = 3, skip_values = NULL, inspect_weekdays = FALSE)
    expectation <- c("2016-12-31", "2017-01-01", "2017-01-02") %>%
        ymd()
    expect_equal(test, expectation)

    # No skip values, inspect_weekdays = TRUE
    test <- tk_make_future_timeseries(test_date, n_future = 3, skip_values = NULL, inspect_weekdays = TRUE)
    expectation <- c("2017-01-02", "2017-01-03", "2017-01-04") %>%
        ymd()
    expect_equal(test, expectation)

    # Skip values, inspect_weekdays = TRUE
    holidays <- c("2017-01-03", "2017-01-04") %>% ymd()
    test <- tk_make_future_timeseries(test_date, n_future = 3, skip_values = holidays, inspect_weekdays = TRUE)
    expectation <- c("2017-01-02", "2017-01-05", "2017-01-06") %>%
        ymd()
    expect_equal(test, expectation)

    # inspect_weekdays = T: Test when skip values are not within future index
    skip <- ymd(c("2018-01-01", "2016-12-31"))
    expect_message(test <- tk_make_future_timeseries(test_date, n_future = 3, skip_values = skip, inspect_weekdays = F))
    expectation <- c("2017-01-01", "2017-01-02", "2017-01-03") %>%
        ymd()
    expect_equal(test, expectation)

    # inspect_weekdays = F: Test when skip values are not within future index
    skip <- ymd(c("2018-01-01", "2016-12-31"))
    expect_message(test <- tk_make_future_timeseries(test_date, n_future = 3, skip_values = skip, inspect_weekdays = F))
    expectation <- c("2017-01-01", "2017-01-02", "2017-01-03") %>%
        ymd()
    expect_equal(test, expectation)

    # inspect_weekdays = T: n_future missing
    expect_warning(test <- tk_make_future_timeseries(test_date))
    expect_equal(test, NA)

    # Inspect validation of skip_values
    expect_warning(test <- tk_make_future_timeseries(test_date, n_future = 10, skip_values = 1))
    expect_equal(test, NA)
})

test_yearmon <- c("2016-01",
                  "2016-02",
                  "2016-03") %>%
    as.yearmon()

test_that("tk_make_future_timeseries(yearmon) test returns correct format.", {
    # No skip values
    test <- tk_make_future_timeseries(test_yearmon, n_future = 3)
    expectation <- c("2016-04", "2016-05", "2016-06") %>%
        as.yearmon()
    expect_equal(test, expectation)

    # Skip values
    skip <- as.yearmon("2016-05")
    test <- tk_make_future_timeseries(test_yearmon, n_future = 3, skip_values = skip)
    expectation <- c("2016-04", "2016-06", "2016-07")  %>%
        as.yearmon()
    expect_equal(test, expectation)

    # Test when skip values are not within future index
    skip <- as.yearmon(c("2016-10", "2016-11"))
    expect_message(tk_make_future_timeseries(test_yearmon, n_future = 3, skip_values = skip))

    # Inspect validation of skip_values
    expect_warning(test <- tk_make_future_timeseries(test_yearmon, n_future = 10, skip_values = 1))
    expect_equal(test, NA)
})

test_yearqtr <- c("2016 Q1",
                  "2016 Q2",
                  "2016 Q3",
                  "2016 Q4") %>%
    as.yearqtr()

test_that("tk_make_future_timeseries(yearqtr) test returns correct format.", {
    # No skip values
    test <- tk_make_future_timeseries(test_yearqtr, n_future = 4)
    expectation <- c("2017 Q1", "2017 Q2", "2017 Q3", "2017 Q4") %>%
        as.yearqtr()
    expect_equal(test, expectation)

    # Skip values
    skip <- as.yearqtr("2017 Q1")
    test <- tk_make_future_timeseries(test_yearqtr, n_future = 4, skip_values = skip)
    expectation <- c("2017 Q2", "2017 Q3", "2017 Q4", "2018 Q1")  %>%
        as.yearqtr()
    expect_equal(test, expectation)

    # Test when skip values are not within future index
    skip <- as.yearqtr(c("2017 Q1", "2018 Q2"))
    expect_message(test <- tk_make_future_timeseries(test_yearqtr, n_future = 4, skip_values = skip))
    expectation <- c("2017 Q2", "2017 Q3", "2017 Q4", "2018 Q1")  %>%
        as.yearqtr()
    expect_equal(test, expectation)

    # Inspect validation of skip_values
    expect_warning(test <- tk_make_future_timeseries(test_yearqtr, n_future = 10, skip_values = 1))
    expect_equal(test, NA)
})
