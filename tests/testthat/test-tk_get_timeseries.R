context("Testing tk_get_timeseries functions")
library(lubridate)
n <- 29

# tk_get_timeseries_signature -----

test_datetime <- c("2016-01-01 00:00:00",
                   "2016-01-01 00:00:03",
                   "2016-01-01 00:00:06") %>%
    lubridate::ymd_hms()

test_that("tk_get_timeseries_signature(datetime) test returns correct format.", {
    test <- tk_get_timeseries_signature(test_datetime)
    expect_s3_class(test, "tbl_df")
    expect_equal(nrow(test), 3)
    expect_equal(ncol(test), n)
})

test_date <- c("2016-01-01",
               "2016-01-02",
               "2016-01-03") %>%
    lubridate::as_date()

test_that("tk_get_timeseries_signature(date) test returns correct format.", {
    test <- tk_get_timeseries_signature(test_date)
    expect_s3_class(test, "tbl_df")
    expect_equal(nrow(test), 3)
    expect_equal(ncol(test), n)
})

test_yearmon <- c("2016-01-01",
                  "2016-02-01",
                  "2016-03-01") %>%
    lubridate::ymd() %>%
    zoo::as.yearmon()

test_that("tk_get_timeseries_signature(yearmon) test returns correct format.", {
    test <- tk_get_timeseries_signature(test_yearmon)
    expect_s3_class(test, "tbl_df")
    expect_equal(nrow(test), 3)
    expect_equal(ncol(test), n)
})

test_yearqtr <- c("2016-01-01",
                  "2016-04-01",
                  "2016-07-01",
                  "2016-10-01") %>%
    lubridate::ymd() %>%
    zoo::as.yearqtr()

test_that("tk_get_timeseries_signature(yearqtr) test returns correct format.", {
    test <- tk_get_timeseries_signature(test_yearqtr)
    expect_s3_class(test, "tbl_df")
    expect_equal(nrow(test), 4)
    expect_equal(ncol(test), n)
})

test_numeric <- c(2016.00, 2016.25, 2016.50, 2016.75)

test_that("tk_get_timeseries_signature(numeric) test returns correct format.", {
    expect_error(tk_get_timeseries_signature(test_numeric))
})

test_default <- letters

test_that("tk_get_timeseries_signature(default) test returns correct format.", {
    expect_error(tk_get_timeseries_signature(test_default))
})


# tk_get_timeseries_summary -----

test_datetime <- c("2016-01-01 00:00:00",
                   "2016-01-01 00:00:03",
                   "2016-01-01 00:00:06") %>%
    lubridate::ymd_hms()

test_that("tk_get_timeseries_summary(datetime) test returns correct format.", {
    test <- tk_get_timeseries_summary(test_datetime)
    expect_s3_class(test, "tbl_df")
    expect_equal(nrow(test), 1)
    expect_equal(ncol(test), 12)
})

test_date <- c("2016-01-01",
               "2016-01-02",
               "2016-01-03") %>%
    lubridate::as_date()

test_that("tk_get_timeseries_summary(date) test returns correct format.", {
    test <- tk_get_timeseries_summary(test_date)
    expect_s3_class(test, "tbl_df")
    expect_equal(nrow(test), 1)
    expect_equal(ncol(test), 12)
})

test_yearmon <- c("2016-01",
                  "2016-02",
                  "2016-03") %>%
    zoo::as.yearmon()

test_that("tk_get_timeseries_summary(yearmon) test returns correct format.", {
    test <- tk_get_timeseries_summary(test_yearmon)
    expect_s3_class(test, "tbl_df")
    expect_equal(nrow(test), 1)
    expect_equal(ncol(test), 12)
})

test_yearqtr <- c("2016 Q1",
                  "2016 Q2",
                  "2016 Q3",
                  "2016 Q4") %>%
    zoo::as.yearqtr()

test_that("tk_get_timeseries_summary(yearqtr) test returns correct format.", {
    test <- tk_get_timeseries_summary(test_yearqtr)
    expect_s3_class(test, "tbl_df")
    expect_equal(nrow(test), 1)
    expect_equal(ncol(test), 12)
})

test_numeric <- c(2016.00, 2016.25, 2016.50, 2016.75)

test_that("tk_get_timeseries_summary(numeric) test returns correct format.", {
    expect_error(tk_get_timeseries_summary(test_numeric))
})

test_default <- letters

test_that("tk_get_timeseries_summary(default) test returns correct format.", {
    expect_error(tk_get_timeseries_summary(test_default))
})


# tk_get_timeseries_variables -----

test_date_vars <- tibble::tibble(
    my.date     = lubridate::ymd(c("2016-01-01", "2016-01-02")),
    my.chr      = c("a", "b"),
    my.datetime = lubridate::ymd_hms(c("2016-01-01 00:00:00", "2016-01-02 00:00:00")),
    my.yearmon  = zoo::as.yearmon(c("2016-01", "2016-01")),
    more.chr    = c("x", "y"),
    my.yearqtr  = zoo::as.yearqtr(c("2016 Q1", "2016 Q1"))
)

test_that("tk_get_timeseries_variables() test returns correct format.", {
    expect_equal(tk_get_timeseries_variables(test_date_vars),
                 c("my.date", "my.datetime", "my.yearmon", "my.yearqtr"))
})


# tk_get_timeseries_unit_frequency -----

test <- tk_get_timeseries_unit_frequency()

test_that("tk_get_timeseries_unit_frequency() test returns correct format.", {
    expect_equal(nrow(test), 1)
})
