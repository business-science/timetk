library(forecast)
library(sweep)
library(tidyquant)
context("Testing sw_index")

AAPL_tbl    <- tq_get("AAPL", from = "2015-01-01", to = "2016-12-31")
AAPL_xts    <- sw_xts(AAPL_tbl, select = -date, date_var = date)
AAPL_zoo    <- sw_zoo(AAPL_tbl, select = -date, date_var = date)
AAPL_zooreg <- sw_zooreg(AAPL_tbl, select = -date, start = 2015, freq = 252)
AAPL_ts     <- sw_ts(AAPL_tbl, select = -date, start = 2015, freq = 252)

# FUNCTION sw_index -----

# time series objects ----

test_that("sw_index(default) test returns correct format.", {
    # Not designed to work with objects of class numeric
    expect_warning(sw_index(4))
})

test_that("sw_index(ts) test returns correct format.", {
    # Return regularized dates
    test_index_1 <- sw_ts(AAPL_tbl, select = -date, freq = 252, start = 2015) %>%
        sw_index(.sweep_idx = FALSE)
    expect_equal(class(test_index_1), "numeric")
    expect_equal(length(test_index_1), 504)

    # Return non-regularized dates (aka sweep index)
    test_index_2 <- sw_ts(AAPL_tbl, select = -date, freq = 252, start = 2015) %>%
        sw_index(.sweep_idx = TRUE)
    expect_equal(class(test_index_2), "Date")
    expect_equal(length(test_index_2), 504)

})

test_that("sw_index(zooreg) test returns correct format.", {
    # Return regularized dates
    test_index_3 <- sw_zooreg(AAPL_tbl, select = -date, freq = 252, start = 2015) %>%
        sw_index(.sweep_idx = FALSE)
    expect_equal(class(test_index_3), "numeric")
    expect_equal(length(test_index_3), 504)

    # Return non-regularized dates (aka sweep index)
    test_index_4 <- sw_zooreg(AAPL_tbl, select = -date, freq = 252, start = 2015) %>%
        sw_index(.sweep_idx = TRUE)
    expect_equal(class(test_index_4), "Date")
    expect_equal(length(test_index_4), 504)

})

test_that("sw_index(tbl) test returns correct format.", {
    # Return vector of dates
    test_index_3 <- AAPL_tbl %>%
        sw_index(.sweep_idx = FALSE)
    expect_equal(class(test_index_3), "Date")
    expect_equal(length(test_index_3), 504)

    # No date or date time
    expect_error(sw_index(mtcars))

})

test_that("sw_index(xts) test returns correct format.", {
    # Return vector of dates
    test_index_5 <- AAPL_xts %>%
        sw_index(.sweep_idx = FALSE)
    expect_equal(class(test_index_5), "Date")
    expect_equal(length(test_index_5), 504)

})

test_that("sw_index(zoo) test returns correct format.", {
    # Return vector of dates
    test_index_6 <- AAPL_zoo %>%
        sw_index(.sweep_idx = FALSE)
    expect_equal(class(test_index_6), "Date")
    expect_equal(length(test_index_6), 504)

})

# models and forecasts ----

fit_ets <- USAccDeaths %>%
    forecast::ets()

test_that("sw_index(ets) test returns correct format.", {

    # Return vector of numeric regularized dates
    test_index_7 <- fit_ets %>%
        sw_index(.sweep_idx = FALSE)
    expect_equal(class(test_index_7), "numeric")
    expect_equal(length(test_index_7), 72)

})

test_that("sw_index(forecast) test returns correct format.", {

    fcast_ets <- fit_ets %>%
        forecast::forecast()

    # Return vector of numeric regularized dates
    test_index_8 <- fcast_ets %>%
        sw_index(.sweep_idx = FALSE)
    expect_equal(class(test_index_8), "numeric")
    expect_equal(length(test_index_8), 72)

})

test_that("sw_index(bats) test returns correct format.", {

    fit_bats <- USAccDeaths %>%
        forecast::bats()

    # Return vector of numeric regularized dates
    test_index_9 <- fit_bats %>%
        sw_index(.sweep_idx = FALSE)
    expect_equal(class(test_index_9), "numeric")
    expect_equal(length(test_index_9), 72)

})


test_that("sw_index(arima) test returns correct format.", {

    fit_arima <- USAccDeaths %>%
        forecast::Arima()

    # Return vector of numeric regularized dates
    test_index_10 <- fit_arima %>%
        sw_index(.sweep_idx = FALSE)
    expect_equal(class(test_index_10), "numeric")
    expect_equal(length(test_index_10), 72)

})

test_that("sw_index(HoltWinters) test returns correct format.", {

    fit_hw <- USAccDeaths %>%
        stats::HoltWinters()

    # Return vector of numeric regularized dates
    test_index_11 <- fit_hw %>%
        sw_index(.sweep_idx = FALSE)
    expect_equal(class(test_index_11), "numeric")
    expect_equal(length(test_index_11), 72)

})

test_that("sw_index(stl) test returns correct format.", {

    fit_stl <- USAccDeaths %>%
        stats::stl(s.window = "periodic")

    # Return vector of numeric regularized dates
    test_index_12 <- fit_stl %>%
        sw_index(.sweep_idx = FALSE)
    expect_equal(class(test_index_12), "numeric")
    expect_equal(length(test_index_12), 72)

})

test_that("sw_index(stlm) test returns correct format.", {

    fit_stlm <- USAccDeaths %>%
        forecast::stlm(s.window = "periodic")

    # Return vector of numeric regularized dates
    test_index_13 <- fit_stlm %>%
        sw_index(.sweep_idx = FALSE)
    expect_equal(class(test_index_13), "numeric")
    expect_equal(length(test_index_13), 72)

})

test_that("sw_index(stlf) test returns correct format.", {

    fcast_stlf <- USAccDeaths %>%
        forecast::stlf(s.window = "periodic")

    # Return vector of numeric regularized dates
    test_index_14 <- fcast_stlf %>%
        sw_index(.sweep_idx = FALSE)
    expect_equal(class(test_index_14), "numeric")
    expect_equal(length(test_index_14), 72)

})

test_that("sw_index(StructTS) test returns correct format.", {

    fit_StructTS <- USAccDeaths %>%
        StructTS()

    # Return vector of numeric regularized dates
    test_index_15 <- fit_StructTS %>%
        sw_index(.sweep_idx = FALSE)
    expect_equal(class(test_index_15), "numeric")
    expect_equal(length(test_index_15), 72)

})


test_that("sw_index(baggedETS) test returns correct format.", {

    fit_baggedETS <- WWWusage %>%
        forecast::baggedETS()

    # Return vector of numeric regularized dates
    test_index_16 <- fit_baggedETS %>%
        sw_index(.sweep_idx = FALSE)
    expect_equal(class(test_index_16), "numeric")
    expect_equal(length(test_index_16), 100)

})

test_that("sw_index(nnetar) test returns correct format.", {

    fit_nnetar <- USAccDeaths %>%
        nnetar()

    # Return vector of numeric regularized dates
    test_index_17 <- fit_nnetar %>%
        sw_index(.sweep_idx = FALSE)
    expect_equal(class(test_index_17), "numeric")
    expect_equal(length(test_index_17), 72)

})
