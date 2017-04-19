library(forecast)
library(timekit)
library(tidyquant)
context("Testing tk_index")

AAPL_tbl    <- tq_get("AAPL", from = "2015-01-01", to = "2016-12-31")
AAPL_xts    <- tk_xts(AAPL_tbl, select = -date, date_var = date)
AAPL_zoo    <- tk_zoo(AAPL_tbl, select = -date, date_var = date)
AAPL_zooreg <- tk_zooreg(AAPL_tbl, select = -date, start = 2015, freq = 252)
AAPL_ts     <- tk_ts(AAPL_tbl, select = -date, start = 2015, freq = 252)

# FUNCTION tk_index -----

# time series objects ----

test_that("tk_index(default) test returns correct format.", {
    # Not designed to work with objects of class numeric
    expect_warning(tk_index(4))
    expect_warning(
        expect_false(
            has_timekit_idx(4)
            )
        )
})

test_that("tk_index(ts) test returns correct format.", {

    # Test if object has timekit index
    expect_true(tk_ts(AAPL_tbl, select = -date, freq = 252, start = 2015) %>%
                    has_timekit_idx())

    # Return regularized dates
    test_index_1 <- tk_ts(AAPL_tbl, select = -date, freq = 252, start = 2015) %>%
        tk_index(timekit_idx = FALSE)
    expect_equal(class(test_index_1), "numeric")
    expect_equal(length(test_index_1), 504)

    # Return non-regularized dates (aka timekit index)
    test_index_2 <- tk_ts(AAPL_tbl, select = -date, freq = 252, start = 2015) %>%
        tk_index(timekit_idx = TRUE)
    expect_equal(class(test_index_2), "Date")
    expect_equal(length(test_index_2), 504)

    # No timekit index
    expect_warning(
        WWWusage %>%
            tk_index(timekit_idx = TRUE)
    )



})

test_that("tk_index(zooreg) test returns correct format.", {

    # Test if object has timekit index
    expect_true(tk_zooreg(AAPL_tbl, select = -date, freq = 252, start = 2015) %>%
                          has_timekit_idx())

    # Return regularized dates
    test_index_3 <- tk_zooreg(AAPL_tbl, select = -date, freq = 252, start = 2015) %>%
        tk_index(timekit_idx = FALSE)
    expect_equal(class(test_index_3), "numeric")
    expect_equal(length(test_index_3), 504)

    # Return non-regularized dates (aka timekit index)
    test_index_4 <- tk_zooreg(AAPL_tbl, select = -date, freq = 252, start = 2015) %>%
        tk_index(timekit_idx = TRUE)
    expect_equal(class(test_index_4), "Date")
    expect_equal(length(test_index_4), 504)

})

test_that("tk_index(tbl) test returns correct format.", {

    # Test if object has timekit index
    expect_false(AAPL_tbl %>%
                    has_timekit_idx())

    # Return vector of dates
    test_index_3 <- AAPL_tbl %>%
        tk_index(timekit_idx = FALSE)
    expect_equal(class(test_index_3), "Date")
    expect_equal(length(test_index_3), 504)

    # No date or date time
    expect_error(tk_index(mtcars))

})

test_that("tk_index(xts) test returns correct format.", {

    # Test if object has timekit index
    expect_false(AAPL_xts %>%
                     has_timekit_idx())

    # Return vector of dates
    test_index_5 <- AAPL_xts %>%
        tk_index(timekit_idx = FALSE)
    expect_equal(class(test_index_5), "Date")
    expect_equal(length(test_index_5), 504)

})

test_that("tk_index(zoo) test returns correct format.", {

    # Test if object has timekit index
    expect_false(AAPL_zoo %>%
                     has_timekit_idx())

    # Return vector of dates
    test_index_6 <- AAPL_zoo %>%
        tk_index(timekit_idx = FALSE)
    expect_equal(class(test_index_6), "Date")
    expect_equal(length(test_index_6), 504)

})

# models and forecasts ----

fit_ets <- USAccDeaths %>%
    forecast::ets()

test_that("tk_index(ets) test returns correct format.", {

    # Test if object has timekit index
    expect_false(has_timekit_idx(fit_ets))

    # Return vector of numeric regularized dates
    test_index_7 <- fit_ets %>%
        tk_index(timekit_idx = FALSE)
    expect_equal(class(test_index_7), "numeric")
    expect_equal(length(test_index_7), 72)

})

test_that("tk_index(forecast) test returns correct format.", {

    fcast_ets <- fit_ets %>%
        forecast::forecast()

    # Test if object has timekit index
    expect_false(has_timekit_idx(fcast_ets))

    # Return vector of numeric regularized dates
    test_index_8 <- fcast_ets %>%
        tk_index(timekit_idx = FALSE)
    expect_equal(class(test_index_8), "numeric")
    expect_equal(length(test_index_8), 72)

})

test_that("tk_index(bats) test returns correct format.", {

    fit_bats <- USAccDeaths %>%
        forecast::bats()

    # Test if object has timekit index
    expect_false(has_timekit_idx(fit_bats))

    # Return vector of numeric regularized dates
    test_index_9 <- fit_bats %>%
        tk_index(timekit_idx = FALSE)
    expect_equal(class(test_index_9), "numeric")
    expect_equal(length(test_index_9), 72)

})


test_that("tk_index(arima) test returns correct format.", {

    fit_arima <- USAccDeaths %>%
        forecast::Arima()

    # Test if object has timekit index
    expect_false(has_timekit_idx(fit_arima))

    # Return vector of numeric regularized dates
    test_index_10 <- fit_arima %>%
        tk_index(timekit_idx = FALSE)
    expect_equal(class(test_index_10), "numeric")
    expect_equal(length(test_index_10), 72)

})

test_that("tk_index(HoltWinters) test returns correct format.", {

    fit_hw <- USAccDeaths %>%
        stats::HoltWinters()

    # Test if object has timekit index
    expect_false(has_timekit_idx(fit_hw))

    # Return vector of numeric regularized dates
    test_index_11 <- fit_hw %>%
        tk_index(timekit_idx = FALSE)
    expect_equal(class(test_index_11), "numeric")
    expect_equal(length(test_index_11), 72)

})

test_that("tk_index(stl) test returns correct format.", {

    fit_stl <- USAccDeaths %>%
        stats::stl(s.window = "periodic")

    # Test if object has timekit index
    expect_false(has_timekit_idx(fit_stl))

    # Return vector of numeric regularized dates
    test_index_12 <- fit_stl %>%
        tk_index(timekit_idx = FALSE)
    expect_equal(class(test_index_12), "numeric")
    expect_equal(length(test_index_12), 72)

})

test_that("tk_index(stlm) test returns correct format.", {

    fit_stlm <- USAccDeaths %>%
        forecast::stlm(s.window = "periodic")

    # Test if object has timekit index
    expect_false(has_timekit_idx(fit_stlm))

    # Return vector of numeric regularized dates
    test_index_13 <- fit_stlm %>%
        tk_index(timekit_idx = FALSE)
    expect_equal(class(test_index_13), "numeric")
    expect_equal(length(test_index_13), 72)

})

test_that("tk_index(StructTS) test returns correct format.", {

    fit_StructTS <- USAccDeaths %>%
        StructTS()

    # Test if object has timekit index
    expect_false(has_timekit_idx(fit_StructTS))

    # Return vector of numeric regularized dates
    test_index_15 <- fit_StructTS %>%
        tk_index(timekit_idx = FALSE)
    expect_equal(class(test_index_15), "numeric")
    expect_equal(length(test_index_15), 72)

})


test_that("tk_index(baggedETS) test returns correct format.", {

    fit_baggedETS <- WWWusage %>%
        forecast::baggedETS()

    # Test if object has timekit index
    expect_false(has_timekit_idx(fit_baggedETS))

    # Return vector of numeric regularized dates
    test_index_16 <- fit_baggedETS %>%
        tk_index(timekit_idx = FALSE)
    expect_equal(class(test_index_16), "numeric")
    expect_equal(length(test_index_16), 100)

})

test_that("tk_index(nnetar) test returns correct format.", {

    fit_nnetar <- USAccDeaths %>%
        nnetar()

    # Test if object has timekit index
    expect_false(has_timekit_idx(fit_nnetar))

    # Return vector of numeric regularized dates
    test_index_17 <- fit_nnetar %>%
        tk_index(timekit_idx = FALSE)
    expect_equal(class(test_index_17), "numeric")
    expect_equal(length(test_index_17), 72)

})

test_that("tk_index(fracdiff) test returns correct format.", {

    # ARFIMA model (class = "fracdiff")
    x <- fracdiff::fracdiff.sim( 100, ma=-.4, d=.3)$series
    fit_arfima <- arfima(x)

    # Test if object has timekit index
    expect_warning(
        expect_false(
            has_timekit_idx(fit_arfima)
            )
    )

    # Not designed to work with class numeric
    expect_warning(
        fit_arfima %>%
            tk_index(timekit_idx = FALSE)
    )

})



