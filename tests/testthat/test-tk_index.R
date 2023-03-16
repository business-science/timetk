context("Testing tk_index")

FB_tbl    <- FANG %>% filter(symbol == "FB")
FB_xts    <- FB_tbl %>% tk_xts(silent = TRUE)
FB_zoo    <- FB_tbl %>% tk_zoo(silent = TRUE)
FB_zooreg <- FB_tbl %>% tk_zooreg(start = 2015, freq = 252, silent = TRUE)
FB_ts     <- FB_tbl %>% tk_ts(start = 2015, freq = 252, silent = TRUE)

# FUNCTION tk_index -----

# time series objects ----

test_that("tk_index(default) test returns correct format.", {
    # Not designed to work with objects of class numeric
    expect_warning(tk_index(4))
    expect_warning(
        expect_false(
            has_timetk_idx(4)
            )
        )
})

test_that("tk_index(ts) test returns correct format.", {

    # Test if object has timetk index
    expect_true(tk_ts(FB_tbl, freq = 252, start = 2015, silent = TRUE) %>%
                    has_timetk_idx())

    # Return regularized dates
    test_index_1 <- tk_ts(FB_tbl, freq = 252, start = 2015, silent = TRUE) %>%
        tk_index(timetk_idx = FALSE)
    expect_equal(class(test_index_1), "numeric")
    expect_equal(length(test_index_1), 1008)

    # Return non-regularized dates (aka timetk index)
    test_index_2 <- tk_ts(FB_tbl, freq = 252, start = 2015, silent = TRUE) %>%
        tk_index(timetk_idx = TRUE)
    expect_equal(class(test_index_2), "Date")
    expect_equal(length(test_index_2), 1008)

    # No timetk index
    expect_warning(
        WWWusage %>%
            tk_index(timetk_idx = TRUE)
    )



})

test_that("tk_index(zooreg) test returns correct format.", {

    # Test if object has timetk index
    expect_true(tk_zooreg(FB_tbl, freq = 252, start = 2015, silent = TRUE) %>%
                          has_timetk_idx())

    # Return regularized dates
    test_index_3 <- tk_zooreg(FB_tbl, freq = 252, start = 2015, silent = TRUE) %>%
        tk_index(timetk_idx = FALSE)
    expect_equal(class(test_index_3), "numeric")
    expect_equal(length(test_index_3), 1008)

    # Return non-regularized dates (aka timetk index)
    test_index_4 <- tk_zooreg(FB_tbl, freq = 252, start = 2015, silent = TRUE) %>%
        tk_index(timetk_idx = TRUE)
    expect_equal(class(test_index_4), "Date")
    expect_equal(length(test_index_4), 1008)

})

test_that("tk_index(tbl) test returns correct format.", {

    # Test if object has timetk index
    expect_false(FB_tbl %>% has_timetk_idx())

    # Return vector of dates
    test_index_3 <- FB_tbl %>% tk_index(timetk_idx = FALSE)
    expect_equal(class(test_index_3), "Date")
    expect_equal(length(test_index_3), 1008)

    # No date or date time
    expect_error(tk_index(mtcars))

})

test_that("tk_index(xts) test returns correct format.", {

    # Test if object has timetk index
    expect_false(FB_xts %>% has_timetk_idx())

    # Return vector of dates
    test_index_5 <- FB_xts %>% tk_index(timetk_idx = FALSE)
    expect_equal(class(test_index_5), "Date")
    expect_equal(length(test_index_5), 1008)

})

test_that("tk_index(zoo) test returns correct format.", {

    # Test if object has timetk index
    expect_false(FB_zoo %>% has_timetk_idx())

    # Return vector of dates
    test_index_6 <- FB_zoo %>%
        tk_index(timetk_idx = FALSE)
    expect_equal(class(test_index_6), "Date")
    expect_equal(length(test_index_6), 1008)

})

# models and forecasts ----

fit_ets <- USAccDeaths %>%
    forecast::ets()

test_that("tk_index(ets) test returns correct format.", {

    # Test if object has timetk index
    expect_false(has_timetk_idx(fit_ets))

    # Return vector of numeric regularized dates
    test_index_7 <- fit_ets %>%
        tk_index(timetk_idx = FALSE)
    expect_equal(class(test_index_7), "numeric")
    expect_equal(length(test_index_7), 72)

})


test_that("tk_index(forecast) test returns correct format.", {

    fcast_ets <- fit_ets %>%
        forecast::forecast()

    # Test if object has timetk index
    expect_false(has_timetk_idx(fcast_ets))

    # Return vector of numeric regularized dates
    test_index_8 <- fcast_ets %>%
        tk_index(timetk_idx = FALSE)
    expect_equal(class(test_index_8), "numeric")
    expect_equal(length(test_index_8), 72)

})

test_that("tk_index(bats) test returns correct format.", {

    fit_bats <- USAccDeaths %>%
        forecast::bats()

    # Test if object has timetk index
    expect_false(has_timetk_idx(fit_bats))

    # Return vector of numeric regularized dates
    test_index_9 <- fit_bats %>%
        tk_index(timetk_idx = FALSE)
    expect_equal(class(test_index_9), "numeric")
    expect_equal(length(test_index_9), 72)

})


test_that("tk_index(arima) test returns correct format.", {

    fit_arima <- USAccDeaths %>%
        forecast::Arima()

    # Test if object has timetk index
    expect_false(has_timetk_idx(fit_arima))

    # Return vector of numeric regularized dates
    test_index_10 <- fit_arima %>%
        tk_index(timetk_idx = FALSE)
    expect_equal(class(test_index_10), "numeric")
    expect_equal(length(test_index_10), 72)

})

test_that("tk_index(HoltWinters) test returns correct format.", {

    fit_hw <- USAccDeaths %>%
        stats::HoltWinters()

    # Test if object has timetk index
    expect_false(has_timetk_idx(fit_hw))

    # Return vector of numeric regularized dates
    test_index_11 <- fit_hw %>%
        tk_index(timetk_idx = FALSE)
    expect_equal(class(test_index_11), "numeric")
    expect_equal(length(test_index_11), 72)

})

test_that("tk_index(stl) test returns correct format.", {

    fit_stl <- USAccDeaths %>%
        stats::stl(s.window = "periodic")

    # Test if object has timetk index
    expect_false(has_timetk_idx(fit_stl))

    # Return vector of numeric regularized dates
    test_index_12 <- fit_stl %>%
        tk_index(timetk_idx = FALSE)
    expect_equal(class(test_index_12), "numeric")
    expect_equal(length(test_index_12), 72)

})

test_that("tk_index(stlm) test returns correct format.", {

    fit_stlm <- USAccDeaths %>%
        forecast::stlm(s.window = "periodic")

    # Test if object has timetk index
    expect_false(has_timetk_idx(fit_stlm))

    # Return vector of numeric regularized dates
    test_index_13 <- fit_stlm %>%
        tk_index(timetk_idx = FALSE)
    expect_equal(class(test_index_13), "numeric")
    expect_equal(length(test_index_13), 72)

})

test_that("tk_index(StructTS) test returns correct format.", {

    fit_StructTS <- USAccDeaths %>%
        StructTS()

    # Test if object has timetk index
    expect_false(has_timetk_idx(fit_StructTS))

    # Return vector of numeric regularized dates
    test_index_15 <- fit_StructTS %>%
        tk_index(timetk_idx = FALSE)
    expect_equal(class(test_index_15), "numeric")
    expect_equal(length(test_index_15), 72)

})


test_that("tk_index(baggedETS) test returns correct format.", {

    fit_baggedETS <- WWWusage %>%
        forecast::baggedETS()

    # Test if object has timetk index
    expect_false(has_timetk_idx(fit_baggedETS))

    # Return vector of numeric regularized dates
    test_index_16 <- fit_baggedETS %>%
        tk_index(timetk_idx = FALSE)
    expect_equal(class(test_index_16), "numeric")
    expect_equal(length(test_index_16), 100)

})

test_that("tk_index(nnetar) test returns correct format.", {

    fit_nnetar <- USAccDeaths %>%
        forecast::nnetar()

    # Test if object has timetk index
    expect_false(has_timetk_idx(fit_nnetar))

    # Return vector of numeric regularized dates
    test_index_17 <- fit_nnetar %>%
        tk_index(timetk_idx = FALSE)
    expect_equal(class(test_index_17), "numeric")
    expect_equal(length(test_index_17), 72)

})

test_that("tk_index(fracdiff) test returns correct format.", {

    # ARFIMA model (class = "fracdiff")
    x <- fracdiff::fracdiff.sim( 100, ma=-.4, d=.3)$series
    fit_arfima <- forecast::arfima(x)

    # Test if object has timetk index
    expect_warning(
        expect_false(
            has_timetk_idx(fit_arfima)
            )
    )

    # Not designed to work with class numeric
    expect_warning(
        fit_arfima %>%
            tk_index(timetk_idx = FALSE)
    )

})

test_that("tk_index(decomposed.ts) test returns correct format.", {

    data_ts <- USAccDeaths %>%
        tk_tbl() %>%
        mutate(index = as_date(index)) %>%
        tk_ts(start = 1973, freq = 12, silent = T)

    fit <- decompose(data_ts)

    test <- has_timetk_idx(fit)
    expect_true(test)

    expect_equal(tk_index(fit) %>% class(), "numeric")

    expect_equal(tk_index(fit, timetk_idx = T) %>% class(), "Date")


})




