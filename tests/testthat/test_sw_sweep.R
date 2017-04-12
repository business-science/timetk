library(sweep)
library(forecast)
context("Testing sw_sweep function")


test_that("sw_sweep test returns tibble with correct rows and columns.", {

    # ETS forecasts
    test_sweep_1 <- USAccDeaths %>%
        ets %>%
        forecast(level = c(80, 95, 99)) %>%
        sw_sweep()

    expect_is(test_sweep_1, "tbl")
    expect_equal(nrow(test_sweep_1), 96)
    expect_equal(ncol(test_sweep_1), 9)
    expect_equal(colnames(test_sweep_1)[[9]], "hi.99")

    # Automatic ARIMA forecasts
    expect_equal(
        WWWusage %>%
            auto.arima %>%
            forecast(h=20) %>%
            sw_sweep() %>%
            nrow(),
        120
    )

    # ARFIMA forecasts
    x <- fracdiff::fracdiff.sim(100, ma=-.4, d=.3)$series
    expect_equal(
        # Warning: no index
        arfima(x) %>%
            forecast(h=30) %>%
            sw_sweep() %>%
            nrow(),
        130
    )

    # STL forecasts
    test_sweep_2 <- USAccDeaths %>%
        stlm(modelfunction=ar) %>%
        forecast(h=36) %>%
        sw_sweep()

    expect_is(test_sweep_2, "tbl")
    expect_equal(nrow(test_sweep_2), 108)
    expect_equal(ncol(test_sweep_2), 7)
    expect_equal(colnames(test_sweep_2)[[7]], "hi.95")

    # STLF
    test_sweep_3 <- AirPassengers %>%
        stlf(lambda=0) %>%
        sw_sweep()

    expect_is(test_sweep_3, "tbl")
    expect_equal(nrow(test_sweep_3), 168)
    expect_equal(ncol(test_sweep_3), 7)
    expect_equal(colnames(test_sweep_3)[[7]], "hi.95")

    # STL
    test_sweep_4 <- USAccDeaths %>%
        stl(s.window='periodic') %>%
        forecast %>%
        sw_sweep()

    expect_is(test_sweep_4, "tbl")
    expect_equal(nrow(test_sweep_4), 96)
    expect_equal(ncol(test_sweep_4), 7)
    expect_equal(colnames(test_sweep_4)[[7]], "hi.95")

    # TBATS forecast
    test_sweep_5 <- USAccDeaths %>%
        tbats %>%
        forecast(level = c(80, 95)) %>%
        sw_sweep()

    expect_is(test_sweep_5, "tbl")
    expect_equal(nrow(test_sweep_5), 96)
    expect_equal(ncol(test_sweep_5), 7)
    expect_equal(colnames(test_sweep_5)[[7]], "hi.95")

    # # sweep.default()
    # expect_warning(sw_sweep(datasets::mtcars)) # Returns original data and warning message

})










