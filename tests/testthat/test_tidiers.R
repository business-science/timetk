library(sweep)
library(forecast)
context("Testing sw_tidy, sw_glance, sw_augment functions")


# FUNCTION: sw_*.default -----
test_that("sw_*.default test returns tibble with correct rows and columns.", {

    # lm model test (class = "lm") ----
    fit_lm <- lm(mtcars$mpg ~ mtcars$wt)

    # sw_tidy.default() ----
    test <- sw_tidy(fit_lm)
    expect_is(test, "tbl")
    # expect_false(any(lapply(test, is.factor) %>% unlist())) # No factors
    expect_equal(nrow(test), 2)
    expect_equal(ncol(test), 5)

    # sw_glance.default() ----
    test <- sw_glance(fit_lm)
    expect_is(test, "tbl")
    # expect_false(any(lapply(test, is.factor) %>% unlist())) # No factors
    expect_equal(nrow(test), 1)
    expect_equal(ncol(test), 11)

    # sw_augment.default() ----
    test <- sw_augment(fit_lm)
    expect_is(test, "tbl")
    # expect_false(any(lapply(test, is.factor) %>% unlist())) # No factors
    expect_equal(nrow(test), 32)
    expect_equal(ncol(test), 9)

})


# FUNCTION: sw_*.Arima -----
test_that("sw_*.Arima test returns tibble with correct rows and columns.", {

    # forecast::Arima (class = "lm") ----
    fit_arima <- WWWusage %>%
        forecast::auto.arima()

    # sw_tidy.Arima() ----
    test <- sw_tidy(fit_arima)
    expect_is(test, "tbl")
    # expect_false(any(lapply(test, is.factor) %>% unlist())) # No factors
    expect_equal(nrow(test), 2)
    expect_equal(ncol(test), 5)

    # sw_glance.Arima() ----
    test <- sw_glance(fit_arima)
    expect_is(test, "tbl")
    # expect_false(any(lapply(test, is.factor) %>% unlist())) # No factors
    expect_equal(nrow(test), 1)
    expect_equal(ncol(test), 12)

    # sw_augment.Arima() ----
    test <- suppressWarnings(sw_augment(fit_arima))
    expect_is(test, "tbl")
    # expect_false(any(lapply(test, is.factor) %>% unlist())) # No factors
    expect_equal(nrow(test), 100)
    expect_equal(ncol(test), 3)

    # stats::Arima (class = "lm") ----
    fit_arima_stats <- WWWusage %>%
        stats::arima(order = c(1, 1, 1))

    # sw_tidy.Arima() ----
    test <- sw_tidy(fit_arima_stats)
    expect_is(test, "tbl")
    # expect_false(any(lapply(test, is.factor) %>% unlist())) # No factors
    expect_equal(nrow(test), 2)
    expect_equal(ncol(test), 5)

    # sw_glance.Arima() ----
    test <- suppressWarnings(sw_glance(fit_arima_stats))
    expect_is(test, "tbl")
    # expect_false(any(lapply(test, is.factor) %>% unlist())) # No factors
    expect_equal(nrow(test), 1)
    expect_equal(ncol(test), 10)
    expect_warning(sw_glance(fit_arima_stats)) # Warning: training accuracy must be within sample

    # sw_augment.Arima() ----
    test <- suppressWarnings(sw_augment(fit_arima_stats))
    expect_is(test, "tbl")
    # expect_false(any(lapply(test, is.factor) %>% unlist())) # No factors
    expect_equal(nrow(test), 100)
    expect_equal(ncol(test), 1) # stats::arima() returns only one column for residuals
    expect_warning(sw_augment(fit_arima_stats)) # stats::arima() vs forecast::Arima()

})

# FUNCTION: sw_*.ets -----
test_that("sw_*.ets test returns tibble with correct rows and columns.", {

    # forecast::Arima (class = "lm") ----
    fit_ets <- WWWusage %>%
        forecast::ets()

    # sw_tidy.Arima() ----
    test <- sw_tidy(fit_ets)
    expect_is(test, "tbl")
    # expect_false(any(lapply(test, is.factor) %>% unlist())) # No factors
    expect_equal(nrow(test), 5)
    expect_equal(ncol(test), 2)

    # sw_glance.Arima() ----
    test <- sw_glance(fit_ets)
    expect_is(test, "tbl")
    # expect_false(any(lapply(test, is.factor) %>% unlist())) # No factors
    expect_equal(nrow(test), 1)
    expect_equal(ncol(test), 12)

    # sw_augment.Arima() ----
    test <- suppressWarnings(sw_augment(fit_ets))
    expect_is(test, "tbl")
    # expect_false(any(lapply(test, is.factor) %>% unlist())) # No factors
    expect_equal(nrow(test), 100)
    expect_equal(ncol(test), 3)

})
