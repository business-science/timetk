library(sweep)
library(forecast)
context("Testing sw_tidy, sw_glance, sw_augment functions")


# FUNCTION: sw_*.default -----
test_that("sw_*.default test returns tibble with correct rows and columns.", {

    # lm() ----
    fit_lm <- lm(mtcars$mpg ~ mtcars$wt)

    # sw_tidy ----
    test <- sw_tidy(fit_lm)
    expect_is(test, "tbl")
    # expect_false(any(lapply(test, is.factor) %>% unlist())) # No factors
    expect_equal(nrow(test), 2)
    expect_equal(ncol(test), 5)

    # sw_glance ----
    test <- sw_glance(fit_lm)
    expect_is(test, "tbl")
    # expect_false(any(lapply(test, is.factor) %>% unlist())) # No factors
    expect_equal(nrow(test), 1)
    expect_equal(ncol(test), 11)

    # sw_augment ----
    test <- sw_augment(fit_lm)
    expect_is(test, "tbl")
    # expect_false(any(lapply(test, is.factor) %>% unlist())) # No factors
    expect_equal(nrow(test), 32)
    expect_equal(ncol(test), 9)

})


# FUNCTION: sw_*.Arima -----
test_that("sw_*.Arima test returns tibble with correct rows and columns.", {

    # Arima ----
    fit_arima <- WWWusage %>%
        forecast::auto.arima()

    # sw_tidy ----
    test <- sw_tidy(fit_arima)
    expect_is(test, "tbl")
    # expect_false(any(lapply(test, is.factor) %>% unlist())) # No factors
    expect_equal(nrow(test), 2)
    expect_equal(ncol(test), 2)

    # sw_glance ----
    test <- sw_glance(fit_arima)
    expect_is(test, "tbl")
    # expect_false(any(lapply(test, is.factor) %>% unlist())) # No factors
    expect_equal(nrow(test), 1)
    expect_equal(ncol(test), 12)

    # sw_augment ----
    test <- sw_augment(fit_arima, index_rename = "date")
    expect_is(test, "tbl")
    # expect_false(any(lapply(test, is.factor) %>% unlist())) # No factors
    expect_equal(nrow(test), 100)
    expect_equal(ncol(test), 4)
    expect_equal(colnames(test)[[1]], "date")

    # arima() ----
    fit_arima_stats <- WWWusage %>%
        stats::arima(order = c(1, 1, 1))

    # sw_tidy ----
    test <- sw_tidy(fit_arima_stats)
    expect_is(test, "tbl")
    # expect_false(any(lapply(test, is.factor) %>% unlist())) # No factors
    expect_equal(nrow(test), 2)
    expect_equal(ncol(test), 2)

    # sw_glance ----
    test <- suppressWarnings(sw_glance(fit_arima_stats))
    expect_is(test, "tbl")
    # expect_false(any(lapply(test, is.factor) %>% unlist())) # No factors
    expect_equal(nrow(test), 1)
    expect_equal(ncol(test), 10)
    expect_warning(sw_glance(fit_arima_stats)) # Warning: training accuracy must be within sample

    # sw_augment ----
    test <- suppressWarnings(sw_augment(fit_arima_stats, index_rename = "date"))
    expect_is(test, "tbl")
    # expect_false(any(lapply(test, is.factor) %>% unlist())) # No factors
    expect_equal(nrow(test), 100)
    expect_equal(ncol(test), 2) # stats::arima() returns only one column for residuals
    expect_equal(colnames(test)[[1]], "date")
    expect_warning(sw_augment(fit_arima_stats)) # stats::arima() vs forecast::Arima()

})

# FUNCTION: sw_*.ets -----
test_that("sw_*.ets test returns tibble with correct rows and columns.", {

    # ets() ----
    fit_ets <- WWWusage %>%
        forecast::ets()

    # sw_tidy ----
    test <- sw_tidy(fit_ets)
    expect_is(test, "tbl")
    # expect_false(any(lapply(test, is.factor) %>% unlist())) # No factors
    expect_equal(nrow(test), 5)
    expect_equal(ncol(test), 2)

    # sw_glance ----
    test <- sw_glance(fit_ets)
    expect_is(test, "tbl")
    # expect_false(any(lapply(test, is.factor) %>% unlist())) # No factors
    expect_equal(nrow(test), 1)
    expect_equal(ncol(test), 12)

    # sw_augment ----
    test <- sw_augment(fit_ets, index_rename = "date")
    expect_is(test, "tbl")
    expect_equal(nrow(test), 100)
    expect_equal(ncol(test), 4)
    expect_equal(colnames(test)[[1]], "date")

    # sw_tidy_decomp ----
    test <- sw_tidy_decomp(fit_ets)
    expect_is(test, "tbl")
    expect_equal(nrow(test), 101)
    expect_equal(ncol(test), 4)

})

# FUNCTION: sw_*.bats -----
test_that("sw_*.bats test returns tibble with correct rows and columns.", {

    # bats()  ----
    fit_bats <- WWWusage %>%
        forecast::bats()

    # sw_tidy ----
    test <- sw_tidy(fit_bats)
    expect_is(test, "tbl")
    expect_equal(nrow(test), 7)
    expect_equal(ncol(test), 2)

    # sw_glance ----
    test <- sw_glance(fit_bats)
    expect_is(test, "tbl")
    expect_equal(nrow(test), 1)
    expect_equal(ncol(test), 12)

    # sw_augment ----
    test <- sw_augment(fit_bats, index_rename = "date")
    expect_is(test, "tbl")
    expect_equal(nrow(test), 100)
    expect_equal(ncol(test), 4)
    expect_equal(colnames(test)[[1]], "date")

    # sw_tidy_decomp ----
    test <- sw_tidy_decomp(fit_bats)
    expect_is(test, "tbl")
    expect_equal(nrow(test), 100)
    expect_equal(ncol(test), 3)


    # tbats()  ----
    fit_tbats <- WWWusage %>%
        forecast::tbats()

    # sw_tidy ----
    test <- sw_tidy(fit_tbats)
    expect_is(test, "tbl")
    expect_equal(nrow(test), 7)
    expect_equal(ncol(test), 2)

    # sw_glance ----
    test <- sw_glance(fit_tbats)
    expect_is(test, "tbl")
    expect_equal(nrow(test), 1)
    expect_equal(ncol(test), 12)

    # sw_augment ----
    test <- sw_augment(fit_tbats, index_rename = "date")
    expect_is(test, "tbl")
    expect_equal(nrow(test), 100)
    expect_equal(ncol(test), 4)
    expect_equal(colnames(test)[[1]], "date")

    # sw_tidy_decomp ----
    test <- sw_tidy_decomp(fit_tbats)
    expect_is(test, "tbl")
    expect_equal(nrow(test), 100)
    expect_equal(ncol(test), 3)

})

# FUNCTION: sw_*.HoltWinters -----
test_that("sw_*.HoltWinters test returns tibble with correct rows and columns.", {

    # HoltWinters()  ----
    fit_hw <- USAccDeaths %>%
        HoltWinters()

    # sw_tidy ----
    test <- sw_tidy(fit_hw)
    expect_is(test, "tbl")
    expect_equal(nrow(test), 17)
    expect_equal(ncol(test), 2)

    # sw_glance ----
    test <- sw_glance(fit_hw)
    expect_is(test, "tbl")
    expect_equal(nrow(test), 1)
    expect_equal(ncol(test), 12)

    # sw_augment ----
    # Test normal
    test <- sw_augment(fit_hw, index_rename = "date")
    expect_is(test, "tbl")
    expect_equal(nrow(test), 72)
    expect_equal(ncol(test), 4)

    # Test passing data
    test <- sw_augment(fit_hw, data = USAccDeaths, index_rename = "date")
    expect_is(test, "tbl")
    expect_equal(nrow(test), 72)
    expect_equal(ncol(test), 4)

    # Test passing incorrect data
    expect_warning(sw_augment(fit_hw,
                              data = sw_ts(USAccDeaths[1:50], freq = 12, start = 1973),
                              index_rename = "date")
                   )

    # sw_tidy_decomp ----
    test <- sw_tidy_decomp(fit_hw)
    expect_is(test, "tbl")
    expect_equal(nrow(test), 72)
    expect_equal(ncol(test), 5)

})

# FUNCTION: sw_*.nnetar -----
test_that("sw_*.nnetar test returns tibble with correct rows and columns.", {

    # nnetar()  ----
    fit_nnetar <- USAccDeaths %>%
        nnetar()

    # sw_tidy ----
    test <- sw_tidy(fit_nnetar)
    expect_is(test, "tbl")
    # expect_false(any(lapply(test, is.factor) %>% unlist())) # No factors
    expect_equal(nrow(test), 4)
    expect_equal(ncol(test), 2)

    # sw_glance ----
    test <- sw_glance(fit_nnetar)
    expect_is(test, "tbl")
    # expect_false(any(lapply(test, is.factor) %>% unlist())) # No factors
    expect_equal(nrow(test), 1)
    expect_equal(ncol(test), 12)

    # sw_augment ----
    # Test normal
    test <- sw_augment(fit_nnetar, index_rename = "date")
    expect_is(test, "tbl")
    # expect_false(any(lapply(test, is.factor) %>% unlist())) # No factors
    expect_equal(nrow(test), 72)
    expect_equal(ncol(test), 4)

    # Test passing data
    test <- sw_augment(fit_nnetar, data = USAccDeaths, index_rename = "date")
    expect_is(test, "tbl")
    # expect_false(any(lapply(test, is.factor) %>% unlist())) # No factors
    expect_equal(nrow(test), 72)
    expect_equal(ncol(test), 4)

    # Test passing incorrect data
    expect_warning(sw_augment(fit_nnetar,
                              data = sw_ts(USAccDeaths[1:50], freq = 12, start = 1973),
                              index_rename = "date")
    )

})


# FUNCTION: sw_*.StructTS -----
test_that("sw_*.StructTS test returns tibble with correct rows and columns.", {

    # StructTS()  ----
    fit_StructTS <- WWWusage %>%
        StructTS()

    # sw_tidy ----
    test <- sw_tidy(fit_StructTS)
    expect_is(test, "tbl")
    # expect_false(any(lapply(test, is.factor) %>% unlist())) # No factors
    expect_equal(nrow(test), 3)
    expect_equal(ncol(test), 2)

    # sw_glance ----
    test <- sw_glance(fit_StructTS)
    expect_is(test, "tbl")
    # expect_false(any(lapply(test, is.factor) %>% unlist())) # No factors
    expect_equal(nrow(test), 1)
    expect_equal(ncol(test), 12)

    # sw_augment ----
    # Test normal
    test <- sw_augment(fit_StructTS, index_rename = "date")
    expect_is(test, "tbl")
    # expect_false(any(lapply(test, is.factor) %>% unlist())) # No factors
    expect_equal(nrow(test), 100)
    expect_equal(ncol(test), 4)

})

# FUNCTION sw_*.stl -----
test_that("sw_*.stl test returns tibble with correct rows and columns.", {

    # stl()  ----
    fit_stl <- USAccDeaths %>%
        stl(s.window = 'periodic')

    # sw_tidy_decomp ----
    test <- sw_tidy_decomp(fit_stl)
    expect_is(test, "tbl")
    expect_equal(nrow(test), 72)
    expect_equal(ncol(test), 5)

    # stlm() ----
    fit_stlm <- USAccDeaths %>%
        stlm(modelfunction=ar)

    # sw_tidy_decomp ----
    test <- sw_tidy_decomp(fit_stlm)
    expect_is(test, "tbl")
    expect_equal(nrow(test), 72)
    expect_equal(ncol(test), 5)
})

# FUNCTION sw_*.decomposed.ts ----
test_that("sw_*.decomposed.ts test returns tibble with correct rows and columns.", {

    # decompose()  ----
    fit_decomposed_ts <- USAccDeaths %>%
        decompose()

    # sw_tidy_decomp ----
    test <- sw_tidy_decomp(fit_decomposed_ts)
    expect_is(test, "tbl")
    expect_equal(nrow(test), 72)
    expect_equal(ncol(test), 6)

})
