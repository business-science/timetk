context("Test tk_tbl")

FB_tbl <- FANG %>% dplyr::filter(symbol == "FB")

# FUNCTION: tk_tbl -----

# tbl to tbl -----
test_that("tbl tot tbl test returns tibble with correct rows and columns.", {
    test_tbl_1 <- tk_tbl(FB_tbl, preserve_index = FALSE, rename_index = "date")
    expect_is(test_tbl_1, "tbl")
    expect_equal(nrow(test_tbl_1), 1008)
    expect_equal(ncol(test_tbl_1), 8)
    expect_equal(colnames(test_tbl_1)[[2]], "date")
    expect_warning(tk_tbl(FB_tbl, preserve_index = T)) # Expect warning - No index to preserve
})

# xts to tbl -----
FB_xts <- tk_xts(FB_tbl, select = -c(date, symbol), date_var = date)
test_that("xts to tbl test returns tibble with correct rows and columns.", {
    test_tbl_2 <- tk_tbl(FB_xts, preserve_index = TRUE, rename_index = "date")
    expect_equal(nrow(test_tbl_2), 1008)
    expect_equal(ncol(test_tbl_2), 7)
    expect_equal(colnames(test_tbl_2)[[1]], "date")
    expect_equal(ncol(tk_tbl(FB_xts, preserve_index = FALSE, rename_index = "date")), 6)
})

# zoo to tbl -----
FB_zoo <- tk_zoo(FB_tbl, silent = TRUE)
test_that("zoo to tbl test returns tibble with correct rows and columns.", {
    test_tbl_3 <- tk_tbl(FB_zoo, preserve_index = TRUE, rename_index = "date")
    expect_equal(nrow(test_tbl_3), 1008)
    expect_equal(ncol(test_tbl_3), 7)
    expect_equal(colnames(test_tbl_3)[[1]], "date")
    expect_equal(ncol(tk_tbl(FB_zoo, preserve_index = FALSE, rename_index = "date")), 6)
})

# zooreg to tbl -----
FB_zooreg <- tk_zooreg(FB_tbl, start = 2015, frequency = 250, silent = TRUE)
test_that("zooreg to tbl test returns tibble with correct rows and columns.", {
    test_tbl_3a <- tk_tbl(FB_zooreg, preserve_index = TRUE, rename_index = "date")
    expect_equal(nrow(test_tbl_3a), 1008)
    expect_equal(ncol(test_tbl_3a), 7)
    expect_equal(colnames(test_tbl_3a)[[1]], "date")
    expect_equal(ncol(tk_tbl(FB_zooreg, preserve_index = FALSE, rename_index = "date")), 6)

    # zooreg reverse coercion test ----
    test_tbl_3b <- FB_zooreg %>%
        tk_tbl(rename_index = "date", timetk_idx = TRUE)
    expect_identical(test_tbl_3b, FB_tbl %>% dplyr::select(-symbol))

    # Test different start/end types

    # Inherits date
    zooreg_1 <- zoo::zooreg(1:5, start = as.Date("2000-01-01"))
    expect_true(inherits(tk_tbl(zooreg_1)$index, "Date"))

    # Inherits zoo yearmon
    zooreg_2 <- zoo::zooreg(1:5, end = zoo::yearmon(2000))
    expect_true(inherits(tk_tbl(zooreg_2)$index, "yearmon"))

    # Inherits zoo yearqtr
    zooreg_3 <- zoo::zooreg(1:5, start = zoo::yearqtr(2000), frequency = 4)
    expect_true(inherits(tk_tbl(zooreg_3)$index, "yearqtr"))

})

# ts to tbl -----
FB_mts <- tk_ts(FB_tbl, select = -c(date, symbol), start = 2015, frequency = 252)
test_that("mts to tbl test returns tibble with correct rows and columns.", {
    test_tbl_4 <- tk_tbl(FB_mts, preserve_index = TRUE, rename_index = "date")
    expect_equal(nrow(test_tbl_4), 1008)
    expect_equal(ncol(test_tbl_4), 7)
    expect_equal(colnames(test_tbl_4)[[1]], "date")
    expect_equal(ncol(tk_tbl(FB_mts, preserve_index = FALSE, rename_index = "date")), 6)

    # Warning if no index to preserve
    expect_warning(tk_tbl(tk_ts(FB_mts, start = 1), select = -date, preserve_index = T))

    # Warning if no timetk index attribute
    expect_warning(
        WWWusage %>%
            tk_tbl(timetk_idx = TRUE)
        )

    # ts reverse coercion test ----
    expect_tz_warning(test_tbl_4b <- tk_tbl(FB_mts, rename_index = "date", timetk_idx = TRUE))
    expect_identical(test_tbl_4b, FB_tbl %>% dplyr::select(-symbol))
})

# matrix to tbl -----
FB_matrix <- FB_xts %>% as.matrix()
test_that("matrix to tbl test returns tibble with correct rows and columns.", {
    test_tbl_5 <- tk_tbl(FB_matrix, preserve_index = TRUE, rename_index = "date")
    expect_equal(nrow(test_tbl_5), 1008)
    expect_equal(ncol(test_tbl_5), 7)
    expect_equal(colnames(test_tbl_5)[[1]], "date")
    expect_equal(ncol(tk_tbl(FB_matrix, preserve_index = FALSE, rename_index = "date")), 6)
    # Warning if no index to prserve
    rownames(FB_matrix) <- NULL
    expect_warning(tk_tbl(FB_matrix))
})


# timeSeries::timeSeries to tbl -----

test_that("timeSeries to tbl test returns tibble with correct rows and columns.", {
    skip_if_not_installed("timeSeries")
    test_timeSeries <- timeSeries::timeSeries(1:100, timeDate::timeSequence(length.out = 100, by = "sec"))
    test_tbl_6 <- tk_tbl(test_timeSeries, preserve_index = TRUE, rename_index = "date-time")
    expect_equal(nrow(test_tbl_6), 100)
    expect_equal(ncol(test_tbl_6), 2)
    expect_equal(colnames(test_tbl_6)[[1]], "date-time")
})

# tseries::irts to tbl -----
n <- 10
t <- cumsum(rexp(n, rate = 0.1))
v <- rnorm(n)
test_that("tseries to tbl test returns tibble with correct rows and columns.", {
    skip_if_not_installed("tseries")
    test_tseries <- tseries::irts(t, v)

    test_tbl_7 <- tk_tbl(test_tseries, preserve_index = TRUE, rename_index = "date-time")
    expect_equal(nrow(test_tbl_7), 10)
    expect_equal(ncol(test_tbl_7), 2)
    expect_equal(colnames(test_tbl_7)[[1]], "date-time")
})

# forecast::msts to tbl -----
test_that("forecast::msts to tbl test returns tibble with correct rows and columns.", {
    test_msts <- forecast::msts(forecast::taylor, seasonal.periods=c(48,336), start=2000+22/52)

    test_tbl_8 <- tk_tbl(test_msts, preserve_index = TRUE, rename_index = "index")
    expect_equal(nrow(test_tbl_8), 4032)
    expect_equal(ncol(test_tbl_8), 2)
    expect_equal(colnames(test_tbl_8)[[1]], "index")
})

# default -----
test_that("forecast::msts to tbl test returns tibble with correct rows and columns.", {

    # Test a numeric integer
    test_default <- 4

    expect_warning(
        tk_tbl(test_default, preserve_index = TRUE, rename_index = "index")
    )
    test_tbl_9 <- tk_tbl(test_default, preserve_index = FALSE, rename_index = "index")
    expect_equal(nrow(test_tbl_9), 1)
    expect_equal(ncol(test_tbl_9), 1)

})
