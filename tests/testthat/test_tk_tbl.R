library(timetk)
library(forecast)
context("Test tk_tbl")


AAPL_tbl <- tidyquant::tq_get("AAPL", from = "2015-01-01", to = "2016-12-31")

# FUNCTION: tk_tbl -----

# tbl to tbl -----
test_that("tbl tot tbl test returns tibble with correct rows and columns.", {
    test_tbl_1 <- tk_tbl(AAPL_tbl, preserve_index = F, rename_index = "date")
    expect_is(test_tbl_1, "tbl")
    expect_equal(nrow(test_tbl_1), 504)
    expect_equal(ncol(test_tbl_1), 7)
    expect_equal(colnames(test_tbl_1)[[1]], "date")
    expect_warning(tk_tbl(AAPL_tbl, preserve_index = T)) # Expect warning - No index to preserve
})

# xts to tbl -----
AAPL_xts <- tk_xts(AAPL_tbl, select = -date, date_var = date)
test_that("xts to tbl test returns tibble with correct rows and columns.", {
    test_tbl_2 <- tk_tbl(AAPL_xts, preserve_index = T, rename_index = "date")
    expect_equal(nrow(test_tbl_2), 504)
    expect_equal(ncol(test_tbl_2), 7)
    expect_equal(colnames(test_tbl_2)[[1]], "date")
    expect_equal(ncol(tk_tbl(AAPL_xts, preserve_index = F, rename_index = "date")), 6)
})

# zoo to tbl -----
AAPL_zoo <- tk_zoo(AAPL_tbl, select = -date, date_var = date)
test_that("zoo to tbl test returns tibble with correct rows and columns.", {
    test_tbl_3 <- tk_tbl(AAPL_zoo, preserve_index = T, rename_index = "date")
    expect_equal(nrow(test_tbl_3), 504)
    expect_equal(ncol(test_tbl_3), 7)
    expect_equal(colnames(test_tbl_3)[[1]], "date")
    expect_equal(ncol(tk_tbl(AAPL_zoo, preserve_index = F, rename_index = "date")), 6)
})

# zooreg to tbl -----
AAPL_zooreg <- tk_zooreg(AAPL_tbl, select = -date, start = 2015, frequency = 250)
test_that("zooreg to tbl test returns tibble with correct rows and columns.", {
    test_tbl_3a <- tk_tbl(AAPL_zooreg, preserve_index = T, rename_index = "date")
    expect_equal(nrow(test_tbl_3a), 504)
    expect_equal(ncol(test_tbl_3a), 7)
    expect_equal(colnames(test_tbl_3a)[[1]], "date")
    expect_equal(ncol(tk_tbl(AAPL_zooreg, preserve_index = F, rename_index = "date")), 6)

    # zooreg reverse coercion test ----
    test_tbl_3b <- AAPL_zooreg %>%
        tk_tbl(rename_index = "date", timetk_idx = TRUE)
    expect_identical(test_tbl_3b, AAPL_tbl)

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
AAPL_mts <- tk_ts(AAPL_tbl, select = -date, start = 2015, frequency = 252)
test_that("mts to tbl test returns tibble with correct rows and columns.", {
    test_tbl_4 <- tk_tbl(AAPL_mts, preserve_index = T, rename_index = "date")
    expect_equal(nrow(test_tbl_4), 504)
    expect_equal(ncol(test_tbl_4), 7)
    expect_equal(colnames(test_tbl_4)[[1]], "date")
    expect_equal(ncol(tk_tbl(AAPL_mts, preserve_index = F, rename_index = "date")), 6)

    # Warning if no index to preserve
    expect_warning(tk_tbl(tk_ts(AAPL_mts, start = 1), select = -date, preserve_index = T))

    # Warning if no timetk index attribute
    expect_warning(
        WWWusage %>%
            tk_tbl(timetk_idx = TRUE)
        )

    # ts reverse coercion test ----
    test_tbl_4b <- AAPL_mts %>%
        tk_tbl(rename_index = "date", timetk_idx = TRUE)
    expect_identical(test_tbl_4b, AAPL_tbl)
})

# matrix to tbl -----
AAPL_matrix <- AAPL_xts %>% as.matrix()
test_that("matrix to tbl test returns tibble with correct rows and columns.", {
    test_tbl_5 <- tk_tbl(AAPL_matrix, preserve_index = T, rename_index = "date")
    expect_equal(nrow(test_tbl_5), 504)
    expect_equal(ncol(test_tbl_5), 7)
    expect_equal(colnames(test_tbl_5)[[1]], "date")
    expect_equal(ncol(tk_tbl(AAPL_matrix, preserve_index = F, rename_index = "date")), 6)
    # Warning if no index to prserve
    rownames(AAPL_matrix) <- NULL
    expect_warning(tk_tbl(AAPL_matrix))
})


# timeSeries::timeSeries to tbl -----
test_timeSeries <- timeSeries::timeSeries(1:100, timeDate::timeSequence(length.out = 100, by = "sec"))
test_that("timeSeries to tbl test returns tibble with correct rows and columns.", {
    test_tbl_6 <- tk_tbl(test_timeSeries, preserve_index = T, rename_index = "date-time")
    expect_equal(nrow(test_tbl_6), 100)
    expect_equal(ncol(test_tbl_6), 2)
    expect_equal(colnames(test_tbl_6)[[1]], "date-time")
})

# tseries::irts to tbl -----
n <- 10
t <- cumsum(rexp(n, rate = 0.1))
v <- rnorm(n)
test_tseries <- tseries::irts(t, v)
test_that("tseries to tbl test returns tibble with correct rows and columns.", {
    test_tbl_7 <- tk_tbl(test_tseries, preserve_index = T, rename_index = "date-time")
    expect_equal(nrow(test_tbl_7), 10)
    expect_equal(ncol(test_tbl_7), 2)
    expect_equal(colnames(test_tbl_7)[[1]], "date-time")
})

# forecast::msts to tbl -----
test_msts <- forecast::msts(forecast::taylor, seasonal.periods=c(48,336), start=2000+22/52)
test_that("forecast::msts to tbl test returns tibble with correct rows and columns.", {
    test_tbl_8 <- tk_tbl(test_msts, preserve_index = T, rename_index = "index")
    expect_equal(nrow(test_tbl_8), 4032)
    expect_equal(ncol(test_tbl_8), 2)
    expect_equal(colnames(test_tbl_8)[[1]], "index")
})

# default -----
test_that("forecast::msts to tbl test returns tibble with correct rows and columns.", {

    # Test a numeric integer
    test_default <- 4

    expect_warning(
        tk_tbl(test_default, preserve_index = T, rename_index = "index")
    )
    test_tbl_9 <- tk_tbl(test_default, preserve_index = F, rename_index = "index")
    expect_equal(nrow(test_tbl_9), 1)
    expect_equal(ncol(test_tbl_9), 1)

})
