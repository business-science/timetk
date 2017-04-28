library(timekit)
context("Test tk_ts")

AAPL_tbl <- tidyquant::tq_get("AAPL", from = "2015-01-01", to = "2016-12-31")
AAPL_xts <- tk_xts(AAPL_tbl, silent = TRUE)
AAPL_zoo <- tk_zoo(AAPL_tbl, silent = TRUE)

# FUNCTION: tk_ts -----

# tbl to ts -----
test_that("tbl to ts test returns ts with correct rows and columns.", {
    # Use date column to specify order
    test_ts_1 <- tk_ts(AAPL_tbl, select = -date, freq = 252, start = 2015)
    expect_equal(nrow(test_ts_1), 504)
    expect_equal(ncol(test_ts_1), 6)

    # Reverse coercion
    test_ts_2 <- test_ts_1 %>%
        tk_tbl(rename_index = "date", timekit_idx = TRUE)
    expect_identical(AAPL_tbl, test_ts_2)

    # Auto-drop columns
    expect_warning(tk_ts(AAPL_tbl, freq = 252, start = 2015)) # dropping date column

    # NSE
    select     <- "adjusted"
    test_ts_3 <- tk_ts_(AAPL_tbl, select = select, freq = 252, start = 2015)
    expect_equal(nrow(test_ts_3), 504)
    expect_equal(ncol(test_ts_3), 1)
    expect_equal(colnames(test_ts_3), select)

})

# xts to ts -----
test_that("xts to ts test returns ts with correct rows and columns.", {
    # Use date column to specify order
    test_ts_4 <- tk_ts(AAPL_xts, freq = 252, start = 2015)
    expect_equal(nrow(test_ts_4), 504)
    expect_equal(ncol(test_ts_4), 6)

    # Reverse coercion
    test_ts_5 <- test_ts_4 %>%
        tk_xts()
    expect_identical(AAPL_xts, test_ts_5)

    # Warning if using select field
    expect_warning(tk_ts(AAPL_xts, select = -date,
                         freq = 252, start = 2015))  # only for use with data.frames

    # Warning if using date_var field
    expect_error(tk_ts(AAPL_xts, date_var = date,
                       freq = 252, start = 2015)) # date_var not used

})

# zoo to ts -----
test_that("zoo to ts test returns ts with correct rows and columns.", {
    # Use date column to specify order
    test_ts_6 <- tk_ts(AAPL_zoo, freq = 252, start = 2015)
    expect_equal(nrow(test_ts_6), 504)
    expect_equal(ncol(test_ts_6), 6)

    # Reverse coercion
    test_ts_7 <- test_ts_6 %>%
        tk_zoo()
    expect_identical(AAPL_zoo, test_ts_7)

    # Warning if using select field
    expect_warning(tk_ts(AAPL_xts, select = -date,
                         freq = 252, start = 2015))  # only for use with data.frames

    # Warning if using date_var field
    expect_error(tk_ts(AAPL_xts, date_var = date,
                       freq = 252, start = 2015)) # date_var not used

})



