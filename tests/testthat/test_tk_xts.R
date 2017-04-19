library(timekit)
context("Test tk_xts")

AAPL_tbl <- tidyquant::tq_get("AAPL", from = "2015-01-01", to = "2016-12-31")
AAPL_zoo <- tk_zoo(AAPL_tbl, silent = TRUE)

# FUNCTION: tk_xts -----

# tbl to xts -----
test_that("tbl to xts test returns xts with correct rows and columns.", {
    # Use date column to specify order
    test_xts_1 <- tk_xts(AAPL_tbl, select = -date, date_var = date)
    expect_equal(nrow(test_xts_1), 504)
    expect_equal(ncol(test_xts_1), 6)

    # Use order.by to specify order
    test_xts_2 <- tk_xts(AAPL_tbl[,-1], order.by = AAPL_tbl$date)
    expect_equal(nrow(test_xts_2), 504)
    expect_equal(ncol(test_xts_2), 6)

    # Auto-index date
    expect_message(tk_xts(AAPL_tbl, select = -date))  # using `date` column as date

    # Auto-drop columns
    expect_warning(tk_xts(AAPL_tbl, date_var = date)) # dropping date column

    # Auto-index and auto-drop columns
    expect_message(expect_warning(tk_xts(AAPL_tbl))) # using `date` column as date, dropping date column

    # NSE
    select     <- "adjusted"
    date_var   <- "date"
    test_xts_3 <- tk_xts_(AAPL_tbl, select = select, date_var = date_var)
    expect_equal(nrow(test_xts_3), 504)
    expect_equal(ncol(test_xts_3), 1)
    expect_equal(colnames(test_xts_3), select)

})


# zoo to xts -----
# Default is xts::xts() for other objects; only test zoo
test_that("zoo to xts test returns xts with correct rows and columns.", {
    # Use date column to specify order
    test_xts_4 <- tk_xts(AAPL_zoo)
    expect_equal(nrow(test_xts_4), 504)
    expect_equal(ncol(test_xts_4), 6)

    # Use order.by to specify order
    test_xts_5 <- tk_xts(AAPL_zoo, order.by = zoo::index(AAPL_zoo))
    expect_equal(nrow(test_xts_5), 504)
    expect_equal(ncol(test_xts_5), 6)

    # Warning if using select field
    expect_warning(tk_xts(AAPL_zoo, select = -date))  # only for use with data.frames

    # Warning if using date_var field
    expect_warning(tk_xts(AAPL_zoo, date_var = date)) # only for use with data.frames

    # Two warnings: select and date_var args only applicable to data.frames
    expect_warning(tk_xts(AAPL_zoo, select = -date, date_var = date))

})

# ts to xts -----
# Default is xts::xts() for other objects; only test zoo
test_that("ts to xts test returns xts with correct rows and columns.", {
    # Use date column to specify order
    test_xts_6 <- tk_ts(AAPL_tbl, select = -date) %>%
        tk_xts()
    expect_equal(nrow(test_xts_6), 504)
    expect_equal(ncol(test_xts_6), 6)

})
