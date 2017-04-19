library(timekit)
context("Test tk_zoo")

AAPL_tbl <- tidyquant::tq_get("AAPL", from = "2015-01-01", to = "2016-12-31")
AAPL_xts <- tk_xts(AAPL_tbl, silent = TRUE)

# FUNCTION: tk_zoo -----

# tbl to zoo -----
test_that("tbl to zoo test returns zoo with correct rows and columns.", {
    # Use date column to specify order
    test_zoo_1 <- tk_zoo(AAPL_tbl, select = -date, date_var = date)
    expect_equal(nrow(test_zoo_1), 504)
    expect_equal(ncol(test_zoo_1), 6)

    # Use order.by to specify order
    test_zoo_2 <- tk_zoo(AAPL_tbl[,-1], order.by = AAPL_tbl$date)
    expect_equal(nrow(test_zoo_2), 504)
    expect_equal(ncol(test_zoo_2), 6)

    # Auto-index date
    expect_message(tk_zoo(AAPL_tbl, select = -date))  # using `date` column as date

    # Auto-drop columns
    expect_warning(tk_zoo(AAPL_tbl, date_var = date)) # dropping date column

    # Auto-index and auto-drop columns
    expect_message(expect_warning(tk_zoo(AAPL_tbl))) # using `date` column as date, dropping date column

    # NSE
    select     <- "adjusted"
    date_var   <- "date"
    test_zoo_3 <- tk_zoo_(AAPL_tbl, select = select, date_var = date_var)
    expect_equal(nrow(test_zoo_3), 504)
    expect_equal(ncol(test_zoo_3), 1)
    expect_equal(colnames(test_zoo_3), select)

})

# xts to zoo -----
# Default is xts::xts() for other objects; only test zoo
test_that("xts to zoo test returns zoo with correct rows and columns.", {
    # Use date column to specify order
    test_zoo_4 <- tk_zoo(AAPL_xts)
    expect_equal(nrow(test_zoo_4), 504)
    expect_equal(ncol(test_zoo_4), 6)

    # Use order.by to specify order
    test_zoo_5 <- tk_zoo(AAPL_xts, order.by = zoo::index(AAPL_xts))
    expect_equal(nrow(test_zoo_5), 504)
    expect_equal(ncol(test_zoo_5), 6)

    # Warning if using select field
    expect_warning(tk_zoo(AAPL_xts, select = -date))  # only for use with data.frames

    # Warning if using date_var field
    expect_warning(tk_zoo(AAPL_xts, date_var = date)) # only for use with data.frames

    # Two warnings: select and date_var args only applicable to data.frames
    expect_warning(tk_zoo(AAPL_xts, select = -date, date_var = date))

})
