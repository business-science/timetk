context("Test tk_zoo")

FB_tbl <- FANG %>% filter(symbol == "FB")
FB_xts <- tk_xts(FB_tbl, silent = TRUE)

# FUNCTION: tk_zoo -----

# tbl to zoo -----
test_that("tbl to zoo test returns zoo with correct rows and columns.", {
    # Use date column to specify order
    test_zoo_1 <- tk_zoo(FB_tbl, silent = TRUE)
    expect_equal(nrow(test_zoo_1), 1008)
    expect_equal(ncol(test_zoo_1), 6)

    # Use order.by to specify order
    test_zoo_2 <- tk_zoo(FB_tbl[,-1], order.by = FB_tbl$date, silent = TRUE)
    expect_equal(nrow(test_zoo_2), 1008)
    expect_equal(ncol(test_zoo_2), 6)

    # Auto-index date
    expect_message(tk_zoo(FB_tbl, select = -c(symbol, date)))  # using `date` column as date

    # Auto-drop columns
    expect_warning(tk_zoo(FB_tbl, date_var = date)) # dropping date column

    # Auto-index and auto-drop columns
    expect_message(expect_warning(tk_zoo(FB_tbl))) # using `date` column as date, dropping date column

    # NSE
    select     <- "adjusted"
    date_var   <- "date"
    test_zoo_3 <- tk_zoo_(FB_tbl, select = select, date_var = date_var)
    expect_equal(nrow(test_zoo_3), 1008)
    expect_equal(ncol(test_zoo_3), 1)
    expect_equal(colnames(test_zoo_3), select)

})

# xts to zoo -----
# Default is xts::xts() for other objects; only test zoo
test_that("xts to zoo test returns zoo with correct rows and columns.", {
    # Use date column to specify order
    test_zoo_4 <- tk_zoo(FB_xts)
    expect_equal(nrow(test_zoo_4), 1008)
    expect_equal(ncol(test_zoo_4), 6)

    # Use order.by to specify order
    test_zoo_5 <- tk_zoo(FB_xts, order.by = zoo::index(FB_xts))
    expect_equal(nrow(test_zoo_5), 1008)
    expect_equal(ncol(test_zoo_5), 6)

    # Warning if using select field
    expect_warning(tk_zoo(FB_xts, select = -date))  # only for use with data.frames

    # Warning if using date_var field
    expect_warning(tk_zoo(FB_xts, date_var = date)) # only for use with data.frames

    # Two warnings: select and date_var args only applicable to data.frames
    expect_warning(tk_zoo(FB_xts, select = -date, date_var = date))

})
