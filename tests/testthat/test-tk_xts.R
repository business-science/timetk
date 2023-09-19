context("Test tk_xts")

FB_tbl <- FANG %>% dplyr::filter(symbol == "FB")
FB_zoo <- tk_zoo(FB_tbl, silent = TRUE)

# FUNCTION: tk_xts -----

# tbl to xts -----
test_that("tbl to xts test returns xts with correct rows and columns.", {
    # Use date column to specify order
    test_xts_1 <- tk_xts(FB_tbl, select = -c(date, symbol), date_var = date)
    expect_equal(nrow(test_xts_1), 1008)
    expect_equal(ncol(test_xts_1), 6)

    # Use order.by to specify order
    test_xts_2 <- tk_xts(FB_tbl[,-2], order.by = FB_tbl$date, silent = TRUE)
    expect_equal(nrow(test_xts_2), 1008)
    expect_equal(ncol(test_xts_2), 6)

    # Auto-index date
    expect_message(tk_xts(FB_tbl, select = -c(symbol, date)))  # using `date` column as date

    # Auto-drop columns
    expect_warning(tk_xts(FB_tbl, date_var = date)) # dropping date column

    # Auto-index and auto-drop columns
    expect_message(expect_warning(tk_xts(FB_tbl))) # using `date` column as date, dropping date column

    # NSE
    select     <- "adjusted"
    date_var   <- "date"
    test_xts_3 <- tk_xts_(FB_tbl, select = select, date_var = date_var)
    expect_equal(nrow(test_xts_3), 1008)
    expect_equal(ncol(test_xts_3), 1)
    expect_equal(colnames(test_xts_3), select)

    # Test back-ticked columns
    tib <- tibble(
        `date column` = seq.Date(from = as.Date("2017-01-01"), by = "day", length.out = 10),
        `my value` = 1:10
    )
    test <- tk_xts(tib, silent = T)
    expect_equal(nrow(test), 10)
    expect_equal(ncol(test), 1)
    expect_is(test, "xts")

})


# zoo to xts -----
# Default is xts::xts() for other objects; only test zoo
test_that("zoo to xts test returns xts with correct rows and columns.", {
    # Use date column to specify order
    test_xts_4 <- tk_xts(FB_zoo)
    expect_equal(nrow(test_xts_4), 1008)
    expect_equal(ncol(test_xts_4), 6)

    # Use order.by to specify order
    test_xts_5 <- tk_xts(FB_zoo, order.by = zoo::index(FB_zoo))
    expect_equal(nrow(test_xts_5), 1008)
    expect_equal(ncol(test_xts_5), 6)

    # Warning if using select field
    expect_warning(tk_xts(FB_zoo, select = -date))  # only for use with data.frames

    # Warning if using date_var field
    expect_warning(tk_xts(FB_zoo, date_var = date)) # only for use with data.frames

    # Two warnings: select and date_var args only applicable to data.frames
    expect_warning(tk_xts(FB_zoo, select = -date, date_var = date))

})

# ts to xts -----
# Default is xts::xts() for other objects; only test zoo
test_that("ts to xts test returns xts with correct rows and columns.", {
    # Use date column to specify order
    test_xts_6 <- tk_ts(FB_tbl, silent = TRUE) %>% tk_xts()
    expect_equal(nrow(test_xts_6), 1008)
    expect_equal(ncol(test_xts_6), 6)

})
