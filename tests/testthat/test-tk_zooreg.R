library(timetk)
context("Test tk_zooreg")

FB_tbl    <- FANG %>% filter(symbol == "FB")
FB_xts    <- FB_tbl %>% tk_xts(silent = TRUE)

# FUNCTION: tk_zooreg -----

# tbl to zooreg -----
test_that("tbl to zooreg test returns zooreg with correct rows and columns.", {
    # Use date column to specify order
    test_zooreg_1 <- tk_zooreg(FB_tbl, freq = 252, start = 2015, silent = TRUE)
    expect_equal(nrow(test_zooreg_1), 1008)
    expect_equal(ncol(test_zooreg_1), 6)
    expect_equal(rownames(test_zooreg_1), as.character(FB_tbl$date))

    # Use order.by to specify order
    test_zooreg_2 <- tk_zooreg(FB_tbl, order.by = FB_tbl$date, freq = 252, start = 2015, silent = TRUE)
    expect_equal(nrow(test_zooreg_2), 1008)
    expect_equal(ncol(test_zooreg_2), 6)

    # # Auto-index date
    # expect_message(tk_zooreg(FB_tbl, freq = 252, start = 2015))  # using `date` column as date

    # Auto-drop columns
    expect_warning(tk_zooreg(FB_tbl, freq = 252, start = 2015)) # dropping date column

    # # Auto-index and auto-drop columns
    # expect_message(expect_warning(tk_zooreg(FB_tbl, freq = 252, start = 2015))) # using `date` column as date, dropping date column

    # NSE
    select     <- "adjusted"
    date_var   <- "date"
    test_zooreg_3 <- tk_zooreg_(FB_tbl, select = select, date_var = date_var,
                                freq = 252, start = 2015)
    expect_equal(nrow(test_zooreg_3), 1008)
    expect_equal(ncol(test_zooreg_3), 1)
    expect_equal(colnames(test_zooreg_3), select)

})

# xts to zooreg -----
# Default is xts::xts() for other objects; only test zoo
test_that("xts to zooreg test returns zooreg with correct rows and columns.", {
    # Use date column to specify order
    test_zooreg_4 <- tk_zooreg(FB_xts, freq = 252, start = 2015, silent = TRUE)
    expect_equal(nrow(test_zooreg_4), 1008)
    expect_equal(ncol(test_zooreg_4), 6)

    # Use order.by to specify order
    test_zooreg_5 <- tk_zooreg(FB_xts, order.by = zoo::index(FB_xts))
    expect_equal(nrow(test_zooreg_5), 1008)
    expect_equal(ncol(test_zooreg_5), 6)

    # Warning if using select field
    expect_warning(tk_zooreg(FB_xts, select = -date,
                             freq = 252, start = 2015))  # only for use with data.frames

    # Warning if using date_var field
    expect_warning(tk_zooreg(FB_xts, date_var = date,
                             freq = 252, start = 2015)) # only for use with data.frames

    # Two warnings: select and date_var args only applicable to data.frames
    expect_warning(tk_zooreg(FB_xts, select = -date, date_var = date,
                             freq = 252, start = 2015))

})
