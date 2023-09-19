context("Test tk_ts")

FB_tbl <- FANG %>% dplyr::filter(symbol == "FB")
FB_xts <- tk_xts(FB_tbl, silent = TRUE)
FB_zoo <- tk_zoo(FB_tbl, silent = TRUE)

# FUNCTION: tk_ts -----

# tbl to ts -----
test_that("tbl to ts test returns ts with correct rows and columns.", {
    # Use date column to specify order
    test_ts_1 <- tk_ts(FB_tbl, select = -c(date, symbol), freq = 252, start = 2015)
    expect_equal(nrow(test_ts_1), 1008)
    expect_equal(ncol(test_ts_1), 6)

    # Reverse coercion
    test_ts_2 <- test_ts_1 %>%
        tk_tbl(rename_index = "date", timetk_idx = TRUE)
    expect_identical(FB_tbl %>% select(-symbol), test_ts_2)

    # Auto-drop columns
    expect_warning(tk_ts(FB_tbl, freq = 252, start = 2015)) # dropping date column

    # NSE
    select     <- "adjusted"
    test_ts_3 <- tk_ts_(FB_tbl, select = select, freq = 252, start = 2015)
    expect_equal(nrow(test_ts_3), 1008)
    expect_equal(ncol(test_ts_3), 1)
    expect_equal(colnames(test_ts_3), select)

})

# xts to ts -----
test_that("xts to ts test returns ts with correct rows and columns.", {
    # Use date column to specify order
    test_ts_4 <- tk_ts(FB_xts, freq = 252, start = 2015)
    expect_equal(nrow(test_ts_4), 1008)
    expect_equal(ncol(test_ts_4), 6)

    # Reverse coercion
    test_ts_5 <- test_ts_4 %>%
        tk_xts()
    expect_identical(FB_xts, test_ts_5)

    # Warning if using select field
    expect_warning(tk_ts(FB_xts, select = -date,
                         freq = 252, start = 2015))  # only for use with data.frames

    # Warning if using date_var field
    expect_error(tk_ts(FB_xts, date_var = date,
                       freq = 252, start = 2015)) # date_var not used

})

# zoo to ts -----
test_that("zoo to ts test returns ts with correct rows and columns.", {
    # Use date column to specify order
    test_ts_6 <- tk_ts(FB_zoo, freq = 252, start = 2015)
    expect_equal(nrow(test_ts_6), 1008)
    expect_equal(ncol(test_ts_6), 6)

    # Reverse coercion
    test_ts_7 <- test_ts_6 %>%
        tk_zoo()
    expect_identical(FB_zoo, test_ts_7)

    # Warning if using select field
    expect_warning(tk_ts(FB_xts, select = -date,
                         freq = 252, start = 2015))  # only for use with data.frames

    # Warning if using date_var field
    expect_error(tk_ts(FB_xts, date_var = date,
                       freq = 252, start = 2015)) # date_var not used

})



