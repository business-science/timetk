library(sweep)
context("Testing sw_tbl, sw_xts, sw_zoo, sw_zooreg, sw_ts functions")

# FUNCTION: sw_tbl -----

# tbl to tbl -----
AAPL_tbl <- tidyquant::tq_get("AAPL", from = "2015-01-01", to = "2016-12-31")
test_that("tbl tot tbl test returns tibble with correct rows and columns.", {
    test_tbl_1 <- sw_tbl(AAPL_tbl, preserve_index = F, index_rename = "date")
    expect_is(test_tbl_1, "tbl")
    expect_equal(nrow(test_tbl_1), 504)
    expect_equal(ncol(test_tbl_1), 7)
    expect_equal(colnames(test_tbl_1)[[1]], "date")
    expect_warning(sw_tbl(AAPL_tbl, preserve_index = T)) # Expect warning - No index to preserve
})

# xts to tbl -----
AAPL_xts <- sw_xts(AAPL_tbl, select = -date, date_var = date)
test_that("xts to tbl test returns tibble with correct rows and columns.", {
    test_tbl_2 <- sw_tbl(AAPL_xts, preserve_index = T, index_rename = "date")
    expect_equal(nrow(test_tbl_2), 504)
    expect_equal(ncol(test_tbl_2), 7)
    expect_equal(colnames(test_tbl_2)[[1]], "date")
    expect_equal(ncol(sw_tbl(AAPL_xts, preserve_index = F, index_rename = "date")), 6)
})

# zoo to tbl -----
AAPL_zoo <- sw_zoo(AAPL_tbl, select = -date, date_var = date)
test_that("zoo to tbl test returns tibble with correct rows and columns.", {
    test_tbl_3 <- sw_tbl(AAPL_zoo, preserve_index = T, index_rename = "date")
    expect_equal(nrow(test_tbl_3), 504)
    expect_equal(ncol(test_tbl_3), 7)
    expect_equal(colnames(test_tbl_3)[[1]], "date")
    expect_equal(ncol(sw_tbl(AAPL_zoo, preserve_index = F, index_rename = "date")), 6)
})

# zooreg to tbl -----
AAPL_zooreg <- sw_zooreg(AAPL_tbl, select = -date, start = 2015, frequency = 250)
test_that("zooreg to tbl test returns tibble with correct rows and columns.", {
    test_tbl_3a <- sw_tbl(AAPL_zooreg, preserve_index = T, index_rename = "date")
    expect_equal(nrow(test_tbl_3a), 504)
    expect_equal(ncol(test_tbl_3a), 7)
    expect_equal(colnames(test_tbl_3a)[[1]], "date")
    expect_equal(ncol(sw_tbl(AAPL_zooreg, preserve_index = F, index_rename = "date")), 6)

    # zooreg reverse coercion test ----
    test_tbl_3b <- AAPL_zooreg %>%
        sw_tbl(index_rename = "date", .sweep_idx = TRUE)
    expect_identical(test_tbl_3b, AAPL_tbl)

    # Test different start/end types

    # Inherits date
    zooreg_1 <- zoo::zooreg(1:5, start = as.Date("2000-01-01"))
    expect_true(inherits(sw_tbl(zooreg_1)$index, "Date"))

    # Inherits zoo yearmon
    zooreg_2 <- zoo::zooreg(1:5, end = zoo::yearmon(2000))
    expect_true(inherits(sw_tbl(zooreg_2)$index, "yearmon"))

    # Inherits zoo yearqtr
    zooreg_3 <- zoo::zooreg(1:5, start = zoo::yearqtr(2000), frequency = 4)
    expect_true(inherits(sw_tbl(zooreg_3)$index, "yearqtr"))

})

# ts to tbl -----
AAPL_mts <- sw_ts(AAPL_tbl, select = -date, start = 2015, frequency = 252)
test_that("mts to tbl test returns tibble with correct rows and columns.", {
    test_tbl_4 <- sw_tbl(AAPL_mts, preserve_index = T, index_rename = "date")
    expect_equal(nrow(test_tbl_4), 504)
    expect_equal(ncol(test_tbl_4), 7)
    expect_equal(colnames(test_tbl_4)[[1]], "date")
    expect_equal(ncol(sw_tbl(AAPL_mts, preserve_index = F, index_rename = "date")), 6)
    # Warning if no index to preserve
    expect_warning(sw_tbl(sw_ts(AAPL_mts, start = 1), select = -date, preserve_index = T))

    # ts reverse coercion test ----
    test_tbl_4b <- AAPL_mts %>%
        sw_tbl(index_rename = "date", .sweep_idx = TRUE)
    expect_identical(test_tbl_4b, AAPL_tbl)
})

# matrix to tbl -----
AAPL_matrix <- AAPL_xts %>% as.matrix()
test_that("matrix to tbl test returns tibble with correct rows and columns.", {
    test_tbl_5 <- sw_tbl(AAPL_matrix, preserve_index = T, index_rename = "date")
    expect_equal(nrow(test_tbl_5), 504)
    expect_equal(ncol(test_tbl_5), 7)
    expect_equal(colnames(test_tbl_5)[[1]], "date")
    expect_equal(ncol(sw_tbl(AAPL_matrix, preserve_index = F, index_rename = "date")), 6)
    # Warning if no index to prserve
    rownames(AAPL_matrix) <- NULL
    expect_warning(sw_tbl(AAPL_matrix))
})


# timeSeries::timeSeries to tbl -----
test_timeSeries <- timeSeries::timeSeries(1:100, timeDate::timeSequence(length.out = 100, by = "sec"))
test_that("timeSeries to tbl test returns tibble with correct rows and columns.", {
    test_tbl_6 <- sw_tbl(test_timeSeries, preserve_index = T, index_rename = "date-time")
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
    test_tbl_7 <- sw_tbl(test_tseries, preserve_index = T, index_rename = "date-time")
    expect_equal(nrow(test_tbl_7), 10)
    expect_equal(ncol(test_tbl_7), 2)
    expect_equal(colnames(test_tbl_7)[[1]], "date-time")
})

# forecast::msts to tbl -----
test_msts <- forecast::msts(forecast::taylor, seasonal.periods=c(48,336), start=2000+22/52)
test_that("forecast::msts to tbl test returns tibble with correct rows and columns.", {
    test_tbl_8 <- sw_tbl(test_msts, preserve_index = T, index_rename = "index")
    expect_equal(nrow(test_tbl_8), 4032)
    expect_equal(ncol(test_tbl_8), 2)
    expect_equal(colnames(test_tbl_8)[[1]], "index")
})




# FUNCTION: sw_xts -----

# tbl to xts -----
test_that("tbl to xts test returns xts with correct rows and columns.", {
    # Use date column to specify order
    test_xts_1 <- sw_xts(AAPL_tbl, select = -date, date_var = date)
    expect_equal(nrow(test_xts_1), 504)
    expect_equal(ncol(test_xts_1), 6)

    # Use order.by to specify order
    test_xts_2 <- sw_xts(AAPL_tbl[,-1], order.by = AAPL_tbl$date)
    expect_equal(nrow(test_xts_2), 504)
    expect_equal(ncol(test_xts_2), 6)

    # Auto-index date
    expect_message(sw_xts(AAPL_tbl, select = -date))  # using `date` column as date

    # Auto-drop columns
    expect_warning(sw_xts(AAPL_tbl, date_var = date)) # dropping date column

    # Auto-index and auto-drop columns
    expect_message(expect_warning(sw_xts(AAPL_tbl))) # using `date` column as date, dropping date column

    # NSE
    select     <- "adjusted"
    date_var   <- "date"
    test_xts_3 <- sw_xts_(AAPL_tbl, select = select, date_var = date_var)
    expect_equal(nrow(test_xts_3), 504)
    expect_equal(ncol(test_xts_3), 1)
    expect_equal(colnames(test_xts_3), select)

})


# zoo to xts -----
# Default is xts::xts() for other objects; only test zoo
test_that("zoo to xts test returns xts with correct rows and columns.", {
    # Use date column to specify order
    test_xts_4 <- sw_xts(AAPL_zoo)
    expect_equal(nrow(test_xts_4), 504)
    expect_equal(ncol(test_xts_4), 6)

    # Use order.by to specify order
    test_xts_5 <- sw_xts(AAPL_zoo, order.by = zoo::index(AAPL_zoo))
    expect_equal(nrow(test_xts_5), 504)
    expect_equal(ncol(test_xts_5), 6)

    # Warning if using select field
    expect_warning(sw_xts(AAPL_zoo, select = -date))  # only for use with data.frames

    # Warning if using date_var field
    expect_warning(sw_xts(AAPL_zoo, date_var = date)) # only for use with data.frames

    # Two warnings: select and date_var args only applicable to data.frames
    expect_warning(sw_xts(AAPL_zoo, select = -date, date_var = date))

})

# ts to xts -----
# Default is xts::xts() for other objects; only test zoo
test_that("ts to xts test returns xts with correct rows and columns.", {
    # Use date column to specify order
    test_xts_6 <- sw_ts(AAPL_tbl, select = -date) %>%
        sw_xts()
    expect_equal(nrow(test_xts_6), 504)
    expect_equal(ncol(test_xts_6), 6)

})

# FUNCTION: sw_zoo -----

# tbl to zoo -----
test_that("tbl to zoo test returns zoo with correct rows and columns.", {
    # Use date column to specify order
    test_zoo_1 <- sw_zoo(AAPL_tbl, select = -date, date_var = date)
    expect_equal(nrow(test_zoo_1), 504)
    expect_equal(ncol(test_zoo_1), 6)

    # Use order.by to specify order
    test_zoo_2 <- sw_zoo(AAPL_tbl[,-1], order.by = AAPL_tbl$date)
    expect_equal(nrow(test_zoo_2), 504)
    expect_equal(ncol(test_zoo_2), 6)

    # Auto-index date
    expect_message(sw_zoo(AAPL_tbl, select = -date))  # using `date` column as date

    # Auto-drop columns
    expect_warning(sw_zoo(AAPL_tbl, date_var = date)) # dropping date column

    # Auto-index and auto-drop columns
    expect_message(expect_warning(sw_zoo(AAPL_tbl))) # using `date` column as date, dropping date column

    # NSE
    select     <- "adjusted"
    date_var   <- "date"
    test_zoo_3 <- sw_zoo_(AAPL_tbl, select = select, date_var = date_var)
    expect_equal(nrow(test_zoo_3), 504)
    expect_equal(ncol(test_zoo_3), 1)
    expect_equal(colnames(test_zoo_3), select)

})

# xts to zoo -----
# Default is xts::xts() for other objects; only test zoo
test_that("xts to zoo test returns zoo with correct rows and columns.", {
    # Use date column to specify order
    test_zoo_4 <- sw_zoo(AAPL_xts)
    expect_equal(nrow(test_zoo_4), 504)
    expect_equal(ncol(test_zoo_4), 6)

    # Use order.by to specify order
    test_zoo_5 <- sw_zoo(AAPL_xts, order.by = zoo::index(AAPL_xts))
    expect_equal(nrow(test_zoo_5), 504)
    expect_equal(ncol(test_zoo_5), 6)

    # Warning if using select field
    expect_warning(sw_zoo(AAPL_xts, select = -date))  # only for use with data.frames

    # Warning if using date_var field
    expect_warning(sw_zoo(AAPL_xts, date_var = date)) # only for use with data.frames

    # Two warnings: select and date_var args only applicable to data.frames
    expect_warning(sw_zoo(AAPL_xts, select = -date, date_var = date))

})


# FUNCTION: sw_zooreg -----

# tbl to zooreg -----
test_that("tbl to zooreg test returns zooreg with correct rows and columns.", {
    # Use date column to specify order
    test_zooreg_1 <- sw_zooreg(AAPL_tbl, select = -date, date_var = date,
                               freq = 252, start = 2015)
    expect_equal(nrow(test_zooreg_1), 504)
    expect_equal(ncol(test_zooreg_1), 6)
    expect_equal(rownames(test_zooreg_1), as.character(AAPL_tbl$date))

    # Use order.by to specify order
    test_zooreg_2 <- sw_zooreg(AAPL_tbl, select = -date, order.by = AAPL_tbl$date,
                               freq = 252, start = 2015)
    expect_equal(nrow(test_zooreg_2), 504)
    expect_equal(ncol(test_zooreg_2), 6)

    # # Auto-index date
    # expect_message(sw_zooreg(AAPL_tbl, freq = 252, start = 2015))  # using `date` column as date

    # Auto-drop columns
    expect_warning(sw_zooreg(AAPL_tbl, freq = 252, start = 2015)) # dropping date column

    # # Auto-index and auto-drop columns
    # expect_message(expect_warning(sw_zooreg(AAPL_tbl, freq = 252, start = 2015))) # using `date` column as date, dropping date column

    # NSE
    select     <- "adjusted"
    date_var   <- "date"
    test_zooreg_3 <- sw_zooreg_(AAPL_tbl, select = select, date_var = date_var,
                                freq = 252, start = 2015)
    expect_equal(nrow(test_zooreg_3), 504)
    expect_equal(ncol(test_zooreg_3), 1)
    expect_equal(colnames(test_zooreg_3), select)

})

# xts to zooreg -----
# Default is xts::xts() for other objects; only test zoo
test_that("xts to zooreg test returns zooreg with correct rows and columns.", {
    # Use date column to specify order
    test_zooreg_4 <- sw_zooreg(AAPL_xts, freq = 252, start = 2015)
    expect_equal(nrow(test_zooreg_4), 504)
    expect_equal(ncol(test_zooreg_4), 6)

    # Use order.by to specify order
    test_zooreg_5 <- sw_zooreg(AAPL_xts, order.by = zoo::index(AAPL_xts))
    expect_equal(nrow(test_zooreg_5), 504)
    expect_equal(ncol(test_zooreg_5), 6)

    # Warning if using select field
    expect_warning(sw_zooreg(AAPL_xts, select = -date,
                             freq = 252, start = 2015))  # only for use with data.frames

    # Warning if using date_var field
    expect_warning(sw_zooreg(AAPL_xts, date_var = date,
                             freq = 252, start = 2015)) # only for use with data.frames

    # Two warnings: select and date_var args only applicable to data.frames
    expect_warning(sw_zooreg(AAPL_xts, select = -date, date_var = date,
                             freq = 252, start = 2015))

})



# FUNCTION: sw_ts -----

# tbl to ts -----
test_that("tbl to ts test returns ts with correct rows and columns.", {
    # Use date column to specify order
    test_ts_1 <- sw_ts(AAPL_tbl, select = -date, freq = 252, start = 2015)
    expect_equal(nrow(test_ts_1), 504)
    expect_equal(ncol(test_ts_1), 6)

    # Reverse coercion
    test_ts_2 <- test_ts_1 %>%
        sw_tbl(index_rename = "date", .sweep_idx = TRUE)
    expect_identical(AAPL_tbl, test_ts_2)

    # Auto-drop columns
    expect_warning(sw_ts(AAPL_tbl, freq = 252, start = 2015)) # dropping date column

    # NSE
    select     <- "adjusted"
    test_ts_3 <- sw_ts_(AAPL_tbl, select = select, freq = 252, start = 2015)
    expect_equal(nrow(test_ts_3), 504)
    expect_equal(ncol(test_ts_3), 1)
    expect_equal(colnames(test_ts_3), select)

})

# xts to ts -----
test_that("xts to ts test returns ts with correct rows and columns.", {
    # Use date column to specify order
    test_ts_4 <- sw_ts(AAPL_xts, freq = 252, start = 2015)
    expect_equal(nrow(test_ts_4), 504)
    expect_equal(ncol(test_ts_4), 6)

    # Reverse coercion
    test_ts_5 <- test_ts_4 %>%
        sw_xts()
    expect_identical(AAPL_xts, test_ts_5)

    # Warning if using select field
    expect_warning(sw_ts(AAPL_xts, select = -date,
                         freq = 252, start = 2015))  # only for use with data.frames

    # Warning if using date_var field
    expect_error(sw_ts(AAPL_xts, date_var = date,
                       freq = 252, start = 2015)) # date_var not used

})

# zoo to ts -----
test_that("zoo to ts test returns ts with correct rows and columns.", {
    # Use date column to specify order
    test_ts_6 <- sw_ts(AAPL_zoo, freq = 252, start = 2015)
    expect_equal(nrow(test_ts_6), 504)
    expect_equal(ncol(test_ts_6), 6)

    # Reverse coercion
    test_ts_7 <- test_ts_6 %>%
        sw_zoo()
    expect_identical(AAPL_zoo, test_ts_7)

    # Warning if using select field
    expect_warning(sw_ts(AAPL_xts, select = -date,
                         freq = 252, start = 2015))  # only for use with data.frames

    # Warning if using date_var field
    expect_error(sw_ts(AAPL_xts, date_var = date,
                       freq = 252, start = 2015)) # date_var not used

})



