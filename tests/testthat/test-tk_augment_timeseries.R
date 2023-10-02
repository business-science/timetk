context("Testing tk_augment_timeseries_signature function")

FB_tbl <- FANG %>%
    dplyr::filter(symbol == "FB")

test_tbl <- FB_tbl

# Number of tk_get_signature columns
n <- 29

# Test tbl
test_that("tk_augment_timeseries_signature(tbl) test returns correct format.", {
    test <- tk_augment_timeseries_signature(test_tbl)
    expect_true(inherits(test, "tbl"))
    expect_equal(nrow(test), 1008)
    expect_equal(ncol(test), n + 7)
})

test_xts <- FB_tbl %>%
    tk_xts(silent = TRUE)

# Test xts
test_that("tk_augment_timeseries_signature(xts) test returns correct format.", {
    test <- tk_augment_timeseries_signature(test_xts)
    expect_true(inherits(test, "xts"))
    expect_equal(nrow(test), 1008)
    expect_equal(ncol(test), n + 3)
})

# Test zoo
test_zoo <- FB_tbl %>%
    tk_zoo(silent = TRUE)

test_that("tk_augment_timeseries_signature(zoo) test returns correct format.", {
    test <- tk_augment_timeseries_signature(test_zoo)
    expect_true(inherits(test, "zoo"))
    expect_equal(nrow(test), 1008)
    expect_equal(ncol(test), n + 3)
})

# Test non-data frame
test_default <- 1
test_that("tk_augment_timeseries_signature(default) test returns correct format.", {
    expect_error(tk_augment_timeseries_signature(test_default))
})

# Test back-ticked columns
test_that("tk_augment_timeseries_signature() works with back ticked columns.", {
    tib <- dplyr::tibble(
        `date column` = seq.Date(from = as.Date("2017-01-01"), by = "day", length.out = 10),
        `my value` = 1:10
    )
    test <- tib %>%
        tk_augment_timeseries_signature()
    expect_is(test, "tbl")

})



