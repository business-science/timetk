context("Testing tk_make_timeseries")


test_that("Date Sequences", {

    # Daily
    seq_0 <- seq.Date(as.Date("2017-01-01"), as.Date("2017-12-31"), by = "day")
    seq_1 <- tk_make_timeseries("2017-01-01", "2017-12-31")
    seq_2 <- tk_make_timeseries("2017")
    expect_identical(seq_0, seq_1, seq_2)

    seq_0 <- seq.Date(as.Date("2017-01-01"), as.Date("2017-01-31"), by = "day")
    seq_1 <- tk_make_timeseries("2017-01")
    expect_identical(seq_0, seq_1)

    # Week
    seq_0 <- seq.Date(as.Date("2017-01-01"), as.Date("2017-01-31"), by = "week")
    seq_1 <- tk_make_timeseries("2017-01", by = "week")
    expect_identical(seq_0, seq_1)

    # Month
    seq_0 <- seq.Date(from = as.Date("2012-01-01"), by = "-1 month", length.out = 18)
    seq_1 <- tk_make_timeseries(
        end_date = "2012-01-01",
        by = "1 month",
        length_out = "1 year 6 months",
        include_endpoints = FALSE)
    expect_identical(sort(seq_0), seq_1)

    seq_0 <- c("2011-01-31", "2011-02-28", "2011-03-31") %>% ymd()
    seq_1 <- tk_make_timeseries("2011-01-31", by = "month", length_out = 3)
    expect_identical(seq_0, seq_1)

    # Quarter
    seq_0 <- seq.Date(ymd("2011-02-15"), by = "quarter", length.out = 4)
    seq_1 <- tk_make_timeseries("2011-02-15", by = "3 month", length_out = "1 year", include_endpoints = FALSE)
    expect_identical(seq_0, seq_1)

    # GitHub Issue 60
    seq_0 <- seq.Date(as.Date("2011-01-01"), by = "8 day", length.out = 10)
    seq_1 <- tk_make_timeseries("2011-01-01", by = "8 day", length_out = 10)
    expect_identical(seq_0, seq_1)


})



test_that("Time Sequences", {

    # Start + End, Guesses by second
    seq_0 <- seq.POSIXt(ymd_hms("2016-01-01 01:01:02"), ymd_hms("2016-01-01 01:01:04"), by = "sec")
    seq_1 <- tk_make_timeseries("2016-01-01 01:01:02", "2016-01-01 01:01:04")
    expect_identical(seq_0, seq_1)

    # Date-Time Sequence - By 10 Minutes
    # - Converts to date-time automatically & applies 10-min interval
    seq_0 <- seq.POSIXt(ymd_hms("2017-01-01 00:00:00"), ymd_hms("2017-01-02 00:00:00"), by = "10 min")
    seq_1 <- tk_make_timeseries("2017-01-01", "2017-01-02", by = "10 min")
    expect_identical(seq_0, seq_1)

    # Date-Time Sequence - By 1 Month
    # - Converts to date-time automatically & applies 10-min interval
    seq_0 <- seq.POSIXt(ymd_hms("2017-01-15 00:00:00"), by = "1 month", length.out = 12)
    seq_1 <- tk_make_timeseries("2017-01-15 00:00:00", by = "1 month", length_out = 12)
    expect_identical(seq_0, seq_1)

})

test_that("Skip/Insert Values", {

    seq_0 <- c("2011-01-01", "2011-01-02", "2011-01-03", "2011-01-04", "2011-01-06") %>% ymd()
    seq_1 <- tk_make_timeseries(
        "2011-01-01", length_out = 5,
        skip_values   = "2011-01-05",
        insert_values = "2011-01-06"
    )
    expect_identical(seq_0, seq_1)
})
