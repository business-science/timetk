context("TEST TIME SERIES CV")

library(tidyverse)
library(timetk)

# DATA ----
m750 <- m4_monthly %>% filter(id == "M750")

# RESAMPLE SPEC ----
resample_spec <- time_series_cv(data = m750,
                                initial     = "6 years",
                                assess      = "24 months",
                                skip        = "24 months",
                                cumulative  = FALSE,
                                slice_limit = 3)

resamples_unnested <- resample_spec %>% tk_time_series_cv_plan()

resample_groups <- resamples_unnested %>%
    select(.id, .key, date) %>%
    group_by(.id, .key)

test_that("Check Structure: time_series_cv()", {

    # Structure
    expect_s3_class(resample_spec, "time_series_cv")
    expect_s3_class(resample_spec, "rset")

    expect_equal(nrow(resample_spec), 3)
    expect_equal(ncol(resample_spec), 2)

    expect_equal(names(resample_spec), c("splits", "id"))

})

test_that("Check Structure: tk_time_series_cv_plan()", {

    # Structure
    expect_equal(names(resamples_unnested), c(".id", ".key", "id", "date", "value"))

})

test_that("Inspect Results: time_series_cv()", {

    # Check Max Dates
    dates_tbl <- resample_groups %>%
        slice_max(date)

    dates_vec <- dates_tbl %>% filter(.key == "training") %>% pull(date)
    expect_equal(
        c("2013-06-01", "2011-06-01", "2009-06-01") %>% as.Date(),
        dates_vec
    )

    dates_vec <- dates_tbl %>% filter(.key == "testing") %>% pull(date)
    expect_equal(
        c("2015-06-01", "2013-06-01", "2011-06-01") %>% as.Date(),
        dates_vec
    )

    # Check Min Dates
    dates_tbl <- resample_groups %>%
        slice_min(date)

    dates_vec <- dates_tbl %>% filter(.key == "training") %>% pull(date)
    expect_equal(
        c("2007-07-01", "2005-07-01", "2003-07-01") %>% as.Date(),
        dates_vec
    )

    dates_vec <- dates_tbl %>% filter(.key == "testing") %>% pull(date)
    expect_equal(
        c("2013-07-01", "2011-07-01", "2009-07-01") %>% as.Date(),
        dates_vec
    )


})
