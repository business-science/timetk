context("TEST TIME SERIES CV")


# SINGLE TIME SERIES ----

m750 <- m4_monthly %>% dplyr::filter(id == "M750") %>% dplyr::arrange(desc(date))

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

    expect_named(resample_spec, c("splits", "id"))

})

test_that("Check Structure: tk_time_series_cv_plan()", {

    # Structure
    expect_equal(names(resamples_unnested), c(".id", ".key", "id", "date", "value"))

})

test_that("Inspect Results: time_series_cv()", {

    # Check Max Dates
    dates_tbl <- resample_groups %>%
        slice_max(date)

    dates_vec <- dates_tbl %>% dplyr::filter(.key == "training") %>% dplyr::pull(date) %>% unique()
    expect_equal(
        c("2013-06-01", "2011-06-01", "2009-06-01") %>% as.Date(),
        dates_vec
    )

    dates_vec <- dates_tbl %>% dplyr::filter(.key == "testing") %>% dplyr::pull(date) %>% unique()
    expect_equal(
        c("2015-06-01", "2013-06-01", "2011-06-01") %>% as.Date(),
        dates_vec
    )

    # Check Min Dates
    dates_tbl <- resample_groups %>%
        slice_min(date)

    dates_vec <- dates_tbl %>% dplyr::filter(.key == "training") %>% dplyr::pull(date) %>% unique()
    expect_equal(
        c("2007-07-01", "2005-07-01", "2003-07-01") %>% as.Date(),
        dates_vec
    )

    dates_vec <- dates_tbl %>% dplyr::filter(.key == "testing") %>% dplyr::pull(date) %>% unique()
    expect_equal(
        c("2013-07-01", "2011-07-01", "2009-07-01") %>% as.Date(),
        dates_vec
    )


})

# PANEL DATA ----

walmart_tscv <- walmart_sales_weekly %>%
    time_series_cv(
        date_var    = Date,
        initial     = "12 months",
        assess      = "3 months",
        skip        = "3 months",
        slice_limit = 4
    )


resamples_unnested <- walmart_tscv %>%  tk_time_series_cv_plan()

resample_groups <- resamples_unnested %>%
    dplyr::select(.id, .key, Date) %>%
    dplyr::group_by(.id, .key)

test_that("Inspect Results: time_series_cv()", {

    # Check Max Dates
    dates_tbl <- resample_groups %>%
        dplyr::slice_max(Date) %>%
        dplyr::ungroup()

    dates_vec <- dates_tbl %>% dplyr::filter(.key == "training") %>% dplyr::pull(Date) %>% unique()
    expect_equal(
        c("2012-08-03", "2012-05-11", "2012-02-17", "2011-11-25") %>% as.Date(),
        dates_vec
    )

    dates_vec <- dates_tbl %>% dplyr::filter(.key == "testing") %>% dplyr::pull(Date) %>% unique()
    expect_equal(
        c("2012-10-26", "2012-08-03", "2012-05-11", "2012-02-17") %>% as.Date(),
        dates_vec
    )

    # Check Min Dates
    dates_tbl <- resample_groups %>%
        dplyr::group_by(.id, .key) %>%
        dplyr::filter(Date == min(Date)) %>%
        dplyr::ungroup()

    dates_vec <- dates_tbl %>% dplyr::filter(.key == "training") %>% dplyr::pull(Date) %>% unique()
    expect_equal(
        c("2011-08-12", "2011-05-20", "2011-02-25", "2010-12-03") %>% as.Date(),
        dates_vec
    )

    dates_vec <- dates_tbl %>% dplyr::filter(.key == "testing") %>% dplyr::pull(Date) %>% unique()
    expect_equal(
        c("2012-08-10", "2012-05-18", "2012-02-24", "2011-12-02") %>% as.Date(),
        dates_vec
    )


})


walmart_tscv <- walmart_sales_weekly %>%
    time_series_cv(
        date_var    = Date,
        initial     = "12 months",
        assess      = 2,
        skip        = "3 months",
        slice_limit = 4
    )

walmart_tscv %>% tk_time_series_cv_plan() %>% plot_time_series_cv_plan(Date, Weekly_Sales)

resamples_unnested <- walmart_tscv %>%  tk_time_series_cv_plan()

resample_groups <- resamples_unnested %>%
    dplyr::select(.id, .key, Date) %>%
    dplyr::group_by(.id, .key)

resample_count <- resample_groups %>%
    dplyr::group_by(.id, .key) %>%
    dplyr::count()

test_that("Inspect Results: time_series_cv()", {


    # Training: All should be 364 (52 * 7 Groups)
    expect_true({
        all({
            resample_count %>%
                filter(.key == "training") %>%
                pull(n) == 52*7
        })
    })

    # Testing: All should be 14 (2 * 7 Groups)
    expect_true({
        all({
            resample_count %>%
                filter(.key == "testing") %>%
                pull(n) == 2*7
        })
    })


})
