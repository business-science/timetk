context("Test recipe: step_timeseries_signature()")


FB_tbl <- FANG %>%
    dplyr::filter(symbol == "FB") %>%
    mutate(date_2 = date + dyears(1))

rec_obj <- recipe(adjusted ~ ., data = FB_tbl) %>%
    step_timeseries_signature(date, date_2)


test_that("Recipe step becomes added properly.", {

    step_added_tbl <- rec_obj %>% tidy()

    expect_equal(nrow(step_added_tbl), 1)
    expect_equal(ncol(step_added_tbl), 6)
    expect_false(step_added_tbl$trained)
})

test_that("Recipe becomes prepped properly.", {


    prepped_tbl <- rec_obj %>% prep() %>% tidy()

    expect_equal(nrow(prepped_tbl), 1)
    expect_equal(ncol(prepped_tbl), 6)
    expect_true(prepped_tbl$trained)
})

test_that("Recipe becomes baked properly.", {

    baked_tbl <- bake(prep(rec_obj), FB_tbl)

    expect_equal(ncol(baked_tbl), 63)
    expect_equal(nrow(FB_tbl), nrow(baked_tbl))
})

test_that("Tidy works.", {

    tidied_step_tbl <- prep(rec_obj) %>% tidy(1)

    expect_equal(ncol(tidied_step_tbl), 3)
    expect_equal(nrow(tidied_step_tbl), 54)
})
