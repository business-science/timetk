testthat::context("Pad By Time")


testthat::test_that("Single Pad By Time Works", {
    df <- tibble(
        date  = c("2011-01-01", "2011-01-03") %>% parse_date2(),
        value = c(1, 3)

    ) %>%
        pad_by_time(.by = "1 day", .pad_value = 2)

    expect_equal(df$value, 1:3)
})

testthat::test_that("Grouped Pad By Time Works", {
    df <- tibble(
        group = c(rep("A", 2), rep("B", 2)),
        date  = rep(c("2011-01-01", "2011-01-03"), 2) %>% parse_date2(),
        value = rep(c(1, 3), 2)
    ) %>%
        group_by(group) %>%
        pad_by_time(.by = "1 day", .pad_value = 2) %>%
        ungroup()

    expect_equal(df$value, c(1:3, 1:3))
})


