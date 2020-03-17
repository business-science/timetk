## ---- echo = FALSE, message = FALSE, warning = FALSE--------------------------
knitr::opts_chunk$set(
    message = FALSE,
    warning = FALSE,
    fig.width = 8, 
    fig.height = 4.5,
    fig.align = 'center',
    out.width='95%', 
    dpi = 200
)

# devtools::load_all() # Travis CI fails on load_all()

## ---- message=FALSE-----------------------------------------------------------
library(tidyverse)
library(tidyquant)
library(timetk)

## -----------------------------------------------------------------------------
# Create sequence with omitted weekends and omitted last two weeks of each year
idx <- seq.Date(ymd("2010-01-01"), by = "day", length.out = 1110) %>%
    tk_get_timeseries_signature() %>%
    filter(!(wday.lbl) %in% c("Saturday", "Sunday")) %>%
    filter(!(week %in% c(51, 52, 53))) %>%
    tk_index()

idx_train <- idx[1:500]
idx_test  <- idx[501:length(idx)]

## -----------------------------------------------------------------------------
idx_test %>%
    tk_get_timeseries_signature() %>%
    ggplot(aes(x = index, y = diff)) +
    geom_line(color = palette_light()[[1]]) +
    theme_tq() +
    labs(title = "Simple Test: Frequency of test set", 
         subtitle = "Missing weekends and missing last two weeks of year") +
    scale_y_continuous(limits = c(0, 2.2e6))

## -----------------------------------------------------------------------------
idx_train %>%
    tk_make_future_timeseries(n_future = 395, inspect_weekdays = TRUE) %>%
    tk_get_timeseries_signature() %>%
    ggplot(aes(x = index, y = diff)) +
    geom_line(color = palette_light()[[1]]) +
    theme_tq() +
    labs(title = "Simple Test: Frequency of predection with inspect_weekdays = T", 
         subtitle = "Catches missing weekends only") +
    scale_y_continuous(limits = c(0, 2.2e6))

## -----------------------------------------------------------------------------
idx %>%
    tk_make_future_timeseries(n_future = 395, inspect_months = TRUE) %>%
    tk_get_timeseries_signature() %>%
    ggplot(aes(x = index, y = diff)) +
    geom_line(color = palette_light()[[1]]) +
    theme_tq() +
    labs(title = "Simple Test: Frequency of predection with inspect_months = T", 
         subtitle = "Catches missing last two weeks of year only") +
    scale_y_continuous(limits = c(0, 2.2e6))

## -----------------------------------------------------------------------------
idx_future_wdays_and_months <- idx_train %>%
    tk_make_future_timeseries(n_future = 395, inspect_weekdays = T, inspect_months = T) 
idx_future_wdays_and_months %>%
    tk_get_timeseries_signature() %>%
    ggplot(aes(x = index, y = diff)) +
    geom_line(color = palette_light()[[1]]) +
    theme_tq() +
    labs(title = "Simple Test: Both inspect_weekdays = T and inspect_months = T", 
         subtitle = "For most part catches missing weekends and last two weeks of year") +
    scale_y_continuous(limits = c(0, 2.2e6))

## -----------------------------------------------------------------------------
idx_test[!(idx_test %in% idx_future_wdays_and_months)]

## -----------------------------------------------------------------------------
idx_future_wdays_and_months[!(idx_future_wdays_and_months %in% idx_test)]

## -----------------------------------------------------------------------------
idx_future_wdays_months_skip_vals <- idx_train %>%
    tk_make_future_timeseries(n_future = 395, 
                              inspect_weekdays = T, 
                              inspect_months   = T,
                              skip_values      = ymd("2012-12-01")) 
idx_future_wdays_months_skip_vals %>%
    tk_get_timeseries_signature() %>%
    ggplot(aes(x = index, y = diff)) +
    geom_line(color = palette_light()[[1]]) +
    theme_tq() +
    labs(title = "Simple Test: inspect_weekdays = T, inspect_months = T, skip_values", 
         subtitle = "Get the exact solution using skip values") +
    scale_y_continuous(limits = c(0, 2.2e6))

## -----------------------------------------------------------------------------
FB_tbl <- FANG %>%
    filter(symbol %in% "FB") 

FB_train <- FB_tbl %>%
    filter(year(date) < 2016)
FB_test  <- FB_tbl %>%
    filter(year(date) >= 2016)

idx_train <- tk_index(FB_train)
idx_test  <- tk_index(FB_test)

## -----------------------------------------------------------------------------
idx_test %>%
    tk_get_timeseries_signature() %>%
    ggplot(aes(x = index, y = diff)) +
    geom_line(color = palette_light()[[1]]) +
    theme_tq() +
    labs(title = "FB Test: Frequency of test set", 
         subtitle = "Combination of regularly spaced weekends and irregular holidays") +
    scale_y_continuous(limits = c(0, 2.2e6))

## -----------------------------------------------------------------------------
# Inspect weekdays: Removes weekends from future series
idx_future_wdays <- idx_train %>% 
    tk_make_future_timeseries(n_future = 366, inspect_weekdays = TRUE, inspect_months = FALSE)

# Visualize frequency
idx_future_wdays %>% 
    tk_get_timeseries_signature() %>%
    ggplot(aes(x = index, y = diff)) +
    geom_line(color = palette_light()[[1]]) +
    theme_tq() +
    labs(title = "FB Test: Frequency of predection with only inspect_weekdays = T", 
         subtitle = "Catches weekends, but not holidays") +
    scale_y_continuous(limits = c(0, 2.2e6))

## -----------------------------------------------------------------------------
# Type I Errors
idx_test[!(idx_test %in% idx_future_wdays)]

## -----------------------------------------------------------------------------
# Type II Errors
idx_future_wdays[!(idx_future_wdays %in% idx_test)]

## -----------------------------------------------------------------------------
# Inspect weekdays: Removes weekends from future series
idx_future_wdays_and_months <- idx_train %>% 
    tk_make_future_timeseries(n_future = 366, inspect_weekdays = TRUE, inspect_months = TRUE)

# Visualize frequency
idx_future_wdays_and_months %>%
    tk_get_timeseries_signature() %>%
    ggplot(aes(x = index, y = diff)) +
    geom_line(color = palette_light()[[1]]) +
    theme_tq() +
    labs(title = "FB Test: inspect_weekdays = T and inspect_months = T", 
         subtitle = "For most part catches missing weekends and some holidays, but some incorrect days are removed") +
    scale_y_continuous(limits = c(0, 2.2e6))

## -----------------------------------------------------------------------------
# Type I Errors
idx_test[!(idx_test %in% idx_future_wdays_and_months)] 

## -----------------------------------------------------------------------------
# Type II Errors
idx_future_wdays_and_months[!(idx_future_wdays_and_months %in% idx_test)] 

## -----------------------------------------------------------------------------
# Build vector of holidays in correct timeseries class using ymd()
holidays <- c(
    "2016-01-01", "2016-01-18", "2016-02-15", "2016-03-25", "2016-05-30",
    "2016-07-04", "2016-09-05", "2016-11-24", "2016-12-26"
) %>% ymd()

# Create future index
idx_future <- idx_train %>%
    tk_make_future_timeseries(n_future = 366, inspect_weekdays = TRUE, skip_values = holidays) 

# Plot using ggplot
idx_future %>%
    tk_get_timeseries_signature() %>%
    ggplot(aes(x = index, y = diff)) +
    geom_line(color = palette_light()[[1]]) +
    theme_tq() +
    labs(title = "FB Test: inspect_weekdays = T and skip_values", 
         subtitle = "Use weekdays to target frequently occuring dates missed and skip values to target known irregular missing dates") +
    scale_y_continuous(limits = c(0, 2.2e6))
    

