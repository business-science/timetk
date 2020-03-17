## ---- echo = FALSE, message = FALSE, warning = FALSE--------------------------
knitr::opts_chunk$set(
    # message = FALSE,
    # warning = FALSE,
    fig.width = 8, 
    fig.height = 4.5,
    fig.align = 'center',
    out.width='95%', 
    dpi = 200
)

# devtools::load_all() # Travis CI fails on load_all()

## ---- message = F-------------------------------------------------------------
library(tidyverse)
library(tidyquant)
library(timetk)

## -----------------------------------------------------------------------------
FB_tbl <- FANG %>% filter(symbol == "FB")
FB_tbl

## -----------------------------------------------------------------------------
FB_vol_date <- FB_tbl %>% select(date, volume)
FB_vol_date

## -----------------------------------------------------------------------------
FB_vol_yearqtr <- FB_vol_date %>%
    mutate(date = as.yearqtr(date)) %>%
    group_by(date) %>%
    summarize(volume = sum(volume))
FB_vol_yearqtr

## -----------------------------------------------------------------------------
# idx_date
idx_date <- tk_index(FB_vol_date)
str(idx_date)

## -----------------------------------------------------------------------------
# idx_yearqtr
idx_yearqtr <- tk_index(FB_vol_yearqtr)
paste0("class: ", class(idx_yearqtr), "\n",
       "n.obs: ", length(idx_yearqtr), "\n",
       "head:  ", stringr::str_c(head(idx_yearqtr), collapse = ", ")) %>%
    cat()

## -----------------------------------------------------------------------------
# idx_date signature
tk_get_timeseries_signature(idx_date)

## -----------------------------------------------------------------------------
# idx_yearqtr signature
tk_get_timeseries_signature(idx_yearqtr)

## -----------------------------------------------------------------------------
# Augmenting a data frame
FB_vol_date_signature <- tk_augment_timeseries_signature(FB_vol_date)
FB_vol_date_signature

## ---- fig.height=6------------------------------------------------------------
# Example Benefit 1: Making a month plot
FB_vol_monthly <- FB_vol_date_signature %>%
    group_by(year, month.lbl) %>%
    summarize(volume = sum(volume)) 

FB_vol_monthly %>%
    ggplot(aes(x = month.lbl, y = volume, fill = factor(year))) +
    geom_bar(stat = "identity") +
    labs(title = "Month Plot of FB Volume", x ="", fill = "Year",
         subtitle = "Analyzing time-based metrics is easy with time series signature") +
    theme_tq() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_tq() +
    scale_y_continuous(labels = scales::comma)
        

## -----------------------------------------------------------------------------
# Example Benefit 2: Modeling is easier
fit <- lm(volume ~ year + month.lbl, data = FB_vol_monthly)
summary(fit)

## -----------------------------------------------------------------------------
# idx_date: First six columns, general summary
tk_get_timeseries_summary(idx_date)[,1:6]

## -----------------------------------------------------------------------------
# idx_date: Last six columns, difference summary
tk_get_timeseries_summary(idx_date)[,7:12]

## -----------------------------------------------------------------------------
# idx_yearqtr: First six columns, general summary
tk_get_timeseries_summary(idx_yearqtr)[,1:6]

## -----------------------------------------------------------------------------
# idx_yearqtr: Last six columns, difference summary
tk_get_timeseries_summary(idx_yearqtr)[,7:12]

