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

## ---- message = FALSE---------------------------------------------------------
library(workflows)
library(parsnip)
library(recipes)
library(yardstick)

library(tidyverse)
library(tidyquant)

## -----------------------------------------------------------------------------
# Read data
bikes <- read_csv("day.csv")

# Select date and count
bikes_tbl <- bikes %>%
    select(dteday, cnt) %>%
    rename(date  = dteday,
           value = cnt)

## -----------------------------------------------------------------------------
# Visualize data and training/testing regions
bikes_tbl %>%
    ggplot(aes(x = date, y = value)) +
    geom_rect(xmin = as.numeric(ymd("2012-07-01")),
              xmax = as.numeric(ymd("2013-01-01")),
              ymin = 0, ymax = 10000,
              fill = palette_light()[[4]], alpha = 0.01) +
    annotate("text", x = ymd("2011-10-01"), y = 7800,
             color = palette_light()[[1]], label = "Train Region") +
    annotate("text", x = ymd("2012-10-01"), y = 1550,
             color = palette_light()[[1]], label = "Test Region") +
    geom_point(alpha = 0.5, color = palette_light()[[1]]) +
    labs(title = "Bikes Sharing Dataset: Daily Scale", x = "") +
    theme_tq()

