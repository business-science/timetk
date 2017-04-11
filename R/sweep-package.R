#' sweep: A "tidy" toolkit for forecasting and time series analysis
#'
#' The `sweep` package is a swiss army knife for working with time-series
#' within the "tidyverse".
#' While the package is geared towards the workflow required to perform
#' forecasts using Rob Hyndman's `forecast` package,
#' it contains elements that can help when performing time series analysis
#' using tibbles ("tidy" data frames).
#'
#'
#'
#' @details
#' The goal of the `sweep` package is to "tidy" up the
#' forecasting workflow. The `sweep` package provides the following tools:
#'
#' 1. It provides functions to easily coerce between
#' "tibbles" ("tidy" data frames) and the various time series data structures.
#' Refer to [sw_tbl()], [sw_xts()], [sw_zoo()], [sw_zooreg()], and [sw_ts()].
#' 2. It extends the `broom` package by providing [sw_tidy()], [sw_glance()],
#' [sw_augment()], [sw_tidy_decomp()] functions for various time-series models such as
#' `Arima`, `ets`, `HoltzWinters`, `tbats`, etc.
#' 3. It provides the [sw_sweep()] function which coerces
#' `forecast` objects to "tibbles" for easy visualization with `ggplot2`
#' and manipulation with `dplyr`.
#'
#' To learn more about `sweep`, start with the vignettes:
#'  `browseVignettes(package = "sweep")`
#'
#' @docType package
#' @name sweep_package
#'
#' @importFrom dplyr %>%
#' @importFrom xts xts

NULL
