#' sweep: Tidy Tools for Forecasting and Working with Time Series
#'
#' The `sweep` package is a swiss army knife for "tidy" time series forecasting.
#'
#' The main benefit of `sweep` is to
#' enable coercion of time series and `forecast` objects between the various data structures
#' that exist within R such as `ts`, `mts`, `irts`, `zoo`, `zooreg`, `xts`, and
#' `tibble`. Further, time-series-like objects (such as
#' `forecast`, `stl`, `HoltzWinters`, etc) can be coerced to "tidy" tibbles.
#'
#'
#' @details
#' The goal of the `sweep` package is to "tidy" up the
#' forecasting workflow. The `sweep` package provides tools to solve three
#' forecasting / time-series scenarios:
#'
#' 1. It provides functions to easily coerce between
#' tibbles ("tidy" data frames) and the various time series data structures.
#' Refer to [sw_tbl()], [sw_xts()], [sw_zoo()], and [sw_ts()].
#' 2. It extends the `broom` package by providing [tidy()], [glance()], and
#' [augment()] functions for various models (such as
#' `stl`, `HoltzWinters`, `tbats`, etc).
#' 3. It provides the [sweep()] function which coerces
#' `forecast` objects to tibbles for easy visualization.
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
