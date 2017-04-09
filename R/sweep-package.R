#' sweep: Tidy Tools for Forecasting and Working with Time Series
#'
#' The `sweep` package is a swiss army knife for working with time-series
#' within the "tidyverse".
#' It combines elements from the `broom` package for "tidy" analysis of time series models
#' along with time-series coercion functions including `sw_sweep()` and
#' `sw_decompose()` to coerce `forecast` and time-series decompositions to
#' "tibbles". Last, the package includes functions to easily coerce between
#' the primary time series object classes and "tibbles" with a time-series
#' index.
#'
#'
#'
#' @details
#' The goal of the `sweep` package is to "tidy" up the
#' forecasting workflow. The `sweep` package provides the following tools:
#'
#' 1. It extends the `broom` package by providing [sw_tidy()], [sw_glance()], and
#' [sw_augment()] functions for various time-series models such as
#' `Arima`, `ets`, `HoltzWinters`, `tbats`, etc.
#' 2. It provides the [sw_sweep()] function which coerces
#' `forecast` objects to "tibbles" for easy visualization with `ggplot2`
#' and manipulation with `dplyr`.
#' 3. It provides the [sw_decompose()] function which coerces time-series
#' decomposition objects including `stl`, `ets`, `decomposed.ts`, and `tbats`.
#' 4. It provides functions to easily coerce between
#' "tibbles" ("tidy" data frames) and the various time series data structures.
#' Refer to [sw_tbl()], [sw_xts()], [sw_zoo()], [sw_zooreg()], and [sw_ts()].
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
