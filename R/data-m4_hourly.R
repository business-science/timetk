#' Sample of 4 Hourly Time Series Datasets from the M4 Competition
#'
#' The fourth M Competition.
#' M4, started on 1 January 2018 and ended in 31 May 2018.
#' The competition included 100,000 time series datasets.
#' This dataset includes __a sample of 4 hourly time series from the competition.__
#'
#' @details
#' This is a sample of 4 hourly data sets from the M4 competition.
#'
#' @format A tibble: 3,060 x 3
#' - `id` Factor. Unique series identifier (4 total)
#' - `date` Date-time. Timestamp information. Hourly format.
#' - `value` Numeric. Value at the corresponding timestamp.
#'
#' @source
#' - [M4 Competition Website](https://www.unic.ac.cy/iff/research/forecasting/m-competitions/m4/)
#'
#' @examples
#' m4_hourly
#'
"m4_hourly"
