#' Sample Daily Time Series Data from the Web Traffic Forecasting (Wikipedia) Competition
#'
#' The Kaggle "Web Traffic Forecasting" (Wikipedia) Competition
#' used __Google Analytics Web Traffic Data__ for 145,000 websites.
#' Each of these time series represent a number of daily views of a different Wikipedia articles.
#' The competition began July 13th, 2017 and ended November 15th, 2017.
#' This dataset includes a __Sample of 10 article pages (10 total time series)__.
#'
#' @details
#' This is a sample of 10 Daily data sets from the Kaggle Web Traffic Forecasting (Wikipedia) Competition
#'
#'
#' @format A tibble: 9,743 x 3
#' - `Page` Character. Page information.
#' - `date` Date. Daily timestamp.
#' - `value` Numeric. Daily views of the wikipedia article.
#'
#'
#'
#' @source
#' - [Kaggle Competition Website](https://www.kaggle.com/c/web-traffic-time-series-forecasting)
#'
#' @examples
#'
#' wikipedia_traffic_daily
#'
"wikipedia_traffic_daily"
