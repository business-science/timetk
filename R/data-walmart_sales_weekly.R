#' Sample Time Series Retail Data from the Walmart Recruiting Store Sales Forecasting Competition
#'
#' The Kaggle "Walmart Recruiting - Store Sales Forecasting" Competition
#' used __retail data__ for combinations of stores and departments within each store.
#' The competition began February 20th, 2014 and ended May 5th, 2014.
#' The competition included data from 45 retail stores located in different regions.
#' The dataset included various external features including Holiday information,
#' Temperature, Fuel Price, and Markdown.
#' This dataset includes a __Sample of 7 departments from the Store ID 1 (7 total time series)__.
#'
#' @details
#' This is a sample of 7 Weekly data sets from the Kaggle Walmart Recruiting
#' Store Sales Forecasting competition.
#'
#' __Holiday Features__
#'
#' The four holidays fall within the following weeks in the dataset (not all holidays are in the data):
#'
#' - Super Bowl: 12-Feb-10, 11-Feb-11, 10-Feb-12, 8-Feb-13
#' - Labor Day: 10-Sep-10, 9-Sep-11, 7-Sep-12, 6-Sep-13
#' - Thanksgiving: 26-Nov-10, 25-Nov-11, 23-Nov-12, 29-Nov-13
#' - Christmas: 31-Dec-10, 30-Dec-11, 28-Dec-12, 27-Dec-13
#'
#' @format A tibble: 9,743 x 3
#' - `id` Factor. Unique series identifier (4 total)
#' - `Store` Numeric. Store ID.
#' - `Dept` Numeric. Department ID.
#' - `Date` Date. Weekly timestamp.
#' - `Weekly_Sales` Numeric. Sales for the given department in the given store.
#' - `IsHoliday` Logical. Whether the week is a "special" holiday for the store.
#' - `Type` Character. Type identifier of the store.
#' - `Size` Numeric. Store square-footage
#' - `Temperature` Numeric. Average temperature in the region.
#' - `Fuel_Price` Numeric. Cost of fuel in the region.
#' - `MarkDown1`, `MarkDown2`, `MarkDown3`, `MarkDown4`, `MarkDown5` Numeric.
#'   Anonymized data related to promotional markdowns that Walmart is running.
#'   MarkDown data is only available after Nov 2011, and is not available for
#'   all stores all the time. Any missing value is marked with an NA.
#' - `CPI` Numeric. The consumer price index.
#' - `Unemployment` Numeric. The unemployment rate in the region.
#'
#'
#' @source
#' - [Kaggle Competition Website](https://www.kaggle.com/c/walmart-recruiting-store-sales-forecasting)
#'
#' @examples
#' walmart_sales_weekly
#'
"walmart_sales_weekly"
