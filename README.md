
<!-- README.md is generated from README.Rmd. Please edit that file -->
timekit
=======

[![Travis-CI Build Status](https://travis-ci.org/business-science/timekit.svg?branch=master)](https://travis-ci.org/business-science/timekit.svg?branch=master) [![codecov](https://codecov.io/gh/business-science/timekit/branch/master/graph/badge.svg)](https://codecov.io/gh/business-science/timekit) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/timekit)](https://cran.r-project.org/package=timekit) ![](http://cranlogs.r-pkg.org/badges/timekit?color=brightgreen) ![](http://cranlogs.r-pkg.org/badges/grand-total/timekit?color=brightgreen)

> A collection of tools for working with time series in R

Benefits
--------

The `timekit` package enables a user to work with time objects. The package has tools for inspecting and manipulating the time-based index (e.g. a date or date time column from a `tbl` or the dates stored as rownames from an `xts` object) and converting objects to and from the many time series classes. The following are key benefits:

-   **Index extraction: get the time series index from any time series object**
-   **Understand time series: create a signature decomposition and summary from a time series index**
-   **Build future time series: create a future time series from an index**
-   **Simplifies the coercion process between time-based tibbles (`tbl`) and the major time series data types `xts`, `zoo`, `zooreg`, and `ts`. Maximizes time-based data retention during coercion to regularized time series**

Tools
-----

The package contains the following elements:

1.  **Get an index**: `tk_index` returns the time series index of time series objects, models. The argument `timekit_idx` can be used to return a special timekit "index" attribute for regularized `ts` objects that returns a non-regularized date / date-time index if present.

2.  **Get critical timeseries information**: `tk_get_timeseries_signature` and `tk_get_timeseries_summary` takes an index and provides a time series decomposition and key summary attributes of the index, respectively. The `tk_augment_timeseries_signature` adds the time series decomposition to the time series object.

3.  **Make a future timeseries**: `tk_make_future_timeseries` models a future time series after an existing time series index.

4.  **Coercion functions**: `tk_tbl`, `tk_ts`, `tk_xts`, `tk_zoo`, and `tk_zooreg` coerce time-based tibbles `tbl` to and from each of the main time-series data types `xts`, `zoo`, `zooreg`, `ts`, maintaining the time-based index.

Getting started
---------------

Load libraries and start with some time series data

``` r
library(tidyquant)
library(timekit)

FB_tbl <- FANG %>%
    filter(symbol == "FB")
FB_tbl
#> # A tibble: 1,008 × 8
#>    symbol       date  open  high   low close    volume adjusted
#>     <chr>     <date> <dbl> <dbl> <dbl> <dbl>     <dbl>    <dbl>
#> 1      FB 2013-01-02 27.44 28.18 27.42 28.00  69846400    28.00
#> 2      FB 2013-01-03 27.88 28.47 27.59 27.77  63140600    27.77
#> 3      FB 2013-01-04 28.01 28.93 27.83 28.76  72715400    28.76
#> 4      FB 2013-01-07 28.69 29.79 28.65 29.42  83781800    29.42
#> 5      FB 2013-01-08 29.51 29.60 28.86 29.06  45871300    29.06
#> 6      FB 2013-01-09 29.67 30.60 29.49 30.59 104787700    30.59
#> 7      FB 2013-01-10 30.60 31.45 30.28 31.30  95316400    31.30
#> 8      FB 2013-01-11 31.28 31.96 31.10 31.72  89598000    31.72
#> 9      FB 2013-01-14 32.08 32.21 30.62 30.95  98892800    30.95
#> 10     FB 2013-01-15 30.64 31.71 29.88 30.10 173242600    30.10
#> # ... with 998 more rows
```

Understand and work with a time series index
--------------------------------------------

Get the timeseries index

``` r
idx <- tk_index(FB_tbl)
```

Get the time series signature from the index, a tibble of decomposed features that are useful for understanding the time series observations.

``` r
tk_get_timeseries_signature(idx)
#> # A tibble: 1,008 × 17
#>         index  index.num   diff  year quarter month month.xts month.lbl
#>        <date>      <int>  <int> <int>   <int> <int>     <int>     <ord>
#> 1  2013-01-02 1357084800     NA  2013       1     1         0   January
#> 2  2013-01-03 1357171200  86400  2013       1     1         0   January
#> 3  2013-01-04 1357257600  86400  2013       1     1         0   January
#> 4  2013-01-07 1357516800 259200  2013       1     1         0   January
#> 5  2013-01-08 1357603200  86400  2013       1     1         0   January
#> 6  2013-01-09 1357689600  86400  2013       1     1         0   January
#> 7  2013-01-10 1357776000  86400  2013       1     1         0   January
#> 8  2013-01-11 1357862400  86400  2013       1     1         0   January
#> 9  2013-01-14 1358121600 259200  2013       1     1         0   January
#> 10 2013-01-15 1358208000  86400  2013       1     1         0   January
#> # ... with 998 more rows, and 9 more variables: day <int>, wday <int>,
#> #   wday.xts <int>, wday.lbl <ord>, mday <int>, yday <int>, hour <int>,
#> #   minute <int>, second <int>
```

Get the time series summary from the index, a single-row tibble of key summary information from the time series.

``` r
tk_get_timeseries_summary(idx)
#> # A tibble: 1 × 12
#>   n.obs      start        end units scale tzone diff.minimum diff.q1
#>   <int>     <date>     <date> <chr> <chr> <chr>        <dbl>   <dbl>
#> 1  1008 2013-01-02 2016-12-30  days   day   UTC        86400   86400
#> # ... with 4 more variables: diff.median <dbl>, diff.mean <dbl>,
#> #   diff.q3 <dbl>, diff.maximum <dbl>
```

Make a future time series
-------------------------

Use an index to make a future time series.

``` r
holidays <- c("2017-01-02", "2017-01-16", "2017-02-20",
              "2017-04-14", "2017-05-29", "2017-07-04",
              "2017-09-04", "2017-11-23", "2017-12-25") %>%
    ymd()

idx_future <- tk_make_future_timeseries(
    idx, 
    n_future         = 251, 
    skip_values      = holidays, 
    inspect_weekdays = TRUE) 
```

Simplified time series coercion
-------------------------------

Coercion to `xts`, `zoo`, or `ts` is simplified. The data is ordered correctly automatically using the column containing the date or datetime information. Non-numeric columns are automatically dropped with a warning to the user (the `silent = TRUE` hides the warnings).

``` r
# xts
FB_xts <- tk_xts(FB_tbl, silent = TRUE)
```

``` r
# zoo
FB_zoo <- tk_zoo(FB_tbl, silent = TRUE)
```

``` r
# ts
FB_ts <- tk_ts(FB_tbl, start = 2013, freq = 252, silent = TRUE)
```

This covers the basics of the `timekit` package time series coercion capabilities. Here's how to get started.

Installation
------------

*Download development version with latest features*:

``` r
# install.packages("devtools")
devtools::install_github("business-science/timekit")
```

*Or, download CRAN approved version*:

``` r
install.packages("timekit")
```

Further Information
-------------------

The `timekit` package includes a vignette to help users get up to speed quickly:

-   TK00 - Time Series Coercion Using `timekit`
-   TK01 - Working with the Time Series Index using `timekit`
