
<!-- README.md is generated from README.Rmd. Please edit that file -->
timekit
=======

[![Travis-CI Build Status](https://travis-ci.org/business-science/timekit.svg?branch=master)](https://travis-ci.org/business-science/timekit.svg?branch=master) [![codecov](https://codecov.io/gh/business-science/timekit/branch/master/graph/badge.svg)](https://codecov.io/gh/business-science/timekit) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/timekit)](https://cran.r-project.org/package=timekit) ![](http://cranlogs.r-pkg.org/badges/timekit?color=brightgreen) ![](http://cranlogs.r-pkg.org/badges/grand-total/timekit?color=brightgreen)

> Simplified and extensible time series coercion tools

The `timekit` package combines a collection of coercion tools for time series analysis.

Benefits
--------

-   **Simplifies the coercion process between time-based tibbles (`tbl`) and the major time series data types `xts`, `zoo`, `zooreg`, and `ts`**
-   **Maximizes time-based data retention during coercion to regularized time series**

Tools
-----

The package contains the following elements:

1.  **coercion functions**: `tk_tbl`, `tk_ts`, `tk_xts`, `tk_zoo`, and `tk_zooreg` coerce time-based tibbles `tbl` to and from each of the main time-series data types `xts`, `zoo`, `zooreg`, `ts`, maintaining the time-based index.

2.  **index function**: `tk_index` returns the time series index of time series objects, models, and `forecast` objects. The argument `timekit_idx` can be used to return a special timekit "index" attribute for regularized `ts` objects that returns a non-regularized date / date-time index if present.

Simplified time series coercion
-------------------------------

The coercion functions `tk_tbl`, `tk_xts`, `tk_zoo`, `tk_zooreg`, and `tk_ts` maximize data retention and simplify the coercion process. Further working with regularized time series (`ts`) class has been a particular pain until now.

The data process often starts with a time-based tibble:

``` r
# Time based tibble
data_tbl <- tibble(
    date = seq.Date(from = as.Date("2010-01-01"), by = 1, length.out = 5),
    x    = seq(100, 120, by = 5)
)
data_tbl
#> # A tibble: 5 × 2
#>         date     x
#>       <date> <dbl>
#> 1 2010-01-01   100
#> 2 2010-01-02   105
#> 3 2010-01-03   110
#> 4 2010-01-04   115
#> 5 2010-01-05   120
```

Coercion to `xts`, `zoo`, or `ts` is simplified. The data is ordered correctly automatically using the column containing the date or datetime information. Non-numeric columns are automatically dropped with a warning to the user (the `silent = TRUE` hides the warnings).

``` r
# xts
data_xts <- tk_xts(data_tbl, silent = TRUE)
```

``` r
# zoo
data_zoo <- tk_zoo(data_tbl, silent = TRUE)
```

``` r
# ts
data_ts <- tk_ts(data_tbl, start = 2010, freq = 365, silent = TRUE)
```

Maximum data retention
----------------------

-   **Problem**: The `ts()` function drops the time series data for a regularized index using the `start` and `freq` arguments.
-   **Solution**: The new `tk_ts()` function stores the original date or datetime index as a second index ("timekit index").

The index is retrieved with `tk_index()`. The default index for `ts` class is the regularized index.

``` r
# Default index is regularized numeric index for ts class
tk_index(data_ts) 
#> [1] 2010.000 2010.003 2010.005 2010.008 2010.011
```

If a "timekit index" is present, the original date or datetime index can be retrieved by setting `timekit_idx = TRUE`. First, let's test with the function `has_timekit_idx()`.

``` r
# Does the data have a timekit index?
has_timekit_idx(data_ts)
#> [1] TRUE
```

Great! We can retrieve the secondary "timekit index" which is the original date or datetime index.

``` r
# Secondary "timekit index": date or date-time index now retrievable
tk_index(data_ts, timekit_idx = TRUE)
#> [1] "2010-01-01" "2010-01-02" "2010-01-03" "2010-01-04" "2010-01-05"
```

The `tk_tbl()` function also has the `timekit_idx` argument. We can now go from `ts` back to `tbl` using the regularized or secondary "timekit index".

``` r
# Default regularized index
data_ts %>%
    tk_tbl(timekit_idx = FALSE)
#> # A tibble: 5 × 2
#>      index     x
#>      <dbl> <dbl>
#> 1 2010.000   100
#> 2 2010.003   105
#> 3 2010.005   110
#> 4 2010.008   115
#> 5 2010.011   120

# Timekit index
data_ts %>%
    tk_tbl(timekit_idx = TRUE)
#> # A tibble: 5 × 2
#>        index     x
#>       <date> <dbl>
#> 1 2010-01-01   100
#> 2 2010-01-02   105
#> 3 2010-01-03   110
#> 4 2010-01-04   115
#> 5 2010-01-05   120
```

This covers the basics of the `timekit` package time series coercion capabilities. Here's how to get started.

Installation
------------

*Download development version with latest features*:

``` r
# install.packages("devtools")
devtools::install_github("business-science/timekit")
```

<!-- _Or, download CRAN approved version_: -->
<!-- ```{r, eval = FALSE} -->
<!-- install.packages("timekit") -->
<!-- ``` -->
Further Information
-------------------

The `timekit` package includes a vignette to help users get up to speed quickly:

-   TK00 - Time Series Coercion Using `timekit`

<!-- See the [`tidyquant` vignettes](https://cran.r-project.org/package=tidyquant) for further details on the package. -->
