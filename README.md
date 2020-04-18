
<!-- README.md is generated from README.Rmd. Please edit that file -->

# timetk <img src="man/figures/logo.png" width="147" height="170" align="right" />

[![Travis build
status](https://travis-ci.org/business-science/timetk.svg?branch=master)](https://travis-ci.org/business-science/timetk)
[![codecov](https://codecov.io/gh/business-science/timetk/branch/master/graph/badge.svg)](https://codecov.io/gh/business-science/timetk)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/timetk)](https://cran.r-project.org/package=timetk)
![](http://cranlogs.r-pkg.org/badges/timetk?color=brightgreen)
![](http://cranlogs.r-pkg.org/badges/grand-total/timetk?color=brightgreen)

> A toolkit for working with time series in R

## Mission

To make it easy to ***visualize, wrangle and preprocess time series
data*** for forecasting and machine learning prediction.

## Documentation

  - [Full Time Series Machine Learning and Feature Engineering
    Tutorial:](https://www.business-science.io/time-series/2020/03/18/time-series-machine-learning.html)
    Showcases the (NEW) `step_timeseries_signature()` for building
    ***200+ time series features*** using `parsnip`, `recipes`, and
    `workflows`.

  - [Visit the timetk website
    documentation](https://business-science.github.io/timetk/) for
    tutorials and a [complete list of function
    references](https://business-science.github.io/timetk/reference/index.html).

## Package Functionality

There are *many* R packages for working with Time Series data. Here’s
how `timetk` compares.

| Task                               | [timetk](https://business-science.github.io/timetk/) | [tsibble](https://tsibble.tidyverts.org/index.html) | [feasts](https://feasts.tidyverts.org/index.html) | [tibbletime](https://business-science.github.io/tibbletime/) |
| ---------------------------------- | ---------------------------------------------------- | --------------------------------------------------- | ------------------------------------------------- | ------------------------------------------------------------ |
| **Structure**                      |                                                      |                                                     |                                                   |                                                              |
| Data Structure                     | tibble (tbl)                                         | tsibble (tbl\_ts)                                   | tsibble (tbl\_ts)                                 | tibbletime (tbl\_time)                                       |
| **Visualization**                  |                                                      |                                                     |                                                   |                                                              |
| Interactive Plots (plotly)         | ✅                                                    | :x:                                                 | :x:                                               | :x:                                                          |
| Static Plots (ggplot)              | ✅                                                    | :x:                                                 | ✅                                                 | :x:                                                          |
| **Data Wrangling**                 |                                                      |                                                     |                                                   |                                                              |
| Time-Based Summarization           | ✅                                                    | :x:                                                 | :x:                                               | ✅                                                            |
| Time-Based Filtering               | ✅                                                    | :x:                                                 | :x:                                               | ✅                                                            |
| Padding Gaps                       | ✅                                                    | ✅                                                   | :x:                                               | :x:                                                          |
| Low to High Frequency              | ✅                                                    | :x:                                                 | :x:                                               | :x:                                                          |
| Imputation                         | ✅                                                    | ✅                                                   | :x:                                               | :x:                                                          |
| Sliding / Rolling                  | ✅                                                    | ✅                                                   | :x:                                               | ✅                                                            |
| **Preprocessing (recipes)**        |                                                      |                                                     |                                                   |                                                              |
| Date Feature Engineering           | ✅                                                    | :x:                                                 | :x:                                               | :x:                                                          |
| Holiday Feature Engineering        | ✅                                                    | :x:                                                 | :x:                                               | :x:                                                          |
| Fourier Series                     | ✅                                                    | :x:                                                 | :x:                                               | :x:                                                          |
| Smoothing & Rolling                | ✅                                                    | :x:                                                 | :x:                                               | :x:                                                          |
| Imputation                         | ✅                                                    | :x:                                                 | :x:                                               | :x:                                                          |
| **Cross Validation (rsample)**     |                                                      |                                                     |                                                   |                                                              |
| Time Series Cross Validation       | ✅                                                    | :x:                                                 | :x:                                               | :x:                                                          |
| Time Series CV Plan Visualization  | ✅                                                    | :x:                                                 | :x:                                               | :x:                                                          |
| **More Awesomeness**               |                                                      |                                                     |                                                   |                                                              |
| Making Time Series (Intelligently) | ✅                                                    | ✅                                                   | :x:                                               | ✅                                                            |
| Handling Holidays & Weekends       | ✅                                                    | :x:                                                 | :x:                                               | :x:                                                          |
| Class Conversion                   | ✅                                                    | ✅                                                   | :x:                                               | :x:                                                          |
| Automatic Frequency & Trend        | ✅                                                    | :x:                                                 | :x:                                               | :x:                                                          |

## What can you do in 1 line of code?

Investigate a time series…

``` r
taylor_30_min %>%
    plot_time_series(date, value, .color_var = week(date), 
                     .interactive = FALSE, .color_lab = "Week")
```

![](man/figures/README-unnamed-chunk-3-1.png)<!-- -->

Make a seasonality plot…

``` r
taylor_30_min %>%
    plot_seasonal_diagnostics(date, value, .interactive = FALSE)
```

![](man/figures/README-unnamed-chunk-4-1.png)<!-- -->

Inspect autocorrelation, partial autocorrelation (and cross correlations
too)…

``` r
taylor_30_min %>%
    plot_acf_diagnostics(date, value, .lags = 0:(48*7), .interactive = FALSE)
```

![](man/figures/README-unnamed-chunk-5-1.png)<!-- -->

## Installation

What are you waiting for? *Download the development version with latest
features*:

``` r
# install.packages("devtools")
devtools::install_github("business-science/timetk")
```

*Or, download CRAN approved version*:

``` r
install.packages("timetk")
```

## Acknowledgements

The `timetk` package wouldn’t be possible without other amazing time
series packages.

  - [`lubridate`](https://github.com/hadley/lubridate): This package
    makes heavy use of `floor_date()`, `ceiling_date`, `duration()`, and
    more.
  - [`xts`](https://github.com/joshuaulrich/xts): Used to calculate
    periodicity and fast lag automation.
  - [`padr`](https://edwinth.github.io/padr/): Used for padding time
    series using `pad_by_time()`
  - [`tibbletime`
    (retired)](https://business-science.github.io/tibbletime/): While
    `timetk` does not import `tibbletime`, it uses much of the
    innovative functionality to interpret time-based phrases in
    `tk_make_timeseries()`, `filter_by_time()`, `between_time()`,
    `%+time%`, and `%-time%` are based on `tibbletime` code. `slidify()`
    is basically `rollify()` using `slider` (see below).
  - [`slider`](https://davisvaughan.github.io/slider/): A newer R
    package that provides a `purrr`-syntax for complex rolling (sliding)
    calculations. `slidify()` uses `pslide` under the hood.
    `roll_apply_vec()` uses `slide_vec()` for simple vectorized rolls
    (slides).
  - [`forecast` (retired)](https://pkg.robjhyndman.com/forecast/):
    Possibly my favorite R package of all time. The `impute_ts_vec()`
    for low-level vectorized imputation using STL + Linear Interpolation
    uses `na.interp()` under the hood. Box Cox transformation
    `auto_lambda()` uses `BoxCox.Lambda()`.

# Learning More

If you are interested in learning from my advanced **Time Series
Analysis & Forecasting Course**, then [join my
waitlist](https://mailchi.mp/business-science/time-series-forecasting-course-coming-soon).
The course is coming soon.

![](vignettes/time_series_course.jpg)<!-- -->

You will learn:

  - Time Series Preprocessing, Noise Reduction, & Anomaly Detection
  - Feature engineering using lagged variables & external regressors
  - Hyperparameter Tuning
  - Time series cross-validation
  - Ensembling Multiple Machine Learning & Univariate Modeling
    Techniques (Competition Winner)
  - NEW - Deep Learning with RNNs (Competition Winner)
  - and more.

<p class="text-center" style="font-size:30px;">

<a href="https://mailchi.mp/business-science/time-series-forecasting-course-coming-soon">Signup
for the Time Series Course waitlist</a>

</p>
