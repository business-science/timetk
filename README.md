
<!-- README.md is generated from README.Rmd. Please edit that file -->
sweep
=====

> A "tidy" toolkit for forecasting and time series analysis

The `sweep` package combines a set of tools for performing forecasts and time series analysis in the "tidyverse". While the package is geared towards the workflow required to perform forecasts using Rob Hyndman's `forecast` package, it contains elements that can help when performing time series analysis using tibbles ("tidy" data frames).

Benefits
--------

-   **Designed for modeling and scaling forecast analyses using the the `tidyverse` tools in [*R for Data Science*](http://r4ds.had.co.nz/)**
-   **Extends `broom` for forecast model analysis**
-   **Combined with the `forecast` and `tidyquant` packages enables end-to-end time series analysis capability**
-   **Eliminates time series coercion issues between time-based tibbles ("tidy" data frames) and the major time series data types `xts`, `zoo`, `zooreg`, and `ts`**

Tools
-----

The package contains three primary elements:

1.  **coercion functions**: `sw_tbl`, `sw_ts`, `sw_xts`, `sw_zoo`, and `sw_zooreg`. These functions coerce time-based tibbles `tbl` to and from each of the main time-series data types `xts`, `zoo`, `zooreg`, `ts`, maintaining the time-based index.

2.  **broom model tidiers**: `sw_tidy`, `sw_glance`, `sw_augment`, `sw_tidy_decomp`. These functions extend `tidy`, `glance`, and `augment` from the `broom` package specifically for models (`ets()`, `Arima()`, `bats()`, etc) used for forecasting.

3.  **forecast tidier**: `sw_sweep` converts a `forecast` to a tibble that can be easily manipulated in the "tidyverse".

Making forecasts in the tidyverse
---------------------------------

`sweep` enables transitioning from tibble to ts, from ts to model (e.g. Arima, ets, etc), from model to forecast, and then from forecast to tibble. The result is ability to use `dplyr`, `tidyr`, and `ggplot` natively to manipulate, analyze and visualize forecasts.

<img src="img/forecast.png" width="100%" />

Forecasting multiple time series groups at scale
------------------------------------------------

Often forecasts are required on grouped data to analyse trends in sub-categories. The good news is scaling from one time series to many is easy with the various `sw_` functions and the enhanced mapping function `map_list_column`.

<img src="img/time_series_groups.png" width="100%" />

Forecasting multiple models for accuracy
----------------------------------------

TODO

broom extensions for forecasting
--------------------------------

If you are familiar with `broom`, you know how useful it is for retrieving "tidy" format model coefficients (`tidy`), accuracy statistics (`glance`), and residuals (`augment`). The `sweep` package extends these functions to forecast modeling functions such as ARIMA, ETS, BATS, TBATS, NNETAR, and more: just use the `sweep` functions, `sw_tidy`, `sw_glance`, and `sw_augment`. In addition, a new tidier, `sw_tidy_decomp` is designed to specifically tidy models that produce seasonal decompositions. The compatibility chart is listed below.

| Function      | sw\_tidy() | sw\_glance() | sw\_augment() | sw\_tidy\_decomp() |
|:--------------|:----------:|:------------:|:-------------:|:------------------:|
| ar()          |            |              |               |                    |
| arima()       |      X     |       X      |       X       |                    |
| Arima()       |      X     |       X      |       X       |                    |
| ets()         |      X     |       X      |       X       |          X         |
| baggedETS()   |            |              |               |                    |
| bats()        |      X     |       X      |       X       |          X         |
| tbats()       |      X     |       X      |       X       |          X         |
| nnetar()      |      X     |       X      |       X       |                    |
| stl()         |            |              |               |          X         |
| HoltWinters() |      X     |       X      |       X       |          X         |
| StructTS      |      X     |       X      |       X       |          X         |
| tslm()        |      X     |       X      |       X       |                    |
| decompose()   |            |              |               |          X         |
| adf.test()    |      X     |       X      |               |                    |
| Box.test()    |      X     |       X      |               |                    |
| kpss.test()   |      X     |       X      |               |                    |

This just scratches the surface of `sweep`. Here's how to install to get started.

Installation
------------

Development version with latest features:

``` r
# install.packages("devtools")
devtools::install_github("business-science/sweep")
```

<!-- CRAN approved version: -->
<!-- ```{r, eval = FALSE} -->
<!-- install.packages("sweep") -->
<!-- ``` -->
Further Information
-------------------

The `sweep` package includes several vignettes to help users get up to speed quickly:

-   SW00 - Introduction to `sweep`
-   SW01 - Forecasting Time Series Groups in the tidyverse
-   SW02 - TODO

<!-- See the [`tidyquant` vignettes](https://cran.r-project.org/package=tidyquant) for further details on the package. -->
