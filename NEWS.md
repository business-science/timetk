# timetk 0.1.3.9000

<!--
__TODO:__

MAJOR ITEMS:
1. (DONE) recipes steps - Split transformations from predictor creators
2. (DONE) rsample - backwards rolling origin
3. tests
4. vignettes

SMALLER ITEMS:
1. Seasonal Decomposition
1. Outliers
2. Missing Data - imputation & irregular series padding
3. weather signature
4. tsibble integration

- Plot Diagnostics
    - (DONE) Plot Time Series
    - (DONE) Plot Lag (ACF, PACF, & CCF) Diagnostics
    - (DONE) Plot Seasonality Diagnostics 
    - Missing Data Diagnostics
- Fourier series
    - (DONE) `step_fourier_series()`
    - (DONE) `fourier_vec()`
    - `tk_get_fourier_series()` (is this needed?)
    - `tk_augment_fourier_series()` (is this needed?)
- (DONE) Rolling functionality 
    - (DONE) `step_roll_apply()`
    - (DONE) `tk_augment_roll_apply()` 
    - (DONE) `roll_apply_vec()` 
- Smooth Loess functionality 
    - (DONE) `step_smooth()` 
    - `tk_augment_smooth()`
    - (DONE) `smooth_vec()` 
- Differencing functionality
    - (DONE) `diff_vec`
    - (DONE) `diff_inv_vec`
    - (DONE) `step_diff`
- Lag functionality
    - (DONE) `lag_vec()`
    - (DONE) `tk_augment_lags()`
    - `recipes::step_lag()`
- Rsample automation 
    - (DONE) Lags - [Rsample PR #136](https://github.com/tidymodels/rsample/pull/136)
    - (DONE) `time_series_cv()` - Rewrite rolling_origin - for Backtesting
- Box Cox functionality
    - (DONE) `box_cox_vec`
    - (DONE) `box_cox_inv_vec`
    - (DONE) `auto_lambda`
    - (DONE) `step_box_cox`
- Dplyr Transition 
    - (DONE) `tidyquant::summarise_by_time`
    - (DONE) `tibbletime::filter_by_time`
    - (DONE) `between_time()`
    - `tibbletime::as_period` - Can use `summarise_by_time()` for this. 
    - (DONE) `slidify` - `tibbletime::rollify`
- Holidays 
    - (DONE) `tk_make_holiday_sequence()`
    - (DONE) `tk_get_holiday_signature()`, `tk_augment_holiday_signature()`
    - (DONE) `step_holiday_signature()`
- Weather
    - Reim integration
    - `tk_get_weather_signature()`, `tk_augment_weather_signature()`
    - `step_weather_signature()`
- Support for `grouped_df`
    - (DONE) `tk_augment_roll_apply()`
    - (DONE) `tk_augment_lags()`
    - `tk_augment_smooth()`
- Diagnostics
    - Missing Value Diagnostics - Weekends, Holidays - Correlation Funnel???
    - (DONE) ACF Diagnostics - `tk_acf_diagnostics` - Tidy acf, pacf, ccf
    - (DONE) Seasonality Diagnostics - Review FB Prophet Plot Components
    - (DONE) Rsample Validation Diagnostics
- Tests (Boooo)
- Update Examples
- Update Readme
- New Vignettes
  - Plotting
  - Seasonality
  - Correlation
-->

__New Interactive Plotting Functions__:

* `plot_time_series()` - __A workhorse time-series plotting function__ that generates interactive `plotly` plots, consolidates 20+ lines of `ggplot2` code, and scales well to many time series using dplyr groups. 
* `plot_acf_diagnostics()` - Visualize the ACF, PACF, and any number of CCFs in one plot for Multiple Time Series. Interactive `plotly` by default. 
* `plot_seasonal_diagnostics()` - Visualize Multiple Seasonality Features for One or More Time Series. Interactive `plotly` by default. 
* `plot_stl_diagnostics()` - Visualize STL Decomposition Features for One or More Time Series.
* `plot_time_series_cv_plan()` - Visualize the Time Series Cross Validation plan made with `time_series_cv()`.

__New Time Series Data Wrangling__:

* `summarise_by_time()` - A time-based variant of `dplyr::summarise()` for flexible summarization using common time-based criteria. 
* `filter_by_time()` - A time-based variant of `dplyr::filter()` for flexible filtering by time-ranges.
* `pad_by_time()` - Insert time series rows with regularly spaced timestamps.
* `slidify()` - Make any function a rolling / sliding function. 
* `between_time()` - A time-based variant of `dplyr::between()` for flexible time-range detection. 
* `add_time()` - Add for time series index. Shifts an index by a `period`. 

__New Recipe Functions:__

Feature Generators:

* `step_holiday_signature()` - New recipe step for adding 130 holiday features based on individual holidays, locales, and stock exchanges / business holidays. 
* `step_fourier()` - New recipe step for adding fourier transforms for adding seasonal features to time series data
* `step_roll_apply()` - New recipe step for adding rolling summary functions. Similar to `recipes::step_window()` but is more flexible by enabling application of any summary function. 
* `step_smooth()` - New recipe step for adding Local Polynomial Regression (LOESS) for smoothing noisy time series
* `step_diff()` - New recipe for adding multiple differenced columns. Similar to `recipes::step_lag()`.
* `step_box_cox()` - New recipe for transforming predictors. Similar to `step_BoxCox()` with improvements for forecasting including "guerrero" method for lambda selection and handling of negative data. 
* `step_impute_ts()` - New recipe for imputing a time series. 

__New Rsample Functions__

* `time_series_cv()` - Create `rsample` cross validation sets for time series. This function produces a sampling plan starting with the most recent time series observations, rolling backwards. 

__New Vector Functions:__

These functions are useful on their own inside of `mutate()` and power many of the new plotting and recipes functions.

* `roll_apply_vec()` - Vectorized rolling apply function - wraps `slider::slide_vec()`
* `smooth_vec()` - Vectorized smoothing function - Applies Local Polynomial Regression (LOESS)
* `diff_vec()` and `diff_inv_vec()` - Vectorized differencing function. Pads `NA`'s by default (unlike `stats::diff`).
* `lag_vec()` - Vectorized lag functions. Returns both lags and leads (negative lags) by adjusting the `.lag` argument. 
* `box_cox_vec()`, `box_cox_inv_vec()`, & `auto_lambda()` - Vectorized Box Cox transformation. Leverages `forecast::BoxCox.lambda()` for automatic lambda selection. 
* `fourier_vec()` - Vectorized Fourier Series calculation.
* `impute_ts_vec()` - Vectorized imputation of missing values for time series. Leverages `forecast::na.interp()`.

__New Augment Functions__:

All of the functions are designed for scale. They respect `dplyr::group_by()`.

* `tk_augment_holiday_signature()` - Add holiday features to a `data.frame` using only a time-series index.
* `tk_augment_roll_apply()` - Add multiple columns of rolling window calculations to a `data.frame`.
* `tk_augment_differences()` - Add multiple columns of differences to a `data.frame`. 
* `tk_augment_lags()` - Add multiple columns of lags to a `data.frame`. 
* `tk_augment_fourier()` - Add multiple columns of fourier series to a `data.frame`.


__New Make Functions__:

Make date and date-time sequences between start and end dates.

* `tk_make_timeseries()` -  Super flexible function for creating daily and sub-daily time series. 
* `tk_make_weekday_sequence()` - Weekday sequence that accounts for both __stripping weekends and holidays__
* `tk_make_holiday_sequence()` - Makes a sequence of dates corresponding to business holidays in calendars from `timeDate` (common non-working days)
* `tk_make_weekend_sequence()` - Weekday sequence of dates for Saturday and Sunday (common non-working days)

__New Get Functions__:

* `tk_get_holiday_signature()` - Get 100+ holiday features using only a time-series index.
* `tk_get_frequency()` and `tk_get_trend()` - Automatic frequency and trend calculation from a time series index. 


__New Diagnostic / Data Processing Functions__

* `tk_summary_diagnostics()`  - Group-wise time series summary. 
* `tk_acf_diagnostics()` - The data preparation function for `plot_acf_diagnostics()`
* `tk_seasonal_diagnostics()` - The data preparation function for `plot_seasonal_diagnostics()`
* `tk_stl_diagnostics()` - Group-wise STL Decomposition (Season, Trend, Remainder). Data prep for `plot_stl_diagnostics()`.
* `tk_time_series_cv_plan` - The data preparation function for `plot_time_series_cv_plan()`


__New Datasets__

- __M4 Competition__ - Sample "economic" datasets from hourly, daily, weekly, monthly, quarterly, and yearly.
- __Walmart Recruiting Retail Sales Forecasting Competition__ - Sample of 7 retail time series
- __Web Traffic Forecasting (Wikipedia) Competition__ - Sample of 10 website time series
- __Taylor's Energy Demand__ - Single time series with 30-minute interval of energy demand
- __UCI Bike Sharing Daily__ - A time series consisting of Capital Bikesharing Transaction Counts and related time-based features. 


__Improvements:__
* `tk_make_future_timeseries()` - Now accepts `n_future` as a time-based phrase like "12 seconds" or "1 year".

__Bug Fixes:__

* [Don't set timezone on date](https://github.com/business-science/timetk/pull/32) - Accommodate recent changes to `lubridate::tz<-` which now returns POSIXct when used Date objects. Fixed in PR32 by @vspinu. 

__Potential Breaking Changes:__

* `tk_augment_timeseries_signature()` - Changed from `data` to `.data` to prevent name collisions when piping. 




# timetk 0.1.3 

__New Features:__

* `recipes` Integration - Ability to apply ___time series feature engineering___ in the `tidymodels` machine learning workflow. 
    * `step_timeseries_signature()` - New `step_timeseries_signature()` for adding date and date-time features.
* New Vignette - _"Time Series Machine Learning"_ (previously forecasting using the time series signature)

__Bug Fixes:__

* `xts::indexTZ` is deprecated. Use `tzone` instead.
* Replace `arrange_` with `arrange`.
* Fix failing tests due to `tidyquant` 1.0.0 upagrade (single stocks now return an extra symbol column).

# timetk 0.1.2

* Compatability with `tidyquant` v0.5.7 - Removed dependency on `tidyverse`
* Dependency cleanup - removed devtools and other unncessary dependencies.

# timetk 0.1.1

* Added `timeSeries` to Suggests to satisfy a CRAN issue.

# timetk 0.1.0
* Renamed package `timetk`. Was formerly `timekit`. 
* Improvements:
    * Fixed issue with back-ticked date columns
    * Update pkgdown
    * support for `robets`

<!-- 
The information below is kept for historical reasons since this package was converted from the previous `timekit` package

# timetk 0.3.1
* Improvements:
    * `tk_index` and `tk_has_index`: Added `decomposed.ts` 
    * `tk_get_timeseries_signature` and `tk_augment_timeseries_signature`:
        * Added "year.iso" (Issue #4)
    * Cleaned up Imports:
        * Moved `tidyquant`, `broom`, `forcats`, `scales` to Suggests 
        * Removed `quantmod`, `TTR`, `timeSeries` and `tseries` from Imports
    


# timetk 0.3.0
* Improvements:
    * `tk_make_future_timeseries`: 
        * Improved future date picking algorithm to look for seasonal trends by way of `inspect_months`. The data must span at least two calendar years to gain this benefit. 
        * Improved handling of index in situations with units = days and scale greater than day (e.g. weekly, monthly, quarterly, +). Now values returned will be a regular series of the appropriate scale versus an irregular series indexed by regularly spaced seconds. For example, monthly dates in units = days will be predicted as the first day of each month rather than by the median frequency in seconds.
        * Includes a new argument, `insert_values` to add time-based values into a time series. Before you could remove via `skip_values`, and now you can add via `insert_values`.
        * Changed `n_future` to be inclusive of skip_values and weekends, which means that the end date of the future period is the same regardless of `inspect_weekdays`, `inspect_months`, `skip_values`, and now `insert_values`. Now the end dates are fixed which helps when manipulating future days.
* Documentation:
    * (New Vignette) TK02 - Making a Future Time Series Index using timetk
    * (New Vignette) TK03 - Forecasting Using a Time Series Signature with timetk
         

# timetk 0.2.0

* New Functions:
    * `tk_make_future_timeseries` creates a future time-based index from an existing time-based index. 
    * `tk_get_timeseries_signature` returns a tibble with the time series index decomposed into year, quarter, month, etc.
    * `tk_get_timeseries_summary` returns summary metrics for the time series index including number of observations, start, end, units, scale, diff summary (summary for frequency in seconds), etc.
    * `tk_augment_timeseries_signature` adds the time series signature to a `tbl` (with time base), `xts` or `zoo` object.
    * `tk_get_timeseries_variables` returns the variable column names for date, datetime, yearmon, or yearqtr variables in a data frame. 
* Fixes:
    * `tk_index`: Fixed timezone issue with yearmon and yearqtr classes. 
    * `tk_tbl`: Changed argument from `index_rename` to `rename_index` for consistency.
    


# timetk 0.1.0 

* Initial release of `timetk`, a time series toolkit that simplifies coercion between time series classes and time-based tibbles ("tidy" data frames).

-->
