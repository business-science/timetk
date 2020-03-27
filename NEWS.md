# timetk 0.1.3.9000

__TODO:__

- Plot Diagnostics
    - Plot Time Series
    - Plot Lag Diagnostics
    - Plot Seasonality Diagnostics - Correlation Funnel
- Lag automation 
    - [Rsample PR #136](https://github.com/tidymodels/rsample/pull/136)
    - Until accepted: `initial_time_split_2()`, `rolling_origin_2()`
    - `recipes::step_lag()` 
    - `tk_augment_lags()`
    - `tk_get_lags()`
- Fourier series
    - (DONE) `step_fourier_series()`
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
- Box Cox functionality
    - `box_cox_vec`
    - `box_cox_inv_vec`
    - `guerrero`
    - `step_box_cox`
- Dplyr Transition 
    - `tidyquant::summarise_by_time`
    - `tibbletime::filter_by_time`
    - `tibbletime::as_period`
- Date Sequence & Holiday Transition 
    - (DONE) `tk_make_holiday_sequence()`
    - (DONE) `tk_get_holiday_signature()`, `tk_augment_holiday_signature()`
    - (DONE) `step_holiday_signature()`
- Support for `grouped_df`
    - (DONE) `tk_augment_roll_apply()`
    - `tk_augment_lags()`
    - `tk_augment_smooth()`
- Diagnostics
    - Missing values - Weekends, Holidays - Correlation Funnel???
    - Tidy acf, pacf, ccf
    - Tidy dft
- Tests

__New Make Functions__:

Make date and date-time sequences between start and end dates.

* `tk_make_index_sequence()` -  Super flexible - Makes both daily and subdaily time series. 
* `tk_make_weekday_sequence()` - Weekday sequence that accounts for both __stripping weekends and holidays!__
* `tk_make_holiday_sequence()` - Makes a sequence of dates corresponding to holidays in calendars from `timeDate`
* `tk_make_weekend_sequence()` - Weekday sequence of dates for Saturday and Sunday (common non-working days)

__New Get Functions__:

* `tk_get_holiday_signature()` - Get 100+ holiday features using only a time-series index.

__New Augment Functions__:

* `tk_augment_holiday_signature()` - Add holiday features to a `data.frame` using only a time-series index.
* `tk_augment_roll_apply()` - Add multiple columns of rolling window calculations to a `data.frame`.

__New Vector Functions:__

* `roll_apply_vec()` - Vectorized rolling apply function - wraps `slider::slide_vec()`
* `smooth_vec()` - Vectorized smoothing function - Applies Local Polynomial Regression (LOESS)

__New Recipe Functions:__

* `step_holiday_signature()` - New recipe step for adding 130 holiday features based on individual holidays, locales, and stock exchanges / business holidays. 
* `step_fourier_series()` - New recipe step for adding fourier transforms for adding seasonal features to time series data
* `step_roll_apply()` - New recipe step for adding rolling summary functions
* `step_smooth()` - New recipe step for adding Local Polynomial Regression (LOESS) for smoothing noisy time series

__New Rsample Functions__

* `initial_time_split_2()` and `rolling_origin_2()` - Added parameter `overlap` to accomodate lagged predictors. 

__Bug Fixes:__

* [Don't set timezone on date](https://github.com/business-science/timetk/pull/32) - Accommodate recent changes to `lubridate::tz<-` which now returns POSIXct when used Date objects. Fixed in PR32 by @vspinu. 

__(Potential) Breaking Changes:__

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
