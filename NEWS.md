# timetk 2.9.0

### Anomalize Integration:

`anomalize` R package is now available in `timetk`:

1. `anomlize()`: 1 function that breaks down, identifies, and cleans anomalies
2. `plot_anomalies()`: Visualize the anomalies and anomaly bands
3. `plot_anomalies_decomp()`: Visualize the time series decomposition. Make adjustments as needed.
4. `plot_anomalies_cleaned()`: Visualize the before/after of cleaning anomalies. 

Note - `anomalize(.method)`: Only the `.method = "stl"` is supported at this time. The `"twitter"` method is also planned.

### Other Changes:

- Removed dependency on tidymodels. (#154, @olivroy).


# timetk 2.8.4

Update forecasting vignette: Use `glmnet` for time series forecasting. 

CRAN Fixes:
- `tzdata` time zone fixes:
   - GB            -> Europe/London
   - NZ            -> Pacific/Auckland     
   - US/Eastern    -> America/New_York     
   - US/Pacific    -> America/Los_Angeles 
- Add `@aliases` to timetk-package

# timetk 2.8.3

- remove support for `robets`
- remove `tidyquant` from examples 
- remove `tidyverse` from examples
- add `FANG` dataset to `timetk` (port from `tidyquant`)
- cran: fix return, dontrun -> donttest, options(max.print)

# timetk 2.8.2

__New Features__

- `plot_time_series()`: Gets new arguments to specify `.x_intercept` and `.x_intercept_color`. #131

__Fixes__

- Fix error in `plot_time_series()` when `.group_names` is not found. #121
- Merge variable checking update needed for `recipes >= 1.0.3` #132


# timetk 2.8.1

### Trelliscope Plotting

- Expose the `facet_trelliscope()` plotting parameters. 
  - `plot_time_series()`
  - `plot_time_series_boxplot()`
  - `plot_anomaly_diagnostics()`

# timetk 2.8.0

__New Features__

Many of the plotting functions have been upgraded for use with `trelliscopejs` for 
easier visualization of many time series. 

- `plot_time_series()`: 
    - Gets a new argument `trelliscope`: Used for visualizing many time series.
    - Gets a new argument `.facet_strip_remove` to remove facet strips since trelliscope is automatically labeled.
    - Gets a new argument `.facet_nrow` to adjust grid with trelliscope.
    - The default argument for `facet_collapse = TRUE` was changed to `FALSE` for better compatibility with Trelliscope JS. This may cause some plots to have multiple groups take up extra space in the strip.
    
- `plot_time_series_boxplot()`: 
    - Gets a new argument `trelliscope`: Used for visualizing many time series.
    - Gets a new argument `.facet_strip_remove` to remove facet strips since trelliscope is automatically labeled.
    - Gets a new argument `.facet_nrow` to adjust grid with trelliscope.
    - The default argument for `.facet_collapse = TRUE` was changed to `FALSE` for better compatibility with Trelliscope JS. This may cause some plots to have multiple groups take up extra space in the strip.
  
- `plot_anomaly_diagnostics()`: 
    - Gets a new argument `trelliscope`: Used for visualizing many time series.
    - Gets a new argument `.facet_strip_remove` to remove facet strips since trelliscope is automatically labeled.
    - Gets a new argument `.facet_nrow` to adjust grid with trelliscope.
    - The default argument for `.facet_collapse = TRUE` was changed to `FALSE` for better compatibility with Trelliscope JS. This may cause some plots to have multiple groups take up extra space in the strip.


__Updates & Bug Fixes__

- Recipes steps (e.g. `step_timeseries_signature()`) use the new `recipes::print_step()` function. Requires `recipes >= 0.2.0`. #110

- Offset parameter in `step_log_interval()` was not working properly. Now works. #103


__Potential Breaking Changes__

- The default argument for `.facet_collapse = TRUE` was changed to `FALSE` for better compatibility with Trelliscope JS. This may cause some plots to have multiple groups take up extra space in the strip. 

# timetk 2.7.0

__New Features__

- `tk_tsfeatures()`: A new function that makes it easy to generate time series feature matrix using `tsfeatures`. The main benefit is that you can pipe time series data in `tibbles` with `dplyr` groups. The features will be produced by group. #95 #84

- `plot_time_series_boxplot()`: A new function that makes plotting time series boxplots simple using a `.period` argument for time series aggregation. 

__New Vignettes__

- [Time Series Clustering:](https://business-science.github.io/timetk/articles/TK09_Clustering.html) Uses the new `tk_tsfeatures()` function to perform time series clustering. #95 #84

- [Time Series Visualization:](https://business-science.github.io/timetk/articles/TK04_Plotting_Time_Series.html) Updated to include `plot_time_series_boxplot()` and `plot_time_series_regression()`.

__Improvements__

Improvements for __point forecasting__ when the target is n-periods into the future.

- `time_series_cv()`, `time_series_split()`: New parameter `point_forecast`. This is useful for testing / assessing the n-th prediction in the future. When set to `TRUE`, will return a single point that returns on the last value in `assess`.  

__Fixes__

- Updates for rlang > 0.4.11 (dev version) #98
- `plot_time_series()`: Smoother no longer fails when time series has 1 observation #106

# timetk 2.6.2

__Improvements__

- `summarize_by_time()`: Added a `.week_start` argument to allow specifying `.week_start = 1` for Monday start. Default is 7 for Sunday Start. This can also be changed with the `lubridate` by setting the `lubridate.week.start` option. 

- Plotting Functions: 
  
  - Several plotting functions gain a new `.facet_dir` argument for adjusting the direction of `facet_wrap(dir)`. #94 
  - Plot ACF Diagnostics (`plot_acf_diagnostics()`): Change default parameter to `.show_white_noise_bars = TRUE`. #85
  - `plot_timeseries_regression()`: Can now `show_summary` for group-wise models when visualizing groups

- Time Series CV (`time_series_cv()`): Add Label for `tune_results`

- Improve speed of `pad_by_time()`. #93

__Bug Fixes__

- `tk_make_timeseries()` and `tk_make_future_timeseries()` are now able to handle end of months. #72

- `tk_tbl.zoo()`: Fix an issue when `readr::type_convert()` produces warning messages about not having character columns in inputs. #89

- `plot_time_series_regression()`: Fixed an issue when lags are added to `.formula`. Pads lags with NA. 

- `step_fourier()` and `fourier_vec()`: Fixed issue with step_fourier failing with one observation. Added scale_factor argument to override date sequences with the stored scale factor. #77 



# timetk 2.6.1

__Improvements__

- `tk_augment_slidify()`, `tk_augment_lags()`, `tk_augment_leads()`, `tk_augment_differences()`: Now works with multiple columns (passed via `.value`) and `tidyselect` (e.g. `contains()`).

__Fixes__

- Reduce "New names" messages. 
```
#> New names: 
#> * NA -> ...1 
```

- Remove dependency on `lazyeval`. #24
- Fix deprecated functions:  `select_()` used with `tk_xts_()`. #52

# timetk 2.6.0

__New Functions__

- `filter_period()` (#64): Applies filtering expressions within time-based periods (windows). 
- `slice_period()` (#64): Applies slices within time-based periods (windows).
- `condense_period()` (#64): Converts a periodicity from a higher (e.g. daily) to lower (e.g. monthly) frequency. Similar to `xts::to.period()` and `tibbletime::as_period()`.
- `tk_augment_leads()` and `lead_vec()` (#65): Added to make it easier / more obvious on how to create leads. 

__Fixes__

- `time_series_cv()`: Fix bug with Panel Data. Train/Test Splits only returning 1st observation in final time stamp. Should return all observations. 
- `future_frame()` and `tk_make_future_timeseries()`: Now sort the incoming index to ensure dates returned go into the future. 
- `tk_augment_lags()` and `tk_augment_slidify()`: Now overwrite column names to match the behavior of `tk_augment_fourier()` and `tk_augment_differences()`.


# timetk 2.5.0

__Improvements__

- `time_series_cv()`: Now works with time series groups. This is great for working with panel data. 
- `future_frame()`: Gets a new argument called `.bind_data`. When set to `TRUE`, it performs a data
  binding operation with the incoming data and the future frame. 

__Miscellaneous__

- Tune startup messages (#63)

# timetk 2.4.0

- `step_slidify_augment()` - A variant of step slidify that adds multiple rolling columns inside of a recipe. 

__Bug Fixes__

- Add warning when `%+time%` and `%-time%` return missing values
- Fix issues with `tk_make_timeseries()` and `tk_make_future_timeseries()` providing odd results for regular time series. [GitHub Issue 60](https://github.com/business-science/timetk/issues/60)

# timetk 2.3.0

__New Functionality__

- `tk_time_series_cv_plan()` - Now works with k-fold cross validation objects from `vfold_cv()` function. 

- `pad_by_time()` - Added new argument `.fill_na_direction` to specify a `tidyr::fill()` strategy for filling missing data. 

__Bug Fixes__

- Augment functions (e.g. `tk_augment_lags()`) - Fix bug with grouped functions not being exported
- Vectorized Functions - Compatabiliy with `ts` class 



# timetk 2.2.1

__New Functions__

- `step_log_interval_vec()` - Extends the `log_interval_vec()` for `recipes` preprocessing.

__Parallel Processing__

- Parallel backend for use with `tune` and `recipes`

__Bug Fixes__

- `log_interval_vec()` - Correct the messaging
- `complement.ts_cv_split` - Helper to show time series cross validation splits in list explorer. 

# timetk 2.2.0 

__New Functions__

- `mutate_by_time()`: For applying mutates by time windows
- `log_interval_vec()` & `log_interval_inv_vec()`: For constrained interval forecasting. 

__Improvements__

- `plot_acf_diagnostics()`: A new argument, `.show_white_noise_bars` for adding white noise bars to an ACF / PACF Plot.
- `pad_by_time()`: New arguments `.start_date` and `.end_date` for expanding/contracting the padding windows. 

# timetk 2.1.0 

__New Functions__

* `plot_time_series_regression()`: Convenience function to visualize & explore features using Linear Regression (`stats::lm()` formula).
* `time_series_split()`: A convenient way to return a single split from `time_series_cv()`. Returns the split in the same format as `rsample::initial_time_split()`.

__Improvements__

* __Auto-detect date and date-time__: Affects `summarise_by_time()`, `filter_by_time()`, `tk_summary_diagnostics`
* `tk_time_series_cv_plan()`: Allow a single resample from `rsample::initial_time_split` or `timetk::time_series_split`
* __Updated Vignette:__ The vignette, "Forecasting Using the Time Series Signature", has been updated with `modeltime` and `tidymodels`. 

__Plotting Improvements__

* __All plotting functions now support Tab Completion__ (a minor breaking change was needed to do so, see breaking changes below)
* `plot_time_series()`: 
    - Add `.legend_show` to toggle on/off legends. 
    - Permit numeric index (fix issue with smoother failing)


__Breaking Changes__

* __Tab Completion__: Replace `...` with `.facet_vars` or `.ccf_vars`. This change is needed to improve tab-completion. It affects : 
    - `plot_time_series()`
    - `plot_acf_diagnostics()`
    - `plot_anomaly_diagnostics()`
    - `plot_seasonal_diagnostics()`
    - `plot_stl_diagnostics()`
    
__Bug Fixes__

- `fourier_vec()` and `step_fourier_vec()`: Add error if observations have zero difference. [Issue #40.](https://github.com/business-science/timetk/issues/40)

# timetk 2.0.0

__New Interactive Plotting Functions__

* `plot_anomaly_diagnostics()`: Visualize Anomalies for One or More Time Series

__New Data Wrangling Functions__

* `future_frame()`: Make a future tibble from an existing time-based tibble.

__New Diagnostic / Data Processing Functions__

* `tk_anomaly_diagnostics()`  - Group-wise anomaly detection and diagnostics. A wrapper for the `anomalize` R package functions without importing `anomalize`.  

__New Vectorized Functions__:

* `ts_clean_vec()` - Replace Outliers & Missing Values in a Time Series
* `standardize_vec()` - Centers and scales a time series to mean 0, standard deviation 1
* `normalize_vec()` - Normalizes a time series to Range: (0, 1)

__New Recipes Preprocessing Steps__:

* `step_ts_pad()` - Preprocessing for padding time series data. Adds rows to fill in gaps and can be used with `step_ts_impute()` to interpolate going from low to high frequency!
* `step_ts_clean()` - Preprocessing step for cleaning outliers and imputing missing values in a time series.



__New Parsing Functions__

* `parse_date2()` and `parse_datetime2()`: These are similar to `readr::parse_date()` and `lubridate::as_date()` in that they parse character vectors to date and datetimes. The key advantage is SPEED. `parse_date2()` uses `anytime` package to process using C++ `Boost.Date_Time` library.

__Improvements__:

* `plot_acf_diagnostics()`: The `.lags` argument now handles time-based phrases (e.g. `.lags = "1 month"`).
* `time_series_cv()`: Implements time-based phrases (e.g. `initial = "5 years"` and `assess = "1 year"`)
* `tk_make_future_timeseries()`: The `n_future` argument has been deprecated for a new `length_out` argument that accepts both numeric input (e.g. `length_out = 12`) and time-based phrases (e.g. `length_out = "12 months"`). A major improvement is that numeric values define the number of timestamps returned even if weekends are removed or holidays are removed. Thus, you can always anticipate the length. ([Issue #19](https://github.com/business-science/timetk/issues/19)).
* `diff_vec`: Now reports the initial values used in the differencing calculation. 

__Bug Fixes__:

* `plot_time_series()`: 
    - Fix name collision when `.value = .value`. 
* `tk_make_future_timeseries()`: 
    - Respect timezones
* `time_series_cv()`: 
    - Fix incorrect calculation of starts/stops
    - Make `skip = 1` default. `skip = 0` does not make sense. 
    - Fix issue with `skip` adding 1 to stops. 
    - Fix printing method
* `plot_time_series_cv_plan()` & `tk_time_series_cv_plan()`: 
    - Prevent name collisions when underlying data has column "id" or "splits"
* `tk_make_future_timeseries()`: 
    - Fix bug when day of month doesn't exist. Lubridate `period()` returns `NA`. Fix implemented with `ceiling_date()`.
* `pad_by_time()`: 
    - Fix `pad_value` so only inserts pad values where new row was inserted. 
* `step_ts_clean()`, `step_ts_impute()`: 
    - Fix issue with `lambda = NULL`
    
__Breaking Changes__:

These should not be of major impact since the 1.0.0 version was just released. 

* Renamed `impute_ts_vec()` to `ts_impute_vec()` for consistency with `ts_clean_vec()`
* Renamed `step_impute_ts()` to `step_ts_impute()` for consistency with underlying function
* Renamed `roll_apply_vec()` to `slidify_vec()` for consistency with `slidify()` & relationship to `slider` R package
* Renamed `step_roll_apply` to `step_slidify()` for consistency with `slidify()` & relationship to `slider` R package
* Renamed `tk_augment_roll_apply` to `tk_augment_slidify()` for consistency with `slidify()` & relationship to `slider` R package
* `plot_time_series_cv_plan()` and `tk_time_series_cv_plan()`: Changed argument from `.rset` to `.data`. 


# timetk 1.0.0 

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
