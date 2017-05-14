# timekit 0.2.0.9000
* Improvements:
    * `tk_make_future_timeseries`: 
        * Improved future date picking algorithm to look for seasonal trends by way of `inspect_months`. The data must span at least two calendar years to gain this benefit. 
        * Improved handling of index in situations with units = days and scale greater than day (e.g. weekly, monthly, quarterly, +). Now values returned will be a regular series of the appropriate scale versus an irregular series indexed by regularly spaced seconds. For example, monthly dates in units = days will be predicted as the first day of each month rather than by the median frequency in seconds.
        * Changed `n_future` to be inclusive of skip_values and weekends, which means that the end date of the future period is the same regardless of `inspect_weekdays`, `inspect_months`, and/or `skip_values`.

# timekit 0.2.0

* New Functions:
    * `tk_make_future_timeseries` creates a future time-based index from an existing time-based index. 
    * `tk_get_timeseries_signature` returns a tibble with the time series index decomposed into year, quarter, month, etc.
    * `tk_get_timeseries_summary` returns summary metrics for the time series index including number of observations, start, end, units, scale, diff summary (summary for frequency in seconds), etc.
    * `tk_augment_timeseries_signature` adds the time series signature to a `tbl` (with time base), `xts` or `zoo` object.
    * `tk_get_timeseries_variables` returns the variable column names for date, datetime, yearmon, or yearqtr variables in a data frame. 
* Fixes:
    * `tk_index`: Fixed timezone issue with yearmon and yearqtr classes. 
    * `tk_tbl`: Changed argument from `index_rename` to `rename_index` for consistency.
    


# timekit 0.1.0 

* Initial release of `timekit`, a time series toolkit that simplifies coercion between time series classes and time-based tibbles ("tidy" data frames).
