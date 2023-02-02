# naturecounts 0.3.0.9000
* Add tools for COSEWIC assessments

# naturecounts 0.3.0

* Add `nc_query_table()` for arbitrary and custom table queries
* Implement in-memory query caching (lasts 4 hours or until session restarts)
* Added `nc_remove_cache()` to remove the in-memory cache as needed

# naturecounts 0.2.3

* switch back to regular server (not sandbox)
* use house finches as example data frame rather than barred owls

# naturecounts 0.2.2

* Arrange returned data fields more sensibly

# naturecounts 0.2.1

* Fixed bug with data extraction for `nc_data_dl()` and `nc_counts()`

# naturecounts 0.2.0

* Clarified access in `nc_counts()`
* Added extra event columns to `format_zero_fill()`
* No funny dates in Databases and no warnings for data frames when missing 
  `survey_year`, `survey_month` or `survey_day` in `format_dates()`

# naturecounts 0.1.0

* Initial package development
