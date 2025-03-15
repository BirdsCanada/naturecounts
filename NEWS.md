# naturecounts dev
* Add option to clip EOO in `cosewic_ranges()` to a particular shapefile before calculating area
* Add option to scale records in `cosewic_plot()`
* Change API to naturecounts.ca
* Add option to change the CRS for all COSEWIC-related functions.
* Update default CRS for `cosweic_ranges()` to an equal area projection 
  (add `crs_albers_canada()` to this effect)
* Fix bug with request ids where tried to download unapproved collections.

# naturecounts 0.4.1
* Fix use of species_id and record_id in `cosewic_ranges()`.
  * Truly allow different columns
  * Also allow no `species_id` or `record_id` columns

# naturecounts 0.4.0
* Add tools for COSEWIC assessments
* Fix bug in search_species() which returned all authorities
* Updated dependency to dplyr 1.1.0
* Updated tests to testthat 3rd edition
* Re-wrote internal requests formatting to improve speed
* `nc_dl_data()` returns a warning (not an error) if there are no data for the 
  given set of filters (to facilitate looping downloads)
* Warn (not error) when database has been created with an older version of the package

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
