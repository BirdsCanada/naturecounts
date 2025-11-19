# Changelog

## naturecounts 0.5.0

- Generalize
  [`cosewic_ranges()`](https://birdscanada.github.io/naturecounts/dev/reference/cosewic_ranges.md)
  (fixes [\#40](https://github.com/BirdsCanada/naturecounts/issues/40))
  - argument *species* now deprecated in favour of *group*
  - Add option to supply custom IAO grid to
    [`cosewic_ranges()`](https://birdscanada.github.io/naturecounts/dev/reference/cosewic_ranges.md)
  - Add option to clip EOO in
    [`cosewic_ranges()`](https://birdscanada.github.io/naturecounts/dev/reference/cosewic_ranges.md)
    to a particular shapefile before calculating area
  - Add option to change the CRS
  - Update default CRS for
    [`cosewic_ranges()`](https://birdscanada.github.io/naturecounts/dev/reference/cosewic_ranges.md)
    to an Albers Equal Area Projection for Canada (ESRI:102001)
  - Use a general `prop_include = 1` rather than `eoo_p = 1` and default
    to 1 rather than 0.95.
- Make
  [`cosewic_plot()`](https://birdscanada.github.io/naturecounts/dev/reference/cosewic_plot.md)s
  report-ready (fixes
  [\#41](https://github.com/BirdsCanada/naturecounts/issues/41))
  - Add option to scale records in
    [`cosewic_plot()`](https://birdscanada.github.io/naturecounts/dev/reference/cosewic_plot.md)
  - Add North arrow and scale to `cosweic_plot()`
  - Add option to change the CRS
  - Add more details to
    [`cosewic_plot()`](https://birdscanada.github.io/naturecounts/dev/reference/cosewic_plot.md)
    caption
- Change API to naturecounts.ca
  ([\#29](https://github.com/BirdsCanada/naturecounts/issues/29))
- Fix bug with request ids where tried to download unapproved
  collections
  ([\#32](https://github.com/BirdsCanada/naturecounts/issues/32))
- Clarify metadata (fixes
  [\#30](https://github.com/BirdsCanada/naturecounts/issues/30)
  - Clarify metadata versions as dates
  - Clarify which metadata is stored locally
  - Add checks and warn users if local metadata \>4 weeks old
- Make ‘extended’ the default field set
  ([\#31](https://github.com/BirdsCanada/naturecounts/issues/31))
- Explicitly alert users if
  [`format_zero_fill()`](https://birdscanada.github.io/naturecounts/dev/reference/format_zero_fill.md)
  used when there are ‘X’ values
  ([\#8](https://github.com/BirdsCanada/naturecounts/issues/8))

## naturecounts 0.4.1

- Fix use of species_id and record_id in
  [`cosewic_ranges()`](https://birdscanada.github.io/naturecounts/dev/reference/cosewic_ranges.md).
  - Truly allow different columns
  - Also allow no `species_id` or `record_id` columns

## naturecounts 0.4.0

- Add tools for COSEWIC assessments
- Fix bug in search_species() which returned all authorities
- Updated dependency to dplyr 1.1.0
- Updated tests to testthat 3rd edition
- Re-wrote internal requests formatting to improve speed
- `nc_dl_data()` returns a warning (not an error) if there are no data
  for the given set of filters (to facilitate looping downloads)
- Warn (not error) when database has been created with an older version
  of the package

## naturecounts 0.3.0

- Add
  [`nc_query_table()`](https://birdscanada.github.io/naturecounts/dev/reference/nc_query_table.md)
  for arbitrary and custom table queries
- Implement in-memory query caching (lasts 4 hours or until session
  restarts)
- Added
  [`nc_remove_cache()`](https://birdscanada.github.io/naturecounts/dev/reference/nc_remove_cache.md)
  to remove the in-memory cache as needed

## naturecounts 0.2.3

- switch back to regular server (not sandbox)
- use house finches as example data frame rather than barred owls

## naturecounts 0.2.2

- Arrange returned data fields more sensibly

## naturecounts 0.2.1

- Fixed bug with data extraction for
  [`nc_data_dl()`](https://birdscanada.github.io/naturecounts/dev/reference/nc_data_dl.md)
  and `nc_counts()`

## naturecounts 0.2.0

- Clarified access in `nc_counts()`
- Added extra event columns to
  [`format_zero_fill()`](https://birdscanada.github.io/naturecounts/dev/reference/format_zero_fill.md)
- No funny dates in Databases and no warnings for data frames when
  missing `survey_year`, `survey_month` or `survey_day` in
  [`format_dates()`](https://birdscanada.github.io/naturecounts/dev/reference/format_dates.md)

## naturecounts 0.1.0

- Initial package development
