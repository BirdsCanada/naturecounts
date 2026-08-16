# Extract Data from Daymet.

Extracts all available variables from [Daymet](https://daymet.ornl.gov/)
and matches them to input observation data. All variables are available
at a daily resolution since 1980 in North America and Hawaii, and since
1950 in Puerto Rico, and at a ~ 1 km spatial resolution. This data can
be requested from the NASA AppEEARS service using
[`daymet_request()`](https://birdscanada.github.io/naturecounts/dev/reference/daymet_request.md)
and downloaded using
[`daymet_download()`](https://birdscanada.github.io/naturecounts/dev/reference/daymet_download.md).

## Usage

``` r
daymet_extract(
  data,
  daymet_reqs,
  covariates = "daymet_prcp",
  site_name = NULL,
  date_year = NULL,
  date_month = NULL,
  date_day = NULL,
  dl_path = NULL,
  verbose = TRUE,
  retain = TRUE,
  ...
)
```

## Arguments

- data:

  A `data.frame`, `sf` 'POINT' or 'POLYGON' object, or `terra` 'points'
  or 'polygons' object containing columns with the year, month, and day
  an observation was made either named the BMDE defaults `survey_year`,
  `survey_month` , and `survey_day` respectively or another name
  specified in arguments `date_year`, `date_month`, and/or `date_day`.

- daymet_reqs:

  `data.frame`. A `data.frame` with columns 1) `request_name` containing
  AppEEARS request names, 2) `request_id` containing AppEEARS request
  IDs, and optionally 3) `date` containing the date for which the
  associated request is downloading data for, or a filepath to a `.rds`
  file containing such data. The direct output of
  [`daymet_request()`](https://birdscanada.github.io/naturecounts/dev/reference/daymet_request.md)
  can be supplied here.

- covariates:

  Character, vector if multiple Daymet data types desired. By default,
  extracts Daymet precipitation data.

- site_name:

  Character. Optional argument to provide the name of the column
  containing site names if not contained within the BMDE column
  `SurveyAreaIdentifier`. Can be left `NULL` and still function properly
  if originally specified in a call to
  [`data_fmt()`](https://birdscanada.github.io/naturecounts/dev/reference/data_fmt.md)
  or
  [`daymet_download()`](https://birdscanada.github.io/naturecounts/dev/reference/daymet_download.md).

- date_year:

  Character. Optional argument to provide the name of the column
  containing year data if not contained within the BMDE column
  `survey_year`. Can be left `NULL` and still function properly if
  originally specified in a call to
  [`data_fmt()`](https://birdscanada.github.io/naturecounts/dev/reference/data_fmt.md)
  or
  [`daymet_download()`](https://birdscanada.github.io/naturecounts/dev/reference/daymet_download.md).

- date_month:

  Character. Optional argument to provide the name of the column
  containing month data if not contained within the BMDE column
  `survey_month`. Can be left `NULL` and still function properly if
  originally specified in a call to
  [`data_fmt()`](https://birdscanada.github.io/naturecounts/dev/reference/data_fmt.md)
  or
  [`daymet_download()`](https://birdscanada.github.io/naturecounts/dev/reference/daymet_download.md).

- date_day:

  Character. Optional argument to provide the name of the column
  containing day-of-month (i.e., a number from 1 to 31) data if not
  contained within the BMDE column `survey_day`. Can be left `NULL` and
  still function properly if originally specified in a call to
  [`data_fmt()`](https://birdscanada.github.io/naturecounts/dev/reference/data_fmt.md)
  or
  [`daymet_download()`](https://birdscanada.github.io/naturecounts/dev/reference/daymet_download.md).

- dl_path:

  Character. Optional argument to provide path to downloaded data. By
  default, data is downloaded to a subfolder `daymet/` in the working
  directory.

- verbose:

  Logical. Should messages be displayed?

- retain:

  Logical. Should Daymet data files be kept after extraction? If
  `FALSE`, files will be deleted.

- ...:

  Other arguments passed to
  [`terra::extract()`](https://rspatial.github.io/terra/reference/extract.html)
  for `sf` 'POINT' or `terra` 'points' input data or
  [`exactextractr::exact_extract()`](https://isciences.gitlab.io/exactextractr/reference/exact_extract.html)
  `sf` 'POLYGON' or `terra` 'polygons' input data. Primarily useful for
  specifying alternate summary statistics to extract for `sf` 'POLYGON'
  or `terra` 'polygons' input data.

## Value

For sf 'POINT' or terra 'points' input data, original data with
column(s) appended containing the Daymet data value(s) at each point.

For sf 'POLYGON' or terra 'polygons' input data, original data with
column(s) appended containing the requested Daymet data value(s) within
each polygon.

## Details

One (or multiple) Daymet variable(s) can be extracted by specifying the
following values to the `covariates` argument. The appropriate variables
must be available in the AppEEARS request supplied to `daymet_reqs`.
Requests can be submitted via
[`daymet_request()`](https://birdscanada.github.io/naturecounts/dev/reference/daymet_request.md)
and downloaded via
[`daymet_download()`](https://birdscanada.github.io/naturecounts/dev/reference/daymet_download.md):

- Day length (s/day): `daymet_dayl`

- Precipitation (mm/day): `daymet_prcp`

- Shortwave radiation (W/m^2): `dayment_srad`

- Snow water equivalent (kg/m^2): `daymet_swe`

- Maximum air temperature (°C): `daymet_tmax`

- Minimum air temperature (°C): `daymet_tmin`

- Water vapor pressure (Pa): `daymet_vp`

By default, for `sf` 'POLYGON' or `terra` 'polygons' input data the mean
Daymet variable value will be returned. Other summary statistics can be
extracted by specifying the `fun` argument, which is passed to
[`exactextractr::exact_extract()`](https://isciences.gitlab.io/exactextractr/reference/exact_extract.html).
The available summary statistics are:

- `min` - the minimum non-`NA` value in any raster cell wholly or
  partially covered by the polygon

- `max` - the maximum non-`NA` value in any raster cell wholly or
  partially covered by the polygon

- `count` - the sum of fractions of raster cells with non-`NA` values
  covered by the polygon

- `sum` - the sum of non-`NA` raster cell values, multiplied by the
  fraction of the cell that is covered by the polygon

- `mean` - the mean cell value, weighted by the fraction of each cell
  that is covered by the polygon

- `median` - the median cell value, weighted by the fraction of each
  cell that is covered by the polygon

- `quantile` - arbitrary quantile(s) of cell values, specified in
  `quantiles`, weighted by the fraction of each cell that is covered by
  the polygon

- `mode` - the most common cell value, weighted by the fraction of each
  cell that is covered by the polygon. Where multiple values occupy the
  same maximum number of weighted cells, the largest value will be
  returned.

- `majority` - synonym for `mode`

- `minority` - the least common cell value, weighted by the fraction of
  each cell that is covered by the polygon. Where multiple values occupy
  the same minimum number of weighted cells, the smallest value will be
  returned.

- `variety` - the number of distinct values in cells that are wholly or
  partially covered by the polygon.

- `variance` - the population variance of cell values, weighted by the
  fraction of each cell that is covered by the polygon.

- `stdev` - the population standard deviation of cell values, weighted
  by the fraction of each cell that is covered by the polygon.

- `coefficient_of_variation` - the population coefficient of variation
  of cell values, weighted by the fraction of each cell that is covered
  by the polygon.

- `weighted_mean` - the mean cell value, weighted by the product of the
  fraction of each cell covered by the polygon and the value of a second
  weighting raster provided as `weights`

- `weighted_sum` - the sum of defined raster cell values, multiplied by
  the fraction of each cell that is covered by the polygon and the value
  of a second weighting raster provided as `weights`

- `weighted_stdev` - the population standard deviation of cell values,
  weighted by the product of the fraction of each cell covered by the
  polygon and the value of a second weighting raster provided as
  `weights`

- `weighted_variance` - the population variance of cell values, weighted
  by the product of the fraction of each cell covered by the polygon and
  the value of a second weighting raster provided as `weights`

- `frac` - returns one column for each possible value of `x`, with the
  the fraction of defined raster cells that are equal to that value.

- `weighted_frac` - returns one column for each possible value of `x`,
  with the fraction of defined cells that are equal to that value,
  weighted by \`weights.

User defined functions can also be passed to `fun`, but these must
return a single value. More information can be found in the
documentation for
[`exactextractr::exact_extract()`](https://isciences.gitlab.io/exactextractr/reference/exact_extract.html).

## See also

[`daymet_request()`](https://birdscanada.github.io/naturecounts/dev/reference/daymet_request.md)
which can be used to submit requests for Daymet data.

[`daymet_download()`](https://birdscanada.github.io/naturecounts/dev/reference/daymet_download.md)
to execute downloads once requests have been submitted and are complete.

[`nc_covariates_merge()`](https://birdscanada.github.io/naturecounts/dev/reference/nc_covariates_merge.md)
to merge extracted covariate data into data originally provided to the
`data` argument of
[`data_fmt()`](https://birdscanada.github.io/naturecounts/dev/reference/data_fmt.md).

## Examples

``` r
if (FALSE) { # interactive()
# Convert included test data on black-capped chickadees to sf POINT object
bcch <- sf::st_as_sf(
  bcch,
  coords = c("longitude", "latitude"),
  crs = 4326
)

# Grab data from a single year
bcch <- bcch[bcch$survey_year == 2011,]

# Enter EarthData username
ed_username <- "your EarthData username"

# Submit Daymet requests
requests <- daymet_download(data = bcch,
                            covariates = "daymet_prcp",
                            ed_username = ed_username)

# Once email is received confirming that request has been processed, execute
# download!
downloaded <- daymet_download(daymet_reqs = requests,
                              covariates = "daymet_prcp",
                              ed_username = ed_username)

# Once download is complete, extract!
extracted <- daymet_extract(data = bcch,
                            daymet_reqs = requests,
                            covariates = "daymet_prcp")

}
```
