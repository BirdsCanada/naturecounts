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
  retain = TRUE
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

## Value

For sf 'POINT' or terra 'points' input data, original data with
column(s) appended containing the Daymet data value(s) at each point.

For sf 'POLYGON' or terra 'polygons' input data, original data with
column(s) appended containing the mean Daymet data value(s) within each
polygon.

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

## See also

[`daymet_request()`](https://birdscanada.github.io/naturecounts/dev/reference/daymet_request.md)
which can be used to submit requests for Dayment data.

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
