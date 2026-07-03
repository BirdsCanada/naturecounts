# Submit Request for Daymet Data.

Submits requests for all available variables from the
[Daymet](https://daymet.ornl.gov/) at the spatial extent of provided
input observation data. All variables are available at a daily
resolution since 1980 in North America and Hawaii, and since 1950 in
Puerto Rico, and at a ~ 1 km spatial resolution. Requests are submitted
to the NASA AppEEARS service, requiring an EarthData account to be made.
This can be done at the following link: [register for an EarthData
account](https://urs.earthdata.nasa.gov/users/new).

## Usage

``` r
daymet_request(
  data,
  covariates = "daymet_prcp",
  ed_username,
  request_name = NULL,
  site_name = NULL,
  date_year = NULL,
  date_month = NULL,
  date_day = NULL,
  dl_path = NULL,
  save = FALSE,
  verbose = TRUE
)
```

## Arguments

- data:

  A `data.frame`, `sf` 'POINT' or 'POLYGON' object, or `terra` 'points'
  or 'polygons' object containing columns with the year, month, and day
  an observation was made either named the BMDE defaults `survey_year`,
  `survey_month` , and `survey_day` respectively or another name
  specified in arguments `date_year`, `date_month`, and/or `date_day`.

- covariates:

  Character, vector if multiple Daymet data types desired. By default,
  downloads Daymet precipitation data.

- ed_username:

  Character. The username associated with your EarthData account.

- request_name:

  Character. Optional argument to provide informative name for the
  AppEEARS request. This can make file management more intuitive for the
  user.

- site_name:

  Character. Optional argument to provide the name of the column
  containing site names if not contained within the BMDE column
  `SurveyAreaIdentifier`. Can be left `NULL` and still function properly
  if originally specified in a call to
  [`data_fmt()`](https://birdscanada.github.io/naturecounts/dev/reference/data_fmt.md).

- date_year:

  Character. Optional argument to provide the name of the column
  containing year data if not contained within the BMDE column
  `survey_year`. Can be left `NULL` and still function properly if
  originally specified in a call to
  [`data_fmt()`](https://birdscanada.github.io/naturecounts/dev/reference/data_fmt.md).

- date_month:

  Character. Optional argument to provide the name of the column
  containing month data if not contained within the BMDE column
  `survey_month`. Can be left `NULL` and still function properly if
  originally specified in a call to
  [`data_fmt()`](https://birdscanada.github.io/naturecounts/dev/reference/data_fmt.md).

- date_day:

  Character. Optional argument to provide the name of the column
  containing day-of-month (i.e., a number from 1 to 31) data if not
  contained within the BMDE column `survey_day`. Can be left `NULL` and
  still function properly if originally specified in a call to
  [`data_fmt()`](https://birdscanada.github.io/naturecounts/dev/reference/data_fmt.md).
  `survey_year`.

- dl_path:

  Character. Optional argument to provide path to save request
  information to. By default, data is downloaded to a subfolder
  `daymet/` in the working directory.

- save:

  Logical. Should Daymet request ID information be saved externally in a
  .rds file?

- verbose:

  Logical. Should messages be displayed?

## Value

A `data.frame` with three columns: 1) `request_name` containing AppEEARS
request names, 2) `request_id` containing AppEEARS request IDs, and 3)
`date` containing the date for which the associated request is
downloading data for.

## Details

One (or multiple) Daymet variable(s) can be requested by specifying the
following values to the `covariates` argument:

- Day length (s/day): `daymet_dayl`

- Precipitation (mm/day): `daymet_prcp`

- Shortwave radiation (W/m^2): `dayment_srad`

- Snow water equivalent (kg/m^2): `daymet_swe`

- Maximum air temperature (°C): `daymet_tmax`

- Minimum air temperature (°C): `daymet_tmin`

- Water vapor pressure (Pa): `daymet_vp`

Due to API limitations, one request will be submitted for each day in
`data`. Unfortunately, AppEEARS automatically sends an email upon
request receipt and completion for each request, so for users with many
observation dates, we recommend considering setting rules in their email
client for handling these emails (address <appeears-noreply@nasa.gov>).

To preserve request information in the event the R session ends, users
can choose to set `save = TRUE` and have request information saved
externally in a `.rds` file. Users can then provide the path to this
file to
[`daymet_check()`](https://birdscanada.github.io/naturecounts/dev/reference/daymet_check.md),
[`daymet_download()`](https://birdscanada.github.io/naturecounts/dev/reference/daymet_download.md)
or
[`daymet_extract()`](https://birdscanada.github.io/naturecounts/dev/reference/daymet_extract.md)
or read it back into the R environment using
[`base::readRDS()`](https://rdrr.io/r/base/readRDS.html).

Once requests are submitted, users can use
[`daymet_check()`](https://birdscanada.github.io/naturecounts/dev/reference/daymet_check.md)
to check the status of their requests. In the author's experience,
requests take from 1-24 hrs to process. Once requests are complete,
downloads can be executed with
[`daymet_download()`](https://birdscanada.github.io/naturecounts/dev/reference/daymet_download.md).

Requests are facilitated by a call to
[`appeears::rs_request()`](https://bluegreen-labs.github.io/appeears/reference/rs_request.html).

## See also

[`daymet_download()`](https://birdscanada.github.io/naturecounts/dev/reference/daymet_download.md)
to execute downloads once requests have been submitted and are complete.

[`daymet_check()`](https://birdscanada.github.io/naturecounts/dev/reference/daymet_check.md)
to check the status of existing requests.

[`daymet_extract()`](https://birdscanada.github.io/naturecounts/dev/reference/daymet_extract.md)
which can be used to extract data from downloaded Daymet files.

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
requests <- daymet_request(data = bcch,
                           covariates = "daymet_prcp",
                           ed_username = ed_username)

}
```
