# Download Data from Daymet.

Downloads all available variables from the
[Daymet](https://daymet.ornl.gov/) from request data either fetched via
[`daymet_request()`](https://birdscanada.github.io/naturecounts/dev/reference/daymet_request.md)
or supplied by the user. All variables are available at a daily
resolution since 1980 in North America and Hawaii, and since 1950 in
Puerto Rico, and at a ~ 1 km spatial resolution. This data is retreived
via the NASA AppEEARS service, requiring an EarthData account to be
made. This can be done at the following link: [register for an EarthData
account](https://urs.earthdata.nasa.gov/users/new). Users should be
aware that since these data are at a daily resolution a large number of
files will be downloaded for datasets with many dates.

## Usage

``` r
daymet_download(daymet_reqs, ed_username, dl_path = NULL, verbose = TRUE)
```

## Arguments

- daymet_reqs:

  `data.frame`. A `data.frame` with columns 1) `request_name` containing
  AppEEARS request names, 2) `request_id` containing AppEEARS request
  IDs, and optionally 3) `date` containing the date for which the
  associated request is downloading data for, or a filepath to a `.rds`
  file containing such data. The direct output of
  [`daymet_request()`](https://birdscanada.github.io/naturecounts/dev/reference/daymet_request.md)
  can be supplied here.

- ed_username:

  Character. The username associated with your EarthData account.

- dl_path:

  Character. Optional argument to provide path to download data to. By
  default, data is downloaded to a subfolder `scanfi/` in the working
  directory.

- verbose:

  Logical. Should messages be displayed?

## Value

A `data.frame` with three columns: 1) `request_name` containing AppEEARS
request names, 2) `request_id` containing AppEEARS request IDs, and 3)
`date` containing the date for which the associated request is
downloading data for.

## Details

One (or multiple) Daymet variable(s) can be downloaded by specifying the
following values to the `covariates` argument in
[`daymet_request()`](https://birdscanada.github.io/naturecounts/dev/reference/daymet_request.md):

- Day length (s/day): `daymet_dayl`

- Precipitation (mm/day): `daymet_prcp`

- Shortwave radiation (W/m^2): `dayment_srad`

- Snow water equivalent (kg/m^2): `daymet_swe`

- Maximum air temperature (°C): `daymet_tmax`

- Minimum air temperature (°C): `daymet_tmin`

- Water vapor pressure (Pa): `daymet_vp`

Downloads are facilitated by a call to
[`appeears::rs_transfer()`](https://bluegreen-labs.github.io/appeears/reference/rs_transfer.html).

## See also

[`daymet_request()`](https://birdscanada.github.io/naturecounts/dev/reference/daymet_request.md)
which can be used to submit requests for Dayment data.

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
# Once email is received confirming that request has been processed, execute
# download!
downloaded <- daymet_download(daymet_reqs = requests,
                              covariates = "daymet_prcp",
                              ed_username = ed_username)

}
```
