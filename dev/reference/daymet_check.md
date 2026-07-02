# Check the status of Daymet Data Requests

Returns status information on requests submitted through
[`daymet_request()`](https://birdscanada.github.io/naturecounts/dev/reference/daymet_request.md).
Requests are submitted to the NASA AppEEARS service, requiring an
EarthData account to be made. This can be done at the following link:
[register for an EarthData
account](https://urs.earthdata.nasa.gov/users/new).

## Usage

``` r
daymet_check(daymet_reqs, ed_username, verbose = TRUE)
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

- verbose:

  Logical. Should messages be displayed?

## Value

A `data.frame` containing request status information.

## Details

Status checks are facilitated by a call to
[`appeears::rs_list_task()`](https://bluegreen-labs.github.io/appeears/reference/rs_list_task.html).

## See also

[`daymet_request()`](https://birdscanada.github.io/naturecounts/dev/reference/daymet_request.md)
which can be used to submit requests for Dayment data.
[`daymet_download()`](https://birdscanada.github.io/naturecounts/dev/reference/daymet_download.md)
to execute downloads once requests have been submitted and are complete.
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

# Check status
status_check <- daymet_check(daymet_reqs = requests,
                             ed_username = ed_username)

}
```
