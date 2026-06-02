# Extract MODIS NDVI/EVI Data

Extracts [16-day NDVI/EVI
data](https://doi.org/10.5067/MODIS/MOD13A1.061) derived from imagery
from the MODIS Terra and Aqua satellites at approximately 500 m spatial
resolution. This data can be downloaded using
[`vegetation_download()`](https://birdscanada.github.io/naturecounts/dev/reference/vegetation_download.md).
The user guide for these data can be found
[here](https://lpdaac.usgs.gov/documents/621/MOD13_User_Guide_V61.pdf).

## Usage

``` r
vegetation_extract(
  data,
  covariates = "modis_ndvi",
  vegetation_files,
  reliability = FALSE,
  site_name = NULL,
  date_year = NULL,
  date_month = NULL,
  date_day = NULL,
  retain = TRUE
)
```

## Arguments

- data:

  An `sf` 'POINT' or 'POLYGON' object, or `terra` 'points' or 'polygons'
  object.

- covariates:

  Character, vector if both NDVI and EVI desired. By default, extracts
  NDVI (`modis_ndvi`).

- vegetation_files:

  Character, vector if multiple files. File-path(s) to downloaded MODIS
  vegetation data file(s). We recommend using
  [`vegetation_download()`](https://birdscanada.github.io/naturecounts/dev/reference/vegetation_download.md)
  to download MODIS files to ensure all files necessary for your data
  are captured. Direct output of
  [`vegetation_download()`](https://birdscanada.github.io/naturecounts/dev/reference/vegetation_download.md)
  can be supplied here.

- reliability:

  Logical. Should pixel reliability information be extracted at each
  site?

- site_name:

  Character. Optional argument to provide name of the column containing
  site names if not contained within the BMDE column
  `SurveyAreaIdentifier`. Can be left `NULL` and still function properly
  if originally specified in a call to
  [`data_fmt()`](https://birdscanada.github.io/naturecounts/dev/reference/data_fmt.md)
  or
  [`vegetation_download()`](https://birdscanada.github.io/naturecounts/dev/reference/vegetation_download.md).

- date_year:

  Character. Optional argument to provide the name of the column
  containing year data if not contained within the BMDE column
  `survey_year`. Can be left `NULL` and still function properly if
  originally specified in a call to
  [`data_fmt()`](https://birdscanada.github.io/naturecounts/dev/reference/data_fmt.md)
  or
  [`vegetation_download()`](https://birdscanada.github.io/naturecounts/dev/reference/vegetation_download.md).

- date_month:

  Character. Optional argument to provide the name of the column
  containing month data if not contained within the BMDE column
  `survey_month`. Can be left `NULL` and still function properly if
  originally specified in a call to
  [`data_fmt()`](https://birdscanada.github.io/naturecounts/dev/reference/data_fmt.md)
  or
  [`vegetation_download()`](https://birdscanada.github.io/naturecounts/dev/reference/vegetation_download.md).

- date_day:

  Character. Optional argument to provide the name of the column
  containing day-of-month (i.e., a number from 1 to 31) data if not
  contained within the BMDE column `survey_day`. Can be left `NULL` and
  still function properly if originally specified in a call to
  [`data_fmt()`](https://birdscanada.github.io/naturecounts/dev/reference/data_fmt.md)
  or
  [`vegetation_download()`](https://birdscanada.github.io/naturecounts/dev/reference/vegetation_download.md).

- retain:

  Logical. Should MODIS data files be kept after extraction. If `FALSE`,
  files will be deleted.

## Value

For `sf` 'POINT' or `terra` 'points' input data, original data with
numeric column(s) `ndvi` and/or `evi` appended containing the NDVI/EVI
value at that point. If reliability information requested, an additional
`vegetation_reliability` column is appended containing the reliability
assessment as defined in table 4 of the [product's user
manual](https://lpdaac.usgs.gov/documents/621/MOD13_User_Guide_V61.pdf).

For `sf` 'POLYGON' or `terra` 'polygons' input data, original data with
numeric column(s) `ndvi` and/or `evi` appended containing the mean
NDVI/EVI value within each polygon. If reliability information
requested, an additional `vegetation_reliability` column is appended
containing the percentage of pixels overlapped by each polygon in each
reliability assessment as defined in table 4 of the [product's user
manual](https://lpdaac.usgs.gov/documents/621/MOD13_User_Guide_V61.pdf).

## Details

Both NDVI and EVI are available through this function and can be
accessed by supplying the following arguments to the `covariates`
argument:

- `modis_ndvi` - NDVI

- `modis_evi` - EVI

Details on the calculation of these indices can be found in the [MOD13
user
guide](https://lpdaac.usgs.gov/documents/621/MOD13_User_Guide_V61.pdf).

NDVI/EVI calculations are sensitive to the presence of snow/ice and
cloudiness. So users can assess the quality of data extracted at each
site, we have included the option to extract pixel reliability
assessments included in these NDVI/EVI products by setting argument
`reliability = TRUE`. The reliability scale is as in the [user
manual](https://lpdaac.usgs.gov/documents/621/MOD13_User_Guide_V61.pdf),
and is supplied to the user in the `vegetation_reliability` column if
requested.

## See also

[`terra::extract()`](https://rspatial.github.io/terra/reference/extract.html)
which is used to extract values from MODIS data for `sf` 'POINT' and
`terra` 'points' input data.

[`exactextractr::exact_extract()`](https://isciences.gitlab.io/exactextractr/reference/exact_extract.html)
which is used to extract values from MODIS data for `sf` 'POLYGON' or
`terra` 'polygons' input data.

## Examples

``` r
if (FALSE) { # interactive()

# Using the included, test data on black-capped chickadees
bcch # look at the data

# Grab one year to reduce number of files to download
bcch <- dplyr::filter(bcch, survey_year == 2010)

# Convert to sf POINT object
bcch <- sf::st_as_sf(
  bcch,
  coords = c("longitude", "latitude"),
  crs = 4326
)

# Enter EarthData email
ed_email <- readline(prompt = "Enter EarthData email: ")

# Download MODIS data
modis_files <- vegetation_download(
  bcch,
  ed_email = ed_email
)

# Extract vegetation data
output <- vegetation_extract(
  data = bcch,
  covariates = "modis_ndvi",
  vegetation_files = modis_files,
  retain = FALSE
)
}
```
