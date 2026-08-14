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
  retain = TRUE,
  ...
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

- ...:

  Other arguments passed to
  [`terra::extract()`](https://rspatial.github.io/terra/reference/extract.html)
  for `sf` 'POINT' or `terra` 'points' input data or
  [`exactextractr::exact_extract()`](https://isciences.gitlab.io/exactextractr/reference/exact_extract.html)
  `sf` 'POLYGON' or `terra` 'polygons' input data. Primarily useful for
  specifying alternate summary statistics to extract for `sf` 'POLYGON'
  or `terra` 'polygons' input data.

## Value

For `sf` 'POINT' or `terra` 'points' input data, original data with
numeric column(s) `ndvi` and/or `evi` appended containing the NDVI/EVI
value at that point. If reliability information requested, an additional
`vegetation_reliability` column is appended containing the reliability
assessment as defined in table 4 of the [product's user
manual](https://lpdaac.usgs.gov/documents/621/MOD13_User_Guide_V61.pdf).

For `sf` 'POLYGON' or `terra` 'polygons' input data, original data with
numeric column(s) appended containing the requested NDVI/EVI value
within each polygon. If reliability information requested, an additional
`vegetation_reliability` column is appended containing the percentage of
pixels overlapped by each polygon in each reliability assessment as
defined in table 4 of the [product's user
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

By default, for `sf` 'POLYGON' or `terra` 'polygons' input data the mean
NDVI and/or EVI value will be returned. Other summary statistics can be
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

NDVI/EVI calculations are sensitive to the presence of snow/ice and
cloudiness. So users can assess the quality of data extracted at each
site, we have included the option to extract pixel reliability
assessments included in these NDVI/EVI products by setting argument
`reliability = TRUE`. The reliability scale is as in the [user
manual](https://lpdaac.usgs.gov/documents/621/MOD13_User_Guide_V61.pdf),
and is supplied to the user in the `vegetation_reliability` column if
requested.

## See also

[`vegetation_download()`](https://birdscanada.github.io/naturecounts/dev/reference/vegetation_download.md)
which can be used to download data from the MODIS Vegetation Indices
database.

[`nc_covariates_merge()`](https://birdscanada.github.io/naturecounts/dev/reference/nc_covariates_merge.md)
to merge extracted covariate data into data originally provided to the
`data` argument of
[`data_fmt()`](https://birdscanada.github.io/naturecounts/dev/reference/data_fmt.md).

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
