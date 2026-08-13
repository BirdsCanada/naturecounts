# Extract MODIS Landcover Data

Extracts [annual landcover
data](https://doi.org/10.5067/MODIS/MCD12Q1.061) derived from imagery
from the MODIS Terra and Aqua satellites at approximately 500 m spatial
resolution. This data can be downloaded using
[`landcover_download()`](https://birdscanada.github.io/naturecounts/dev/reference/landcover_download.md).

## Usage

``` r
landcover_extract(
  data,
  covariates = "modis_lctype1",
  landcover_files,
  site_name = NULL,
  date_year = NULL,
  retain = TRUE,
  ...
)
```

## Arguments

- data:

  An `sf` 'POINT' or 'POLYGON' object, or `terra` 'points' or 'polygons'
  object.

- covariates:

  Character, vector if multiple landcover types desired. By default,
  extracts the IGBP global vegetation classification scheme
  (`modis_lctype1`).

- landcover_files:

  Character, vector if multiple files. File-path(s) to downloaded MODIS
  landcover data file(s). We recommend using
  [`landcover_download()`](https://birdscanada.github.io/naturecounts/dev/reference/landcover_download.md)
  to download MODIS files to ensure all files necessary for your data
  are captured. Direct output of
  [`landcover_download()`](https://birdscanada.github.io/naturecounts/dev/reference/landcover_download.md)
  can be supplied here.

- site_name:

  Character. Optional argument to provide name of the column containing
  site names if not contained within the BMDE column
  `SurveyAreaIdentifier`. Can be left `NULL` and still function properly
  if originally specified in a call to
  [`data_fmt()`](https://birdscanada.github.io/naturecounts/dev/reference/data_fmt.md)
  or
  [`landcover_download()`](https://birdscanada.github.io/naturecounts/dev/reference/landcover_download.md).

- date_year:

  Character. Optional argument to provide the name of the column
  containing year data if not contained within the BMDE column
  `survey_year`. Can be left NULL and still function properly if
  originally specified in a call to
  [`data_fmt()`](https://birdscanada.github.io/naturecounts/dev/reference/data_fmt.md)
  or
  [`landcover_download()`](https://birdscanada.github.io/naturecounts/dev/reference/landcover_download.md).

- retain:

  Logical. Should MODIS data files be kept after extraction. If `FALSE`,
  files will be deleted.

- ...:

  Other arguments passed to
  [`landscapemetrics::calculate_lsm()`](https://r-spatialecology.github.io/landscapemetrics/reference/calculate_lsm.html)
  for `sf` 'POLYGON' or `terra` 'polygons' input data. Primarily useful
  for specifying metrics other than the proportional cover of each
  landcover class. See
  [`landscapemetrics::list_lsm()`](https://r-spatialecology.github.io/landscapemetrics/reference/list_lsm.html)
  for other options.

## Value

For `sf` 'POINT' or `terra` 'points' input data, original data with a
character column `lctype1` appended containing the name of the landcover
class that point falls within.

For `sf` 'POLYGON' or `terra` 'polygons' input data, original data with
numeric columns containing the requested landscape metrics (see
[`landscapemetrics::list_lsm()`](https://r-spatialecology.github.io/landscapemetrics/reference/list_lsm.html)
for options). By default, returns columns containing the proportion of
each polygon that is covered by each landcover type.

## Details

Five landcover classification schemes are available through this
function and can be accessed by supplying the following arguments to the
`covariates` argument:

- `modis_lctype1` - IGBP global vegetation classification scheme

- `modis_lctype2` - University of Maryland (UMD) scheme

- `modis_lctype3` - MODIS-derived LAI/fPAR scheme

- `modis_lctype4` - MODIS-derived Net Primary Production (NPP) scheme

- `modis_lctype5` - Plant Functional Type (PFT) scheme

Details on these classification schemes can be found in Chapter 5 of the
[MODIS User
Guide](https://lpdaac.usgs.gov/documents/101/MCD12_User_Guide_V6.pdf).
By default, the function extracts the University of Maryland scheme
(`modis_lctype1`), but we strongly recommend users consider the
strengths and weaknesses of each classification scheme in the context of
their analysis and choose their desired classification scheme
appropriately.

## See also

[`landcover_download()`](https://birdscanada.github.io/naturecounts/dev/reference/landcover_download.md)
which can be used to download data from the MODIS Landcover dataset.

[`nc_covariates_merge()`](https://birdscanada.github.io/naturecounts/dev/reference/nc_covariates_merge.md)
to merge extracted covariate data into data originally provided to the
`data` argument of
[`data_fmt()`](https://birdscanada.github.io/naturecounts/dev/reference/data_fmt.md).

[`landscapemetrics::list_lsm()`](https://r-spatialecology.github.io/landscapemetrics/reference/list_lsm.html)
to view options for landscape metrics that can be calculated for
buffered input data.

## Examples

``` r
if (FALSE) { # interactive()
# Using the included, test data on black-capped chickadees
bcch # look at the data

# Convert to sf POINT object
bcch <- sf::st_as_sf(
  bcch,
  coords = c("longitude", "latitude"),
  crs = 4326
)

# Enter EarthData email
ed_email <- readline(prompt = "Enter EarthData email: ")

# Download MODIS data
modis_files <- landcover_download(
  bcch,
  ed_email = ed_email
)

# Extract landcover data
output <- landcover_extract(
  data = bcch,
  covariates = "modis_lctype1",
  landcover_files = modis_files,
  retain = FALSE
)
}
```
