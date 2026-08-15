# Extract Data from the Spatialized Canadian National Forest Inventory (SCANFI)

Extracts all available variables from the [SCANFI v2
dataset](https://open.canada.ca/data/en/dataset/07653869-f303-46c2-a04e-9ab479b73cbf).
All variables are available in snapshots every 5 years between 1985 and
2025 at a 30 m resolution. Necessary files can be downloaded and loaded
with
[`scanfi_download()`](https://birdscanada.github.io/naturecounts/dev/reference/scanfi_download.md).

## Usage

``` r
scanfi_extract(
  data,
  scanfi_data,
  covariates = "scanfi_height",
  interpolate = FALSE,
  site_name = NULL,
  date_year = NULL,
  dl_path = NULL,
  retain = TRUE,
  ...
)
```

## Arguments

- data:

  A
  sf`'POINT' or 'POLYGON' object, or`terra`'points' or 'polygons' object containing a column with observation years either named the BMDE default`survey_year`or another name specified in argument`date_year\`.

- scanfi_data:

  Named `list` of `terra SpatRaster`s. First index names should be the
  snapshot years contained data is from, and second index names should
  be variable names as in
  [`nc_covariate_table()`](https://birdscanada.github.io/naturecounts/dev/reference/nc_covariate_table.md),
  with the "scanfi\_" removed. We recommend using
  [`scanfi_download()`](https://birdscanada.github.io/naturecounts/dev/reference/scanfi_download.md)
  to ensure that all data necessary to match your input data are
  captured and that list formatting is correct. Direct output of
  [`scanfi_download()`](https://birdscanada.github.io/naturecounts/dev/reference/scanfi_download.md)
  can be supplied here.

- covariates:

  Character, vector if multiple SCANFI data types desired. By default,
  downloads SCANFI forest height data.

- interpolate:

  Logical. Should years in between snapshots be assigned the nearest
  snapshot's value?

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

- dl_path:

  Character. Optional argument to provide path to download data to. By
  default, data is downloaded to a subfolder `scanfi/` in the working
  directory.

- retain:

  Logical. Should SCANFI data files be kept after extraction? If
  `FALSE`, files will be deleted.

- ...:

  Other arguments passed to
  [`landscapemetrics::calculate_lsm()`](https://r-spatialecology.github.io/landscapemetrics/reference/calculate_lsm.html)
  if NFI Landcover data requested, or
  [`exactextractr::exact_extract()`](https://isciences.gitlab.io/exactextractr/reference/exact_extract.html)
  or for other requested variables with `sf` 'POLYGON' or `terra`
  'polygons' input data. Primarily useful for specifying metrics other
  than the proportional cover of each landcover class when NFI Landcover
  requested (see
  [`landscapemetrics::list_lsm()`](https://r-spatialecology.github.io/landscapemetrics/reference/list_lsm.html)
  for other options) or the mean for other SCANFI variables.

## Value

For sf 'POINT' or terra 'points' input data, original data with
column(s) appended containing the SCANFI data value(s) at each point.

For sf 'POLYGON' or terra 'polygons' input data, original data with
column(s) appended containing the requested SCANFI data value(s) within
each polygon.

## Details

One (or multiple) SCANFI variable(s) can be extracted by specifying the
following values to the `covariates` argument:

- Forest age (years): `scanfi_age`

- Forest biomass (tons/ha): `scanfi_biomass`

- Crown closure (% of pixel covered by tree canopy): `scanfi_closure`

- Forest height (m): `scanfi_height`

- National Forest Inventory land cover (NFILC) class: `scanfi_nfilc`

- Balsam Fir cover (% of pixel): `scanfi_balsamfir`

- Black Spruce cover (% of pixel): `scanfi_blackspruce`

- Douglas Fir cover (% of pixel): `scanfi_douglasfir`

- Jack Pine cover (% of pixel): `scanfi_jackpine`

- Lodgepole Pine cover (% of pixel): `scanfi_lodgepolepine`

- Ponderosa Pine cover (% of pixel): `scanfi_ponderosapine`

- Tamarack cover (% of pixel): `scanfi_tamarack`

- White and Red Pine cover (% of pixel): `scanfi_whiteredpine`

- Broadleaf tree species cover (% of pixel): `scanfi_broadleaf`

- Other conifer species cover (% of pixel): `scanfi_otherconifer`

By default, for `sf` 'POLYGON' or `terra` 'polygons' input data the mean
SCANFI variable value will be returned, or the proportion of polygon
area covered by each NFI Landcover class (`pland`) will be returned if
`scanfi_nfilc` requested. Other summary statistics can be extracted for
non-NFI Landcover variables by specifying the `fun` argument, which is
passed to
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

For extraction of NFI Landcover data with `sf` 'POLYGON' or `terra`
'polygons' input data, other summary metrics can be requested by
specifying the `level`, `class`, `metric`, or `name` arguments, which
are passed to
[`landscapemetrics::calculate_lsm()`](https://r-spatialecology.github.io/landscapemetrics/reference/calculate_lsm.html).
See
[`landscapemetrics::list_lsm()`](https://r-spatialecology.github.io/landscapemetrics/reference/list_lsm.html)
for metric options. At this time, only metrics at the `landscape` or
`class` level are accepted.

## References

Guindon L., Correia D.L.P, Manka F. and Smiley B. 2026. SCANFI v2:
Spatialized CAnadian National Forest Inventory data product v2. Natural
Resources Canada, Canadian Forest Service, Laurentian Forestry Centre,
Quebec, Canada.
<https://doi.org/10.23687/07653869-f303-46c2-a04e-9ab479b73cbf>.

## See also

[`scanfi_download()`](https://birdscanada.github.io/naturecounts/dev/reference/scanfi_download.md)
which can be used to download data from SCANFI data files and load them
into the environment.

[`nc_covariates_merge()`](https://birdscanada.github.io/naturecounts/dev/reference/nc_covariates_merge.md)
to merge extracted covariate data into data originally provided to the
`data` argument of
[`data_fmt()`](https://birdscanada.github.io/naturecounts/dev/reference/data_fmt.md).

## Examples

``` r
# Convert included test data on black-capped chickadees to sf POINT object
bcch <- sf::st_as_sf(
  bcch,
  coords = c("longitude", "latitude"),
  crs = 4326
)

# Download SCANFI data - uses the dates in the data to determine which
# snapshot years to download.
scanfi <- scanfi_download(data = bcch,
                          covariates = "scanfi_ponderosapine",
                          progress = FALSE)
#> Warning: [SCANFI Download] Data contains years more than 5 years away from nearest SCANFI snapshot (1978, 1979). No value will be returned for observations in these years.

# Create sf object to use in extraction.
bcch <- data_fmt(bcch)
#> [Data Formatting] beginning formatting.

# Extract first only for the snapshot years.
output <- scanfi_extract(data = bcch,
                         scanfi_data = scanfi)
#> Warning: [SCANFI Extraction] no covariates specified in the covariates argument. Proceeding to extract the covariates found in scanfi_data layers: scanfi_ponderosapine.
#> [SCANFI Extraction] extracting SCANFI ponderosapine.
#> [SCANFI Extraction] extracting SCANFI ponderosapine.
#> [SCANFI Extraction] extracting SCANFI ponderosapine.
#> [SCANFI Extraction] extracting SCANFI ponderosapine.
#> [SCANFI Extraction] extracting SCANFI ponderosapine.
#> [SCANFI Extraction] extracting SCANFI ponderosapine.
#> [SCANFI Extraction] extracting SCANFI ponderosapine.

# Extract with interpolation for interceding years.
output <- scanfi_extract(data = bcch,
                         scanfi_data = scanfi,
                         interpolate = TRUE)
#> Warning: [SCANFI Extraction] no covariates specified in the covariates argument. Proceeding to extract the covariates found in scanfi_data layers: scanfi_ponderosapine.
#> Warning: [SCANFI Download] Data contains years more than 5 years away from nearest SCANFI snapshot (1978, 1979). No value will be returned for observations in these years.
#> [SCANFI Extraction] extracting SCANFI ponderosapine.
#> [SCANFI Extraction] extracting SCANFI ponderosapine.
#> [SCANFI Extraction] extracting SCANFI ponderosapine.
#> [SCANFI Extraction] extracting SCANFI ponderosapine.
#> [SCANFI Extraction] extracting SCANFI ponderosapine.
#> [SCANFI Extraction] extracting SCANFI ponderosapine.
#> [SCANFI Extraction] extracting SCANFI ponderosapine.
```
