# Extract WorldClim Climate Data

Extracts monthly WorldClim Monthly Climate Norms, averaged over
1970-2000, from downloaded [WorldClim version
2.1](https://www.worldclim.org/data/worldclim21.html) (Fick & Hijmans
2017). Several climate variables can be extracted with this functions:
minimum, maximum, and average temperature (°C), precipitation (mm),
solar radiation (kJ/m^2/day), and wind speed (m/s). Data can be
downloaded with
[`worldclim_download()`](https://birdscanada.github.io/naturecounts/dev/reference/worldclim_download.md)

## Usage

``` r
worldclim_extract(
  data,
  worldclim_data,
  covariates = "worldclim_tavg",
  site_name = NULL,
  date_month = NULL,
  dl_path = NULL,
  retain = TRUE,
  ...
)
```

## Arguments

- data:

  An `sf` 'POINT' or 'POLYGON' object, or `terra` 'points' or 'polygons'
  object.

- worldclim_data:

  `terra SpatRaster` or `list` of `terra SpatRaster`s if extracting
  multiple climate variables. We recommend using
  [`worldclim_download()`](https://birdscanada.github.io/naturecounts/dev/reference/worldclim_download.md)
  to ensure that all data necessary to match your input data are
  captured. Direct output of
  [`worldclim_download()`](https://birdscanada.github.io/naturecounts/dev/reference/worldclim_download.md)
  can be supplied here.

- covariates:

  Character, vector if multiple climate data types desired. By default,
  extracts WorldClim average temperature data.

- site_name:

  Character. Optional argument to provide the name of the column
  containing site names if not contained within the BMDE column
  `SurveyAreaIdentifier`. Can be left `NULL` and still function properly
  if originally specified in a call to
  [`data_fmt()`](https://birdscanada.github.io/naturecounts/dev/reference/data_fmt.md)
  or
  [`worldclim_download()`](https://birdscanada.github.io/naturecounts/dev/reference/worldclim_download.md).

- date_month:

  Character. Optional argument to provide the name of the column
  containing month data if not contained within the BMDE column
  `survey_month`. Can be left `NULL` and still function properly if
  originally specified in a call to
  [`data_fmt()`](https://birdscanada.github.io/naturecounts/dev/reference/data_fmt.md)
  or
  [`worldclim_download()`](https://birdscanada.github.io/naturecounts/dev/reference/worldclim_download.md).

- dl_path:

  Character. Path to downloaded files. Only needed if `retain = TRUE`
  and custom download filepath used.

- retain:

  Logical. Should WorldClim data files be kept after extraction? If
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

For sf 'POINT' or terra 'points' input data, original data with numeric
column(s) appended containing the climate data value(s) at each point.

For sf 'POLYGON' or terra 'polygons' input data, original data with
numeric column(s) appended containing the requested climate data
value(s) within each polygon.

## Details

One (or multiple) climate variable(s) can be extracted by specifying the
following values to the `covariates` argument

- Minimum temperature: `worldclim_tmin`

- Maximum temperature: `worldclim_tmax`

- Average temperature: `worldclim_tavg`

- Precipitation: `worldclim_prec`

- Solar radiation: `wordclim_srad`

- Wind speed: `worldclim_wind`

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

## References

Fick, S.E. and R.J. Hijmans, 2017. WorldClim 2: new 1km spatial
resolution climate surfaces for global land areas. International Journal
of Climatology 37 (12): 4302-4315.

## See also

[`worldclim_download()`](https://birdscanada.github.io/naturecounts/dev/reference/worldclim_download.md)
which can be used to download WorldClim data files.

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

bcch <- data_fmt(bcch)
#> [Data Formatting] beginning formatting.

# Load WorldClim data
wind <- worldclim_download(data = bcch,
                           covariates = "worldclim_wind",
                           progress = FALSE)

# Extract average temperature
output <- worldclim_extract(data = bcch,
                            worldclim_data = wind,
                            covariates = "worldclim_wind",
                            retain = FALSE)
#> [WorldClim Extraction] extracting WorldClim wind.
#> [WorldClim Extraction] task complete. Removing files.
```
