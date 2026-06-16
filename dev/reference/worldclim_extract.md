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
  retain = TRUE
)
```

## Arguments

- data:

  An `sf` 'POINT' or 'POLYGON' object, or `terra` 'points' or 'polygons'
  object. Not required if `countries` is specified, but must be
  specified if `countries` is left unspecified.

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

  Logical. Should WorldClim data files be kept after extraction. If
  `FALSE`, files will be deleted.

## Value

For sf 'POINT' or terra 'points' input data, original data with numeric
column(s) appended containing the climate data value(s) at each point.

For sf 'POLYGON' or terra 'polygons' input data, original data with
numeric column(s) appended containing the mean climate data value(s)
within each polygon.

## Details

One (or multiple) climate variable(s) can be extracted by specifying the
following values to the `covariates` argument

- Minimum temperature: `worldclim_tmin`

- Maximum temperature: `worldclim_tmax`

- Average temperature: `worldclim_tavg`

- Precipitation: `worldclim_prec`

- Solar radiation: `wordclim_srad`

- Wind speed: `worldclim_wind`

## References

Fick, S.E. and R.J. Hijmans, 2017. WorldClim 2: new 1km spatial
resolution climate surfaces for global land areas. International Journal
of Climatology 37 (12): 4302-4315.

## See also

[`geodata::worldclim_country()`](https://rspatial.github.io/geodata/reference/worldclim.html)
which this function wraps.
[`worldclim_download()`](https://birdscanada.github.io/naturecounts/dev/reference/worldclim_download.md)
which can be used to download WorldClim data files.

## Examples

``` r
# Convert included test data on black-capped chickadees to sf POINT object
bcch <- sf::st_as_sf(
  bcch,
  coords = c("longitude", "latitude"),
  crs = 4326
)

# Load WorldClim data
tavg <- worldclim_download(data = bcch, 
                           covariates = "worldclim_wind",
                           progress = FALSE)
#> [Worldclim Download] downloading WorldClim 'wind' data for Canada.
#> The geodata server is temporary out of service for maintenance. It should be back on 22 June. 
#> Warning: [WorldClim Download] Download failed for Canada [wind].
                           
# Extract average temperature
output <- worldclim_extract(data = bcch,
                            worldclim_data = tavg,
                            covariates = "worldclim_wind",
                            retain = FALSE)
#> Error in worldclim_data[[1]]: subscript out of bounds
```
