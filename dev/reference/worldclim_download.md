# Load WorldClim Climate Data

Downloads monthly WorldClim Monthly Climate Norms, averaged over
1970-2000, from [WorldClim version
2.1](https://www.worldclim.org/data/worldclim21.html) at a ~ 1 km^2
spatial resolution (Fick & Hijmans 2017). Several climate variables are
available in this dataset: minimum, maximum, and average temperature
(°C), precipitation (mm), solar radiation (kJ/m^2/day), and wind speed
(m/s). Users should note that these files are downloaded at the
country-scale so they can be quite large.

## Usage

``` r
worldclim_download(
  data = NULL,
  covariates = "worldclim_tavg",
  countries = NULL,
  dl_path = NULL,
  progress = TRUE
)
```

## Arguments

- data:

  An `sf` 'POINT' or 'POLYGON' object, or `terra` 'points' or 'polygons'
  object. Not required if `countries` is specified, but must be
  specified if `countries` is left unspecified.

- covariates:

  Character, vector if multiple climate data types desired. By default,
  downloads WorldClim average temperature data.

- countries:

  Character, vector if multiple countries. Country names or [ISO3
  country codes](https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3) for
  which data should be downloaded. If left `NULL`, function will attempt
  to identify countries needed based on locations in `data`.

- dl_path:

  Character. Optional argument to provide path to download data to. By
  default, data is downloaded to a subfolder `WorldClim/` in the working
  directory.

- progress:

  Logical. Should progress bars and download messages be displayed?

## Value

A merged `terra SpatRaster` containing all requested data. A `list` of
multiple `terra SpatRaster` objects if multiple climate data types
requested.

## Details

One (or multiple) climate variable(s) can be downloaded by specifying
the following values to the `covariates` argument

- Minimum temperature: `worldclim_tmin`

- Maximum temperature: `worldclim_tmax`

- Average temperature: `worldclim_tavg`

- Precipitation: `worldclim_prec`

- Solar radiation: `wordclim_srad`

- Wind speed: `worldclim_wind`

Downloads are facilitated by a call to
[`geodata::worldclim_country()`](https://rspatial.github.io/geodata/reference/worldclim.html).

## References

Fick, S.E. and R.J. Hijmans, 2017. WorldClim 2: new 1km spatial
resolution climate surfaces for global land areas. International Journal
of Climatology 37 (12): 4302-4315.

## See also

[`geodata::worldclim_country()`](https://rspatial.github.io/geodata/reference/worldclim.html)
which this function wraps.
[`worldclim_extract()`](https://birdscanada.github.io/naturecounts/dev/reference/worldclim_extract.md)
which can be used to extract data from loaded WorldClim data files.

## Examples

``` r
# Convert included test data on black-capped chickadees to sf POINT object
bcch <- sf::st_as_sf(
  bcch,
  coords = c("longitude", "latitude"),
  crs = 4326
)

# Load WorldClim data
output <- worldclim_download(data = bcch,
                             covariates = "worldclim_wind",
                             progress = FALSE)
#> [Worldclim Download] downloading WorldClim 'wind' data for Canada.
#> The geodata server is temporary out of service for maintenance. It should be back on 22 June. 
#> Warning: [WorldClim Download] Download failed for Canada [wind].
```
