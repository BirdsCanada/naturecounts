# Load Terrain Tiles Elevation Data

Loads [Mapzen Terrain Tiles elevation
data](https://github.com/tilezen/joerd/tree/master/docs), delivered at
varying spatial resolutions. This data is open access via [Amazon Web
Services](https://registry.opendata.aws/terrain-tiles/), and is a global
composite of a variety of [data
sources](https://github.com/tilezen/joerd/blob/master/docs/data-sources.md).
Data is loaded into the R environment, and is not permanently downloaded
onto the user's operating system.

## Usage

``` r
elevation_download(data, site_name = NULL, z = 7)
```

## Arguments

- data:

  An `sf` 'POINT' or 'POLYGON' object, or `terra` 'points' or 'polygons'
  object.

- site_name:

  Character. Optional argument to provide the name of the column
  containing site names if not contained within the BMDE column
  `SurveyAreaIdentifier`. Can be left `NULL` and still function properly
  if originally specified in a call to
  [`data_fmt()`](https://birdscanada.github.io/naturecounts/dev/reference/data_fmt.md).

- z:

  Numeric. Zoom level to fetch, determining the resulting spatial
  resolution of downloaded elevation data. More information can be found
  [here](https://github.com/tilezen/joerd/blob/master/docs/data-sources.md#what-is-the-ground-resolution).

## Value

A `terra SpatRaster` in the projection of the data supplied to the
`data` argument, covering the bounding box of the supplied data.

## Details

Users should be conscious of the final spatial resolution of their
elevation data, as this varies by latitude and zoom level. This can be
accessed using
[`terra::res()`](https://rspatial.github.io/terra/reference/dimensions.html).

Downloads are facilitated by a call to
[`elevatr::get_elev_raster()`](https://rdrr.io/pkg/elevatr/man/get_elev_raster.html).

## See also

[`elevatr::get_elev_raster()`](https://rdrr.io/pkg/elevatr/man/get_elev_raster.html)
which this function wraps.
[`elevation_extract()`](https://birdscanada.github.io/naturecounts/dev/reference/elevation_extract.md)
which can be used to extract data from loaded elevation data files.

## Examples

``` r
# Convert included, test data on black-capped chickadees to sf POINT object
bcch <- sf::st_as_sf(
  bcch,
  coords = c("longitude", "latitude"),
  crs = 4326
)

# Load Terrain Tiles data
output <- elevation_download(data = bcch)
#> [Elevation Download] downloading data.
#> Mosaicing & Projecting
```
