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
  retain = TRUE
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

## Value

For sf 'POINT' or terra 'points' input data, original data with
column(s) appended containing the SCANFI data value(s) at each point.

For sf 'POLYGON' or terra 'polygons' input data, original data with
column(s) appended containing the mean SCANFI data value(s) within each
polygon or, if NFI Landcover requested, the proportion the polygon area
covered by of each land cover class.

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
#> [SCANFI Download] downloading SCANFI ponderosapine. Files are large and may require a fair bit of download and processing time.
#> [SCANFI Download] downloading SCANFI ponderosapine. Files are large and may require a fair bit of download and processing time.
#> [SCANFI Download] downloading SCANFI ponderosapine. Files are large and may require a fair bit of download and processing time.
#> [SCANFI Download] downloading SCANFI ponderosapine. Files are large and may require a fair bit of download and processing time.
#> [SCANFI Download] downloading SCANFI ponderosapine. Files are large and may require a fair bit of download and processing time.

# Create sf object to use in extraction.
bcch <- data_fmt(bcch)
#> [Data Formatting] beginning formatting.

# Extract first only for the snapshot years.
output <- scanfi_extract(data = bcch,
                         scanfi_data = scanfi)
#> Warning: [SCANFI Extraction] no covariates specified in the covariates argument. Proceeding to extract the covariates found in scanfi_data layers: scanfi_ponderosapine.
#> [SCANFI Extraction] extracting SCANFI ponderosapine.
#> Warning: GDAL Error 1: TIFFFillTile:Read error at row 4294967295, col 4294967295, tile 268821; got 0 bytes, expected 84
#> Warning: GDAL Error 1: TIFFReadEncodedTile() failed.
#> Warning: GDAL Error 1: /home/runner/work/naturecounts/naturecounts/docs/dev/reference/scanfi/SCANFI_spsCC_ponderosaPine_1985_v2_20260119.tif, band 1: IReadBlock failed at X offset 476, Y offset 385: TIFFReadEncodedTile() failed.
#> Error: [crop] too few values for writing: 0 < 21132009

# Extract with interpolation for interceding years.
output <- scanfi_extract(data = bcch,
                         scanfi_data = scanfi,
                         interpolate = TRUE)
#> Warning: [SCANFI Extraction] no covariates specified in the covariates argument. Proceeding to extract the covariates found in scanfi_data layers: scanfi_ponderosapine.
#> Warning: [SCANFI Download] Data contains years more than 5 years away from nearest SCANFI snapshot (1978, 1979). No value will be returned for observations in these years.
#> [SCANFI Extraction] extracting SCANFI ponderosapine.
#> Warning: GDAL Error 1: TIFFFillTile:Read error at row 4294967295, col 4294967295, tile 268821; got 0 bytes, expected 84
#> Warning: GDAL Error 1: TIFFReadEncodedTile() failed.
#> Warning: GDAL Error 1: /home/runner/work/naturecounts/naturecounts/docs/dev/reference/scanfi/SCANFI_spsCC_ponderosaPine_1985_v2_20260119.tif, band 1: IReadBlock failed at X offset 476, Y offset 385: TIFFReadEncodedTile() failed.
#> Error: [crop] too few values for writing: 0 < 21132009
```
