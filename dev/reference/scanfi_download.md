# Download and Load Data from the Spatialized Canadian National Forest Inventory (SCANFI)

Downloads and loads into the environment all available variables from
the [SCANFI v2
dataset](https://open.canada.ca/data/en/dataset/07653869-f303-46c2-a04e-9ab479b73cbf).
All variables are available in snapshots every 5 years between 1985 and
2025 at a 30 m resolution. Users should be aware that these are very
large files (usually 1-5 Gb per snapshot per variable).

## Usage

``` r
scanfi_download(
  data = NULL,
  covariates = "scanfi_height",
  use_date = TRUE,
  snapshot_year = NULL,
  date_year = NULL,
  timeout = 32000,
  dl_path = NULL,
  progress = TRUE
)
```

## Arguments

- data:

  A `data.frame`, `sf` 'POINT' or 'POLYGON' object, or `terra` 'points'
  or 'polygons' object containing a column with observation years either
  named the BMDE default `survey_year` or another name specified in
  argument `date_year`. Not required if `use_date = FALSE`, but must be
  specified if `use_date = TRUE`.

- covariates:

  Character, vector if multiple SCANFI data types desired. By default,
  downloads SCANFI forest height data.

- use_date:

  Logical. Should the function use year data provided in `data` to
  choose which snapshot to download? If `FALSE`, `snapshot_year` can be
  used to specify which snapshot(s) should be downloaded and used.

- snapshot_year:

  Numeric, vector if multiple snapshots desired. Snapshot years to
  download. Options include: 1985, 1990, 1995, 2000, 2005, 2010, 2015,
  2020, and 2025. If specified, takes precedent over dates from `data`
  when `use_date = TRUE`.

- date_year:

  Character. Optional argument to provide the name of the column
  containing year data if not contained within the BMDE column
  `survey_year`. Can be left `NULL` and still function properly if
  originally specified in a call to
  [`data_fmt()`](https://birdscanada.github.io/naturecounts/dev/reference/data_fmt.md).

- timeout:

  Numeric. Number of seconds before downloads timeout. This should be in
  the 10s of thousands of seconds, depending on internet download speed.
  Default value assumes largest SCANFI file is being requested, with
  download speeds of 0.2 Mb/s.

- dl_path:

  Character. Optional argument to provide path to download data to. By
  default, data is downloaded to a subfolder `scanfi/` in the working
  directory.

- progress:

  Logical. Should progress bars be displayed?

## Value

A named list containing `terra SpatRaster`s of all requested data. Each
list element represents the SCANFI snapshot year of the data, with a
named `terra SpatRaster` for each requested variable.

## Details

One (or multiple) SCANFI variable(s) can be downloaded by specifying the
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

Downloads are facilitated by a call to
[`utils::download.file()`](https://rdrr.io/r/utils/download.file.html).

## References

Guindon L., Correia D.L.P, Manka F. and Smiley B. 2026. SCANFI v2:
Spatialized CAnadian National Forest Inventory data product v2. Natural
Resources Canada, Canadian Forest Service, Laurentian Forestry Centre,
Quebec, Canada.
<https://doi.org/10.23687/07653869-f303-46c2-a04e-9ab479b73cbf>.

## See also

[`scanfi_extract()`](https://birdscanada.github.io/naturecounts/dev/reference/scanfi_extract.md)
which can be used to extract data from loaded SCANFI data files.

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
output <- scanfi_download(data = bcch,
                          covariates = "scanfi_ponderosapine",
                          progress = FALSE)
#> Warning: [SCANFI Download] Data contains years more than 5 years away from nearest SCANFI snapshot (1978, 1979). No value will be returned for observations in these years.
#> [SCANFI Download] downloading SCANFI ponderosapine. Files are large and may require a fair bit of download and processing time.
#> Warning: downloaded length 13500416 != reported length 79904721
#> Warning: URL 'https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v2/SCANFI_spsCC_ponderosaPine_1985_v2_20260119.tif': status was 'Transferred a partial file'
#> Error: download from 'https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v2/SCANFI_spsCC_ponderosaPine_1985_v2_20260119.tif' failed

# We can also manually specify the snapshot years to download with no input
# data required like this:
output <- scanfi_download(covariates = "scanfi_ponderosapine",
                          use_date = FALSE,
                          snapshot_year = c(2015, 2020),
                          progress = FALSE)
#> [SCANFI Download] downloading SCANFI ponderosapine. Files are large and may require a fair bit of download and processing time.
#> [SCANFI Download] downloading SCANFI ponderosapine. Files are large and may require a fair bit of download and processing time.

```
