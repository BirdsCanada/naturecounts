# Download MODIS Landcover Data

Downloads [annual landcover
data](https://doi.org/10.5067/MODIS/MCD12Q1.061) derived from imagery
from the MODIS Terra and Aqua satellites at approximately 500 m spatial
resolution. This data is retreived via the NASA EarthData Archive,
requiring an EarthData account to be made. This can be done at the
following link: [register for an EarthData
account](https://urs.earthdata.nasa.gov/users/new).

## Usage

``` r
landcover_download(
  data,
  ed_email = NULL,
  ed_transfer = TRUE,
  site_name = NULL,
  date_year = NULL,
  dl_path = NULL
)
```

## Arguments

- data:

  An `sf` 'POINT' or 'POLYGON' object, or `terra` 'points' or 'polygons'
  object.

- ed_email:

  Character. The email address associated with your EarthData account.

- ed_transfer:

  Logical. Should data be downloaded from EarthData? If `FALSE`, a
  vector containing the names of the files that would be downloaded is
  returned.

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
  default, data is downloaded to a subfolder `modis/` in the working
  directory.

## Value

If `ed_transfer = TRUE`, character vector containing file-paths to
downloaded MODIS landcover files. If `ed_transfer = FALSE`, character
vector containing filenames of MODIS landcover files that would be
downloaded.

## Details

All five classification schemes available through
[`landcover_extract()`](https://birdscanada.github.io/naturecounts/dev/reference/landcover_extract.md)
are downloaded by this function without need for extra specification.

Downloads are facilitated by a call to
[`luna::getNASA()`](https://rdrr.io/pkg/luna/man/getNASA.html).

## See also

[`luna::getNASA()`](https://rdrr.io/pkg/luna/man/getNASA.html) which
this function wraps.
[`landcover_extract()`](https://birdscanada.github.io/naturecounts/dev/reference/landcover_extract.md)
which can be used to extract data from downloaded landcover data files.

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

# Get file names that would be downloaded.
output <- landcover_download(
  data = bcch,
  ed_transfer = FALSE
)

# Enter EarthData email
ed_email <- "your EarthData email"

# Download MODIS data
#output <- landcover_download(
#  data = bcch,
#  ed_email = ed_email
#)
}
```
