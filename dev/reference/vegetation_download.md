# Download MODIS NDVI/EVI Data

Downloads [16-day NDVI/EVI
data](https://doi.org/10.5067/MODIS/MOD13A1.061) derived from imagery
from the MODIS Terra and Aqua satellites at approximately 500 m spatial
resolution. This data is retreived via the NASA EarthData Archive,
requiring an EarthData account to be made. This can be done at the
following link: [register for an EarthData
account](https://urs.earthdata.nasa.gov/users/new).

## Usage

``` r
vegetation_download(
  data,
  ed_email = NULL,
  ed_transfer = TRUE,
  site_name = NULL,
  date_year = NULL,
  date_month = NULL,
  date_day = NULL,
  dl_path = NULL,
  progress = TRUE
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

- date_month:

  Character. Optional argument to provide the name of the column
  containing month data if not contained within the BMDE column
  `survey_month`. Can be left `NULL` and still function properly if
  originally specified in a call to
  [`data_fmt()`](https://birdscanada.github.io/naturecounts/dev/reference/data_fmt.md).

- date_day:

  Character. Optional argument to provide the name of the column
  containing day-of-month (i.e., a number from 1 to 31) data if not
  contained within the BMDE column `survey_day`. Can be left `NULL` and
  still function properly if originally specified in a call to
  [`data_fmt()`](https://birdscanada.github.io/naturecounts/dev/reference/data_fmt.md).

- dl_path:

  Character. Optional argument to provide path to download data to. By
  default, data is downloaded to a subfolder `modis/` in the working
  directory.

- progress:

  Logical. Should progress bars for downloads be displayed?

## Value

If `ed_transfer = TRUE`, character vector containing file-paths to
downloaded MODIS landcover files. If `ed_transfer = FALSE`, character
vector containing filenames of MODIS landcover files that would be
downloaded.

## Details

Both NDVI and EVI data are downloaded in a single file, and can be
accessed specifically by specifying `modis_ndvi` and/or `modis_evi` to
the `covariates` argument in a call to
[`vegetation_extract()`](https://birdscanada.github.io/naturecounts/dev/reference/vegetation_extract.md).

Downloads are facilitated by a call to
[`luna::getNASA()`](https://rdrr.io/pkg/luna/man/getNASA.html).

## See also

[`luna::getNASA()`](https://rdrr.io/pkg/luna/man/getNASA.html) which
this function wraps.

[`vegetation_extract()`](https://birdscanada.github.io/naturecounts/dev/reference/vegetation_extract.md)
which can be used to extract data from downloaded vegetation data files.

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

# Get file names that would be downloaded.
output <- vegetation_download(
  data = bcch,
  ed_transfer = FALSE
)

# Enter EarthData email
ed_email <- "your EarthData email"

# Download MODIS data
#output <- vegetation_download(
#  data = bcch,
#  ed_email = ed_email
#)
}
```
