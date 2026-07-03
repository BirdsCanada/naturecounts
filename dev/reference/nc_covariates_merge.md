# Merge Extracted Covariate Data into Original Input Data

Data formatted for covariate extraction using
[`data_fmt()`](https://birdscanada.github.io/naturecounts/dev/reference/data_fmt.md)
is transformed to an `sf` object containing a row for each unique
site-date combination, with columns being appended to this by the
various covariate extraction functions within
[naturecounts](https://birdscanada.github.io/naturecounts/dev/reference/naturecounts-package.md).
Users may wish, instead, to have their covariate data appended to
original data in a different format (e.g., a row for each observation)
and can use this function to merge the two data types accurately.

## Usage

``` r
nc_covariates_merge(
  original_data,
  covariate_data,
  coord_lon = NULL,
  coord_lat = NULL,
  site_name = NULL,
  date_year = NULL,
  date_month = NULL,
  date_day = NULL,
  date_lubridate = NULL,
  date_ordinal = NULL
)
```

## Arguments

- original_data:

  `data.frame`, `sf` 'POINT' or 'POLYGON' object, or `terra` 'points' or
  'polygons'. Object containing data to match covariate data to. For
  example, the original input data to a call to
  [`data_fmt()`](https://birdscanada.github.io/naturecounts/dev/reference/data_fmt.md).

- covariate_data:

  `sf` 'POINT' or 'POLYGON' object. Object containing data output by one
  of the covariate extraction functions within
  [naturecounts](https://birdscanada.github.io/naturecounts/dev/reference/naturecounts-package.md):
  [`landcover_extract()`](https://birdscanada.github.io/naturecounts/dev/reference/landcover_extract.md),
  [`vegetation_extract()`](https://birdscanada.github.io/naturecounts/dev/reference/vegetation_extract.md),
  [`elevation_extract()`](https://birdscanada.github.io/naturecounts/dev/reference/elevation_extract.md),
  [`worldclim_extract()`](https://birdscanada.github.io/naturecounts/dev/reference/worldclim_extract.md),
  [`scanfi_extract()`](https://birdscanada.github.io/naturecounts/dev/reference/scanfi_extract.md),
  or
  [daymet_extract](https://birdscanada.github.io/naturecounts/dev/reference/daymet_extract.md).

- coord_lon:

  Character. Optional argument to provide the name of the column
  containing year data if not contained within the BMDE column
  `longitude`.

- coord_lat:

  Character. Optional argument to provide the name of the column
  containing year data if not contained within the BMDE column
  `latitude`.

- site_name:

  Character. Optional argument to provide the name of the column
  containing site names if not contained within the BMDE column
  `SurveyAreaIdentifier`.

- date_year:

  Character. Optional argument to provide the name of the column
  containing year data if not contained within the BMDE column
  `survey_year`.

- date_month:

  Character. Optional argument to provide the name of the column
  containing month data if not contained within the BMDE column
  `survey_month`.

- date_day:

  Character. Optional argument to provide the name of the column
  containing day of month data if not contained within the BMDE column
  `survey_day`.

- date_lubridate:

  Character. Optional argument to provide the name of a column
  containing date data in `lubridate` formats.

- date_ordinal:

  Character. Optional argument to provide the name of a column
  containing date data in ordinal format.

## Value

Data provided in `original_data` with covariate data columns from
`covariate_data` appended.

## Examples

``` r
if (FALSE) { # interactive()

# Using the included, test data on black-capped chickadees
bcch # look at the data

# Format
formatted <- data_fmt(bcch)

# Download and extract some covariate data.
elev <- elevation_download(data = formatted,
                           progress = FALSE)

extracted <- elevation_extract(data = formatted,
                               elevation_data = elev)

# Merge with original data
merged <- nc_covariates_merge(original_data = bcch,
                              covariate_data = extracted)

merged
}
```
