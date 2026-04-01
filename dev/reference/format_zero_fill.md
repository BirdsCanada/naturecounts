# Zero-fill data

Zero-fill the species presence data by adding zero observation counts
(absences) data to an existing naturecounts dataset.

## Usage

``` r
format_zero_fill(
  df_db,
  by = "SamplingEventIdentifier",
  species = "all",
  fill = "ObservationCount",
  extra_species = NULL,
  extra_event = NULL,
  warn = TRUE,
  verbose = TRUE
)
```

## Arguments

- df_db:

  Either data frame or a connection to database with `naturecounts`
  table (a data frame is returned).

- by:

  Character vector. By default, "SamplingEventIdentifier" or a vector of
  specific column names to fill by (see details)

- species:

  Character vector. Either "all", for species in the data, or a vector
  of species ID codes to fill in.

- fill:

  Character. The column name to fill in. Defaults to "ObservationCount".

- extra_species:

  Character vector. Extra columns/fields uniquely associated with
  `species_id` to keep in the data (all columns not in `by`, `species`,
  `fill`, `extra_species`, or `extra_event` will be omitted from the
  result).

- extra_event:

  Character vector. Extra columns/fields uniquely associated with the
  Sampling Event (the field defined by `by`) to keep in the data (all
  columns not in `by`, `species`, `fill`, `extra_species`, or
  `extra_event`) will be omitted from the result).

- warn:

  Logical. If TRUE, stop zero-filling if \>100 species and \>1000 unique
  sampling events. If FALSE, ignore and proceed.

- verbose:

  Logical. Show messages?

## Value

Data frame

## Details

`by` refers to the combination of columns which are used to detect
missing values. By default `SamplingEventIdentifier` is used. Otherwise
users can specify their own combination of columns.

If `species` is supplied, all records will be used to determine
observation events, but only records (zero-filled or otherwise) which
correspond to a species in `species` will be returned (all others will
be omitted). Note that records where `species_id` is NA (generally for 0
counts for presence/absence), will be converted to a list of 0's for the
individual species.

## Examples

``` r
# Download data
sample <- nc_data_dl(collection = c("SAMPLE1", "SAMPLE2"),
                     username = "sample", info = "nc_example")
#> Using filters: collections (SAMPLE1, SAMPLE2); fields_set (BMDE2.00-ext)
#> Collecting available records...
#>   collection nrecords
#> 1    SAMPLE1      991
#> 2    SAMPLE2      995
#> Total records: 1,986
#> 
#> Downloading records for each collection:
#>   SAMPLE1
#>     Records 1 to 991 / 991
#>   SAMPLE2
#>     Records 1 to 995 / 995
#> Metadata hasn't been updated in >4 weeks, consider using `nc_metadata()` to update local copies.

# Remove casual observations (i.e. 'AllSpeciesReported' = "No")
library(dplyr) # For filter function
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
sample <- filter(sample, AllSpeciesReported == "Yes")

# Remove data with "X" ObservationCount (only keep numeric obs)
sample <- filter(sample, ObservationCount != "X")

# Zero fill by all species present
sample_all_zeros <- format_zero_fill(sample)
#>  - Converted 'fill' column ('ObservationCount') from character to numeric

# Zero fill only for Canada Goose
goose <- format_zero_fill(sample, species = "230")
#> Metadata hasn't been updated in >4 weeks, consider using `nc_metadata()` to update local copies.
#>  - Converted 'fill' column ('ObservationCount') from character to numeric

# Keep species-specific variables
goose <- format_zero_fill(sample, species = "230", extra_species = "CommonName")
#> Metadata hasn't been updated in >4 weeks, consider using `nc_metadata()` to update local copies.
#>  - Converted 'fill' column ('ObservationCount') from character to numeric

# Keep sampling-event-specific variables
coords <- format_zero_fill(sample, extra_event = c("latitude", "longitude"))
#>  - Converted 'fill' column ('ObservationCount') from character to numeric

# By species, keeping extra species variables and event variables
goose_coords <- format_zero_fill(sample, species = "230",
                                 extra_species = "CommonName",
                                 extra_event = c("latitude", "longitude"))
#> Metadata hasn't been updated in >4 weeks, consider using `nc_metadata()` to update local copies.
#>  - Converted 'fill' column ('ObservationCount') from character to numeric

# Only return event information
events <- format_zero_fill(sample, fill = NA,
                           extra_event = c("latitude", "longitude"))
```
