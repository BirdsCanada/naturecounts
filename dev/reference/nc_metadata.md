# Update NatureCounts metadata files

Updates the local copies of meta data used by the package.

## Usage

``` r
nc_metadata(force = FALSE, utm = FALSE, verbose = TRUE)
```

## Arguments

- force:

  Logical. Force update even if the remote version matches local?

- utm:

  Logical. Update
  [`meta_utm_squares()`](https://birdscanada.github.io/naturecounts/dev/reference/meta.md)
  as well? **WARNING**: This is a large and time consuming download!

- verbose:

  Logical. Show progress messages?

## Examples

``` r
nc_metadata()
#> Updating species authority...
#> Updating species codes...
#> Updating species taxonomy...
#> Updating country codes...
#> Updating state/province codes...
#> Updating subnational codes...
#> Updating IBA codes...
#> Updating BCR codes...
#> Updating BMDE Field list...
#> Metadata version updated to 2026-06-07
```
