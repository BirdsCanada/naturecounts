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
#> Local metadata already up-to-date with server
```
