# Remove in-memory cache

All server queries are cached for four hours to reduce server load. You
can reset the cache at any time by either restarting your R session or
running `nc_remove_cache()`.

## Usage

``` r
nc_remove_cache()
```

## Value

`TRUE` if it worked

## Examples

``` r
nc_remove_cache()
#> [1] TRUE
```
