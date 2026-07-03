# Check the last time metadata was updated

Some metadata is stored locally and can be updated with
[`nc_metadata()`](https://birdscanada.github.io/naturecounts/dev/reference/nc_metadata.md).
Use `nc_metadata_version()` to see when these files were last updated.

## Usage

``` r
nc_metadata_version()
```

## Value

Date of the last update

## Details

**Metadata stored locally** - use
[`nc_metadata()`](https://birdscanada.github.io/naturecounts/dev/reference/nc_metadata.md)
to update

- [`meta_country_codes()`](https://birdscanada.github.io/naturecounts/dev/reference/meta.md)

- [`meta_statprov_codes()`](https://birdscanada.github.io/naturecounts/dev/reference/meta.md)

- [`meta_subnational2_codes()`](https://birdscanada.github.io/naturecounts/dev/reference/meta.md)

- [`meta_iba_codes()`](https://birdscanada.github.io/naturecounts/dev/reference/meta.md)

- [`meta_bcr_codes()`](https://birdscanada.github.io/naturecounts/dev/reference/meta.md)

- [`meta_utm_squares()`](https://birdscanada.github.io/naturecounts/dev/reference/meta.md) -
  use `nc_metadata(utm = TRUE)` to update (big update)

- [`meta_species_authority()`](https://birdscanada.github.io/naturecounts/dev/reference/meta.md)

- [`meta_species_codes()`](https://birdscanada.github.io/naturecounts/dev/reference/meta.md)

- [`meta_species_taxonomy()`](https://birdscanada.github.io/naturecounts/dev/reference/meta.md)

## Examples

``` r
nc_metadata_version()
#> metadata_updated 
#>     "2026-07-03" 
```
