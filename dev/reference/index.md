# Package index

## Learning about this package

- [`naturecounts`](https://birdscanada.github.io/naturecounts/dev/reference/naturecounts-package.md)
  [`naturecounts-package`](https://birdscanada.github.io/naturecounts/dev/reference/naturecounts-package.md)
  : Access and download data on plant and animal populations from
  NatureCounts

## Accessing Data

Main functions of `naturecounts` for accessing data or counts or
updating local metadata from the NatureCounts server

- [`nc_permissions()`](https://birdscanada.github.io/naturecounts/dev/reference/nc_permissions.md)
  : Download list of accessible collections
- [`nc_count()`](https://birdscanada.github.io/naturecounts/dev/reference/nc_count.md)
  : Download information about NatureCounts collections
- [`nc_data_dl()`](https://birdscanada.github.io/naturecounts/dev/reference/nc_data_dl.md)
  : Download NatureCounts data records
- [`nc_metadata()`](https://birdscanada.github.io/naturecounts/dev/reference/nc_metadata.md)
  : Update NatureCounts metadata files
- [`nc_metadata_version()`](https://birdscanada.github.io/naturecounts/dev/reference/nc_metadata_version.md)
  : Check the last time metadata was updated
- [`nc_requests()`](https://birdscanada.github.io/naturecounts/dev/reference/nc_requests.md)
  : Check on status of data requests
- [`nc_query_table()`](https://birdscanada.github.io/naturecounts/dev/reference/nc_query_table.md)
  : Custom table queries
- [`nc_remove_cache()`](https://birdscanada.github.io/naturecounts/dev/reference/nc_remove_cache.md)
  : Remove in-memory cache

## Metadata

Functions for returning metadata

- [`meta_country_codes()`](https://birdscanada.github.io/naturecounts/dev/reference/meta.md)
  [`meta_statprov_codes()`](https://birdscanada.github.io/naturecounts/dev/reference/meta.md)
  [`meta_subnational2_codes()`](https://birdscanada.github.io/naturecounts/dev/reference/meta.md)
  [`meta_iba_codes()`](https://birdscanada.github.io/naturecounts/dev/reference/meta.md)
  [`meta_bcr_codes()`](https://birdscanada.github.io/naturecounts/dev/reference/meta.md)
  [`meta_utm_squares()`](https://birdscanada.github.io/naturecounts/dev/reference/meta.md)
  [`meta_species_authority()`](https://birdscanada.github.io/naturecounts/dev/reference/meta.md)
  [`meta_species_codes()`](https://birdscanada.github.io/naturecounts/dev/reference/meta.md)
  [`meta_species_taxonomy()`](https://birdscanada.github.io/naturecounts/dev/reference/meta.md)
  [`meta_collections()`](https://birdscanada.github.io/naturecounts/dev/reference/meta.md)
  [`meta_breeding_codes()`](https://birdscanada.github.io/naturecounts/dev/reference/meta.md)
  [`meta_project_protocols()`](https://birdscanada.github.io/naturecounts/dev/reference/meta.md)
  [`meta_projects()`](https://birdscanada.github.io/naturecounts/dev/reference/meta.md)
  [`meta_protocol_types()`](https://birdscanada.github.io/naturecounts/dev/reference/meta.md)
  [`meta_bmde_versions()`](https://birdscanada.github.io/naturecounts/dev/reference/meta.md)
  [`meta_bmde_fields()`](https://birdscanada.github.io/naturecounts/dev/reference/meta.md)
  : Metadata

## Searching through Metadata

Functions for searching through metadata

- [`search_region()`](https://birdscanada.github.io/naturecounts/dev/reference/search_region.md)
  : Find country, state/province, subnational2, IBA, or BCR codes
- [`search_species()`](https://birdscanada.github.io/naturecounts/dev/reference/search_species.md)
  : Find species codes
- [`search_species_code()`](https://birdscanada.github.io/naturecounts/dev/reference/search_species_code.md)
  : Search for bird species id codes by alphanumeric codes

## Helper functions

Functions for formatting, cleaning or transforming downloaded data, as
well as for working with spatial data

- [`format_dates()`](https://birdscanada.github.io/naturecounts/dev/reference/format_dates.md)
  : Add date and day-of-year field/columns to data
- [`format_zero_fill()`](https://birdscanada.github.io/naturecounts/dev/reference/format_zero_fill.md)
  : Zero-fill data
- [`map_canada()`](https://birdscanada.github.io/naturecounts/dev/reference/map_canada.md)
  : Map of Canada
- [`grid_canada()`](https://birdscanada.github.io/naturecounts/dev/reference/grid_canada.md)
  : Create grid across Canada

## COSEWIC functions

Functions for helping with COSEWIC assessments

- [`cosewic_ranges()`](https://birdscanada.github.io/naturecounts/dev/reference/cosewic_ranges.md)
  : Calculate COSEWIC IAO and EOO
- [`cosewic_plot()`](https://birdscanada.github.io/naturecounts/dev/reference/cosewic_plot.md)
  : Plot COSEWIC IAO and EOO

## Covariate Download and Extraction

Functions for formatting data for use in covariate download and
extraction, as well as for downloading and extracting covariate data

- [`nc_covariate_table()`](https://birdscanada.github.io/naturecounts/dev/reference/nc_covariate_table.md)
  : Metadata for data sources for the covariate download and extraction
  functions.
- [`data_fmt()`](https://birdscanada.github.io/naturecounts/dev/reference/data_fmt.md)
  : Format Data for Covariate Download and Extraction
- [`data_buff()`](https://birdscanada.github.io/naturecounts/dev/reference/data_buff.md)
  : Buffer Data for Covariate Download and Extraction
- [`landcover_download()`](https://birdscanada.github.io/naturecounts/dev/reference/landcover_download.md)
  : Download MODIS Landcover Data
- [`landcover_extract()`](https://birdscanada.github.io/naturecounts/dev/reference/landcover_extract.md)
  : Extract MODIS Landcover Data
- [`vegetation_download()`](https://birdscanada.github.io/naturecounts/dev/reference/vegetation_download.md)
  : Download MODIS NDVI/EVI Data
- [`vegetation_extract()`](https://birdscanada.github.io/naturecounts/dev/reference/vegetation_extract.md)
  : Extract MODIS NDVI/EVI Data

## Data

Included example data sets

- [`bcch`](https://birdscanada.github.io/naturecounts/dev/reference/bcch.md)
  : Example black-capped chickadee data
- [`hofi`](https://birdscanada.github.io/naturecounts/dev/reference/hofi.md)
  : Example house finch data
- [`pops`](https://birdscanada.github.io/naturecounts/dev/reference/pops.md)
  : Example multipopulation data
