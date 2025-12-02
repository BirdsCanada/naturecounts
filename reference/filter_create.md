# Create filter list

Creates a filter list from package variables and matches them to api
query names stored in the internal `queries` data. This is created in
"./data-raw/data_creation.R". Also checks parameters for incorrect types
and redundancy

## Usage

``` r
filter_create(verbose, ...)
```

## Arguments

- verbose:

  Logical. Display progress messages?

- ...:

  The parameters (package names) to create the filter list with

## Value

A list of api-named filter parameters
