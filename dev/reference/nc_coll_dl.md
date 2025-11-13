# Download all records for a single collection

This internal function queries and downloads data for a single
collection

## Usage

``` r
nc_coll_dl(coll, query, filter, timeout, token, df_db, verbose)
```

## Arguments

- coll:

  List. Data frame returned by nc_count() for collection in question

- query:

  List. Queries for server

- filter:

  List. Filter queries for server

- token:

  Character. Authorization token

- df_db:

  Data frame/SQLite database connection. Data source

- verbose:

  Logical. Display progress messages?

## Value

An updated df_db (data.frame), or the database connection (update on
harddrive)
