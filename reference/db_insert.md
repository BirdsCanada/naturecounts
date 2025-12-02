# Add/replace records in a db table from a data frame

Add/replace records in a db table from a data frame

## Usage

``` r
db_insert(con, table, df)
```

## Arguments

- con:

  DBI database connection

- table:

  Character. Name of table to add/replace records into

- df:

  Data frame. Data from which to write data

## References

Adapted from `motus::dbInsertOrReplace()` originally written by John
Brzustowski for the `motus` package.
