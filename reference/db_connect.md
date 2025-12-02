# Connect to or create a SQLite database

Connect to an existing database, or, if the database doesn't exist,
create a new one and fill with the appropriate table (internally stored
empty df called nc_dbs).

## Usage

``` r
db_connect(name = paste0("./naturecounts_", Sys.Date()), verbose = TRUE)
```

## Arguments

- name:

  Character. The file path and name (no extension) of the database to
  create. By default the database is created in the current directory
  and named "naturecounts_DATE.nc".

## Value

A RSQLite connection
