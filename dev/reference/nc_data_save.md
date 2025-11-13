# Save/Return the data to data frame or database

Either save data to database on disk, or bind them into an existing data
frame.

## Usage

``` r
nc_data_save(data, df_db, table = "naturecounts")
```

## Arguments

- data:

  Data frame. Data to be saved

- df_db:

  Data frame/SQLite database connection. Where data should be saved

- table:

  Character. If df_db is a database connection, the database table to
  save to

## Value

Either a data frame or a SQLite database connection
