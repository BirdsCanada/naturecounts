# Add date and day-of-year field/columns to data

Creates and adds columns `date` and `doy` (day-of-year) to the data
source (either data frame or database table `naturecounts`).

## Usage

``` r
format_dates(df_db, overwrite = FALSE)
```

## Arguments

- df_db:

  Either data frame or a connection to database with `naturecounts`
  table. Must have fields/columns of `survey_year`, `survey_month`,
  `survey_day`

- overwrite:

  Logical. Overwrite existing columns `date` and/or `doy`?

## Value

If `df_db`was a data frame, return a data frame with new columns `date`
and `doy`. Otherwise return database connection.

## Examples

``` r
bcch_with_dates <- format_dates(bcch)
```
