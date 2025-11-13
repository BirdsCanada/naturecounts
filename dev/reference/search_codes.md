# Generic function to match code ids

Returns a subset of the data frame where the specified columns match the
provided description.

## Usage

``` r
search_codes(desc, df, code_column, columns)
```

## Arguments

- desc:

  Character. Description to match in the codes

- df:

  Data frame. Data to look in

- code_column:

  Character. Column name which contains the codes

- columns:

  Character vector. Column name(s) which contain the values to match
  against the description.

## Value

data frame of matching rows
