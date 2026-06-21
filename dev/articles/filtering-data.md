# Filtering data after download

The `naturecounts` package restricts users to three types of data
filters in order not to overload the NatureCounts server. However, it is
fairly straightforward to apply data filters after the download.

In this vignette we will go over several different types of filtering
using the `tidyverse` collection of packages. In particular, we will
make extensive use of the
[`filter()`](https://dplyr.tidyverse.org/reference/filter.html) function
from the `dplyr` package. For more information about this function, see
the chapter on [Data
Transformations](https://r4ds.had.co.nz/transform.html#filter-rows-with-filter)
in R for Data Science by Hadley Wickham and Garrett Grolemund.

For each example, we will show case how to filter your data either
directly downloaded as a data frame, or obtained from a SQLite database.

First let’s get a set of Boreal Chickadee data to use (see the [vignette
on species
codes](https://birdscanada.github.io/naturecounts/dev/articles/species-codes.md))
for more information on how to find species codes).

> The following examples use the “testuser” user which is not available
> to you. You can quickly [sign up for a free
> account](https://naturecounts.ca/nc/default/register.jsp) of your own
> to access and play around with these examples. Simply replace
> `testuser` with your own username.

## Setup

**Packages**

``` r

library(naturecounts)
library(dplyr)
library(lubridate)
```

**Get the species code**

``` r

search_species("boreal chickadee")
```

    ## # A tibble: 3 × 5
    ##   species_id scientific_name                english_name french_name taxon_group
    ##        <int> <chr>                          <chr>        <chr>       <chr>      
    ## 1      14320 Poecile hudsonicus             Boreal Chic… Mésange à … BIRDS      
    ## 2      41990 Poecile gambeli x hudsonicus   Mountain x … Hybride Mé… BIRDS      
    ## 3      44462 Poecile atricapillus x hudson… Black-cappe… Hybride Mé… BIRDS

**Downloading data directly to a Data Frame**

``` r

boreal_chickadee_df <- nc_data_dl(
  species = 14320, collection = c("BBS", "ABATLAS1", "ABATLAS2"),
  verbose = FALSE,
  username = "testuser", info = "nc_vignette")
```

**Downloading data to a SQLite DataBase (`boreal_chickadee.nc`)**

``` r

con <- nc_data_dl(
  species = 14320, collection = c("BBS", "ABATLAS1", "ABATLAS2"),
  sql_db = "boreal_chickadee", verbose = FALSE, 
  username = "testuser", info = "nc_vignette")
```

Or, if starting a new session and you want to reconnect to *an existing
SQLite database*, first create the connection to the database

``` r

con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "boreal_chickadee.nc")
```

Select the naturecounts table

``` r

boreal_chickadee_db <- tbl(con, "naturecounts")
```

When using a database, you can choose to
[`collect()`](https://dplyr.tidyverse.org/reference/compute.html) your
data into a data frame (in which case it will be identical to
`chickadee_df`, but if your database is large, it will be faster to
leave it as a database as long as possible.

``` r

boreal_chickadee_df <- collect(boreal_chickadee_db)
```

We will also use a helper function to add columns for `date` and `doy`
(day of year) to our data.

For data frames, you directly modify the data frame object.

``` r

boreal_chickadee_df <- format_dates(boreal_chickadee_df, overwrite = TRUE)
```

For databases, you’ll modify the connection, then update your table
reference.

**Note:** This results in on-disk additions to the SQLite database. If
you do not wish to add fields to your SQLite database, you will have to
[`collect()`](https://dplyr.tidyverse.org/reference/compute.html) the
data into a data frame first.

``` r

con <- format_dates(con)
boreal_chickadee_db <- tbl(con, "naturecounts")
```

Not all dates can be created if `survey_year`, `survey_month` or
`survey_day` are missing.

``` r

filter(boreal_chickadee_df, is.na(date)) |>
  select(survey_year, survey_month, survey_day, date)
```

    ## # A tibble: 56 × 4
    ##    survey_year survey_month survey_day date  
    ##          <int>        <int>      <int> <date>
    ##  1        1991           NA         NA NA    
    ##  2        1990           NA         NA NA    
    ##  3        1991           NA         NA NA    
    ##  4        1991           NA         NA NA    
    ##  5        1990           NA         NA NA    
    ##  6        1991           NA         NA NA    
    ##  7        1988           NA         NA NA    
    ##  8        1989           NA         NA NA    
    ##  9        1987           NA         NA NA    
    ## 10        1988           NA         NA NA    
    ## # ℹ 46 more rows

Unless stated otherwise, in the following examples, we demonstrate code
that works for both data frames and databases.

## Categorical filters

The simplest way to filter your data is to pick out categories to
include or exclude. These functions can apply to dataframes or database.
In each case there are three steps.

1.  What are the categories available?
2.  Filter the categories you a) wish to keep, or b) wish to exclude
3.  Confirm the results

These steps work for categories such as `collection`, `species`,
`survey_year`, `iba_site`, `bcr` or any variable that can be expressed
as a category.

1.  What categories are available?

[`count()`](https://dplyr.tidyverse.org/reference/count.html) will count
the unique values in the column of interest

``` r

count(boreal_chickadee_db, collection)
```

    ## # A query:  ?? x 2
    ## # Database: sqlite 3.53.2 [/home/runner/work/naturecounts/naturecounts/vignettes/boreal_chickadee.nc]
    ##   collection     n
    ##   <chr>      <int>
    ## 1 ABATLAS1     489
    ## 2 ABATLAS2     872
    ## 3 BBS         3119

2.  Filter the categories you wish to…

…keep

``` r

alberta_atlas <- filter(boreal_chickadee_db, 
                        collection %in% c("ABATLAS1", "ABATLAS2"))
```

…exclude (with `!`)

``` r

alberta_atlas <- filter(boreal_chickadee_db, 
                        !collection %in% c("BBS"))
```

3.  Confirm

``` r

count(alberta_atlas, collection)
```

    ## # A query:  ?? x 2
    ## # Database: sqlite 3.53.2 [/home/runner/work/naturecounts/naturecounts/vignettes/boreal_chickadee.nc]
    ##   collection     n
    ##   <chr>      <int>
    ## 1 ABATLAS1     489
    ## 2 ABATLAS2     872

## Numerical filters

Another way to filter your data is to specify numerical ranges include
or exclude. In each case there are the same three steps as above.

1.  What is the numerical range available?
2.  Filter the variable to the range you wish to keep either implicitly
    or explicitly
3.  Confirm the results

These steps work for categories such as `survey_year`, `survey_month`,
`survey_day`, `doy` or any variable that can be expressed as a number.

1.  What values are available?

[`summary()`](https://rspatial.github.io/terra/reference/summary.html)
returns the numerical summary you define. You can also ‘pull’ out the
survey years from the count.

``` r

summarize(
  boreal_chickadee_db,
  min = min(survey_year, na.rm = TRUE), 
  max = max(survey_year, na.rm = TRUE))
```

    ## # A query:  ?? x 2
    ## # Database: sqlite 3.53.2 [/home/runner/work/naturecounts/naturecounts/vignettes/boreal_chickadee.nc]
    ##     min   max
    ##   <int> <int>
    ## 1  1966  2010

``` r

count(boreal_chickadee_db, survey_year) |>
  pull(survey_year)
```

    ##  [1] 1966 1967 1968 1969 1970 1971 1972 1973 1974 1975 1976 1977 1978 1979 1980
    ## [16] 1981 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995
    ## [31] 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010

2.  Filter the variable to the range you wish to…

…keep (note that the `,` is an implied `&`)

``` r

recent <- filter(boreal_chickadee_db, survey_year >= 2005, survey_year <= 2007)
```

…keep (explicitly)

``` r

recent <- filter(boreal_chickadee_db, survey_year %in% c(2005:2007))
```

3.  Confirm

``` r

summarize(
  recent,
  min = min(survey_year, na.rm = TRUE), 
  max = max(survey_year, na.rm = TRUE))
```

    ## # A query:  ?? x 2
    ## # Database: sqlite 3.53.2 [/home/runner/work/naturecounts/naturecounts/vignettes/boreal_chickadee.nc]
    ##     min   max
    ##   <int> <int>
    ## 1  2005  2007

``` r

count(recent, survey_year) |>
  pull(survey_year)
```

    ## [1] 2005 2006 2007

## Date filters

Filtering by dates is usually slightly more complex, but fortunately,
the [`filter()`](https://dplyr.tidyverse.org/reference/filter.html)
function automatically detects dates which means it’s pretty much the
same as a numerical filter.

1.  What values are available?

[`summary()`](https://rspatial.github.io/terra/reference/summary.html)
returns the numerical summary you define

``` r

summarize(boreal_chickadee_db, 
          min = min(date, na.rm = TRUE), 
          max = max(date, na.rm = TRUE))
```

    ## # A query:  ?? x 2
    ## # Database: sqlite 3.53.2 [/home/runner/work/naturecounts/naturecounts/vignettes/boreal_chickadee.nc]
    ##   min        max       
    ##   <chr>      <chr>     
    ## 1 1966-06-02 2010-07-07

2.  Filter the variable to the range you wish to…

…keep (note that the `,` is an implied `&`)

``` r

field_season <- filter(boreal_chickadee_db, 
                       date >= "2010-05-10", date <= "2010-07-01")
```

…exclude (with `!`; note that here we need to use the `&` explicitly)

``` r

omit_field_season <- filter(boreal_chickadee_db, 
                            !(date >= "2010-05-10" & date <= "2010-07-01"))
```

3.  Confirm

``` r

summarize(field_season, 
          min = min(date, na.rm = TRUE), 
          max = max(date, na.rm = TRUE))
```

    ## # A query:  ?? x 2
    ## # Database: sqlite 3.53.2 [/home/runner/work/naturecounts/naturecounts/vignettes/boreal_chickadee.nc]
    ##   min        max       
    ##   <chr>      <chr>     
    ## 1 2010-05-30 2010-06-30
