# Custom table queries

Generate custom table queries with the table name and filter arguments.

## Usage

``` r
nc_query_table(
  table = NULL,
  ...,
  username = NULL,
  timeout = 120,
  verbose = FALSE
)
```

## Arguments

- table:

  Character. Table to query (see details)

- ...:

  Name/value pairs for custom queries/filters (see details)

- username:

  Character vector. Username for <http://naturecounts.ca>. If provided,
  the user will be prompted for a password. If left NULL, only public
  collections will be returned.

- timeout:

  Numeric. Number of seconds before connecting to the server times out.

- verbose:

  Logical. Show messages?

## Value

data.frame()

## Details

`nc_query_table(username = "sample")` for available options

## Examples

``` r

# What tables are available? What 'filters' do they take? Are any 'required'?

nc_query_table(username = "sample")
#>              table_name                        filters required
#> 1    AtlasSquareSummary                 statprove_code       NA
#> 2 bmde_filter_bad_dates project_id,SiteCode,species_id       NA
#> 3                 Rnest                           <NA>       NA
#> 4         RnestMetadata                           <NA>       NA
#> 5           SocbSpecies                           <NA>       NA
#> 6       SocbTrendGroups                        include       NA
#> 7    SpeciesLifeHistory                           <NA>       NA

# Query the bmdefilter_bad_dates table

d <- nc_query_table(table = "bmde_filter_bad_dates", username = "sample")
head(d)
#>   survey_month species_id period                   comments project_id SiteCode
#> 1            0          0 spring              beaver damage       1005     ACBO
#> 2            0          0 spring              beaver damage       1005     ACBO
#> 3            0          0 spring                   missing        1005      BBO
#> 4            0          0 spring                   missing        1005      BBO
#> 5            0          0      0           covid disruption       1005      BBO
#> 6            0       7680      0 began NSWO surveys in 2000       1005      BBO
#>   survey_year start_year end_year survey_day
#> 1        2019          0        0          0
#> 2        2022          0        0          0
#> 3        2002          0        0          0
#> 4        2019          0        0          0
#> 5        2020          0        0          0
#> 6           0          0     1999          0

# Filter our query
d <- nc_query_table(table = "bmde_filter_bad_dates",
                    SiteCode = "DMBO", username = "sample")
d
#>   survey_month species_id period                                   comments
#> 1            1          0      0 Changed sites in spring in 2006 - new site
#> 2            2          0      0 Changed sites in spring in 2006 - new site
#> 3            3          0      0 Changed sites in spring in 2006 - new site
#> 4            4          0      0 Changed sites in spring in 2006 - new site
#> 5            5          0      0 Changed sites in spring in 2006 - new site
#> 6            6          0      0 Changed sites in spring in 2006 - new site
#> 7            0          0      0              Oak Hammock Marsh covid dates
#>   project_id SiteCode survey_year start_year end_year survey_day
#> 1       1005     DMBO           0          0     2005          0
#> 2       1005     DMBO           0          0     2005          0
#> 3       1005     DMBO           0          0     2005          0
#> 4       1005     DMBO           0          0     2005          0
#> 5       1005     DMBO           0          0     2005          0
#> 6       1005     DMBO           0          0     2005          0
#> 7       1005     DMBO        2020          0        0          0

# Filter our query
d <- nc_query_table(table = "bmde_filter_bad_dates",
                    species_id = 15770, username = "sample")

# Want more than one species? Either filter after, or combine two queries

# Filter after
library(dplyr)
d <- nc_query_table(table = "bmde_filter_bad_dates", username = "sample")
d <- filter(d, species_id %in% c(15770, 9750))

# Combine two queries
d1 <- nc_query_table(table = "bmde_filter_bad_dates",
                     species_id = 15770, username = "sample")
d2 <- nc_query_table(table = "bmde_filter_bad_dates",
                     species_id = 9750, username = "sample")
d <- rbind(d1, d2)
```
