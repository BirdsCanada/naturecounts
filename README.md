
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `naturecounts`

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

Access and download data on plant and animal populations from various
databases through NatureCounts, a service managed by Bird Studies
Canada.

## Installation

You can install this developmental version of `naturecounts` from GitHub
with the remotes package:

``` r
install.packages("remotes")
remotes::install_github("BirdStudiesCanada/naturecounts")
```

## Usage

``` r
library(naturecounts)
```

### Fetching counts

Use the `nc_count()` function to return collections and the number of
observations in each for which you have access (here returns all
**public** collections).

``` r
nc_count()
#>     collection nrecords
#> 1     ABATLAS1   123364
#> 2     ABATLAS2   201398
#> 3   ABBIRDRECS   357264
#> 4        BGRMM      476
#> 5      IMMP_A2       24
#> 6      IMMP_BW       19
#> 7         IMQC      761
#> 8         MLMP      425
#> 9           MM     4343
#> 10        PMMM        5
#> 11 RCBIOTABASE    12598
```

Use the `show = "all"` argument to show counts for all collections
available (public or otherwise).

``` r
nc_count(show = "all") %>%
  head()
#>   collection nrecords
#> 1   ABATLAS1   123364
#> 2   ABATLAS2   201398
#> 3 ABBIRDRECS   357264
#> 4     ATOWLS      367
#> 5        BBS  5735895
#> 6  BBS50-CAN  3219534
```

### Fetching data

Fetch all observations of moose which are **publicly** available to a
local data frame.

``` r
moose <- nc_data_dl(species = 133990)
#> Using filters: species (133990); fields_set (BMDE2.00-min)
#> Collecting available records...
#>    collection nrecords
#> 1 RCBIOTABASE        2
#> 
#> Downloading records for each collection:
#>   RCBIOTABASE
#>     Records 1 to 2 / 2
```

Alternatively, save the downloaded data as a SQLite database
(`moose.nc`).

``` r
moose <- nc_data_dl(species = 133990, sql_db = "moose")
#> Using filters: species (133990); fields_set (BMDE2.00-min)
#> Collecting available records...
#>    collection nrecords
#> 1 RCBIOTABASE        2
#> 
#> Database 'moose.nc' does not exist, creating it...
#> 
#> Downloading records for each collection:
#>   RCBIOTABASE
#>     Records 1 to 2 / 2
```

### Authorizations

By default `nc_count()` and `nc_data_dl()` return public data which is
available without a username/password. However, to access
private/semi-public projects/collections you must [sign
up](https://www.birdscanada.org/birdmon/default/profile.jsp) for a free
NatureCounts account and
[register](https://www.birdscanada.org/birdmon/default/projects.jsp) for
the projects you’d like to access. Once registered, you can use the
`username` argument (you will be prompted for a password) for both
`nc_count()` and `nc_data_dl()`, which will then return a different set
of records.

``` r
nc_count(username = "my_user_name")
moose <- nc_data_dl(species = 133990, username = "my_user_name")
```

### More advanced options

`nc_count()` and `nc_data_dl()` have a variety of arguments that allow
you to filter the counts/data prior to downloading. These options
include `collections`, `species`, `years`, `doy` (day-of-year),
`region`, and `site_type` (users can specify up to 3 of these). For
`nc_data_dl()` you have the additional arguments `fields_set` and
`fields` with which you can customize which fields/columns to include in
your download.

See the function examples
([`nc_count()`](https://birdstudiescanada.github.io/naturecounts/reference/nc_count.html),
[`nc_data_dl()`](https://birdstudiescanada.github.io/naturecounts/reference/nc_data_dl.html))
the following articles for more information on these filters:

  - Collections
  - [Species
    Codes](https://birdstudiescanada.github.io/naturecounts/articles/species-codes.html)
  - [Regional
    Codes](https://birdstudiescanada.github.io/naturecounts/articles/region-codes.html)
  - [IBAs and BCRs
    (regions)](https://birdstudiescanada.github.io/naturecounts/articles/region-areas.html)
  - [Using spatial data to filter
    observations](https://birdstudiescanada.github.io/naturecounts/articles/region-spatial.html)

We also have an [article on post-filtering your
data](https://birdstudiescanada.github.io/naturecounts/articles/filtering-data.html)

### Metadata

NatureCounts includes a great deal of metadata which can be accessed
through the functions with the `meta_` prefix. See the [Meta
Documentation](https://birdstudiescanada.github.io/naturecounts/reference/meta.html)
for specifics.
