
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
install_github("BirdStudiesCanada/naturecounts")
```

## Usage

``` r
library(naturecounts)
```

### Fetching data counts

Use the `nc_count()` function to return all public collections and the
number of observations in each.

``` r
nc_count()
#>     collection nrecords
#> 1     ABATLAS1   123364
#> 2     ABATLAS2   201398
#> 3   ABBIRDRECS   357264
#> 4        BGRMM      476
#> 5   EBUTTERFLY    34473
#> 6      IMMP_A2       24
#> 7      IMMP_BW       19
#> 8         IMQC      761
#> 9         MLMP      425
#> 10          MM     4343
#> 11        PMMM        5
#> 12 RCBIOTABASE    12598
```

If you have a [NatureCounts
username](https://www.birdscanada.org/birdmon/default/register.jsp) you
can supply it with the `username` argument (you will be prompted for a
password). `nc_count()` will now return all the records you have access
to.

``` r
nc_count(username = "my_user_name")
```

### Fetching data

Fetch all observations of moose which are publically available to a
local data frame.

``` r
moose <- nc_data_dl(species = 133990)
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
#> Collecting available records...
#>    collection nrecords
#> 1 RCBIOTABASE        2
#> 
#> Downloading records for each collection:
#>   RCBIOTABASE
#>     Records 1 to 2 / 2
```

> See the article on [Species Codes](articles/species-codes.html) to
> determine which codes to use.

### Metadata

NatureCounts includes a great deal of metadata which can be accessed
through the functions with the `meta_` prefix. See the [Meta
Documentation](reference/meta.html) for specifics.
