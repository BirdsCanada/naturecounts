
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `naturecounts`

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/BirdStudiesCanada/naturecounts.svg?branch=master)](https://travis-ci.com/BirdStudiesCanada/naturecounts)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/BirdStudiesCanada/naturecounts?branch=master&svg=true)](https://ci.appveyor.com/project/BirdStudiesCanada/naturecounts)

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

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
collections associated with username **sample**).

``` r
nc_count(username = "sample")
#>   collection akn_level access nrecords
#> 1    SAMPLE1         0   full     1000
#> 2    SAMPLE2         0   full     1000
```

Use the `show = "all"` argument to show counts for all collections
available (public or otherwise).

``` r
nc_count(show = "all") %>%
  head()
#>   collection akn_level     access nrecords
#> 1   ABATLAS1         5       full   123364
#> 2   ABATLAS2         5       full   201398
#> 3 ABBIRDRECS         5       full   357264
#> 4     ABOWLS         4 by request        0
#> 5      ATBBS         4 by request        0
#> 6     ATOWLS         4 by request    25192
```

### Fetching data

Fetch all observations of bittern which are available to user **sample**
into a local data frame.

First find the species id

``` r
search_species("American Bittern")
#>   species_id       scientific_name     english_name      french_name
#> 1       2490 Botaurus lentiginosus American Bittern Butor d'Amérique
#>   taxon_group
#> 1       BIRDS
```

Use this id with `nc_data_dl()`. The `info` parameter is a short
description of what the data is being downloaded for.

``` r
bittern <- nc_data_dl(species = 2490, username = "sample", 
                    info = "readme_example")
#> Using filters: species (2490); fields_set (BMDE2.00-min)
#> Collecting available records...
#>   collection nrecords
#> 1    SAMPLE1        1
#> Total records: 1
#> 
#> Downloading records for each collection:
#>   SAMPLE1
#>     Records 1 to 1 / 1
```

Alternatively, save the downloaded data as a SQLite database
(`bittern`).

``` r
bittern <- nc_data_dl(species = 2490, sql_db = "bittern", username = "sample", 
                    info = "readme_example")
#> Using filters: species (2490); fields_set (BMDE2.00-min)
#> Collecting available records...
#>   collection nrecords
#> 1    SAMPLE1        1
#> Total records: 1
#> 
#> Database 'bittern.nc' does not exist, creating it...
#> 
#> Downloading records for each collection:
#>   SAMPLE1
#>     Records 1 to 1 / 1
```

### Authorizations

To access private/semi-public projects/collections you must [sign
up](https://www.birdscanada.org/birdmon/default/profile.jsp) for a free
NatureCounts account and
[register](https://www.birdscanada.org/birdmon/default/projects.jsp) for
the projects you’d like to access. Once registered, you can use the
`username` argument (you will be prompted for a password) for both
`nc_count()` and `nc_data_dl()`, which will then return a different set
of records.

``` r
nc_count(username = "my_user_name")
bittern <- nc_data_dl(species = 2490, username = "my_user_name", info = "readme_example")
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
