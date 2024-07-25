
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `naturecounts`

<!-- badges: start -->

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R-CMD-check](https://github.com/BirdsCanada/naturecounts/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/BirdsCanada/naturecounts/actions/workflows/R-CMD-check.yaml)
![r-universe](https://birdscanada.r-universe.dev/badges/naturecounts)
<!-- badges: end -->

Access and download data on plant and animal populations from various
databases through NatureCounts, a service managed by Birds Canada.

See tutorials, documentation and articles on the [naturecounts package
Website](https://birdscanada.github.io/naturecounts)

## Installation

You can install the main version of `naturecounts` from our R-Universe

``` r
install.packages("naturecounts", 
                 repos = c(birdscanada = 'https://birdscanada.r-universe.dev',
                           CRAN = 'https://cloud.r-project.org'))
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
#> # A tibble: 2 × 4
#>   collection akn_level access nrecords
#>   <chr>          <int> <chr>     <int>
#> 1 SAMPLE1            0 full        991
#> 2 SAMPLE2            0 full        995
```

Use the `show = "all"` argument to show counts for all collections
available (public or otherwise).

``` r
nc_count(show = "all") %>%
  head()
#> # A tibble: 6 × 4
#>   collection akn_level access     nrecords
#>   <chr>          <int> <chr>         <int>
#> 1 ABATLAS1           5 full         123364
#> 2 ABATLAS2           5 full         201357
#> 3 ABBIRDRECS         5 full         357264
#> 4 ATBANS             3 by request      267
#> 5 ATOWLS             4 by request    33964
#> 6 BBS                5 full        5735895
```

### Fetching data

Fetch all observations of bittern which are available to user **sample**
into a local data frame.

First find the species id

``` r
search_species("American Bittern")
#> # A tibble: 1 × 5
#>   species_id scientific_name       english_name     french_name      taxon_group
#>        <int> <chr>                 <chr>            <chr>            <chr>      
#> 1       2490 Botaurus lentiginosus American Bittern Butor d'Amérique BIRDS
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
([`nc_count()`](https://birdscanada.github.io/naturecounts/reference/nc_count.html),
[`nc_data_dl()`](https://birdscanada.github.io/naturecounts/reference/nc_data_dl.html))
the following articles for more information on these filters:

- Collections
- [Species
  Codes](https://birdscanada.github.io/naturecounts/articles/species-codes.html)
- [Regional
  Codes](https://birdscanada.github.io/naturecounts/articles/region-codes.html)
- [IBAs and BCRs
  (regions)](https://birdscanada.github.io/naturecounts/articles/region-areas.html)
- [Using spatial data to filter
  observations](https://birdscanada.github.io/naturecounts/articles/region-spatial.html)

We also have an [article on post-filtering your
data](https://birdscanada.github.io/naturecounts/articles/filtering-data.html)

### Metadata

NatureCounts includes a great deal of metadata which can be accessed
through the functions with the `meta_` prefix. See the [Meta
Documentation](https://birdscanada.github.io/naturecounts/reference/meta.html)
for specifics.
