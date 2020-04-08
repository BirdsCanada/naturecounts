
<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/BirdStudiesCanada/naturecounts.svg?branch=master)](https://travis-ci.org/BirdStudiesCanada/naturecounts)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/BirdStudiesCanada/naturecounts?branch=master&svg=true)](https://ci.appveyor.com/project/BirdStudiesCanada/naturecounts)
<!-- badges: end -->

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
collections associated with username **sample**).

``` r
nc_count(username = "sample")
#>           collection akn_level access nrecords
#> 1           ABATLAS1         5   full   123364
#> 2           ABATLAS2         5   full   201398
#> 3         ABBIRDRECS         5   full   357264
#> 4                BBS         5   full  5735895
#> 5              BBS50         5   full        0
#> 6          BBS50-CAN         5   full  4168740
#> 7      BBS50-US-EAST         5   full  9940162
#> 8      BBS50-US-WEST         5   full 10749161
#> 9      BCATLAS1BE_DO         5   full    41412
#> 10    BCATLAS1BE_RAW         5   full   344433
#> 11   BCATLAS1BE_SUMM         5   full   165485
#> 12        BCATLAS1PC         5   full   260647
#> 13              BCMA         5   full    11291
#> 14             BGRMM         5   full      476
#> 15      CMMN-DET-HBO         5   full   274839
#> 16     CMMN-DET-RUTH         5   full        0
#> 17     CMMN-DET-SELK         5   full        0
#> 18     CMMN-DET-VLBO         5   full    56287
#> 19        EBUTTERFLY         5   full    26192
#> 20              IMMP         5   full       43
#> 21           IMMP_A2         5   full       24
#> 22           IMMP_BW         5   full       19
#> 23              IMQC         5   full      761
#> 24     MBATLAS1BE_DO         5   full    93876
#> 25    MBATLAS1BE_RAW         5   full   326429
#> 26   MBATLAS1BE_SUMM         5   full   154856
#> 27        MBATLAS1PC         5   full   306094
#> 28       MBBA1BE_RAW         5   full   142920
#> 29      MBBA1BE_SUMM         5   full    93053
#> 30       MBBA2BE_RAW         5   full   263809
#> 31      MBBA2BE_SUMM         5   full   114616
#> 32           MBBA2PC         5   full   127447
#> 33              MEXU         5   full     3251
#> 34              MLMP         5   full      802
#> 35                MM         5   full     4343
#> 36      MONARCHWATCH         5   full   145665
#> 37       NATURALISTA         5   full       30
#> 38 NESTWATCH_KMARTIN         5   full        0
#> 39      NRCS_KMARTIN         5   full        0
#> 40       OBBA1BE_RAW         5   full   407287
#> 41      OBBA1BE_SUMM         5   full   205816
#> 42       OBBA2BE_RAW         5   full   541743
#> 43      OBBA2BE_SUMM         5   full   256702
#> 44           OBBA2PC         5   full   592425
#> 45              OBFS         5   full   324560
#> 46               PFW         5   full  3841194
#> 47              PMMM         5   full        5
#> 48    QCATLAS1BE_RAW         5   full   198816
#> 49   QCATLAS1BE_SUMM         5   full   136333
#> 50     QCATLAS2BE_DO         5   full   283034
#> 51    QCATLAS2BE_RAW         5   full   601540
#> 52   QCATLAS2BE_SUMM         5   full   231775
#> 53        QCATLAS2PC         5   full   341902
#> 54       RCBIOTABASE         5   full    12614
#> 55           SAMPLE1         0   full       NA
#> 56           SAMPLE2         0   full       NA
#> 57              WMMM         5   full     8981
#> 58              WPWI         5   full     3012
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
#> 6     ATOWLS         4 by request     2778
```

### Fetching data

Fetch all observations of moose which are available to user **sample**
into a local data frame. The `info` parameter is a short description of
what the data is being downloaded for.

``` r
moose <- nc_data_dl(species = 133990, username = "sample", 
                    info = "readme_example")
#> Using filters: species (133990); fields_set (BMDE2.00-min)
#> Collecting available records...
#>    collection nrecords
#> 1 RCBIOTABASE        2
#> Total records: 2
#> 
#> Downloading records for each collection:
#>   RCBIOTABASE
#>     Records 1 to 2 / 2
```

Alternatively, save the downloaded data as a SQLite database
(`moose.nc`).

``` r
moose <- nc_data_dl(species = 133990, sql_db = "moose", username = "sample", 
                    info = "readme_example")
#> Using filters: species (133990); fields_set (BMDE2.00-min)
#> Collecting available records...
#>    collection nrecords
#> 1 RCBIOTABASE        2
#> Total records: 2
#> 
#> Database 'moose.nc' does not exist, creating it...
#> 
#> Downloading records for each collection:
#>   RCBIOTABASE
#>     Records 1 to 2 / 2
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
moose <- nc_data_dl(species = 133990, username = "my_user_name", info = "readme_example")
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
