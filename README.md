
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
nc_count(show = "all")
#>                collection nrecords
#> 1                ABATLAS1   123364
#> 2                ABATLAS2   201398
#> 3              ABBIRDRECS   357264
#> 4                  ATOWLS      367
#> 5                     BBS  5735895
#> 6               BBS50-CAN  3219534
#> 7           BBS50-US-EAST  8067776
#> 8           BBS50-US-WEST  8701116
#> 9           BCATLAS1BE_DO    41412
#> 10         BCATLAS1BE_RAW   344433
#> 11        BCATLAS1BE_SUMM   165485
#> 12             BCATLAS1PC   260647
#> 13             BCATLAS1RC    21884
#> 14                BCBEACH     7263
#> 15                  BCCWS   329523
#> 16                   BCMA     4311
#> 17                 BCOWLS    27149
#> 18                  BGRMM      476
#> 19              BIRDSOFBC   498444
#> 20                    CBC  7570427
#> 21         CMMN-BAND-LPBO  1168735
#> 22           CMMN-DET-ABO    85569
#> 23          CMMN-DET-ACBO    26716
#> 24           CMMN-DET-BBO    66632
#> 25          CMMN-DET-BPBO    88397
#> 26          CMMN-DET-DMBO    88280
#> 27           CMMN-DET-HBO   257374
#> 28           CMMN-DET-IBS    29114
#> 29          CMMN-DET-IPBO    49741
#> 30          CMMN-DET-LMBO    56571
#> 31          CMMN-DET-LPBO  1073798
#> 32         CMMN-DET-LSLBO   108383
#> 33          CMMN-DET-MGBO    80063
#> 34          CMMN-DET-MIBO     6844
#> 35           CMMN-DET-MNO    63993
#> 36           CMMN-DET-OOT   106146
#> 37         CMMN-DET-PEPBO   131973
#> 38          CMMN-DET-PIBO   116763
#> 39          CMMN-DET-RPBO   121615
#> 40          CMMN-DET-TCBO   130618
#> 41         CMMN-DET-TLBBS    28037
#> 42          CMMN-DET-TLBO    26771
#> 43        CMMN-DET-TTPBRS    97635
#> 44          CMMN-DET-VLBO    52723
#> 45          CMMN-DET-WPBO    63251
#> 46         CMMN-LPBO-MOBU     2863
#> 47            EBIRD-CA-AT  3901226
#> 48            EBIRD-CA-BC 11218063
#> 49            EBIRD-CA-NO   753993
#> 50            EBIRD-CA-ON 20104013
#> 51            EBIRD-CA-PR  5898226
#> 52            EBIRD-CA-QC 15818301
#> 53          EBIRD-CA-SENS    26678
#> 54                   GBBC   431613
#> 55              HAWKCOUNT   112673
#> 56                IMMP_A2       24
#> 57                IMMP_BW       19
#> 58                   IMQC      761
#> 59          MBATLAS1BE_DO    93876
#> 60         MBATLAS1BE_RAW   326429
#> 61        MBATLAS1BE_SUMM   154856
#> 62             MBATLAS1PC   306094
#> 63             MBATLAS1RC    15622
#> 64            MBBA1BE_RAW   142920
#> 65           MBBA1BE_SUMM    93053
#> 66            MBBA2BE_RAW   263809
#> 67           MBBA2BE_SUMM   114616
#> 68                MBBA2PC   127447
#> 69            MBBA2PC_SAR       13
#> 70                MBBA2RC    14213
#> 71                 MBOWLS    23561
#> 72                   MLMP      425
#> 73                     MM     4343
#> 74               MMPBIRDS   366668
#> 75               MMPFROGS    88438
#> 76            Naturalista       30
#> 77              NESTWATCH   413415
#> 78            OBBA1BE_RAW   407287
#> 79           OBBA1BE_SUMM   205816
#> 80            OBBA2BE_RAW   541743
#> 81           OBBA2BE_SUMM   256702
#> 82                OBBA2PC   592425
#> 83            OBBA2PC_SAR       31
#> 84                OBBA2RC    27970
#> 85                   OBFS   324560
#> 86              OISEAUXQC   666963
#> 87                 ONOWLS    40449
#> 88                    PFW  3616772
#> 89            PFW-US-EAST  8651432
#> 90            PFW-US-WEST  5631851
#> 91                   PMMM        5
#> 92             PRISM-ACSS   117138
#> 93              PRISM-OSS    47456
#> 94              PRISM-PSS     1817
#> 95    QCATLAS_NORTH_BE_DO     7043
#> 96   QCATLAS_NORTH_BE_RAW    19095
#> 97  QCATLAS_NORTH_BE_SUMM    12305
#> 98       QCATLAS_NORTH_PC     8239
#> 99         QCATLAS1BE_RAW   198816
#> 100       QCATLAS1BE_SUMM   136333
#> 101         QCATLAS2BE_DO   283034
#> 102        QCATLAS2BE_RAW   601540
#> 103       QCATLAS2BE_SUMM   231775
#> 104            QCATLAS2PC   341902
#> 105            QCATLAS2RC    39390
#> 106                QCOWLS    11195
#> 107           RCBIOTABASE    12598
#> 108              SAR-RSHA    27696
#> 109         SKATLAS1BE_DO    99872
#> 110       SKATLAS1BE_SUMM    42203
#> 111            SKATLAS1PC    27916
#> 112            SWIFTWATCH    10624
#> 113                  WPWI     3012
#> 114            WRNIGHTJAR     2754
```

### Fetching data

Fetch all observations of moose which are **publically** available to a
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
(`[nc_count()](https://birdstudiescanada.github.io/naturecounts/reference/nc_count.html)`,
`[nc_data_dl()](https://birdstudiescanada.github.io/naturecounts/reference/nc_data_dl.html)`)
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
data](https://birdstudiescanada.github.io/naturecounts/articles/articles/filtering-data.html)

### Metadata

NatureCounts includes a great deal of metadata which can be accessed
through the functions with the `meta_` prefix. See the [Meta
Documentation](reference/meta.html) for specifics.
