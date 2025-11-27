# Download information about NatureCounts collections

Download the number of records available for different collections
filtered by location (if provided). If authorization is provided, the
collections are filtered to only those available to the user (unless
using `show = "all"`). Without authorization all collections are
returned.

## Usage

``` r
nc_count(
  collections = NULL,
  project_ids = NULL,
  species = NULL,
  years = NULL,
  doy = NULL,
  region = NULL,
  site_type = NULL,
  show = "available",
  username = NULL,
  timeout = 120,
  verbose = TRUE
)
```

## Arguments

- collections:

  Character vector. The collection codes from which to download data.
  NULL (default) downloads data from all available collections

- project_ids:

  Character/Numeric vector. The `project id`s from which to download
  data. First the collections associated with a `project_id` are
  determined, and then data is downloaded for each collection. If both
  `collections` and `project_ids` are supplied, they are combined.

- species:

  Numeric vector. Numeric species ids (see details)

- years:

  Numeric vector. The start/end years of data to download. Can use NA
  for either start or end, or a single value to return data from a
  single year.

- doy:

  Character/Numeric vector. The start/end day-of-year to download (1-366
  or dates that can be converted to day of year). Can use NA for either
  start or end

- region:

  List. Named list with *one* of the following options: `country`,
  `statprov`, `subnational2`, `iba`, `bcr`, `utm_squares`, `bbox`. See
  details

- site_type:

  Character vector. The type of site to return (e.g., `IBA`).

- show:

  Character. Either "all" or "available". "all" returns counts from all
  data sources. "available" only returns counts for data available for
  the username provided. If no username is provided, defaults to "all".

- username:

  Character vector. Username for <http://naturecounts.ca>. If provided,
  the user will be prompted for a password. If left NULL, only public
  collections will be returned.

- timeout:

  Numeric. Number of seconds before connecting to the server times out.

- verbose:

  Logical. Show messages?

## Value

Data frame

## Details

The `akn_level` column describes the level of data access for that
collection (see [descriptions
online](https://naturecounts.ca/nc/default/nc_access_levels.jsp)). The
`access` column describes the accessibility of a collection for a given
username (or no access if no username supplied). See the section on
Access and `request_id`s for more details.

## NatureCounts account

All public data is available with a username/password ([sign
up](https://www.naturecounts.ca/nc/default/register.jsp) for a free
NatureCounts account). However, to access private/semi-public
projects/collections you must request access. See the Access and
`request_id`s section for more information.

## Species ids (`species`)

Numeric species id codes can determined from the functions
[`search_species()`](https://birdscanada.github.io/naturecounts/dev/reference/search_species.md)
or
[`search_species_code()`](https://birdscanada.github.io/naturecounts/dev/reference/search_species_code.md).
See also the article on [species
codes](https://birdscanada.github.io/naturecounts/articles/species-codes.html)
for more information.

## Day of Year (`doy`)

The format for day of year (`doy`) is fairly flexible and can be a whole
number between 1 and 366 or anything recognized by
[`lubridate-package`](https://lubridate.tidyverse.org/reference/lubridate-package.html)'s
[`ymd()`](https://lubridate.tidyverse.org/reference/ymd.html) function.
However, it must have the order of year, month, day. Note that year is
ignored when converting to day of year, except that it will result in a
1 day offset for leap years.

## Regions (`region`)

Regions are defined by codes reflecting the country, state/province,
subnational (level 2), Important Bird Areas (IBA), and Bird Conservation
Regions (BCR) (see
[`search_region()`](https://birdscanada.github.io/naturecounts/dev/reference/search_region.md)
for codes). They can also be defined by providing specific UTM squares
to download or a bounding box area which specifies the min/max longitude
and min/max latitude (`bbox`). See the article on [regional
filters](http://birdscanada.github.io/naturecounts/articles/region-codes.md)
for more information.

## Access and `request_id`s

Access to a data collection is either available as "full" or "by
request". Use `nc_count(username = "USER", show = "all")`, to see the
accessibility of collections.

"Full" access means that data can be immediately downloaded directly
through the `naturecounts` R package. "By request" means that a request
must be [submitted
online](https://naturecounts.ca/nc/default/searchquery.jsp) and approved
before the data can be downloaded through `naturecounts`.

This means that there are two types of data requests: ones made through
this `naturecounts` R package (API requests) and those made through the
online [Web Request
Form](https://naturecounts.ca/nc/default/searchquery.jsp) (Web
requests). Every request (from either method) generates a `request_id`
which identifies the filter set and collections requested. Details of
all of requests can be reviewed with the
[`nc_requests()`](https://birdscanada.github.io/naturecounts/dev/reference/nc_requests.md)
function.

To download data with "full" access, users can either specify filters,
or if they are repeating a download, can use the `request_id` from
[`nc_requests()`](https://birdscanada.github.io/naturecounts/dev/reference/nc_requests.md).

Otherwise, if the user doesn't have "full" access, they must supply an
approved `request_id` to the
[`nc_data_dl()`](https://birdscanada.github.io/naturecounts/dev/reference/nc_data_dl.md)
function (e.g., `nc_data_dl(request_id = 152000, username = "USER")`).
Use
[`nc_requests()`](https://birdscanada.github.io/naturecounts/dev/reference/nc_requests.md)
to see `request_id`s, filters, and approval status.

Requests for "full" access to additional collections can be made online
through the [Web Request
Form](https://naturecounts.ca/nc/default/searchquery.jsp) by checking
the "Full access?" box in Step 2 of the form.

## See also

[`nc_requests()`](https://birdscanada.github.io/naturecounts/dev/reference/nc_requests.md)

## Examples

``` r
# Count all publicly available records:
# \donttest{
nc_count()
#> Without a username, using 'show = "all"'
#> # A tibble: 511 × 4
#>    collection    akn_level access     nrecords
#>    <chr>             <int> <chr>         <int>
#>  1 ABATLAS1              5 full         122258
#>  2 ABATLAS2              5 full         201357
#>  3 ABBIRDRECS            5 full         357264
#>  4 ABOWLS                3 by request    20956
#>  5 ACCWS                 3 by request    22889
#>  6 ATBANS                3 by request      267
#>  7 ATOWLS               NA NA            35260
#>  8 AWSGS                 3 by request    17968
#>  9 BBL-1960-1969         5 full       15045208
#> 10 BBL-1970-1979         5 full       12837516
#> # ℹ 501 more rows
# }

# Count publicly available records for Manitoba, Canada
# \donttest{
nc_count(region = list(statprov = "MB"))
#> Without a username, using 'show = "all"'
#> Using filters: statprov (MB)
#> # A tibble: 113 × 4
#>    collection    akn_level access     nrecords
#>    <chr>             <int> <chr>         <int>
#>  1 ABBIRDRECS            5 full            357
#>  2 BBL-1960-1969         5 full         135264
#>  3 BBL-1970-1979         5 full         299843
#>  4 BBL-1980-1989         5 full         232636
#>  5 BBL-1990-1999         5 full         285100
#>  6 BBL-2000-2009         5 full         239238
#>  7 BBL-2010-2019         5 full         175584
#>  8 BBL-2020-2029         5 full          74702
#>  9 BBS                   5 full          86997
#> 10 BBS50-CAN             3 by request   536767
#> # ℹ 103 more rows
# }

# Count all records for all collections user "sample" has access to
if (FALSE) { # \dontrun{
nc_count(username = "sample")
} # }

# Count records with house finches in Ontario
search_species("house finch")
#> # A tibble: 3 × 5
#>   species_id scientific_name                english_name french_name taxon_group
#>        <int> <chr>                          <chr>        <chr>       <chr>      
#> 1      20350 Haemorhous mexicanus           House Finch  Roselin fa… BIRDS      
#> 2      42255 Haemorhous mexicanus [mexican… House Finch… Roselin fa… BIRDS      
#> 3      42256 Haemorhous mexicanus mcgregori House Finch… Roselin fa… BIRDS      
nc_count(species = 20350, region = list(statprov = "ON"), username = "sample")
#> Using filters: species (20350); statprov (ON)
#> # A tibble: 2 × 4
#>   collection akn_level access nrecords
#>   <chr>          <int> <chr>     <int>
#> 1 SAMPLE1            0 full          8
#> 2 SAMPLE2            0 full         11

# Count all records available in the Christmas Bird Count and Breeding Bird
# Survey collections (regardless of user permissions)
nc_count(collections = c("CBC", "BBS"), show = "all", username = "sample")
#> Using filters: collections (CBC, BBS)
#> # A tibble: 2 × 4
#>   collection akn_level access     nrecords
#>   <chr>          <int> <chr>         <int>
#> 1 BBS                5 by request  5735895
#> 2 CBC                3 by request  7570427

```
