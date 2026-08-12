# Download NatureCounts data records

Download data records from various collections filtered by various
options. In order to ease the load on the server, note that only
**three** of `collections`/`project_ids`, `species`, `years`, `doy`,
`region`, and `site_type` can be used in any one request. See the
vignette for filtering your data after download for more options:
`vignette("filtering_data", package = "naturecounts")`.

## Usage

``` r
nc_data_dl(
  collections = NULL,
  project_ids = NULL,
  species = NULL,
  years = NULL,
  doy = NULL,
  region = NULL,
  site_type = NULL,
  fields_set = "extended",
  fields = NULL,
  username,
  info = NULL,
  request_id = NULL,
  sql_db = NULL,
  warn = TRUE,
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

- fields_set:

  Character. Set of fields/columns to download. See details.

- fields:

  Character vector. If `fields_set = custom`, which fields/columns to
  download. See details

- username:

  Character vector. Username for <http://naturecounts.ca>. If provided,
  the user will be prompted for a password. If left NULL, only public
  collections will be returned.

- info:

  Character vector. Short description of reason for the download. E.g.,
  "COSEWIC report", "Impact Assessment Study", "School project", etc.
  This kind of information helps NatureCounts.ca justify the utility of
  the database. Required unless resuming/re-downloaded with a
  `request_id`.

- request_id:

  Numeric. Specific request id to check or download.

- sql_db:

  Character vector. Name and location of SQLite database to either
  create or add to

- warn:

  Logical. Interactive warning if request more than 1,000,000 records to
  download.

- timeout:

  Numeric. Number of seconds before connecting to the server times out.

- verbose:

  Logical. Show messages?

## Value

Data frame or connection to SQLite database

## NatureCounts account

All public data is available with a username/password ([sign
up](https://www.naturecounts.ca/nc/default/register.jsp) for a free
NatureCounts account). However, to access private/semi-public
projects/collections you must request access. See the Access and
`request_id`s section for more information.

## Species ids (`species`)

Numeric species id codes can determined from the functions
[`search_species()`](https://birdscanada.github.io/naturecounts/reference/search_species.md)
or
[`search_species_code()`](https://birdscanada.github.io/naturecounts/reference/search_species_code.md).
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
[`search_region()`](https://birdscanada.github.io/naturecounts/reference/search_region.md)
for codes). They can also be defined by providing specific UTM squares
to download or a bounding box area which specifies the min/max longitude
and min/max latitude (`bbox`). See the article on [regional
filters](http://birdscanada.github.io/naturecounts/articles/region-codes.md)
for more information.

## Data Fields/Columns (`fields_set` and `fields`)

By default data is downloaded with the `extended` set of fields/columns.
However, for more advanced applications, users may wish to specify which
fields/columns to return. The Bird Monitoring Data Exchange (BMDE)
schema keeps track of variables used to augment observation data. There
are different versions reflecting different collections of variables
which can be specified for download in one of four ways:

1.  `fields_set` can be a specific shorthand reflecting a BMDE version:
    `core`, `extended` (default) or `minimum`. See
    [`meta_bmde_versions()`](https://birdscanada.github.io/naturecounts/reference/meta.md)
    to see which BMDE version the shorthand refers to.

2.  `fields_set` can be `default` which uses the default BMDE version
    for a particular collection (note that if you download more than one
    collection, the field sets will expand to cover all fields/columns
    in the combined collections)

3.  `fields_set` can be the exact BMDE version. See
    [`meta_bmde_versions()`](https://birdscanada.github.io/naturecounts/reference/meta.md)
    for options.

4.  `fields_set` can be `custom` and the `fields` argument can be a
    character vector specifying the exact fields/columns to return. See
    [`meta_bmde_fields()`](https://birdscanada.github.io/naturecounts/reference/meta.md))
    for potential `fields` values.

Note that in all cases there are a set of fields/columns that are
*always* returned, no matter what `fields_set` is used.

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
[`nc_requests()`](https://birdscanada.github.io/naturecounts/reference/nc_requests.md)
function.

To download data with "full" access, users can either specify filters,
or if they are repeating a download, can use the `request_id` from
[`nc_requests()`](https://birdscanada.github.io/naturecounts/reference/nc_requests.md).

Otherwise, if the user doesn't have "full" access, they must supply an
approved `request_id` to the `nc_data_dl()` function (e.g.,
`nc_data_dl(request_id = 152000, username = "USER")`). Use
[`nc_requests()`](https://birdscanada.github.io/naturecounts/reference/nc_requests.md)
to see `request_id`s, filters, and approval status.

Requests for "full" access to additional collections can be made online
through the [Web Request
Form](https://naturecounts.ca/nc/default/searchquery.jsp) by checking
the "Full access?" box in Step 2 of the form.

## Examples

``` r
# All observations part of the SAMPLE1 and SAMPLE2 collections
sample <- nc_data_dl(collections = c("SAMPLE1", "SAMPLE2"),
                     username = "sample", info = "nc_example")
#> Using filters: collections (SAMPLE1, SAMPLE2); fields_set (BMDE2.00-ext)
#> Collecting available records...
#>   collection nrecords
#> 1    SAMPLE1      991
#> 2    SAMPLE2      995
#> Total records: 1,986
#> 
#> Downloading records for each collection:
#>   SAMPLE1
#>     Records 1 to 991 / 991
#>   SAMPLE2
#>     Records 1 to 995 / 995
#> Metadata hasn't been updated in >4 weeks, consider using `nc_metadata()` to update local copies.

# All observations part of project_id 1042 accessible by "testuser"
p1042 <- nc_data_dl(project_ids = 1042, username = "testuser",
                    info = "nc_example")
#> Using filters: collections (ABATLAS1, ABATLAS2, ABBIRDRECS); fields_set (BMDE2.00-ext)
#> Collecting available records...
#>   collection nrecords
#> 1   ABATLAS1   122258
#> 2   ABATLAS2   201357
#> 3 ABBIRDRECS   357264
#> Total records: 680,879
#> 
#> Downloading records for each collection:
#>   ABATLAS1
#>     Records 1 to 5000 / 122258
#>     Records 5001 to 10000 / 122258
#>     Records 10001 to 15000 / 122258
#>     Records 15001 to 20000 / 122258
#>     Records 20001 to 25000 / 122258
#>     Records 25001 to 30000 / 122258
#>     Records 30001 to 35000 / 122258
#>     Records 35001 to 40000 / 122258
#>     Records 40001 to 45000 / 122258
#>     Records 45001 to 50000 / 122258
#>     Records 50001 to 55000 / 122258
#>     Records 55001 to 60000 / 122258
#>     Records 60001 to 65000 / 122258
#>     Records 65001 to 70000 / 122258
#>     Records 70001 to 75000 / 122258
#>     Records 75001 to 80000 / 122258
#>     Records 80001 to 85000 / 122258
#>     Records 85001 to 90000 / 122258
#>     Records 90001 to 95000 / 122258
#>     Records 95001 to 1e+05 / 122258
#>     Records 100001 to 105000 / 122258
#>     Records 105001 to 110000 / 122258
#>     Records 110001 to 115000 / 122258
#>     Records 115001 to 120000 / 122258
#>     Records 120001 to 122258 / 122258
#>   ABATLAS2
#>     Records 1 to 5000 / 201357
#>     Records 5001 to 10000 / 201357
#>     Records 10001 to 15000 / 201357
#>     Records 15001 to 20000 / 201357
#>     Records 20001 to 25000 / 201357
#>     Records 25001 to 30000 / 201357
#>     Records 30001 to 35000 / 201357
#>     Records 35001 to 40000 / 201357
#>     Records 40001 to 45000 / 201357
#>     Records 45001 to 50000 / 201357
#>     Records 50001 to 55000 / 201357
#>     Records 55001 to 60000 / 201357
#>     Records 60001 to 65000 / 201357
#>     Records 65001 to 70000 / 201357
#>     Records 70001 to 75000 / 201357
#>     Records 75001 to 80000 / 201357
#>     Records 80001 to 85000 / 201357
#>     Records 85001 to 90000 / 201357
#>     Records 90001 to 95000 / 201357
#>     Records 95001 to 1e+05 / 201357
#>     Records 100001 to 105000 / 201357
#>     Records 105001 to 110000 / 201357
#>     Records 110001 to 115000 / 201357
#>     Records 115001 to 120000 / 201357
#>     Records 120001 to 125000 / 201357
#>     Records 125001 to 130000 / 201357
#>     Records 130001 to 135000 / 201357
#>     Records 135001 to 140000 / 201357
#>     Records 140001 to 145000 / 201357
#>     Records 145001 to 150000 / 201357
#>     Records 150001 to 155000 / 201357
#>     Records 155001 to 160000 / 201357
#>     Records 160001 to 165000 / 201357
#>     Records 165001 to 170000 / 201357
#>     Records 170001 to 175000 / 201357
#>     Records 175001 to 180000 / 201357
#>     Records 180001 to 185000 / 201357
#>     Records 185001 to 190000 / 201357
#>     Records 190001 to 195000 / 201357
#>     Records 195001 to 2e+05 / 201357
#>     Records 200001 to 201357 / 201357
#>   ABBIRDRECS
#>     Records 1 to 5000 / 357264
#>     Records 5001 to 10000 / 357264
#>     Records 10001 to 15000 / 357264
#>     Records 15001 to 20000 / 357264
#>     Records 20001 to 25000 / 357264
#>     Records 25001 to 30000 / 357264
#>     Records 30001 to 35000 / 357264
#>     Records 35001 to 40000 / 357264
#>     Records 40001 to 45000 / 357264
#>     Records 45001 to 50000 / 357264
#>     Records 50001 to 55000 / 357264
#>     Records 55001 to 60000 / 357264
#>     Records 60001 to 65000 / 357264
#>     Records 65001 to 70000 / 357264
#>     Records 70001 to 75000 / 357264
#>     Records 75001 to 80000 / 357264
#>     Records 80001 to 85000 / 357264
#>     Records 85001 to 90000 / 357264
#>     Records 90001 to 95000 / 357264
#>     Records 95001 to 1e+05 / 357264
#>     Records 100001 to 105000 / 357264
#>     Records 105001 to 110000 / 357264
#>     Records 110001 to 115000 / 357264
#>     Records 115001 to 120000 / 357264
#>     Records 120001 to 125000 / 357264
#>     Records 125001 to 130000 / 357264
#>     Records 130001 to 135000 / 357264
#>     Records 135001 to 140000 / 357264
#>     Records 140001 to 145000 / 357264
#>     Records 145001 to 150000 / 357264
#>     Records 150001 to 155000 / 357264
#>     Records 155001 to 160000 / 357264
#>     Records 160001 to 165000 / 357264
#>     Records 165001 to 170000 / 357264
#>     Records 170001 to 175000 / 357264
#>     Records 175001 to 180000 / 357264
#>     Records 180001 to 185000 / 357264
#>     Records 185001 to 190000 / 357264
#>     Records 190001 to 195000 / 357264
#>     Records 195001 to 2e+05 / 357264
#>     Records 200001 to 205000 / 357264
#>     Records 205001 to 210000 / 357264
#>     Records 210001 to 215000 / 357264
#>     Records 215001 to 220000 / 357264
#>     Records 220001 to 225000 / 357264
#>     Records 225001 to 230000 / 357264
#>     Records 230001 to 235000 / 357264
#>     Records 235001 to 240000 / 357264
#>     Records 240001 to 245000 / 357264
#>     Records 245001 to 250000 / 357264
#>     Records 250001 to 255000 / 357264
#>     Records 255001 to 260000 / 357264
#>     Records 260001 to 265000 / 357264
#>     Records 265001 to 270000 / 357264
#>     Records 270001 to 275000 / 357264
#>     Records 275001 to 280000 / 357264
#>     Records 280001 to 285000 / 357264
#>     Records 285001 to 290000 / 357264
#>     Records 290001 to 295000 / 357264
#>     Records 295001 to 3e+05 / 357264
#>     Records 300001 to 305000 / 357264
#>     Records 305001 to 310000 / 357264
#>     Records 310001 to 315000 / 357264
#>     Records 315001 to 320000 / 357264
#>     Records 320001 to 325000 / 357264
#>     Records 325001 to 330000 / 357264
#>     Records 330001 to 335000 / 357264
#>     Records 335001 to 340000 / 357264
#>     Records 340001 to 345000 / 357264
#>     Records 345001 to 350000 / 357264
#>     Records 350001 to 355000 / 357264
#>     Records 355001 to 357264 / 357264
#> Metadata hasn't been updated in >4 weeks, consider using `nc_metadata()` to update local copies.

# Black-capped Chickadees (BCCH) in SAMPLE2 collection in 2013
search_species("black-capped chickadee") # Find the species_id
#> Metadata hasn't been updated in >4 weeks, consider using `nc_metadata()` to update local copies.
#> # A tibble: 4 × 5
#>   species_id scientific_name                english_name french_name taxon_group
#>        <int> <chr>                          <chr>        <chr>       <chr>      
#> 1      14280 Poecile atricapillus           Black-cappe… Mésange à … BIRDS      
#> 2      40668 Poecile carolinensis x atrica… Carolina x … Hybride Mé… BIRDS      
#> 3      40669 Poecile carolinensis/atricapi… Carolina/Bl… Mésange de… BIRDS      
#> 4      44466 Poecile atricapillus x Baeolo… Black-cappe… Hybride Mé… BIRDS      
bcch <- nc_data_dl(collection = "SAMPLE2", species = 14280, year = 2013,
                   username = "sample", info = "nc_example")
#> Metadata hasn't been updated in >4 weeks, consider using `nc_metadata()` to update local copies.
#> Using filters: collections (SAMPLE2); species (14280); fields_set (BMDE2.00-ext); start_year (2013); end_year (2013)
#> Collecting available records...
#>   collection nrecords
#> 1    SAMPLE2       14
#> Total records: 14
#> 
#> Downloading records for each collection:
#>   SAMPLE2
#>     Records 1 to 14 / 14
#> Metadata hasn't been updated in >4 weeks, consider using `nc_metadata()` to update local copies.

# All BCCH observations since 2015 accessible to user "sample"
bcch <- nc_data_dl(species = 14280, years = c(2015, NA), username = "sample",
                    info = "nc_example")
#> Metadata hasn't been updated in >4 weeks, consider using `nc_metadata()` to update local copies.
#> Using filters: species (14280); fields_set (BMDE2.00-ext); start_year (2015)
#> Collecting available records...
#>   collection nrecords
#> 1    SAMPLE1       20
#> 2    SAMPLE2       20
#> Total records: 40
#> 
#> Downloading records for each collection:
#>   SAMPLE1
#>     Records 1 to 20 / 20
#>   SAMPLE2
#>     Records 1 to 20 / 20
#> Metadata hasn't been updated in >4 weeks, consider using `nc_metadata()` to update local copies.

# All BCCH observations from mid-July to late October in all years for user "sample"
bcch <- nc_data_dl(species = 14280, doy = c(200, 300), username = "sample",
                    info = "nc_example")
#> Metadata hasn't been updated in >4 weeks, consider using `nc_metadata()` to update local copies.
#> Using filters: species (14280); fields_set (BMDE2.00-ext); start_doy (200); end_doy (300)
#> Collecting available records...
#>   collection nrecords
#> 1    SAMPLE1        7
#> 2    SAMPLE2        7
#> Total records: 14
#> 
#> Downloading records for each collection:
#>   SAMPLE1
#>     Records 1 to 7 / 7
#>   SAMPLE2
#>     Records 1 to 7 / 7
#> Metadata hasn't been updated in >4 weeks, consider using `nc_metadata()` to update local copies.

# All BCCH observations from a specific bounding box for user "sample"
bcch <- nc_data_dl(species = 14280, username = "sample",
                   region = list(bbox = c(left = -100, bottom = 45,
                                          right = -80, top = 60)),
                    info = "nc_example")
#> Metadata hasn't been updated in >4 weeks, consider using `nc_metadata()` to update local copies.
#> Using filters: species (14280); fields_set (BMDE2.00-ext); bbox_left (-100); bbox_bottom (45); bbox_right (-80); bbox_top (60)
#> Collecting available records...
#>   collection nrecords
#> 1    SAMPLE1        3
#> 2    SAMPLE2        2
#> Total records: 5
#> 
#> Downloading records for each collection:
#>   SAMPLE1
#>     Records 1 to 3 / 3
#>   SAMPLE2
#>     Records 1 to 2 / 2
#> Metadata hasn't been updated in >4 weeks, consider using `nc_metadata()` to update local copies.

# All American Bittern observations from user "sample"
search_species("american bittern")
#> Metadata hasn't been updated in >4 weeks, consider using `nc_metadata()` to update local copies.
#> # A tibble: 1 × 5
#>   species_id scientific_name       english_name     french_name      taxon_group
#>        <int> <chr>                 <chr>            <chr>            <chr>      
#> 1       2490 Botaurus lentiginosus American Bittern Butor d'Amérique BIRDS      
bittern <- nc_data_dl(species = 2490, username = "sample", info = "nc_example")
#> Metadata hasn't been updated in >4 weeks, consider using `nc_metadata()` to update local copies.
#> Using filters: species (2490); fields_set (BMDE2.00-ext)
#> Collecting available records...
#>   collection nrecords
#> 1    SAMPLE1        1
#> Total records: 1
#> 
#> Downloading records for each collection:
#>   SAMPLE1
#>     Records 1 to 1 / 1
#> Metadata hasn't been updated in >4 weeks, consider using `nc_metadata()` to update local copies.

# Different fields/columns
bittern <- nc_data_dl(species = 2490, fields_set = "core",
                      username = "sample", info = "nc_example")
#> Metadata hasn't been updated in >4 weeks, consider using `nc_metadata()` to update local copies.
#> Using filters: species (2490); fields_set (BMDE2.00)
#> Collecting available records...
#>   collection nrecords
#> 1    SAMPLE1        1
#> Total records: 1
#> 
#> Downloading records for each collection:
#>   SAMPLE1
#>     Records 1 to 1 / 1
#> Metadata hasn't been updated in >4 weeks, consider using `nc_metadata()` to update local copies.

bittern <- nc_data_dl(species = 2490, fields_set = "custom",
                      fields = c("Locality", "AllSpeciesReported"),
                      username = "sample", info = "nc_example")
#> Metadata hasn't been updated in >4 weeks, consider using `nc_metadata()` to update local copies.
#> Metadata hasn't been updated in >4 weeks, consider using `nc_metadata()` to update local copies.
#> Using filters: species (2490); fields_set (custom); fields (Locality, AllSpeciesReported)
#> Collecting available records...
#>   collection nrecords
#> 1    SAMPLE1        1
#> Total records: 1
#> 
#> Downloading records for each collection:
#>   SAMPLE1
#>     Records 1 to 1 / 1
#> Metadata hasn't been updated in >4 weeks, consider using `nc_metadata()` to update local copies.

if (FALSE) { # \dontrun{
# All collections by request id

# Specific collection by request id
my_data <- nc_data_dl(collections = "ABATLAS1",
                      request_id = 000000, username = "USER",
                      info = "MY REASON")
} # }
```
