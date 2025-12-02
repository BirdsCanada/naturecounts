# Species codes

``` r
library(naturecounts)
```

In order to download data pertaining to specific species, you’ll have to
get the species id codes. These are numeric codes that reflect species
identity, and in complex cases, take account of subspecies or changes to
species identity.

For all species, you can search by scientific, English or French name
with the
[`search_species()`](https://birdscanada.github.io/naturecounts/reference/search_species.md)
function.

For birds, you can also search by alphanumeric species codes with the
[`search_species_code()`](https://birdscanada.github.io/naturecounts/reference/search_species_code.md)
function. **Note:** These alphanumeric codes are *not* the ones used by
`naturecounts`, but are ones used by various taxonomic authorities. This
function also gives you the option of returning all species codes
(including subspecies, etc.) related to a single species, and is
considered a more robust method for ensuring that you do not miss
observations.

> The following examples use the “testuser” user which is not available
> to you. You can quickly [sign up for a free
> account](https://naturecounts.ca/nc/default/register.jsp) of your own
> to access and play around with these examples. Simply replace
> `testuser` with your own username.

## Searching by name

The simplest way to determine a species id (and the only way, for
non-avian taxa) is to use the
[`search_species()`](https://birdscanada.github.io/naturecounts/reference/search_species.md)
function which searches for a species id according to scientific,
English or French name and returns a data frame of taxonomic information
for all related hits:

``` r
search_species("moose")
#> # A tibble: 1 × 5
#>   species_id scientific_name english_name french_name taxon_group
#>        <int> <chr>           <chr>        <chr>       <chr>      
#> 1     133990 Alces alces     Moose        NA          MAMMALS
search_species("swallowtail")
#> # A tibble: 8 × 5
#>   species_id scientific_name     english_name            french_name taxon_group
#>        <int> <chr>               <chr>                   <chr>       <chr>      
#> 1     252492 Battus philenor     Pipevine Swallowtail    Papillon d… BUTTERFL   
#> 2     252493 Eurytides marcellus Zebra Swallowtail       Papillon z… BUTTERFL   
#> 3     252494 Papilio polyxenes   Black Swallowtail       Papillon d… BUTTERFL   
#> 4     252495 Papilio machaon     Old World Swallowtail   Machaon     BUTTERFL   
#> 5     252496 Papilio cresphontes Giant Swallowtail       Grand port… BUTTERFL   
#> 6     252497 Papilio glaucus     Eastern Tiger Swallowt… Papillon t… BUTTERFL   
#> 7     252498 Papilio canadensis  Canadian Tiger Swallow… Papillon t… BUTTERFL   
#> 8     252499 Papilio troilus     Spicebush Swallowtail   Papillon c… BUTTERFL
search_species("mesange a tete noire")
#> # A tibble: 5 × 5
#>   species_id scientific_name                english_name french_name taxon_group
#>        <int> <chr>                          <chr>        <chr>       <chr>      
#> 1      14280 Poecile atricapillus           Black-cappe… Mésange à … BIRDS      
#> 2      40932 Poecile atricapillus x gambeli Black-cappe… Hybride Mé… BIRDS      
#> 3      44462 Poecile atricapillus x hudson… Black-cappe… Hybride Mé… BIRDS      
#> 4      44466 Poecile atricapillus x Baeolo… Black-cappe… Hybride Mé… BIRDS      
#> 5      45746 Poecile atricapillus/gambeli   Black-cappe… Mésange à … BIRDS
search_species("mésange à tête noire")
#> # A tibble: 5 × 5
#>   species_id scientific_name                english_name french_name taxon_group
#>        <int> <chr>                          <chr>        <chr>       <chr>      
#> 1      14280 Poecile atricapillus           Black-cappe… Mésange à … BIRDS      
#> 2      40932 Poecile atricapillus x gambeli Black-cappe… Hybride Mé… BIRDS      
#> 3      44462 Poecile atricapillus x hudson… Black-cappe… Hybride Mé… BIRDS      
#> 4      44466 Poecile atricapillus x Baeolo… Black-cappe… Hybride Mé… BIRDS      
#> 5      45746 Poecile atricapillus/gambeli   Black-cappe… Mésange à … BIRDS
```

These species ids can then be used to download data either directly:

``` r
moose <- nc_data_dl(
  species = 133990,
  verbose = FALSE,
  username = "testuser",
  info = "nc_vignette"
)
```

Or by saving and referencing the data frame:

``` r
moose_id <- search_species("moose")$species_id
moose <- nc_data_dl(
  species = moose_id,
  verbose = FALSE,
  username = "testuser",
  info = "nc_vignette"
)
```

This might be considered overkill for a single species, but is useful
when you want to download data for multiple species:

``` r
chickadee_ids <- search_species("chickadee")
chickadee_ids
#> # A tibble: 23 × 5
#>    species_id scientific_name               english_name french_name taxon_group
#>         <int> <chr>                         <chr>        <chr>       <chr>      
#>  1      14270 Poecile carolinensis          Carolina Ch… Mésange de… BIRDS      
#>  2      14280 Poecile atricapillus          Black-cappe… Mésange à … BIRDS      
#>  3      14290 Poecile gambeli               Mountain Ch… Mésange de… BIRDS      
#>  4      14300 Poecile sclateri              Mexican Chi… Mésange gr… BIRDS      
#>  5      14310 Poecile rufescens             Chestnut-ba… Mésange à … BIRDS      
#>  6      14320 Poecile hudsonicus            Boreal Chic… Mésange à … BIRDS      
#>  7      14330 Poecile cinctus               Gray-headed… Mésange la… BIRDS      
#>  8      14388 Paridae sp.                   Chickadee sp Mésange sp. BIRDS      
#>  9      40668 Poecile carolinensis x atric… Carolina x … Hybride Mé… BIRDS      
#> 10      40669 Poecile carolinensis/atricap… Carolina/Bl… Mésange de… BIRDS      
#> # ℹ 13 more rows
```

Now we could download all observations for the first three (this is a
lot of data!)

``` r
chickadees <- nc_data_dl(
  species = chickadee_ids$species_id[1:3],
  verbose = FALSE,
  username = "testuser",
  info = "nc_vignette"
)
```

## Searching by alphanumeric code

Taxonomy for bird species has long included various alphanumeric codes
from different taxonomic authorities. The codes often include separate
codes for recognized subdivisions of the species (subspecies,
subpopulations, hybrids, etc.). The
[`search_species_code()`](https://birdscanada.github.io/naturecounts/reference/search_species_code.md)
allows you to search for avian species ids according to these
authorities.

### Authorities

The default taxonomic authority used is `BSCDATA`, which uses 4-letter
alpha codes:

``` r
search_species_code("BCCH")
#> # A tibble: 1 × 5
#>   species_id BSCDATA scientific_name      english_name           french_name    
#>        <int> <chr>   <chr>                <chr>                  <chr>          
#> 1      14280 BCCH    Poecile atricapillus Black-capped Chickadee Mésange à tête…
```

The search function is case insensitive:

``` r
search_species_code("bcch")
#> # A tibble: 1 × 5
#>   species_id BSCDATA scientific_name      english_name           french_name    
#>        <int> <chr>   <chr>                <chr>                  <chr>          
#> 1      14280 BCCH    Poecile atricapillus Black-capped Chickadee Mésange à tête…
```

However, you can search by codes for different authorities, by
specifying the appropriate `authority` argument:

``` r
search_species_code(8868, authority = "CBC")
#> # A tibble: 1 × 5
#>   species_id CBC   scientific_name      english_name           french_name      
#>        <int> <chr> <chr>                <chr>                  <chr>            
#> 1      14280 8868  Poecile atricapillus Black-capped Chickadee Mésange à tête n…
```

### All related subspecies

By default, the `search_species_codes()` returns all species ids related
to the search term.

For example, searching for myrtle warblers (`MYWA`), one of two
subspecies of the yellow-rumped warbler (`YRWA`) returns both subspecies
and the parent species.

``` r
search_species_code("MYWA")
#> # A tibble: 3 × 5
#>   species_id BSCDATA scientific_name             english_name        french_name
#>        <int> <chr>   <chr>                       <chr>               <chr>      
#> 1      16610 YRWA    Setophaga coronata          Yellow-rumped Warb… Paruline à…
#> 2      16620 MYWA    Setophaga coronata coronata Yellow-rumped Warb… Paruline à…
#> 3      16630 AUWA    Setophaga coronata auduboni Yellow-rumped Warb… Paruline à…
```

**Note:** Different taxonomic authorities recognize different subgroups.

For example, `BSCDATA` recognizes 4 groups for dark-eyed juncos:

``` r
search_species_code("DEJU")
#> # A tibble: 4 × 5
#>   species_id BSCDATA scientific_name                    english_name french_name
#>        <int> <chr>   <chr>                              <chr>        <chr>      
#> 1      19090 SCJU    Junco hyemalis hyemalis/carolinen… Dark-eyed J… Junco ardo…
#> 2      19110 PSJU    Junco hyemalis mearnsi             Dark-eyed J… Junco ardo…
#> 3      42218 DEJU    Junco hyemalis                     Dark-eyed J… Junco ardo…
#> 4      47928 ORJU    Junco hyemalis [oreganus Group]    Dark-eyed J… Junco ardo…
```

Whereas `CBC` also recognizes sub group hybrids (and the Guadalupe
junco):

``` r
search_species_code("12385", authority = "CBC")
#> # A tibble: 11 × 5
#>    species_id CBC   scientific_name                     english_name french_name
#>         <int> <chr> <chr>                               <chr>        <chr>      
#>  1      42218 12385 Junco hyemalis                      Dark-eyed J… Junco ardo…
#>  2      19090 12386 Junco hyemalis hyemalis/carolinens… Dark-eyed J… Junco ardo…
#>  3      41434 12388 Junco hyemalis cismontanus          Dark-eyed J… Junco ardo…
#>  4      19100 12389 Junco hyemalis [oreganus Group]     Dark-eyed J… Junco ardo…
#>  5      19110 12390 Junco hyemalis mearnsi              Dark-eyed J… Junco ardo…
#>  6      42219 12391 Junco hyemalis [oreganus Group] x … Dark-eyed J… Junco ardo…
#>  7      19112 12392 Junco hyemalis aikeni               Dark-eyed J… Junco ardo…
#>  8      19111 12394 Junco hyemalis caniceps             Dark-eyed J… Junco ardo…
#>  9      42220 12395 Junco hyemalis mearnsi x caniceps   Dark-eyed J… Junco ardo…
#> 10      40859 12396 Junco hyemalis dorsalis             Dark-eyed J… Junco ardo…
#> 11      39768 12398 Junco insularis                     Guadalupe J… Junco de G…
```

You can search by more than one authority at the same time. Note that
your search term only needs to match one authority (not both), and that
the information returned reflects both authorities combined.

``` r
search_species_code("DEJU", authority = c("BSCDATA", "CBC"))
#> # A tibble: 12 × 6
#>    species_id BSCDATA CBC   scientific_name             english_name french_name
#>         <int> <chr>   <chr> <chr>                       <chr>        <chr>      
#>  1      42218 DEJU    12385 Junco hyemalis              Dark-eyed J… Junco ardo…
#>  2      47928 ORJU    NA    Junco hyemalis [oreganus G… Dark-eyed J… Junco ardo…
#>  3      19110 PSJU    12390 Junco hyemalis mearnsi      Dark-eyed J… Junco ardo…
#>  4      19090 SCJU    12386 Junco hyemalis hyemalis/ca… Dark-eyed J… Junco ardo…
#>  5      41434 NA      12388 Junco hyemalis cismontanus  Dark-eyed J… Junco ardo…
#>  6      19100 NA      12389 Junco hyemalis [oreganus G… Dark-eyed J… Junco ardo…
#>  7      42219 NA      12391 Junco hyemalis [oreganus G… Dark-eyed J… Junco ardo…
#>  8      19112 NA      12392 Junco hyemalis aikeni       Dark-eyed J… Junco ardo…
#>  9      19111 NA      12394 Junco hyemalis caniceps     Dark-eyed J… Junco ardo…
#> 10      42220 NA      12395 Junco hyemalis mearnsi x c… Dark-eyed J… Junco ardo…
#> 11      40859 NA      12396 Junco hyemalis dorsalis     Dark-eyed J… Junco ardo…
#> 12      39768 NA      12398 Junco insularis             Guadalupe J… Junco de G…
```

## Exact species

If you do not want all subgroups, you can use the `results = "exact"`
argument to return only an exact match.

``` r
search_species_code("DEJU", results = "exact")
#> # A tibble: 1 × 5
#>   species_id BSCDATA scientific_name english_name    french_name  
#>        <int> <chr>   <chr>           <chr>           <chr>        
#> 1      42218 DEJU    Junco hyemalis  Dark-eyed Junco Junco ardoisé
search_species_code("ORJU", results = "exact")
#> # A tibble: 1 × 5
#>   species_id BSCDATA scientific_name                 english_name    french_name
#>        <int> <chr>   <chr>                           <chr>           <chr>      
#> 1      47928 ORJU    Junco hyemalis [oreganus Group] Dark-eyed Junc… Junco ardo…
```

## Advanced searches

If you have a very specific type of search you’d like to do, you may be
better off using the
[`filter()`](https://dplyr.tidyverse.org/reference/filter.html) function
from the `dplyr` package.

``` r
library(dplyr)
```

For example, let’s assume you’re interested in collecting species codes
for Chipmunks.

We can get the entire species list by omitting a search term, and also
return all taxonomic information available with the `show = "all"`
argument.

``` r
all <- search_species(show = "all")
all
#> # A tibble: 41,523 × 17
#>    species_id concept_id    scientific_name species_status added_dt english_name
#>         <int> <chr>         <chr>                    <int> <chr>    <chr>       
#>  1          0 avibase-74B5… N/A                          9 NA       No observat…
#>  2         10 avibase-42D7… Tinamus major                1 NA       Great Tinam…
#>  3         20 avibase-66C3… Nothocercus bo…              1 NA       Highland Ti…
#>  4         30 avibase-39DD… Crypturellus s…              1 NA       Little Tina…
#>  5         40 avibase-AAB5… Crypturellus c…              1 NA       Thicket Tin…
#>  6         50 avibase-5183… Crypturellus b…              1 NA       Slaty-breas…
#>  7         60 avibase-2ED9… Crypturellus k…              1 NA       Choco Tinam…
#>  8         70 avibase-A534… Dendrocygna vi…              1 NA       White-faced…
#>  9         80 avibase-C01A… Dendrocygna au…              1 NA       Black-belli…
#> 10         90 avibase-DC65… Dendrocygna ar…              1 NA       West Indian…
#> # ℹ 41,513 more rows
#> # ℹ 11 more variables: french_name <chr>, phylum <chr>,
#> #   family_french_name <chr>, group_id <dbl>, order_taxon <chr>,
#> #   taxon_group <chr>, sort_order <int>, class <chr>, family_name <chr>,
#> #   concept_source <chr>, family_english_name <chr>
```

Now we can search for the order

``` r
chipmunk_ids <- filter(all, family_name == "Tamiini")
chipmunk_ids
#> # A tibble: 28 × 17
#>    species_id concept_id    scientific_name species_status added_dt english_name
#>         <int> <chr>         <chr>                    <int> <chr>    <chr>       
#>  1     137600 84E594FD3600… Neotamias alpi…              1 NA       Alpine Chip…
#>  2     137610 9F7307BBC3E2… Neotamias amoe…              1 NA       Yellow-pine…
#>  3     137620 74CCC160CD03… Neotamias bull…              1 NA       Buller's Ch…
#>  4     137630 224DE8C28159… Neotamias cani…              1 NA       Gray-footed…
#>  5     137640 F58265FDE2B9… Neotamias cine…              1 NA       Gray-collar…
#>  6     137650 7D2D47726C19… Neotamias dors…              1 NA       Cliff Chipm…
#>  7     137660 FAE4F43BFAC7… Neotamias dura…              1 NA       Durango Chi…
#>  8     137670 12D6B42EF3E6… Neotamias merr…              1 NA       Merriam's C…
#>  9     137680 C7431688A812… Neotamias mini…              1 NA       Least Chipm…
#> 10     137690 C0FCFB0E88A7… Neotamias obsc…              1 NA       California …
#> # ℹ 18 more rows
#> # ℹ 11 more variables: french_name <chr>, phylum <chr>,
#> #   family_french_name <chr>, group_id <dbl>, order_taxon <chr>,
#> #   taxon_group <chr>, sort_order <int>, class <chr>, family_name <chr>,
#> #   concept_source <chr>, family_english_name <chr>
length(chipmunk_ids$species_id)
#> [1] 28
```

So there are a fair few species! But probably not that many
observations. We can download all available observations of these
species, by passing the `species_id`s to the
[`nc_data_dl()`](https://birdscanada.github.io/naturecounts/reference/nc_data_dl.md)
function.

``` r
chipmunks <- nc_data_dl(
  species = chipmunk_ids$species_id,
  verbose = TRUE,
  username = "testuser",
  info = "nc_vignette"
)
#> Using filters: species (137600, 137610, 137620, 137630, 137640, 137650, 137660, 137670, 137680, 137690, 137700, 137710, 137720, 137730, 137740, 137750, 137760, 137790, 137800, 137810, 137820, 137830, 157601, 157602, 157603, 157604, 157605, 157606); fields_set (BMDE2.00-ext)
#> Collecting available records...
#>     collection nrecords
#> 1         OBFS     1630
#> 2  RCBIOTABASE        8
#> 3 WILDTRAX1070       58
#> 4 WILDTRAX1394        1
#> 5 WILDTRAX1651        4
#> 6 WILDTRAX1652        3
#> ...
#> Total records: 1,838
#> 
#> Downloading records for each collection:
#>   OBFS
#>     Records 1 to 1630 / 1630
#>   RCBIOTABASE
#>     Records 1 to 8 / 8
#>   WILDTRAX1070
#>     Records 1 to 58 / 58
#>   WILDTRAX1394
#>     Records 1 to 1 / 1
#>   WILDTRAX1651
#>     Records 1 to 4 / 4
#>   WILDTRAX1652
#>     Records 1 to 3 / 3
#>   WILDTRAX1725
#>     Records 1 to 15 / 15
#>   WILDTRAX1855
#>     Records 1 to 57 / 57
#>   WILDTRAX2821
#>     Records 1 to 12 / 12
#>   WILDTRAX3139
#>     Records 1 to 47 / 47
#>   WILDTRAX659
#>     Records 1 to 1 / 1
#>   WILDTRAX963
#>     Records 1 to 2 / 2
tibble(chipmunks)
#> # A tibble: 1,838 × 285
#>    record_id collection project_id protocol_id protocol_type species_id
#>        <int> <chr>           <int>       <int>         <int>      <int>
#>  1  34694257 OBFS             1020          NA            21     137820
#>  2  34694333 OBFS             1020          NA            21     137820
#>  3  34694373 OBFS             1020          NA            21     137820
#>  4  34694739 OBFS             1020          NA            21     137820
#>  5  34694755 OBFS             1020          NA            21     137820
#>  6  34694795 OBFS             1020          NA            21     137820
#>  7  34694817 OBFS             1020          NA            21     137820
#>  8  34695073 OBFS             1020          NA            21     137820
#>  9  34695137 OBFS             1020          NA            21     137820
#> 10  34695221 OBFS             1020          NA            21     137820
#> # ℹ 1,828 more rows
#> # ℹ 279 more variables: statprov_code <chr>, country_code <chr>,
#> #   SiteCode <chr>, latitude <dbl>, longitude <dbl>, bcr <int>,
#> #   subnational2_code <chr>, iba_site <chr>, utm_square <chr>,
#> #   survey_year <int>, survey_month <int>, survey_week <int>, survey_day <int>,
#> #   breeding_rank <lgl>, GlobalUniqueIdentifier <chr>, DateLastModified <chr>,
#> #   BasisOfRecord <chr>, InstitutionID <lgl>, InstitutionCode <chr>, …
```

Actually more than I expected (although not many compared to bird
species).
