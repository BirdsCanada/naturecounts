# Search for bird species id codes by alphanumeric codes

This is an advanced function for returning all Bird-related species id
codes based on the various alphanumeric codes used by different
authorities.

## Usage

``` r
search_species_code(code = NULL, authority = "BSCDATA", results = "all")
```

## Arguments

- code:

  Vector. Character or numeric code indicating a species for a given
  authority.

- authority:

  Character. The authority to compare codes against (defaults to
  "BSCDATA")

- results:

  Character. "all" returns codes for all related species (including
  subspecies and main species). "exact" returns only the code for exact
  species indicated by the code.

## Value

A data frame of numeric species id codes and names

## Details

`species_code_search()` is deprecated in favour of
`search_species_code()`

Species ids returned reflect both species and sub-species levels.

## Examples

``` r
# Show all ids
search_species_code()
#> # A tibble: 1,370 × 5
#>    species_id BSCDATA scientific_name            english_name        french_name
#>         <int> <chr>   <chr>                      <chr>               <chr>      
#>  1         80 BBWD    Dendrocygna autumnalis     Black-bellied Whis… Dendrocygn…
#>  2         90 WIWD    Dendrocygna arborea        West Indian Whistl… Dendrocygn…
#>  3        100 FUWD    Dendrocygna bicolor        Fulvous Whistling-… Dendrocygn…
#>  4        110 BEGO    Anser fabalis/serrirostris Taiga/Tundra Bean-… Oie des mo…
#>  5        111 TABG    Anser fabalis              Taiga Bean-Goose    Oie des mo…
#>  6        112 TUBG    Anser serrirostris         Tundra Bean-Goose   Oie de la …
#>  7        120 PFGO    Anser brachyrhynchus       Pink-footed Goose   Oie à bec …
#>  8        130 GWFG    Anser albifrons            Greater White-fron… Oie rieuse 
#>  9        140 LWFG    Anser erythropus           Lesser White-front… Oie naine  
#> 10        141 GRGO    Anser anser                Graylag Goose       Oie cendrée
#> # ℹ 1,360 more rows

# Get all species ids for house finches
search_species_code("HOFI")
#> # A tibble: 1 × 5
#>   species_id BSCDATA scientific_name      english_name french_name     
#>        <int> <chr>   <chr>                <chr>        <chr>           
#> 1      20350 HOFI    Haemorhous mexicanus House Finch  Roselin familier

# Get all species ids for Dark-eyed Juncos
search_species_code("DEJU")
#> # A tibble: 4 × 5
#>   species_id BSCDATA scientific_name                    english_name french_name
#>        <int> <chr>   <chr>                              <chr>        <chr>      
#> 1      19090 SCJU    Junco hyemalis hyemalis/carolinen… Dark-eyed J… Junco ardo…
#> 2      19110 PSJU    Junco hyemalis mearnsi             Dark-eyed J… Junco ardo…
#> 3      42218 DEJU    Junco hyemalis                     Dark-eyed J… Junco ardo…
#> 4      47928 ORJU    Junco hyemalis [oreganus Group]    Dark-eyed J… Junco ardo…

# Get all species ids related to Yellow-rumped Warbler (Myrtle)
# NOTE! This includes Audubon's and the main, Yellow-rumped Warbler species
search_species_code("MYWA")
#> # A tibble: 3 × 5
#>   species_id BSCDATA scientific_name             english_name        french_name
#>        <int> <chr>   <chr>                       <chr>               <chr>      
#> 1      16610 YRWA    Setophaga coronata          Yellow-rumped Warb… Paruline à…
#> 2      16620 MYWA    Setophaga coronata coronata Yellow-rumped Warb… Paruline à…
#> 3      16630 AUWA    Setophaga coronata auduboni Yellow-rumped Warb… Paruline à…

# Get ONLY specific id related to Yellow-rumped Warbler (Myrtle)
search_species_code("MYWA", results = "exact")
#> # A tibble: 1 × 5
#>   species_id BSCDATA scientific_name             english_name        french_name
#>        <int> <chr>   <chr>                       <chr>               <chr>      
#> 1      16620 MYWA    Setophaga coronata coronata Yellow-rumped Warb… Paruline à…

# Use the Christmas Bird Count authority
search_species_code(11609, authority = "CBC")
#> # A tibble: 6 × 5
#>   species_id CBC   scientific_name                      english_name french_name
#>        <int> <chr> <chr>                                <chr>        <chr>      
#> 1      16610 11609 Setophaga coronata                   Yellow-rump… Paruline à…
#> 2      16620 11610 Setophaga coronata coronata          Yellow-rump… Paruline à…
#> 3      16630 11611 Setophaga coronata auduboni          Yellow-rump… Paruline à…
#> 4      40770 11612 Setophaga coronata coronata x audub… Yellow-rump… Paruline à…
#> 5      40771 11613 Setophaga coronata nigrifrons        Yellow-rump… Paruline à…
#> 6      40772 11614 Setophaga coronata goldmani          Yellow-rump… Paruline à…

# Look in more than one authority (note that the code only needs to match on
# of the authorities)
search_species_code("MYWA", authority = c("BCMA", "CBC"))
#> # A tibble: 6 × 6
#>   species_id BCMA  CBC   scientific_name                english_name french_name
#>        <int> <chr> <chr> <chr>                          <chr>        <chr>      
#> 1      16630 AUWA  11611 Setophaga coronata auduboni    Yellow-rump… Paruline à…
#> 2      16620 MYWA  11610 Setophaga coronata coronata    Yellow-rump… Paruline à…
#> 3      16610 YRWA  11609 Setophaga coronata             Yellow-rump… Paruline à…
#> 4      40770 NA    11612 Setophaga coronata coronata x… Yellow-rump… Paruline à…
#> 5      40771 NA    11613 Setophaga coronata nigrifrons  Yellow-rump… Paruline à…
#> 6      40772 NA    11614 Setophaga coronata goldmani    Yellow-rump… Paruline à…
```
