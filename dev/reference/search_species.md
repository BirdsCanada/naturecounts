# Find species codes

Find species id codes by searching for scientific, English and French
species names.

## Usage

``` r
search_species(name = NULL, show = "names", authority = NULL)
```

## Arguments

- name:

  Character. The species name to search for

- show:

  Character. Either "all" or "names" (default). Whether to return all
  taxonomic information or only a subset with species names

- authority:

  Character. If not NULL (default), return the alphanumeric code
  associated with avian species for this taxonomic authority.

## Value

Data frame of species ids and taxonomic information

## Details

`species_search()` is deprecated in favour of `search_species()`

## Examples

``` r

# Show all ids
search_species()
#> # A tibble: 41,479 × 5
#>    species_id scientific_name          english_name      french_name taxon_group
#>         <int> <chr>                    <chr>             <chr>       <chr>      
#>  1          0 N/A                      No observations   Aucune obs… BIRDS      
#>  2         10 Tinamus major            Great Tinamou     Grand Tina… BIRDS      
#>  3         20 Nothocercus bonapartei   Highland Tinamou  Tinamou de… BIRDS      
#>  4         30 Crypturellus soui        Little Tinamou    Tinamou so… BIRDS      
#>  5         40 Crypturellus cinnamomeus Thicket Tinamou   Tinamou ca… BIRDS      
#>  6         50 Crypturellus boucardi    Slaty-breasted T… Tinamou de… BIRDS      
#>  7         60 Crypturellus kerriae     Choco Tinamou     Tinamou de… BIRDS      
#>  8         70 Dendrocygna viduata      White-faced Whis… Dendrocygn… BIRDS      
#>  9         80 Dendrocygna autumnalis   Black-bellied Wh… Dendrocygn… BIRDS      
#> 10         90 Dendrocygna arborea      West Indian Whis… Dendrocygn… BIRDS      
#> # ℹ 41,469 more rows

search_species("chickadee")
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
search_species("black-capped chickadee")
#> # A tibble: 4 × 5
#>   species_id scientific_name                english_name french_name taxon_group
#>        <int> <chr>                          <chr>        <chr>       <chr>      
#> 1      14280 Poecile atricapillus           Black-cappe… Mésange à … BIRDS      
#> 2      40668 Poecile carolinensis x atrica… Carolina x … Hybride Mé… BIRDS      
#> 3      40669 Poecile carolinensis/atricapi… Carolina/Bl… Mésange de… BIRDS      
#> 4      44466 Poecile atricapillus x Baeolo… Black-cappe… Hybride Mé… BIRDS      

# Add alphanumeric code for BSCDATA authority
search_species("black-capped chickadee", authority = "BSCDATA")
#> # A tibble: 4 × 6
#>   species_id scientific_name        english_name french_name taxon_group BSCDATA
#>        <int> <chr>                  <chr>        <chr>       <chr>       <chr>  
#> 1      14280 Poecile atricapillus   Black-cappe… Mésange à … BIRDS       BCCH   
#> 2      40668 Poecile carolinensis … Carolina x … Hybride Mé… BIRDS       NA     
#> 3      40669 Poecile carolinensis/… Carolina/Bl… Mésange de… BIRDS       NA     
#> 4      44466 Poecile atricapillus … Black-cappe… Hybride Mé… BIRDS       NA     

# Show all taxonomic information
search_species("black-capped chickadee", show = "all")
#> # A tibble: 4 × 17
#>   species_id concept_id     scientific_name species_status added_dt english_name
#>        <int> <chr>          <chr>                    <int> <chr>    <chr>       
#> 1      14280 avibase-B1F0C… Poecile atrica…              1 NA       Black-cappe…
#> 2      40668 avibase-1B9B3… Poecile caroli…              4 NA       Carolina x …
#> 3      40669 avibase-F648A… Poecile caroli…              2 NA       Carolina/Bl…
#> 4      44466 avibase-6D7E1… Poecile atrica…              4 NA       Black-cappe…
#> # ℹ 11 more variables: french_name <chr>, phylum <chr>,
#> #   family_french_name <chr>, group_id <dbl>, order_taxon <chr>,
#> #   taxon_group <chr>, sort_order <int>, class <chr>, family_name <chr>,
#> #   concept_source <chr>, family_english_name <chr>

# Using the codes
nc_count(species = 14280)
#> Without a username, using 'show = "all"'
#> Using filters: species (14280)
#> The server did not respond within 120s. Trying again...
#> # A tibble: 338 × 4
#>    collection    akn_level access nrecords
#>    <chr>             <int> <chr>     <int>
#>  1 ABATLAS1              5 full       1314
#>  2 ABATLAS2              5 full       3772
#>  3 ABBIRDRECS            5 full       2931
#>  4 ACCWS                 5 full         17
#>  5 BBL-1960-1969         5 full     108205
#>  6 BBL-1970-1979         5 full     114970
#>  7 BBL-1980-1989         5 full     158692
#>  8 BBL-1990-1999         5 full     149381
#>  9 BBL-2000-2009         5 full     156289
#> 10 BBL-2010-2019         5 full     138297
#> # ℹ 328 more rows
```
