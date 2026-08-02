# Find country, state/province, subnational2, IBA, or BCR codes

Search for the correct codes to identify countries, states/provinces,
subnational2 areas, Important Bird Areas (IBA), or Bird Conservation
Regions (BCR). These are then used in the
[`nc_data_dl()`](https://birdscanada.github.io/naturecounts/dev/reference/nc_data_dl.md)
and
[`nc_count()`](https://birdscanada.github.io/naturecounts/dev/reference/nc_count.md)
functions.

## Usage

``` r
search_region(name = NULL, type = "country")
```

## Arguments

- name:

  Character. The location name to search for

- type:

  Character. One of "country", "statprov", "subnational2", "iba", or
  "bcr". The type of information to return.

## Value

A data frame with the relevant codes and other information

## Details

`region_search()` is deprecated in favour of `search_region()`

## Examples

``` r

search_region("Mexico", type = "country")   # MX
#> # A tibble: 1 × 3
#>   country_code country_name country_name_fr
#>   <chr>        <chr>        <chr>          
#> 1 MX           Mexico       Mexique        

search_region("Yucatan", type = "statprov") # Yucatán
#> # A tibble: 1 × 5
#>   country_code statprov_code statprov_name_es statprov_name statprov_name_fr
#>   <chr>        <chr>         <chr>            <chr>         <chr>           
#> 1 MX           YP            Yucatán          Yucatán       Yucatán         
search_region("Alberta", type = "statprov") # AB
#> # A tibble: 1 × 5
#>   country_code statprov_code statprov_name_es statprov_name statprov_name_fr
#>   <chr>        <chr>         <chr>            <chr>         <chr>           
#> 1 CA           AB            Alberta          Alberta       Alberta         

search_region("Edmonton", type = "subnational2") # CA.AB.11
#> # A tibble: 1 × 5
#>   country_code statprov_code subnational2_code subnational2_name      ebird_code
#>   <chr>        <chr>         <chr>             <chr>                  <chr>     
#> 1 CA           AB            CA.AB.11          Division No. 11 - Edm… CA-AB-EL  
search_region("Brandon", type = "subnational2")  # CA.MB.07
#> # A tibble: 1 × 5
#>   country_code statprov_code subnational2_code subnational2_name      ebird_code
#>   <chr>        <chr>         <chr>             <chr>                  <chr>     
#> 1 CA           MB            CA.MB.07          Division No. 7 - Bran… CA-MB-SE  

search_region("hays reservoir", type = "iba") # AB075
#> # A tibble: 1 × 13
#>   iba_site iba_name_fr   latitude statprov area_ha alt_min iba_name nearest_town
#>   <chr>    <chr>            <dbl> <chr>      <dbl>   <dbl> <chr>    <chr>       
#> 1 AB075    Hays Reservo…     50.1 AB          8.99     785 Hays Re… Hays        
#> # ℹ 5 more variables: bcr <chr>, ncc_region <chr>, alt_max <dbl>,
#> #   longitude <dbl>, status <chr>
search_region("rainforest", type = "bcr")     # 5
#> # A tibble: 1 × 4
#>     bcr bcr_name                    bcr_name_es                      bcr_name_fr
#>   <int> <chr>                       <chr>                            <chr>      
#> 1     5 Northern Pacific Rainforest Bosque Lluvioso del Pacífico No… Forêts plu…


# Show all codes
search_region(type = "country")
#> # A tibble: 115 × 3
#>    country_code country_name        country_name_fr   
#>    <chr>        <chr>               <chr>             
#>  1 AG           Antigua and Barbuda Antigua-et-Barbuda
#>  2 AI           Anguilla            Anguilla          
#>  3 AO           Angola              Angola            
#>  4 AQ           Antarctica          Antarctique       
#>  5 AR           Argentina           Argentine         
#>  6 AS           American Samoa      Samoa américaines 
#>  7 AU           Australia           Australie         
#>  8 BB           Barbados            Barbade           
#>  9 BE           Belgium             Belgique          
#> 10 BJ           Benin               Bénin             
#> # ℹ 105 more rows
search_region(type = "statprov")
#> # A tibble: 97 × 5
#>    country_code statprov_code statprov_name_es    statprov_name statprov_name_fr
#>    <chr>        <chr>         <chr>               <chr>         <chr>           
#>  1 CA           AB            Alberta             Alberta       Alberta         
#>  2 CA           BC            Columbia Británica  British Colu… Colombie-Britan…
#>  3 CA           MB            Manitoba            Manitoba      Manitoba        
#>  4 CA           NB            Nuevo Brunswick     New Brunswick Nouveau-Brunswi…
#>  5 CA           NL            Terranova y Labrad… Newfoundland… Terre-Neuve-et-…
#>  6 CA           NS            Nueva Escocia       Nova Scotia   Nouvelle-Écosse 
#>  7 CA           NT            Territorios del No… Northwest Te… Territoires-du-…
#>  8 CA           NU            Nunavut             Nunavut       Nunavut         
#>  9 CA           ON            Ontario             Ontario       Ontario         
#> 10 CA           PE            Isla del Príncipe … Prince Edwar… Île-du-Prince-É…
#> # ℹ 87 more rows
search_region(type = "subnational2")
#> # A tibble: 5,309 × 5
#>    country_code statprov_code subnational2_code subnational2_name     ebird_code
#>    <chr>        <chr>         <chr>             <chr>                 <chr>     
#>  1 CA           AB            CA.AB.01          Division No. 1 - Med… CA-AB-ON  
#>  2 CA           AB            CA.AB.02          Division No. 2 - Let… CA-AB-TW  
#>  3 CA           AB            CA.AB.03          Division No. 3 - For… CA-AB-TR  
#>  4 CA           AB            CA.AB.04          Division No. 4 - Han… CA-AB-FO  
#>  5 CA           AB            CA.AB.05          Division No. 5 - Dru… CA-AB-FI  
#>  6 CA           AB            CA.AB.06          Division No. 6 - Cal… CA-AB-SI  
#>  7 CA           AB            CA.AB.07          Division No. 7 - Ste… CA-AB-SE  
#>  8 CA           AB            CA.AB.08          Division No. 8 - Red… CA-AB-EI  
#>  9 CA           AB            CA.AB.09          Division No. 9 - Roc… CA-AB-NI  
#> 10 CA           AB            CA.AB.10          Division No. 10 - Ca… CA-AB-TE  
#> # ℹ 5,299 more rows
search_region(type = "iba")
#> # A tibble: 599 × 13
#>    iba_site iba_name_fr  latitude statprov area_ha alt_min iba_name nearest_town
#>    <chr>    <chr>           <dbl> <chr>      <dbl>   <dbl> <chr>    <chr>       
#>  1 AB001    Beaverhill …     53.5 AB         208.      668 Beaverh… Tofield     
#>  2 AB002    Peace-Athab…     58.7 AB        7585.      250 Peace-A… Fort Chipew…
#>  3 AB003    Lesser Slav…     55.4 AB        2019.      570 Lesser … Slave Lake  
#>  4 AB004    Milk River …     49.1 AB         335.      834 Milk Ri… Milk River  
#>  5 AB006    Lakeland         54.7 AB         741.      500 Lakeland Lac la Biche
#>  6 AB007    Suffield         50.5 AB         461.      610 Suffield Suffield    
#>  7 AB011    Réservoir S…     49.3 AB          62.9    1100 St. Mar… Cardston    
#>  8 AB015    Lake Newell…     50.4 AB         115.      765 Lake Ne… Brooks      
#>  9 AB016    McGregor La…     50.3 AB         251.      850 McGrego… Vulcan      
#> 10 AB022    Little Fish…     51.4 AB          35.7     894 Little … High River  
#> # ℹ 589 more rows
#> # ℹ 5 more variables: bcr <chr>, ncc_region <chr>, alt_max <dbl>,
#> #   longitude <dbl>, status <chr>
search_region(type = "bcr")
#> # A tibble: 67 × 4
#>      bcr bcr_name                       bcr_name_es                  bcr_name_fr
#>    <int> <chr>                          <chr>                        <chr>      
#>  1     1 Aleutian/Bering Sea Islands    Aleutianas/Islas del Mar de… Îles Aléou…
#>  2     2 Western Alaska                 Alaska Occidental            Alaska occ…
#>  3     3 Arctic Plains And Mountains    Planicies Árticas y Montañas Plaines et…
#>  4     4 Northwestern Interior Forest   Bosque Interior del Noroeste Forêts int…
#>  5     5 Northern Pacific Rainforest    Bosque Lluvioso del Pacífic… Forêts plu…
#>  6     6 Boreal Taiga Plains            Planicies de la Taiga Boreal Taïga des …
#>  7     7 Taiga Shield And Hudson Plains Placa de Taiga y Planicies … Taïga du B…
#>  8     8 Boreal Softwood Shield         Placa Boreal de Softwood     Forêts de …
#>  9     9 Great Basin                    Gran Cuenca                  Grand Bass…
#> 10    10 Northern Rockies               Rocallosas del Norte         Nord des R…
#> # ℹ 57 more rows

# \donttest{
# Using the codes
nc_count(region = list(statprov = "AB"), years = 2010)
#> Without a username, using 'show = "all"'
#> Using filters: start_year (2010); end_year (2010); statprov (AB)
#> The server did not respond within 120s. Trying again...
#> # A tibble: 20 × 4
#>    collection     akn_level access     nrecords
#>    <chr>              <int> <chr>         <int>
#>  1 ABOWLS                 3 by request     1139
#>  2 BBL-2010-2019          5 full          36263
#>  3 BBS                    5 full           9550
#>  4 BBS50-CAN              3 by request    31774
#>  5 CBC                    3 by request     1155
#>  6 CMMN-DET-BBO           3 by request     1654
#>  7 CMMN-DET-IBS           3 by request      846
#>  8 CMMN-DET-LSLBO         3 by request     5332
#>  9 EBIRD-CA-PR            3 by request    79131
#> 10 EBIRD-CA-SENS          3 by request      107
#> 11 EBUTTERFLY             5 full              2
#> 12 GBBC                   3 by request     2477
#> 13 GBIF_3E7837D0          5 full              1
#> 14 GBIF_50C9509D          5 full            198
#> 15 GBIF_61BF28A5          5 full             94
#> 16 GBIF_8A863029          5 full            151
#> 17 GBIF_D99823CA          5 full              1
#> 18 NESTWATCH              3 by request        3
#> 19 OISEAUXQC              2 no access         2
#> 20 PFW                    5 full           7872
# }
```
