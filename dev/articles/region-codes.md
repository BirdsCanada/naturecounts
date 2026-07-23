# Region Codes

``` r

library(naturecounts)
```

In the main data download functions for `naturecounts`,
[`nc_data_dl()`](https://birdscanada.github.io/naturecounts/dev/reference/nc_data_dl.md)
and
[`nc_count()`](https://birdscanada.github.io/naturecounts/dev/reference/nc_count.md),
you have the option of filtering data by `region`. In this article we
will explore the various ways of specifying regional filters.

For more details regarding spatial limits to regions, see the articles:

- [Mapping
  observations](https://birdscanada.github.io/naturecounts/articles/articles/mapping-observations.html)
- [IBAs and
  BCRs](https://birdscanada.github.io/naturecounts/dev/articles/region-areas.md)
- [Using spatial data to filter
  observations](https://birdscanada.github.io/naturecounts/dev/articles/region-spatial.md)

## In short

`region` must be a named list with **one** of the following:

- `country` - Country code (e.g., `CA` for Canada)
- `statprov` - State/province code (e.g., `MB` for Manitoba)
- `subnational2` - Subnational (type 2) code (e.g., `CA.MB.07` for the
  Brandon Area)
- `iba` - Important Bird Areas (IBA) code (e.g., `AB001` for Beaverhill
  Lake in Alberta)
- `bcr` - [Bird Conservation
  Regions](https://www.birdscanada.org/research/gislab/index.jsp?targetpg=bcr&targetpg=bcr)
  (e.g., `2` for Western Alaska)
- `utm_squares` - UTM square code (e.g., `10UFE96` for a grid in
  Alberta)
- `bbox` - Bounding box coordinates (e.g.,
  `c(left = -101.097223, bottom = 50.494717, right = -99.511239, top = 51.027557)`
  for a box containing Riding Mountain National Park in Manitoba)

To use the `region` argument:

``` r

nc_count(region = list(statprov = "NB"))
```

    ## Without a username, using 'show = "all"'

    ## Using filters: statprov (NB)

    ## # A tibble: 93 × 4
    ##    collection    akn_level access     nrecords
    ##    <chr>             <int> <chr>         <int>
    ##  1 ATOWLS                3 by request    16908
    ##  2 BBL-1960-1969         5 full          56578
    ##  3 BBL-1970-1979         5 full          68636
    ##  4 BBL-1980-1989         5 full          82650
    ##  5 BBL-1990-1999         5 full          78564
    ##  6 BBL-2000-2009         5 full          80805
    ##  7 BBL-2010-2019         5 full          57888
    ##  8 BBL-2020-2029         5 full          26199
    ##  9 BBS                   5 full          54578
    ## 10 BBS50-CAN             3 by request   247843
    ## # ℹ 83 more rows

You can only use **one** type of region, but you can filter to multiple
regions of that type:

``` r

nc_count(region = list(statprov = c("PE", "NB")))
```

    ## Without a username, using 'show = "all"'

    ## Using filters: statprov (PE, NB)

    ## # A tibble: 95 × 4
    ##    collection    akn_level access     nrecords
    ##    <chr>             <int> <chr>         <int>
    ##  1 ATOWLS                3 by request    23111
    ##  2 BBL-1960-1969         5 full          63292
    ##  3 BBL-1970-1979         5 full          85997
    ##  4 BBL-1980-1989         5 full          92092
    ##  5 BBL-1990-1999         5 full         100999
    ##  6 BBL-2000-2009         5 full          97156
    ##  7 BBL-2010-2019         5 full          67261
    ##  8 BBL-2020-2029         5 full          33123
    ##  9 BBS                   5 full          61249
    ## 10 BBS50-CAN             3 by request   285733
    ## # ℹ 85 more rows

## In Detail - Codes

Here we’ll go through in detail how to use the various codes. Note that
in some examples we also filter by species just so things don’t take so
long to download.

### Country

**Browse the code list**

``` r

meta_country_codes()
```

    ## # A tibble: 115 × 3
    ##    country_code country_name        country_name_fr   
    ##    <chr>        <chr>               <chr>             
    ##  1 AG           Antigua and Barbuda Antigua-et-Barbuda
    ##  2 AI           Anguilla            Anguilla          
    ##  3 AO           Angola              Angola            
    ##  4 AQ           Antarctica          Antarctique       
    ##  5 AR           Argentina           Argentine         
    ##  6 AS           American Samoa      Samoa américaines 
    ##  7 AU           Australia           Australie         
    ##  8 BB           Barbados            Barbade           
    ##  9 BE           Belgium             Belgique          
    ## 10 BJ           Benin               Bénin             
    ## # ℹ 105 more rows

**Search by name** (English or French)

``` r

search_region("États-Unis", type = "country")
```

    ## # A tibble: 3 × 3
    ##   country_code country_name                         country_name_fr                       
    ##   <chr>        <chr>                                <chr>                                 
    ## 1 UM           United States Minor Outlying Islands Îles mineures éloignées des États-Unis
    ## 2 US           United States                        États-Unis                            
    ## 3 VI           Virgin Uslands, U.S.                 Îles Vierges des États-Unis

**Use the resulting code(s)**

``` r

nc_count(species = 13210, region = list(country = "US"))
```

    ## Without a username, using 'show = "all"'

    ## Using filters: species (13210); country (US)

    ## # A tibble: 17 × 4
    ##    collection    akn_level access     nrecords
    ##    <chr>             <int> <chr>         <int>
    ##  1 ABBIRDRECS            5 full              1
    ##  2 BBL-1960-1969         5 full           1887
    ##  3 BBL-1970-1979         5 full           2492
    ##  4 BBL-1980-1989         5 full           4368
    ##  5 BBL-1990-1999         5 full           5892
    ##  6 BBL-2000-2009         5 full           5187
    ##  7 BBL-2010-2019         5 full           2978
    ##  8 BBL-2020-2029         5 full           1625
    ##  9 BBS                   5 full          23090
    ## 10 BBS50-US-EAST         3 by request    14234
    ## 11 BBS50-US-WEST         3 by request    19138
    ## 12 BCN                   5 full              8
    ## 13 CBC                   3 by request    30348
    ## 14 GBIF_50C9509D         5 full              1
    ## 15 NESTWATCH             3 by request        2
    ## 16 PFW-US-EAST           2 no access       497
    ## 17 PFW-US-WEST           2 no access       760

### State/Province

**Browse the code list**

``` r

meta_statprov_codes()
```

    ## # A tibble: 97 × 5
    ##    country_code statprov_code statprov_name_es          statprov_name             statprov_name_fr  
    ##    <chr>        <chr>         <chr>                     <chr>                     <chr>             
    ##  1 CA           AB            Alberta                   Alberta                   Alberta           
    ##  2 CA           BC            Columbia Británica        British Columbia          Colombie-Britanni…
    ##  3 CA           MB            Manitoba                  Manitoba                  Manitoba          
    ##  4 CA           NB            Nuevo Brunswick           New Brunswick             Nouveau-Brunswick 
    ##  5 CA           NL            Terranova y Labrador      Newfoundland and Labrador Terre-Neuve-et-La…
    ##  6 CA           NS            Nueva Escocia             Nova Scotia               Nouvelle-Écosse   
    ##  7 CA           NT            Territorios del Noroeste  Northwest Territories     Territoires-du-No…
    ##  8 CA           NU            Nunavut                   Nunavut                   Nunavut           
    ##  9 CA           ON            Ontario                   Ontario                   Ontario           
    ## 10 CA           PE            Isla del Príncipe Eduardo Prince Edward Island      Île-du-Prince-Édo…
    ## # ℹ 87 more rows

**Search by name** (English, French, or Spanish)

``` r

search_region("Distrito de Colombia", type = "statprov")
```

    ## # A tibble: 1 × 5
    ##   country_code statprov_code statprov_name_es     statprov_name        statprov_name_fr    
    ##   <chr>        <chr>         <chr>                <chr>                <chr>               
    ## 1 US           DC            Distrito de Colombia District of Columbia District de Columbia

**Use the resulting code(s)**

``` r

nc_count(region = list(statprov = "DC"))
```

    ## Without a username, using 'show = "all"'

    ## Using filters: statprov (DC)

    ## # A tibble: 11 × 4
    ##    collection    akn_level access     nrecords
    ##    <chr>             <int> <chr>         <int>
    ##  1 BBL-1960-1969         5 full          19972
    ##  2 BBL-1970-1979         5 full           1968
    ##  3 BBL-1980-1989         5 full            190
    ##  4 BBL-1990-1999         5 full            232
    ##  5 BBL-2000-2009         5 full           7061
    ##  6 BBL-2010-2019         5 full           5295
    ##  7 BBL-2020-2029         5 full            480
    ##  8 CBC                   3 by request     8600
    ##  9 EBUTTERFLY            5 full             23
    ## 10 MONARCHWATCH          5 full             30
    ## 11 PFW-US-EAST           2 no access     23323

### Subnational regions (type 2)

**Browse the code list**

``` r

meta_subnational2_codes()
```

    ## # A tibble: 5,309 × 5
    ##    country_code statprov_code subnational2_code subnational2_name                      ebird_code
    ##    <chr>        <chr>         <chr>             <chr>                                  <chr>     
    ##  1 CA           AB            CA.AB.01          Division No. 1 - Medicine Hat          CA-AB-ON  
    ##  2 CA           AB            CA.AB.02          Division No. 2 - Lethbridge            CA-AB-TW  
    ##  3 CA           AB            CA.AB.03          Division No. 3 - Fort Macleod          CA-AB-TR  
    ##  4 CA           AB            CA.AB.04          Division No. 4 - Hanna                 CA-AB-FO  
    ##  5 CA           AB            CA.AB.05          Division No. 5 - Drumheller            CA-AB-FI  
    ##  6 CA           AB            CA.AB.06          Division No. 6 - Calgary               CA-AB-SI  
    ##  7 CA           AB            CA.AB.07          Division No. 7 - Stettler              CA-AB-SE  
    ##  8 CA           AB            CA.AB.08          Division No. 8 - Red Deer              CA-AB-EI  
    ##  9 CA           AB            CA.AB.09          Division No. 9 - Rocky Mountain House  CA-AB-NI  
    ## 10 CA           AB            CA.AB.10          Division No. 10 - Camrose-Lloydminster CA-AB-TE  
    ## # ℹ 5,299 more rows

**Search by name**  
Language depends on location: - Mexico = Spanish - USA = English -
Quebec = French - Rest of Canada = English

``` r

search_region("Montreal", type = "subnational2")
```

    ## # A tibble: 1 × 5
    ##   country_code statprov_code subnational2_code subnational2_name              ebird_code
    ##   <chr>        <chr>         <chr>             <chr>                          <chr>     
    ## 1 CA           QC            CA.QC.MR          Communauté-Urbaine-de-Montréal CA-QC-MR

**Use the resulting code(s)**

``` r

nc_count(species = 7450, region = list(subnational2 = "CA.QC.MR"))
```

    ## Without a username, using 'show = "all"'

    ## Using filters: species (7450); subnational2 (CA.QC.MR)

    ## # A tibble: 8 × 4
    ##   collection    akn_level access     nrecords
    ##   <chr>             <int> <chr>         <int>
    ## 1 BBL-1980-1989         5 full              5
    ## 2 BBL-2010-2019         5 full             11
    ## 3 BBL-2020-2029         5 full              2
    ## 4 CBC                   3 by request       70
    ## 5 EBIRD-CA-QC           3 by request     1904
    ## 6 GBBC                  3 by request        1
    ## 7 GBIF_50C9509D         5 full             22
    ## 8 GBIF_CE9D17F0         5 full              3

### Important Bird Areas (IBA)

- These are Canadian designations

**Browse the code list**

``` r

meta_iba_codes()
```

    ## # A tibble: 599 × 13
    ##    iba_site iba_name_fr     latitude statprov area_ha alt_min iba_name nearest_town bcr   ncc_region
    ##    <chr>    <chr>              <dbl> <chr>      <dbl>   <dbl> <chr>    <chr>        <chr> <chr>     
    ##  1 AB001    Beaverhill Lake     53.5 AB         208.      668 Beaverh… Tofield      11    66        
    ##  2 AB002    Peace-Athabasc…     58.7 AB        7585.      250 Peace-A… Fort Chipew… 06    00        
    ##  3 AB003    Lesser Slave L…     55.4 AB        2019.      570 Lesser … Slave Lake   06    00        
    ##  4 AB004    Milk River Can…     49.1 AB         335.      834 Milk Ri… Milk River   11    26        
    ##  5 AB006    Lakeland            54.7 AB         741.      500 Lakeland Lac la Biche 06    00        
    ##  6 AB007    Suffield            50.5 AB         461.      610 Suffield Suffield     11    26        
    ##  7 AB011    Réservoir St. …     49.3 AB          62.9    1100 St. Mar… Cardston     10    67        
    ##  8 AB015    Lake Newell an…     50.4 AB         115.      765 Lake Ne… Brooks       11    26        
    ##  9 AB016    McGregor Lake …     50.3 AB         251.      850 McGrego… Vulcan       11    67        
    ## 10 AB022    Little Fish La…     51.4 AB          35.7     894 Little … High River   11    26        
    ## # ℹ 589 more rows
    ## # ℹ 3 more variables: alt_max <dbl>, longitude <dbl>, status <chr>

**Search by name** (English or French)

``` r

search_region("oak hammock", type = "iba")
```

    ## # A tibble: 1 × 13
    ##   iba_site iba_name_fr      latitude statprov area_ha alt_min iba_name nearest_town bcr   ncc_region
    ##   <chr>    <chr>               <dbl> <chr>      <dbl>   <dbl> <chr>    <chr>        <chr> <chr>     
    ## 1 MB010    Oak Hammock Mar…     50.2 MB          67.6     233 Oak Ham… Winnipeg     11    35        
    ## # ℹ 3 more variables: alt_max <dbl>, longitude <dbl>, status <chr>

**Use the resulting code(s)**

``` r

nc_count(region = list(iba = "MB010"))
```

    ## Without a username, using 'show = "all"'

    ## Using filters: iba (IBA.MB010)

    ## # A tibble: 18 × 4
    ##    collection      akn_level access     nrecords
    ##    <chr>               <int> <chr>         <int>
    ##  1 BBL-2000-2009           5 full           1453
    ##  2 BBL-2010-2019           5 full          16698
    ##  3 BBL-2020-2029           5 full          12094
    ##  4 CMMN-DET-OHM            3 by request    21828
    ##  5 EBIRD-CA-PR             3 by request   255680
    ##  6 EBIRD-CA-SENS           3 by request       76
    ##  7 EBUTTERFLY              5 full              3
    ##  8 GBIF_040C5662           5 full              1
    ##  9 GBIF_50C9509D           5 full           1503
    ## 10 GBIF_6AC3F774           5 full             43
    ## 11 GBIF_848586A4           5 full              2
    ## 12 GBIF_B1047888           5 full              5
    ## 13 MBATLAS1BE_DO           5 full            309
    ## 14 MBATLAS1BE_RAW          5 full           1004
    ## 15 MBATLAS1BE_SUMM         5 full            208
    ## 16 MBATLAS1PC              5 full            381
    ## 17 MBATLAS1RC              3 by request       23
    ## 18 NESTWATCH               3 by request        2

### Bird Conservation Regions (BCR)

**Browse the code list**

``` r

meta_bcr_codes()
```

    ## # A tibble: 67 × 4
    ##      bcr bcr_name                       bcr_name_es                          bcr_name_fr            
    ##    <int> <chr>                          <chr>                                <chr>                  
    ##  1     1 Aleutian/Bering Sea Islands    Aleutianas/Islas del Mar de Bering   Îles Aléoutiennes/mer …
    ##  2     2 Western Alaska                 Alaska Occidental                    Alaska occidental      
    ##  3     3 Arctic Plains And Mountains    Planicies Árticas y Montañas         Plaines et montagnes d…
    ##  4     4 Northwestern Interior Forest   Bosque Interior del Noroeste         Forêts intérieures du …
    ##  5     5 Northern Pacific Rainforest    Bosque Lluvioso del Pacífico Norte   Forêts pluviales du no…
    ##  6     6 Boreal Taiga Plains            Planicies de la Taiga Boreal         Taïga des plaines boré…
    ##  7     7 Taiga Shield And Hudson Plains Placa de Taiga y Planicies de Hudson Taïga du Bouclier et p…
    ##  8     8 Boreal Softwood Shield         Placa Boreal de Softwood             Forêts de résineux du …
    ##  9     9 Great Basin                    Gran Cuenca                          Grand Bassin           
    ## 10    10 Northern Rockies               Rocallosas del Norte                 Nord des Rocheuses     
    ## # ℹ 57 more rows

**Search by name** (English, French, or Spanish)

``` r

search_region("rainforest", type = "bcr")
```

    ## # A tibble: 1 × 4
    ##     bcr bcr_name                    bcr_name_es                        bcr_name_fr                  
    ##   <int> <chr>                       <chr>                              <chr>                        
    ## 1     5 Northern Pacific Rainforest Bosque Lluvioso del Pacífico Norte Forêts pluviales du nord de …

**Use the resulting code(s)**

``` r

nc_count(species = 7450, region = list(bcr = 5))
```

    ## Without a username, using 'show = "all"'

    ## Using filters: species (7450); bcr (BCR.5)

    ## # A tibble: 23 × 4
    ##    collection    akn_level access     nrecords
    ##    <chr>             <int> <chr>         <int>
    ##  1 BBL-1960-1969         5 full             10
    ##  2 BBL-1970-1979         5 full             64
    ##  3 BBL-1980-1989         5 full              4
    ##  4 BBL-1990-1999         5 full              8
    ##  5 BBL-2000-2009         5 full              8
    ##  6 BBL-2010-2019         5 full             10
    ##  7 BBL-2020-2029         5 full              3
    ##  8 BCCWS                 3 by request       19
    ##  9 BCOWLS                3 by request        1
    ## 10 CBC                   3 by request      251
    ## # ℹ 13 more rows

### UTM Squares

**Browse the code list**

``` r

meta_utm_squares()
```

    ## Simple feature collection with 219966 features and 6 fields
    ## Geometry type: POLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: -890522.4 ymin: -1404292 xmax: 9028263 ymax: 5252312
    ## Projected CRS: NAD83 / Statistics Canada Lambert
    ## # A tibble: 219,966 × 7
    ##    utm_square admin_region                                 geometry statprov_code ebird_checklist_id
    ##  * <chr>             <int>                            <POLYGON [m]> <chr>         <chr>             
    ##  1 10UFE96               0 ((4425480 2354190, 4432265 2350614, 443… AB            CL27631           
    ##  2 10UFE97               0 ((4430066 2362898, 4436481 2359516, 444… AB            CL27631           
    ##  3 10UFE98               0 ((4434651 2371603, 4440696 2368416, 444… AB            CL27637           
    ##  4 10UFE99               0 ((4439235 2380305, 4444909 2377313, 444… AB            CL27637           
    ##  5 10UFF90               0 ((4443819 2389006, 4449122 2386208, 445… AB            CL27637           
    ##  6 10UFF92               0 ((4452983 2406400, 4457545 2403993, 446… AB            CL27637           
    ##  7 10UFF93               0 ((4457565 2415094, 4461755 2412882, 446… AB            CL27637           
    ##  8 10UFF94               0 ((4462145 2423785, 4465964 2421769, 447… AB            CL27637           
    ##  9 10UFF95               0 ((4466725 2432475, 4470172 2430655, 447… AB            CL27637           
    ## 10 10UFF96               0 ((4471304 2441162, 4474379 2439538, 447… AB            CL27637           
    ## # ℹ 219,956 more rows
    ## # ℹ 2 more variables: longitude_centroid <dbl>, latitude_centroid <dbl>

**Use the resulting code(s)**

``` r

nc_count(species = 7450, region = list(utm_squares = "18TUR35"))
```

    ## Without a username, using 'show = "all"'

    ## Using filters: species (7450); utm_squares (18TUR35)

    ## # A tibble: 2 × 4
    ##   collection    akn_level access     nrecords
    ##   <chr>             <int> <chr>         <int>
    ## 1 EBIRD-CA-ON           3 by request        7
    ## 2 ONATLAS3BE_DO         3 by request        1

### Bounding Box

Bounding box reflects latitude and longitude limits.

**Use a bounding box**

``` r

nc_count(
  species = 7450,
  region = list(bbox = c(left = -125, bottom = 45, right = -100, top = 50))
)
```

    ## Without a username, using 'show = "all"'

    ## Using filters: species (7450); bbox_left (-125); bbox_bottom (45); bbox_right (-100); bbox_top (50)

    ## # A tibble: 30 × 4
    ##    collection    akn_level access nrecords
    ##    <chr>             <int> <chr>     <int>
    ##  1 ABATLAS1              5 full          6
    ##  2 ABATLAS2              5 full         13
    ##  3 ABBIRDRECS            5 full          2
    ##  4 BBL-1960-1969         5 full         12
    ##  5 BBL-1970-1979         5 full         66
    ##  6 BBL-1980-1989         5 full          6
    ##  7 BBL-1990-1999         5 full         11
    ##  8 BBL-2000-2009         5 full         12
    ##  9 BBL-2010-2019         5 full         19
    ## 10 BBL-2020-2029         5 full          7
    ## # ℹ 20 more rows
