# Selecting columns and fields to download

``` r
library(naturecounts)
library(dplyr)
```

In `naturecounts`, by default default data is downloaded with a specific
set of fields/columns. However, for more advanced applications, users
may wish to specify which fields/columns to return. The Bird Monitoring
Data Exchange (BMDE) schema keeps track of variables used to augment
observation data. There are different versions reflecting different
collections of variables.

> The following examples use the “testuser” user which is not available
> to you. You can quickly [sign up for a free
> account](https://naturecounts.ca/nc/default/register.jsp) of your own
> to access and play around with these examples. Simply replace
> `testuser` with your own username.

## BMDE versions

There are several ways to get more information about these different
field/columns sets.

1.  You can get a list of the different versions with the
    [`meta_bmde_versions()`](https://birdscanada.github.io/naturecounts/reference/meta.md)
    function.

``` r
meta_bmde_versions()
```

    ## # A tibble: 8 × 3
    ##   descr                                           shorthand version            
    ##   <chr>                                           <chr>     <chr>              
    ## 1 BMDE 1.38                                       NA        BMDE1.38           
    ## 2 BMDE 2.0                                        core      BMDE2.00           
    ## 3 BMDE 2.0 (extended list)                        extended  BMDE2.00-ext       
    ## 4 BMDE 2.0 (minimum)                              minimum   BMDE2.00-min       
    ## 5 Banding extension to BMDE 2.0                   NA        BMDE-BAND-2.00     
    ## 6 Monarch Knowledge Network extension to BMDE 2.0 NA        BMDE-MKN-2.00      
    ## 7 Nest record extension to BMDE 2.0               NA        BMDE-NEST-2.00     
    ## 8 Nest record extension (visit data) to BMDE 2.0  NA        BMDE-NESTVISIT-2.00

2.  You can get a list of the specific fields/columns included in each
    version using the
    [`meta_bmde_fields()`](https://birdscanada.github.io/naturecounts/reference/meta.md)
    function

``` r
meta_bmde_fields(version = "BMDE-BAND-2.00") |>
  head()
```

    ## # A tibble: 6 × 3
    ##   field_order version        local_name            
    ##         <int> <chr>          <chr>                 
    ## 1           1 BMDE-BAND-2.00 GlobalUniqueIdentifier
    ## 2           2 BMDE-BAND-2.00 DateLastModified      
    ## 3           3 BMDE-BAND-2.00 BasisOfRecord         
    ## 4           4 BMDE-BAND-2.00 InstitutionCode       
    ## 5           5 BMDE-BAND-2.00 CollectionCode        
    ## 6           6 BMDE-BAND-2.00 CatalogNumber

3.  You can see which BMDE versions are used by different collections by
    looking at the metadata associated with that collection.

``` r
meta_collections() |> 
  head()
```

    ## # A tibble: 6 × 11
    ##   bmdr_code  bmde_version project_id n_locations collection_name_fr   collection
    ##   <chr>      <chr>             <int>       <int> <chr>                <chr>     
    ## 1 ABATLAS1   BMDE2.00           1042        2209 Atlas des oiseaux n… ABATLAS1  
    ## 2 ABATLAS2   BMDE2.00           1042        5359 Atlas des oiseaux n… ABATLAS2  
    ## 3 ABBIRDRECS BMDE2.00           1042       21962 Observations d'oise… ABBIRDRECS
    ## 4 ABOWLS     BMDE2.00           1012         213 Inventaire des hibo… ABOWLS    
    ## 5 ACCWS      BMDE2.00           1062          16 NA                   ACCWS     
    ## 6 ATBANS     BMDE2.00           1074         124 NA                   ATBANS    
    ## # ℹ 5 more variables: min_year <int>, max_year <int>, collection_name <chr>,
    ## #   akn_level <int>, n_records <int>

## Specifying fields by version

By default, the `BMDE2.00-min` or `minimum` BMDE version is used when
downloading data, but you can specify other versions with either the
version name, or the shorthand.

``` r
cardinals <- nc_data_dl(species = 19360, fields_set = "core", 
                        collection = "ABATLAS2", verbose = FALSE,
                        username = "testuser", info = "nc_vignette")
names(cardinals)
```

    ##   [1] "record_id"                             
    ##   [2] "collection"                            
    ##   [3] "project_id"                            
    ##   [4] "protocol_id"                           
    ##   [5] "protocol_type"                         
    ##   [6] "species_id"                            
    ##   [7] "statprov_code"                         
    ##   [8] "country_code"                          
    ##   [9] "SiteCode"                              
    ##  [10] "latitude"                              
    ##  [11] "longitude"                             
    ##  [12] "bcr"                                   
    ##  [13] "subnational2_code"                     
    ##  [14] "iba_site"                              
    ##  [15] "utm_square"                            
    ##  [16] "survey_year"                           
    ##  [17] "survey_month"                          
    ##  [18] "survey_week"                           
    ##  [19] "survey_day"                            
    ##  [20] "breeding_rank"                         
    ##  [21] "GlobalUniqueIdentifier"                
    ##  [22] "DateLastModified"                      
    ##  [23] "BasisOfRecord"                         
    ##  [24] "InstitutionCode"                       
    ##  [25] "CollectionCode"                        
    ##  [26] "CatalogNumber"                         
    ##  [27] "ScientificName"                        
    ##  [28] "HigherTaxon"                           
    ##  [29] "Kingdom"                               
    ##  [30] "Phylum"                                
    ##  [31] "Class"                                 
    ##  [32] "OrderTaxon"                            
    ##  [33] "Family"                                
    ##  [34] "Genus"                                 
    ##  [35] "SpecificEpithet"                       
    ##  [36] "InfraspecificRank"                     
    ##  [37] "InfraspecificEpithet"                  
    ##  [38] "ScientificNameAuthor"                  
    ##  [39] "IdentificationQualifier"               
    ##  [40] "HigherGeography"                       
    ##  [41] "Continent"                             
    ##  [42] "WaterBody"                             
    ##  [43] "IslandGroup"                           
    ##  [44] "Island"                                
    ##  [45] "Country"                               
    ##  [46] "StateProvince"                         
    ##  [47] "County"                                
    ##  [48] "Locality"                              
    ##  [49] "MinimumElevationInMeters"              
    ##  [50] "MaximumElevationInMeters"              
    ##  [51] "MinimumDepthInMeters"                  
    ##  [52] "MaximumDepthInMeters"                  
    ##  [53] "DecimalLatitude"                       
    ##  [54] "DecimalLongitude"                      
    ##  [55] "GeodeticDatum"                         
    ##  [56] "CoordinateUncertaintyInMeters"         
    ##  [57] "YearCollected"                         
    ##  [58] "MonthCollected"                        
    ##  [59] "DayCollected"                          
    ##  [60] "TimeCollected"                         
    ##  [61] "JulianDay"                             
    ##  [62] "Collector"                             
    ##  [63] "Sex"                                   
    ##  [64] "LifeStage"                             
    ##  [65] "ImageURL"                              
    ##  [66] "RelatedInformation"                    
    ##  [67] "CollectorNumber"                       
    ##  [68] "FieldNumber"                           
    ##  [69] "FieldNotes"                            
    ##  [70] "OriginalCoordinatesSystem"             
    ##  [71] "LatLongComments"                       
    ##  [72] "GeoreferenceMethod"                    
    ##  [73] "GeoreferenceReferences"                
    ##  [74] "GeoreferenceVerificationStatus"        
    ##  [75] "Remarks"                               
    ##  [76] "FootprintWKT"                          
    ##  [77] "FootprintSRS"                          
    ##  [78] "ProjectCode"                           
    ##  [79] "ProtocolType"                          
    ##  [80] "ProtocolCode"                          
    ##  [81] "ProtocolSpeciesTargeted"               
    ##  [82] "ProtocolReference"                     
    ##  [83] "ProtocolURL"                           
    ##  [84] "SurveyAreaIdentifier"                  
    ##  [85] "SurveyAreaSize"                        
    ##  [86] "SurveyAreaPercentageCovered"           
    ##  [87] "SurveyAreaShape"                       
    ##  [88] "SurveyAreaLongAxisLength"              
    ##  [89] "SurveyAreaShortAxisLength"             
    ##  [90] "SurveyAreaLongAxisOrientation"         
    ##  [91] "CoordinatesScope"                      
    ##  [92] "SamplingEventIdentifier"               
    ##  [93] "SamplingEventStructure"                
    ##  [94] "RouteIdentifier"                       
    ##  [95] "TimeObservationsStarted"               
    ##  [96] "TimeObservationsEnded"                 
    ##  [97] "DurationInHours"                       
    ##  [98] "TimeIntervalStarted"                   
    ##  [99] "TimeIntervalEnded"                     
    ## [100] "TimeIntervalsAdditive"                 
    ## [101] "NumberOfObservers"                     
    ## [102] "EffortMeasurement1"                    
    ## [103] "EffortUnits1"                          
    ## [104] "EffortMeasurement2"                    
    ## [105] "EffortUnits2"                          
    ## [106] "EffortMeasurement3"                    
    ## [107] "EffortUnits3"                          
    ## [108] "EffortMeasurement4"                    
    ## [109] "EffortUnits4"                          
    ## [110] "EffortMeasurement5"                    
    ## [111] "EffortUnits5"                          
    ## [112] "EffortMeasurement6"                    
    ## [113] "EffortUnits6"                          
    ## [114] "EffortMeasurement7"                    
    ## [115] "EffortUnits7"                          
    ## [116] "EffortMeasurement8"                    
    ## [117] "EffortUnits8"                          
    ## [118] "EffortMeasurement9"                    
    ## [119] "EffortUnits9"                          
    ## [120] "EffortMeasurement10"                   
    ## [121] "EffortUnits10"                         
    ## [122] "EffortMeasurement11"                   
    ## [123] "EffortUnits11"                         
    ## [124] "EffortMeasurement12"                   
    ## [125] "EffortUnits12"                         
    ## [126] "EffortMeasurement13"                   
    ## [127] "EffortUnits13"                         
    ## [128] "EffortMeasurement14"                   
    ## [129] "EffortUnits14"                         
    ## [130] "EffortMeasurement15"                   
    ## [131] "EffortUnits15"                         
    ## [132] "EffortMeasurement16"                   
    ## [133] "EffortUnits16"                         
    ## [134] "EffortMeasurement17"                   
    ## [135] "EffortUnits17"                         
    ## [136] "EffortMeasurement18"                   
    ## [137] "EffortUnits18"                         
    ## [138] "NoObservations"                        
    ## [139] "DistanceFromObserver"                  
    ## [140] "DistanceFromObserverMin"               
    ## [141] "DistanceFromObserverMax"               
    ## [142] "DistanceFromStart"                     
    ## [143] "BearingInDegrees"                      
    ## [144] "SpecimenDecimalLatitude"               
    ## [145] "SpecimenDecimalLongitude"              
    ## [146] "SpecimenGeodeticDatum"                 
    ## [147] "SpecimenUTMZone"                       
    ## [148] "SpecimenUTMNorthing"                   
    ## [149] "SpecimenUTMEasting"                    
    ## [150] "ObservationCount"                      
    ## [151] "ObservationDescriptor"                 
    ## [152] "ObservationCount2"                     
    ## [153] "ObservationDescriptor2"                
    ## [154] "ObservationCount3"                     
    ## [155] "ObservationDescriptor3"                
    ## [156] "ObservationCount4"                     
    ## [157] "ObservationDescriptor4"                
    ## [158] "ObservationCount5"                     
    ## [159] "ObservationDescriptor5"                
    ## [160] "ObservationCount6"                     
    ## [161] "ObservationDescriptor6"                
    ## [162] "ObsCountAtLeast"                       
    ## [163] "ObsCountAtMost"                        
    ## [164] "ObservationDate"                       
    ## [165] "DateUncertaintyInDays"                 
    ## [166] "AllIndividualsReported"                
    ## [167] "AllSpeciesReported"                    
    ## [168] "UTMZone"                               
    ## [169] "UTMNorthing"                           
    ## [170] "UTMEasting"                            
    ## [171] "CoordinatesUncertaintyInDecimalDegrees"
    ## [172] "CommonName"                            
    ## [173] "RecordPermissions"                     
    ## [174] "MultiScientificName1"                  
    ## [175] "MultiScientificName2"                  
    ## [176] "MultiScientificName3"                  
    ## [177] "MultiScientificName4"                  
    ## [178] "MultiScientificName5"                  
    ## [179] "MultiScientificName6"                  
    ## [180] "TaxonomicAuthorityAuthors"             
    ## [181] "TaxonomicAuthorityVersion"             
    ## [182] "TaxonomicAuthorityYear"                
    ## [183] "SpeciesCode"                           
    ## [184] "TaxonConceptID"                        
    ## [185] "BreedingBirdAtlasCode"                 
    ## [186] "HabitatDescription"                    
    ## [187] "Remarks2"                              
    ## [188] "LastModifiedAction"                    
    ## [189] "RecordReviewStatus"

## Basic fields

Note that there are extra fields/columns downloaded in every request,
which are **in addition** to the BMDE fields. These are additional,
basic, indexed fields used in the database.

You can figure out exactly which fields these are, by removing all the
fields from the `core` BMDE field set.

``` r
names(cardinals)[!names(cardinals) %in% meta_bmde_fields(version = "core")$local_name]
```

    ##  [1] "record_id"         "collection"        "project_id"       
    ##  [4] "protocol_id"       "protocol_type"     "species_id"       
    ##  [7] "statprov_code"     "country_code"      "SiteCode"         
    ## [10] "latitude"          "longitude"         "bcr"              
    ## [13] "subnational2_code" "iba_site"          "utm_square"       
    ## [16] "survey_year"       "survey_month"      "survey_week"      
    ## [19] "survey_day"        "breeding_rank"

## Custom fields

The basic fields will **always** be included in any data request.
However, you can choose which fields/columns to include in addition to
these basic fields, by specifying `fields_set = custom`, and listing all
additional `fields`.

``` r
cardinals <- nc_data_dl(species = 19360, fields_set = "custom", 
                        fields = c("Sex", "SamplingEventIdentifier"), 
                        collection = "ABATLAS2", verbose = FALSE,
                        username = "testuser", info = "nc_vignette")
head(cardinals)
```

    ##   record_id collection project_id protocol_id protocol_type species_id
    ## 1 225121054   ABATLAS2       1048          NA            NA      19360
    ## 2 225167193   ABATLAS2       1048          NA            NA      19360
    ## 3 225185535   ABATLAS2       1048          NA            NA      19360
    ## 4 225185857   ABATLAS2       1048          NA            NA      19360
    ## 5 225387824   ABATLAS2       1048          NA            NA      19360
    ## 6 225389502   ABATLAS2       1048          NA            NA      19360
    ##   statprov_code country_code SiteCode latitude longitude bcr subnational2_code
    ## 1            AB           CA    12049 53.98632 -111.2793   6          CA.AB.12
    ## 2            AB           CA    45868 53.34350 -114.0790   6          CA.AB.11
    ## 3            AB           CA    45940 53.52760 -113.2996  11          CA.AB.11
    ## 4            AB           CA    45988 53.56476 -113.4267  11          CA.AB.11
    ## 5            AB           CA    15330 53.95000 -115.1361   6          CA.AB.13
    ## 6            AB           CA    45851 53.53472 -113.6407  11          CA.AB.11
    ##   iba_site utm_square survey_year survey_month survey_week survey_day
    ## 1      N/A    12UVE88        2000            2           3         23
    ## 2      N/A    11UPV91        2005            5           1          7
    ## 3      N/A    12UUE43        2005            4           1          4
    ## 4      N/A    12UUE33        2005            5           2         16
    ## 5      N/A    11UPV27        2004           12           1          7
    ## 6      N/A    12UUE23        2005            4           4         26
    ##   breeding_rank Sex SamplingEventIdentifier
    ## 1             0  NA                    5863
    ## 2             0  NA                   92650
    ## 3             0  NA                   92829
    ## 4             0  NA                   92900
    ## 5             0  NA                   11937
    ## 6             0  NA                   92626

## Adding specific fields to a BMDE version

In a more complex example, if you wished to download data with the
`minimum` set of BMDE fields, but including a couple of extra fields,
you could combine the core fields with the extras you want, and pass
them on as a custom field set.

For example, first collect the minimum field/column names.

``` r
my_fields <- meta_bmde_fields(version = "minimum")$local_name
my_fields
```

    ##  [1] "GlobalUniqueIdentifier"  "CatalogNumber"          
    ##  [3] "Locality"                "TimeCollected"          
    ##  [5] "CollectorNumber"         "FieldNumber"            
    ##  [7] "Remarks"                 "ProjectCode"            
    ##  [9] "ProtocolType"            "ProtocolCode"           
    ## [11] "ProtocolURL"             "SurveyAreaIdentifier"   
    ## [13] "SamplingEventIdentifier" "SamplingEventStructure" 
    ## [15] "RouteIdentifier"         "TimeObservationsStarted"
    ## [17] "TimeObservationsEnded"   "DurationInHours"        
    ## [19] "TimeIntervalStarted"     "TimeIntervalEnded"      
    ## [21] "TimeIntervalsAdditive"   "NumberOfObservers"      
    ## [23] "NoObservations"          "ObservationCount"       
    ## [25] "ObservationDescriptor"   "ObservationCount2"      
    ## [27] "ObservationDescriptor2"  "ObservationCount3"      
    ## [29] "ObservationDescriptor3"  "ObservationCount4"      
    ## [31] "ObservationDescriptor4"  "ObservationCount5"      
    ## [33] "ObservationDescriptor5"  "ObservationCount6"      
    ## [35] "ObservationDescriptor6"  "AllIndividualsReported" 
    ## [37] "AllSpeciesReported"

Then add in the extra fields.

``` r
my_fields <- c(my_fields, "Sex", "LifeStage")
my_fields
```

    ##  [1] "GlobalUniqueIdentifier"  "CatalogNumber"          
    ##  [3] "Locality"                "TimeCollected"          
    ##  [5] "CollectorNumber"         "FieldNumber"            
    ##  [7] "Remarks"                 "ProjectCode"            
    ##  [9] "ProtocolType"            "ProtocolCode"           
    ## [11] "ProtocolURL"             "SurveyAreaIdentifier"   
    ## [13] "SamplingEventIdentifier" "SamplingEventStructure" 
    ## [15] "RouteIdentifier"         "TimeObservationsStarted"
    ## [17] "TimeObservationsEnded"   "DurationInHours"        
    ## [19] "TimeIntervalStarted"     "TimeIntervalEnded"      
    ## [21] "TimeIntervalsAdditive"   "NumberOfObservers"      
    ## [23] "NoObservations"          "ObservationCount"       
    ## [25] "ObservationDescriptor"   "ObservationCount2"      
    ## [27] "ObservationDescriptor2"  "ObservationCount3"      
    ## [29] "ObservationDescriptor3"  "ObservationCount4"      
    ## [31] "ObservationDescriptor4"  "ObservationCount5"      
    ## [33] "ObservationDescriptor5"  "ObservationCount6"      
    ## [35] "ObservationDescriptor6"  "AllIndividualsReported" 
    ## [37] "AllSpeciesReported"      "Sex"                    
    ## [39] "LifeStage"

Now download the data.

``` r
cardinals <- nc_data_dl(species = 19360, fields_set = "custom", 
                        fields = my_fields, 
                        collection = "ABATLAS2", verbose = FALSE,
                        username = "testuser", info = "nc_vignette")
head(cardinals)
```

    ##   record_id collection project_id protocol_id protocol_type species_id
    ## 1 225121054   ABATLAS2       1048          NA            NA      19360
    ## 2 225167193   ABATLAS2       1048          NA            NA      19360
    ## 3 225185535   ABATLAS2       1048          NA            NA      19360
    ## 4 225185857   ABATLAS2       1048          NA            NA      19360
    ## 5 225387824   ABATLAS2       1048          NA            NA      19360
    ## 6 225389502   ABATLAS2       1048          NA            NA      19360
    ##   statprov_code country_code SiteCode latitude longitude bcr subnational2_code
    ## 1            AB           CA    12049 53.98632 -111.2793   6          CA.AB.12
    ## 2            AB           CA    45868 53.34350 -114.0790   6          CA.AB.11
    ## 3            AB           CA    45940 53.52760 -113.2996  11          CA.AB.11
    ## 4            AB           CA    45988 53.56476 -113.4267  11          CA.AB.11
    ## 5            AB           CA    15330 53.95000 -115.1361   6          CA.AB.13
    ## 6            AB           CA    45851 53.53472 -113.6407  11          CA.AB.11
    ##   iba_site utm_square survey_year survey_month survey_week survey_day
    ## 1      N/A    12UVE88        2000            2           3         23
    ## 2      N/A    11UPV91        2005            5           1          7
    ## 3      N/A    12UUE43        2005            4           1          4
    ## 4      N/A    12UUE33        2005            5           2         16
    ## 5      N/A    11UPV27        2004           12           1          7
    ## 6      N/A    12UUE23        2005            4           4         26
    ##   breeding_rank                GlobalUniqueIdentifier FieldNumber
    ## 1             0  URN:NatureAlberta:ABATLAS2:5863-NOCA          NA
    ## 2             0 URN:NatureAlberta:ABATLAS2:92650-NOCA          NA
    ## 3             0 URN:NatureAlberta:ABATLAS2:92829-NOCA          NA
    ## 4             0 URN:NatureAlberta:ABATLAS2:92900-NOCA          NA
    ## 5             0 URN:NatureAlberta:ABATLAS2:11937-NOCA          NA
    ## 6             0 URN:NatureAlberta:ABATLAS2:92626-NOCA          NA
    ##   DurationInHours ObservationDescriptor5               Remarks
    ## 1               8                     NA              uncommon
    ## 2               3                     NA                  <NA>
    ## 3               3                     NA Spotted at the feeder
    ## 4               3                     NA                  <NA>
    ## 5               3                     NA                  <NA>
    ## 6               3                     NA                  <NA>
    ##   ObservationDescriptor6 ObservationDescriptor3 ObservationDescriptor4
    ## 1                     NA                     NA                     NA
    ## 2                     NA                     NA                     NA
    ## 3                     NA                     NA                     NA
    ## 4                     NA                     NA                     NA
    ## 5                     NA                     NA                     NA
    ## 6                     NA                     NA                     NA
    ##   ObservationDescriptor ProtocolCode ObservationCount ObservationDescriptor2
    ## 1                    NA           NA                2                     NA
    ## 2                    NA           NA                1                     NA
    ## 3                    NA           NA                1                     NA
    ## 4                    NA           NA                1                     NA
    ## 5                    NA           NA                1                     NA
    ## 6                    NA           NA                1                     NA
    ##   TimeIntervalEnded                      Locality SamplingEventIdentifier
    ## 1                NA All areas within count circle                    5863
    ## 2                NA                          PK91                   92650
    ## 3                NA           Sherwood Park, UQ43                   92829
    ## 4                NA      50st and Ada blvd., UQ33                   92900
    ## 5                NA                   Mayerthorpe                   11937
    ## 6                NA                          UQ23                   92626
    ##   SurveyAreaIdentifier SamplingEventStructure TimeCollected
    ## 1                12049                     NA            NA
    ## 2                45868                     NA            NA
    ## 3                45940                     NA            NA
    ## 4                45988                     NA            NA
    ## 5                15330                     NA            NA
    ## 6                45851                     NA            NA
    ##   TimeObservationsEnded TimeObservationsStarted Sex LifeStage CollectorNumber
    ## 1                    NA                09:00:00  NA        NA            5017
    ## 2                    NA                06:00:00  NA        NA            4882
    ## 3                    NA                06:00:00  NA        NA            4882
    ## 4                    NA                06:00:00  NA        NA           10197
    ## 5                    NA                06:00:00  NA        NA            6810
    ## 6                    NA                06:00:00  NA        NA            4882
    ##   CatalogNumber AllSpeciesReported ProjectCode RouteIdentifier
    ## 1     5863-NOCA            Unknown    ABATLAS2              NA
    ## 2    92650-NOCA            Unknown    ABATLAS2              NA
    ## 3    92829-NOCA            Unknown    ABATLAS2              NA
    ## 4    92900-NOCA            Unknown    ABATLAS2              NA
    ## 5    11937-NOCA            Unknown    ABATLAS2              NA
    ## 6    92626-NOCA            Unknown    ABATLAS2              NA
    ##   AllIndividualsReported TimeIntervalsAdditive ProtocolURL NumberOfObservers
    ## 1                     NA                    NA          NA                 0
    ## 2                     NA                    NA          NA                 1
    ## 3                     NA                    NA          NA                 1
    ## 4                     NA                    NA          NA                 1
    ## 5                     NA                    NA          NA                 0
    ## 6                     NA                    NA          NA                 1
    ##   ObservationCount2 ObservationCount6 ObservationCount5            ProtocolType
    ## 1                NA                NA                NA AT: Breeding Bird Atlas
    ## 2                NA                NA                NA AT: Breeding Bird Atlas
    ## 3                NA                NA                NA AT: Breeding Bird Atlas
    ## 4                NA                NA                NA AT: Breeding Bird Atlas
    ## 5                NA                NA                NA AT: Breeding Bird Atlas
    ## 6                NA                NA                NA AT: Breeding Bird Atlas
    ##   NoObservations ObservationCount4 ObservationCount3 TimeIntervalStarted
    ## 1             NA                NA                NA                  NA
    ## 2             NA                NA                NA                  NA
    ## 3             NA                NA                NA                  NA
    ## 4             NA                NA                NA                  NA
    ## 5             NA                NA                NA                  NA
    ## 6             NA                NA                NA                  NA

Great!
