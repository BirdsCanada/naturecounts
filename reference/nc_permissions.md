# Download list of accessible collections

Returns a list of collections accessible by 'username'.

## Usage

``` r
nc_permissions(username = NULL, timeout = 60)
```

## Arguments

- username:

  Character vector. Username for <http://naturecounts.ca>. If provided,
  the user will be prompted for a password. If left NULL, only public
  collections will be returned.

- timeout:

  Numeric. Number of seconds before connecting to the server times out.

## NatureCounts account

All public data is available with a username/password ([sign
up](https://www.naturecounts.ca/nc/default/register.jsp) for a free
NatureCounts account). However, to access private/semi-public
projects/collections you must request access. See the Access and
`request_id`s section for more information.

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
approved `request_id` to the
[`nc_data_dl()`](https://birdscanada.github.io/naturecounts/reference/nc_data_dl.md)
function (e.g., `nc_data_dl(request_id = 152000, username = "USER")`).
Use
[`nc_requests()`](https://birdscanada.github.io/naturecounts/reference/nc_requests.md)
to see `request_id`s, filters, and approval status.

Requests for "full" access to additional collections can be made online
through the [Web Request
Form](https://naturecounts.ca/nc/default/searchquery.jsp) by checking
the "Full access?" box in Step 2 of the form.

## Examples

``` r
nc_permissions()
#>   [1] "ABATLAS1"          "ABATLAS2"          "ABBIRDRECS"       
#>   [4] "ACCWS_HIST"        "BBL-1960-1969"     "BBL-1970-1979"    
#>   [7] "BBL-1980-1989"     "BBL-1990-1999"     "BBL-2000-2009"    
#>  [10] "BBL-2010-2019"     "BBL-2020-2029"     "BBS"              
#>  [13] "BCATLAS1BE_DO"     "BCATLAS1BE_RAW"    "BCATLAS1BE_SUMM"  
#>  [16] "BCATLAS1PC"        "BCMA"              "BCN"              
#>  [19] "BGRMM"             "CMMN-DET-HBO"      "CMMN-DET-TLBO"    
#>  [22] "CMMN-DET-VLBO"     "EBUTTERFLY"        "GBIF_00D636A3"    
#>  [25] "GBIF_02DD359E"     "GBIF_040C5662"     "GBIF_04169FCE"    
#>  [28] "GBIF_051D8004"     "GBIF_07ECCF22"     "GBIF_09E90DFB"    
#>  [31] "GBIF_09F3BADB"     "GBIF_0C4BF92B"     "GBIF_0CE32496"    
#>  [34] "GBIF_0D697041"     "GBIF_0DAC68D7"     "GBIF_0DEBAFD0"    
#>  [37] "GBIF_0EAD7539"     "GBIF_10431811"     "GBIF_128A8DCD"    
#>  [40] "GBIF_1339958D"     "GBIF_136560FA"     "GBIF_16965647"    
#>  [43] "GBIF_1A58B1F6"     "GBIF_1CDE1EE9"     "GBIF_1E61B812"    
#>  [46] "GBIF_210BABC8"     "GBIF_234705C5"     "GBIF_23BEBE58"    
#>  [49] "GBIF_24A04AA9"     "GBIF_264E6A66"     "GBIF_292A71DF"    
#>  [52] "GBIF_2A975C3C"     "GBIF_2BF7B2C0"     "GBIF_2C12ACD9"    
#>  [55] "GBIF_2DE18034"     "GBIF_2F54CB88"     "GBIF_310DBB60"    
#>  [58] "GBIF_3558B1B0"     "GBIF_36F15A36"     "GBIF_37DA8155"    
#>  [61] "GBIF_39905320"     "GBIF_39F021D5"     "GBIF_3B5D5701"    
#>  [64] "GBIF_3D4CAC0A"     "GBIF_3E7837D0"     "GBIF_403FABE5"    
#>  [67] "GBIF_427A6290"     "GBIF_432C6C7C"     "GBIF_43E0BBA3"    
#>  [70] "GBIF_44165940"     "GBIF_45A84259"     "GBIF_472A9647"    
#>  [73] "GBIF_4852540D"     "GBIF_4BFAC3EA"     "GBIF_4F29B6AB"    
#>  [76] "GBIF_50914B57"     "GBIF_50C9509D"     "GBIF_52DA118D"    
#>  [79] "GBIF_5345B34D"     "GBIF_5A12D9C3"     "GBIF_5C508FE3"    
#>  [82] "GBIF_5F9CAC91"     "GBIF_61BF28A5"     "GBIF_6202C5E9"    
#>  [85] "GBIF_62AD511D"     "GBIF_66DF9620"     "GBIF_6AC3F774"    
#>  [88] "GBIF_7447A228"     "GBIF_75018539"     "GBIF_767F57DD"    
#>  [91] "GBIF_78FF8409"     "GBIF_7A25F7AA"     "GBIF_7C93D290"    
#>  [94] "GBIF_7F6DD0F7"     "GBIF_807A28CF"     "GBIF_80B4CEFF"    
#>  [97] "GBIF_8138EB72"     "GBIF_821CC27A"     "GBIF_829026F2"    
#> [100] "GBIF_830FD460"     "GBIF_83AC301C"     "GBIF_843CC7A8"    
#> [103] "GBIF_843DF0C4"     "GBIF_848586A4"     "GBIF_84A80B12"    
#> [106] "GBIF_84B018DE"     "GBIF_84B26828"     "GBIF_84F728BE"    
#> [109] "GBIF_854CF79E"     "GBIF_857BCE66"     "GBIF_8631295A"    
#> [112] "GBIF_889C91A3"     "GBIF_890C34EE"     "GBIF_89337996"    
#> [115] "GBIF_8A6ACA38"     "GBIF_8A863029"     "GBIF_8BE43F9B"    
#> [118] "GBIF_8D014DC4"     "GBIF_8EDDC200"     "GBIF_8F93BAB2"    
#> [121] "GBIF_90159E39"     "GBIF_905589DD"     "GBIF_91A26B5B"    
#> [124] "GBIF_91AA5E23"     "GBIF_96678E90"     "GBIF_96C383A8"    
#> [127] "GBIF_96C93A1E"     "GBIF_9C007868"     "GBIF_9CE52FF6"    
#> [130] "GBIF_9F2BC0A9"     "GBIF_A307E4D7"     "GBIF_A44840ED"    
#> [133] "GBIF_A48E2384"     "GBIF_A56671B7"     "GBIF_A6A1D66F"    
#> [136] "GBIF_A6F91225"     "GBIF_A79C2B50"     "GBIF_A80ED172"    
#> [139] "GBIF_A82D2421"     "GBIF_ABCACCAD"     "GBIF_AE1CD68B"    
#> [142] "GBIF_AF5FDF89"     "GBIF_AFC4F93E"     "GBIF_B008FB96"    
#> [145] "GBIF_B1047888"     "GBIF_B15D4952"     "GBIF_B211F32F"    
#> [148] "GBIF_B49A165B"     "GBIF_B4AE1720"     "GBIF_B6015B60"    
#> [151] "GBIF_B670372C"     "GBIF_B7EC1BF8"     "GBIF_B84A3711"    
#> [154] "GBIF_B89DCFD3"     "GBIF_B8BB529B"     "GBIF_BA0C03AB"    
#> [157] "GBIF_BA0C046D"     "GBIF_BB5B30B4"     "GBIF_BDCE0B4B"    
#> [160] "GBIF_BE5507B9"     "GBIF_C1ACC137"     "GBIF_C21CD435"    
#> [163] "GBIF_C43384A9"     "GBIF_C50340A3"     "GBIF_C561BAA1"    
#> [166] "GBIF_C6A2ECF7"     "GBIF_C84310F0"     "GBIF_C9076CD3"    
#> [169] "GBIF_CC48F7B9"     "GBIF_CE516027"     "GBIF_CE9D17F0"    
#> [172] "GBIF_D0D5EF85"     "GBIF_D0E133C0"     "GBIF_D235AF7E"    
#> [175] "GBIF_D3061EFD"     "GBIF_D35BDE1E"     "GBIF_D3BC4C7D"    
#> [178] "GBIF_D605ECF2"     "GBIF_D740F242"     "GBIF_D8CD16BA"    
#> [181] "GBIF_D99823CA"     "GBIF_DE5ADC1A"     "GBIF_E17BFFF0"    
#> [184] "GBIF_E1BEB83C"     "GBIF_E1E33F62"     "GBIF_E3B959D6"    
#> [187] "GBIF_E3CE628E"     "GBIF_E44FF4BB"     "GBIF_E58A6887"    
#> [190] "GBIF_E5C5CAD9"     "GBIF_E635240A"     "GBIF_E6667955"    
#> [193] "GBIF_E6ACC36B"     "GBIF_E6C97F6E"     "GBIF_EBC62D60"    
#> [196] "GBIF_EC186B76"     "GBIF_F11DB245"     "GBIF_F269F7B1"    
#> [199] "GBIF_F62C330C"     "GBIF_F6E21753"     "GBIF_F85F5C5C"    
#> [202] "GBIF_F93694B5"     "GBIF_F96A6F8C"     "GBIF_FA375330"    
#> [205] "GBIF_FCBA1F2E"     "GBIF_FDA9A8F8"     "IMMP"             
#> [208] "IMMP_A2"           "IMMP_BW"           "IMQC"             
#> [211] "MBATLAS1BE_DO"     "MBATLAS1BE_RAW"    "MBATLAS1BE_SUMM"  
#> [214] "MBATLAS1PC"        "MBBA1BE_RAW"       "MBBA1BE_SUMM"     
#> [217] "MBBA2BE_RAW"       "MBBA2BE_SUMM"      "MBBA2PC"          
#> [220] "MEXU"              "MLMP"              "MM"               
#> [223] "MONARCHWATCH"      "NATURALISTA"       "NESTWATCH_KMARTIN"
#> [226] "OBBA1BE_RAW"       "OBBA1BE_SUMM"      "OBBA2BE_RAW"      
#> [229] "OBBA2BE_SUMM"      "OBBA2PC"           "OBFS"             
#> [232] "PFW"               "PMMM"              "PRISM-OSS"        
#> [235] "QCATLAS1BE_RAW"    "QCATLAS1BE_SUMM"   "QCATLAS2BE_DO"    
#> [238] "QCATLAS2BE_RAW"    "QCATLAS2BE_SUMM"   "QCATLAS2PC"       
#> [241] "RCBIOTABASE"       "WILDTRAX1"         "WILDTRAX10"       
#> [244] "WILDTRAX1004"      "WILDTRAX105"       "WILDTRAX1070"     
#> [247] "WILDTRAX1092"      "WILDTRAX11"        "WILDTRAX1175"     
#> [250] "WILDTRAX1184"      "WILDTRAX1197"      "WILDTRAX1242"     
#> [253] "WILDTRAX1245"      "WILDTRAX125"       "WILDTRAX1251"     
#> [256] "WILDTRAX1252"      "WILDTRAX1253"      "WILDTRAX1271"     
#> [259] "WILDTRAX1272"      "WILDTRAX1273"      "WILDTRAX1274"     
#> [262] "WILDTRAX13"        "WILDTRAX1310"      "WILDTRAX1313"     
#> [265] "WILDTRAX1330"      "WILDTRAX1391"      "WILDTRAX1394"     
#> [268] "WILDTRAX1398"      "WILDTRAX141"       "WILDTRAX142"      
#> [271] "WILDTRAX1423"      "WILDTRAX1445"      "WILDTRAX1447"     
#> [274] "WILDTRAX1448"      "WILDTRAX1449"      "WILDTRAX145"      
#> [277] "WILDTRAX146"       "WILDTRAX1469"      "WILDTRAX147"      
#> [280] "WILDTRAX1475"      "WILDTRAX148"       "WILDTRAX1493"     
#> [283] "WILDTRAX150"       "WILDTRAX1505"      "WILDTRAX1508"     
#> [286] "WILDTRAX151"       "WILDTRAX1511"      "WILDTRAX1512"     
#> [289] "WILDTRAX152"       "WILDTRAX153"       "WILDTRAX154"      
#> [292] "WILDTRAX1579"      "WILDTRAX16"        "WILDTRAX161"      
#> [295] "WILDTRAX1651"      "WILDTRAX1652"      "WILDTRAX168"      
#> [298] "WILDTRAX169"       "WILDTRAX1725"      "WILDTRAX1735"     
#> [301] "WILDTRAX178"       "WILDTRAX1855"      "WILDTRAX19"       
#> [304] "WILDTRAX2085"      "WILDTRAX2086"      "WILDTRAX2147"     
#> [307] "WILDTRAX2148"      "WILDTRAX2160"      "WILDTRAX2161"     
#> [310] "WILDTRAX2170"      "WILDTRAX2176"      "WILDTRAX223"      
#> [313] "WILDTRAX2254"      "WILDTRAX2274"      "WILDTRAX2277"     
#> [316] "WILDTRAX2289"      "WILDTRAX2292"      "WILDTRAX2294"     
#> [319] "WILDTRAX2295"      "WILDTRAX2368"      "WILDTRAX2376"     
#> [322] "WILDTRAX2452"      "WILDTRAX2460"      "WILDTRAX25"       
#> [325] "WILDTRAX266"       "WILDTRAX282"       "WILDTRAX2821"     
#> [328] "WILDTRAX285"       "WILDTRAX2872"      "WILDTRAX2873"     
#> [331] "WILDTRAX2874"      "WILDTRAX2881"      "WILDTRAX2907"     
#> [334] "WILDTRAX3045"      "WILDTRAX3046"      "WILDTRAX3093"     
#> [337] "WILDTRAX31"        "WILDTRAX3135"      "WILDTRAX3139"     
#> [340] "WILDTRAX3154"      "WILDTRAX3155"      "WILDTRAX3156"     
#> [343] "WILDTRAX3157"      "WILDTRAX3175"      "WILDTRAX3190"     
#> [346] "WILDTRAX32"        "WILDTRAX3219"      "WILDTRAX3225"     
#> [349] "WILDTRAX3283"      "WILDTRAX33"        "WILDTRAX334"      
#> [352] "WILDTRAX3351"      "WILDTRAX34"        "WILDTRAX3436"     
#> [355] "WILDTRAX36"        "WILDTRAX3638"      "WILDTRAX3775"     
#> [358] "WILDTRAX379"       "WILDTRAX3805"      "WILDTRAX381"      
#> [361] "WILDTRAX3814"      "WILDTRAX387"       "WILDTRAX388"      
#> [364] "WILDTRAX3887"      "WILDTRAX3933"      "WILDTRAX41"       
#> [367] "WILDTRAX410"       "WILDTRAX44"        "WILDTRAX446"      
#> [370] "WILDTRAX462"       "WILDTRAX5"         "WILDTRAX516"      
#> [373] "WILDTRAX517"       "WILDTRAX518"       "WILDTRAX519"      
#> [376] "WILDTRAX541"       "WILDTRAX543"       "WILDTRAX592"      
#> [379] "WILDTRAX60"        "WILDTRAX605"       "WILDTRAX609"      
#> [382] "WILDTRAX620"       "WILDTRAX659"       "WILDTRAX662"      
#> [385] "WILDTRAX672"       "WILDTRAX686"       "WILDTRAX715"      
#> [388] "WILDTRAX718"       "WILDTRAX719"       "WILDTRAX772"      
#> [391] "WILDTRAX804"       "WILDTRAX815"       "WILDTRAX828"      
#> [394] "WILDTRAX84"        "WILDTRAX85"        "WILDTRAX86"       
#> [397] "WILDTRAX870"       "WILDTRAX871"       "WILDTRAX872"      
#> [400] "WILDTRAX873"       "WILDTRAX874"       "WILDTRAX875"      
#> [403] "WILDTRAX876"       "WILDTRAX878"       "WILDTRAX879"      
#> [406] "WILDTRAX880"       "WILDTRAX881"       "WILDTRAX882"      
#> [409] "WILDTRAX883"       "WILDTRAX885"       "WILDTRAX887"      
#> [412] "WILDTRAX888"       "WILDTRAX889"       "WILDTRAX890"      
#> [415] "WILDTRAX911"       "WILDTRAX912"       "WILDTRAX924"      
#> [418] "WILDTRAX932"       "WILDTRAX934"       "WILDTRAX935"      
#> [421] "WILDTRAX948"       "WILDTRAX949"       "WILDTRAX950"      
#> [424] "WILDTRAX951"       "WILDTRAX952"       "WILDTRAX953"      
#> [427] "WILDTRAX954"       "WILDTRAX955"       "WILDTRAX956"      
#> [430] "WILDTRAX957"       "WILDTRAX958"       "WILDTRAX963"      
#> [433] "WILDTRAX964"       "WILDTRAX965"       "WILDTRAX966"      
#> [436] "WILDTRAX967"       "WILDTRAX973"       "WILDTRAX99"       
#> [439] "WILDTRAX990"       "WILDTRAX996"       "WILDTRAX997"      
#> [442] "WMMM"              "WPWI"             
nc_permissions(username = "sample")
#> [1] "SAMPLE1" "SAMPLE2"
```
