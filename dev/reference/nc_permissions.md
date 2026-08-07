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

## Examples

``` r

nc_permissions()
#>   [1] "ABATLAS1"          "ABATLAS2"          "ABBIRDRECS"       
#>   [4] "ACCWS"             "ACCWS_HIST"        "BBL-1960-1969"    
#>   [7] "BBL-1970-1979"     "BBL-1980-1989"     "BBL-1990-1999"    
#>  [10] "BBL-2000-2009"     "BBL-2010-2019"     "BBL-2020-2029"    
#>  [13] "BBS"               "BCATLAS1BE_DO"     "BCATLAS1BE_RAW"   
#>  [16] "BCATLAS1BE_SUMM"   "BCATLAS1PC"        "BCMA"             
#>  [19] "BCN"               "BGRMM"             "CMMN-DET-HBO"     
#>  [22] "EBUTTERFLY"        "GBIF_00D636A3"     "GBIF_02DD359E"    
#>  [25] "GBIF_040C5662"     "GBIF_04169FCE"     "GBIF_051D8004"    
#>  [28] "GBIF_07ECCF22"     "GBIF_09E90DFB"     "GBIF_09F3BADB"    
#>  [31] "GBIF_0C4BF92B"     "GBIF_0CE32496"     "GBIF_0D697041"    
#>  [34] "GBIF_0DAC68D7"     "GBIF_0DEBAFD0"     "GBIF_0EAD7539"    
#>  [37] "GBIF_10431811"     "GBIF_128A8DCD"     "GBIF_1339958D"    
#>  [40] "GBIF_136560FA"     "GBIF_16965647"     "GBIF_1A58B1F6"    
#>  [43] "GBIF_1CDE1EE9"     "GBIF_1E61B812"     "GBIF_210BABC8"    
#>  [46] "GBIF_234705C5"     "GBIF_23BEBE58"     "GBIF_24A04AA9"    
#>  [49] "GBIF_264E6A66"     "GBIF_292A71DF"     "GBIF_2A975C3C"    
#>  [52] "GBIF_2BF7B2C0"     "GBIF_2C12ACD9"     "GBIF_2DE18034"    
#>  [55] "GBIF_2F54CB88"     "GBIF_310DBB60"     "GBIF_3558B1B0"    
#>  [58] "GBIF_36F15A36"     "GBIF_37DA8155"     "GBIF_39905320"    
#>  [61] "GBIF_39F021D5"     "GBIF_3B5D5701"     "GBIF_3D4CAC0A"    
#>  [64] "GBIF_3E7837D0"     "GBIF_403FABE5"     "GBIF_427A6290"    
#>  [67] "GBIF_432C6C7C"     "GBIF_43E0BBA3"     "GBIF_44165940"    
#>  [70] "GBIF_45A84259"     "GBIF_472A9647"     "GBIF_4852540D"    
#>  [73] "GBIF_4BFAC3EA"     "GBIF_4F29B6AB"     "GBIF_50914B57"    
#>  [76] "GBIF_50C9509D"     "GBIF_52DA118D"     "GBIF_5345B34D"    
#>  [79] "GBIF_5A12D9C3"     "GBIF_5C508FE3"     "GBIF_5F9CAC91"    
#>  [82] "GBIF_61BF28A5"     "GBIF_6202C5E9"     "GBIF_62AD511D"    
#>  [85] "GBIF_66DF9620"     "GBIF_6AC3F774"     "GBIF_7447A228"    
#>  [88] "GBIF_75018539"     "GBIF_767F57DD"     "GBIF_78FF8409"    
#>  [91] "GBIF_7A25F7AA"     "GBIF_7C93D290"     "GBIF_7F6DD0F7"    
#>  [94] "GBIF_807A28CF"     "GBIF_80B4CEFF"     "GBIF_8138EB72"    
#>  [97] "GBIF_821CC27A"     "GBIF_829026F2"     "GBIF_830FD460"    
#> [100] "GBIF_83AC301C"     "GBIF_843CC7A8"     "GBIF_843DF0C4"    
#> [103] "GBIF_848586A4"     "GBIF_84A80B12"     "GBIF_84B018DE"    
#> [106] "GBIF_84B26828"     "GBIF_84F728BE"     "GBIF_854CF79E"    
#> [109] "GBIF_857BCE66"     "GBIF_8631295A"     "GBIF_889C91A3"    
#> [112] "GBIF_890C34EE"     "GBIF_89337996"     "GBIF_8A6ACA38"    
#> [115] "GBIF_8A863029"     "GBIF_8BE43F9B"     "GBIF_8D014DC4"    
#> [118] "GBIF_8EDDC200"     "GBIF_8F93BAB2"     "GBIF_90159E39"    
#> [121] "GBIF_905589DD"     "GBIF_91A26B5B"     "GBIF_91AA5E23"    
#> [124] "GBIF_96678E90"     "GBIF_96C383A8"     "GBIF_96C93A1E"    
#> [127] "GBIF_9C007868"     "GBIF_9CE52FF6"     "GBIF_9F2BC0A9"    
#> [130] "GBIF_A307E4D7"     "GBIF_A44840ED"     "GBIF_A48E2384"    
#> [133] "GBIF_A56671B7"     "GBIF_A6A1D66F"     "GBIF_A6F91225"    
#> [136] "GBIF_A79C2B50"     "GBIF_A80ED172"     "GBIF_A82D2421"    
#> [139] "GBIF_ABCACCAD"     "GBIF_AE1CD68B"     "GBIF_AF5FDF89"    
#> [142] "GBIF_AFC4F93E"     "GBIF_B008FB96"     "GBIF_B1047888"    
#> [145] "GBIF_B15D4952"     "GBIF_B211F32F"     "GBIF_B49A165B"    
#> [148] "GBIF_B4AE1720"     "GBIF_B6015B60"     "GBIF_B670372C"    
#> [151] "GBIF_B7EC1BF8"     "GBIF_B84A3711"     "GBIF_B89DCFD3"    
#> [154] "GBIF_B8BB529B"     "GBIF_BA0C03AB"     "GBIF_BA0C046D"    
#> [157] "GBIF_BB5B30B4"     "GBIF_BDCE0B4B"     "GBIF_BE5507B9"    
#> [160] "GBIF_C1ACC137"     "GBIF_C21CD435"     "GBIF_C43384A9"    
#> [163] "GBIF_C50340A3"     "GBIF_C561BAA1"     "GBIF_C6A2ECF7"    
#> [166] "GBIF_C84310F0"     "GBIF_C9076CD3"     "GBIF_CC48F7B9"    
#> [169] "GBIF_CE516027"     "GBIF_CE9D17F0"     "GBIF_D0D5EF85"    
#> [172] "GBIF_D0E133C0"     "GBIF_D235AF7E"     "GBIF_D3061EFD"    
#> [175] "GBIF_D35BDE1E"     "GBIF_D3BC4C7D"     "GBIF_D605ECF2"    
#> [178] "GBIF_D740F242"     "GBIF_D8CD16BA"     "GBIF_D99823CA"    
#> [181] "GBIF_DE5ADC1A"     "GBIF_E17BFFF0"     "GBIF_E1BEB83C"    
#> [184] "GBIF_E1E33F62"     "GBIF_E3B959D6"     "GBIF_E3CE628E"    
#> [187] "GBIF_E44FF4BB"     "GBIF_E58A6887"     "GBIF_E5C5CAD9"    
#> [190] "GBIF_E635240A"     "GBIF_E6667955"     "GBIF_E6ACC36B"    
#> [193] "GBIF_E6C97F6E"     "GBIF_EBC62D60"     "GBIF_EC186B76"    
#> [196] "GBIF_F11DB245"     "GBIF_F269F7B1"     "GBIF_F62C330C"    
#> [199] "GBIF_F6E21753"     "GBIF_F85F5C5C"     "GBIF_F93694B5"    
#> [202] "GBIF_F96A6F8C"     "GBIF_FA375330"     "GBIF_FCBA1F2E"    
#> [205] "GBIF_FDA9A8F8"     "IMMP"              "IMMP_A2"          
#> [208] "IMMP_BW"           "IMQC"              "MBATLAS1BE_DO"    
#> [211] "MBATLAS1BE_RAW"    "MBATLAS1BE_SUMM"   "MBATLAS1PC"       
#> [214] "MBBA1BE_RAW"       "MBBA1BE_SUMM"      "MBBA2BE_RAW"      
#> [217] "MBBA2BE_SUMM"      "MBBA2PC"           "MEXU"             
#> [220] "MLMP"              "MM"                "MONARCHWATCH"     
#> [223] "NATURALISTA"       "NESTWATCH_KMARTIN" "OBBA1BE_RAW"      
#> [226] "OBBA1BE_SUMM"      "OBBA2BE_RAW"       "OBBA2BE_SUMM"     
#> [229] "OBBA2PC"           "OBFS"              "PFW"              
#> [232] "PMMM"              "PRISM-OSS"         "QCATLAS1BE_RAW"   
#> [235] "QCATLAS1BE_SUMM"   "QCATLAS2BE_DO"     "QCATLAS2BE_RAW"   
#> [238] "QCATLAS2BE_SUMM"   "QCATLAS2PC"        "RCBIOTABASE"      
#> [241] "WILDTRAX1"         "WILDTRAX10"        "WILDTRAX1004"     
#> [244] "WILDTRAX105"       "WILDTRAX1070"      "WILDTRAX1092"     
#> [247] "WILDTRAX11"        "WILDTRAX1175"      "WILDTRAX1184"     
#> [250] "WILDTRAX1197"      "WILDTRAX1242"      "WILDTRAX1245"     
#> [253] "WILDTRAX125"       "WILDTRAX1251"      "WILDTRAX1252"     
#> [256] "WILDTRAX1253"      "WILDTRAX1271"      "WILDTRAX1272"     
#> [259] "WILDTRAX1273"      "WILDTRAX1274"      "WILDTRAX13"       
#> [262] "WILDTRAX1310"      "WILDTRAX1313"      "WILDTRAX1330"     
#> [265] "WILDTRAX1391"      "WILDTRAX1394"      "WILDTRAX1398"     
#> [268] "WILDTRAX141"       "WILDTRAX142"       "WILDTRAX1423"     
#> [271] "WILDTRAX1445"      "WILDTRAX1447"      "WILDTRAX1448"     
#> [274] "WILDTRAX1449"      "WILDTRAX145"       "WILDTRAX146"      
#> [277] "WILDTRAX1469"      "WILDTRAX147"       "WILDTRAX1475"     
#> [280] "WILDTRAX148"       "WILDTRAX1493"      "WILDTRAX150"      
#> [283] "WILDTRAX1505"      "WILDTRAX1508"      "WILDTRAX151"      
#> [286] "WILDTRAX1511"      "WILDTRAX1512"      "WILDTRAX152"      
#> [289] "WILDTRAX153"       "WILDTRAX154"       "WILDTRAX1579"     
#> [292] "WILDTRAX16"        "WILDTRAX161"       "WILDTRAX1651"     
#> [295] "WILDTRAX1652"      "WILDTRAX168"       "WILDTRAX169"      
#> [298] "WILDTRAX1725"      "WILDTRAX1735"      "WILDTRAX178"      
#> [301] "WILDTRAX1855"      "WILDTRAX19"        "WILDTRAX2085"     
#> [304] "WILDTRAX2086"      "WILDTRAX2147"      "WILDTRAX2148"     
#> [307] "WILDTRAX2160"      "WILDTRAX2161"      "WILDTRAX2170"     
#> [310] "WILDTRAX2176"      "WILDTRAX223"       "WILDTRAX2254"     
#> [313] "WILDTRAX2274"      "WILDTRAX2277"      "WILDTRAX2289"     
#> [316] "WILDTRAX2292"      "WILDTRAX2294"      "WILDTRAX2295"     
#> [319] "WILDTRAX2368"      "WILDTRAX2376"      "WILDTRAX2452"     
#> [322] "WILDTRAX2460"      "WILDTRAX25"        "WILDTRAX266"      
#> [325] "WILDTRAX282"       "WILDTRAX2821"      "WILDTRAX285"      
#> [328] "WILDTRAX2872"      "WILDTRAX2873"      "WILDTRAX2874"     
#> [331] "WILDTRAX2881"      "WILDTRAX2907"      "WILDTRAX3045"     
#> [334] "WILDTRAX3046"      "WILDTRAX3093"      "WILDTRAX31"       
#> [337] "WILDTRAX3135"      "WILDTRAX3139"      "WILDTRAX3154"     
#> [340] "WILDTRAX3155"      "WILDTRAX3156"      "WILDTRAX3157"     
#> [343] "WILDTRAX3175"      "WILDTRAX3190"      "WILDTRAX32"       
#> [346] "WILDTRAX3219"      "WILDTRAX3225"      "WILDTRAX3283"     
#> [349] "WILDTRAX33"        "WILDTRAX334"       "WILDTRAX3351"     
#> [352] "WILDTRAX34"        "WILDTRAX3436"      "WILDTRAX36"       
#> [355] "WILDTRAX3638"      "WILDTRAX3775"      "WILDTRAX379"      
#> [358] "WILDTRAX3805"      "WILDTRAX381"       "WILDTRAX3814"     
#> [361] "WILDTRAX387"       "WILDTRAX388"       "WILDTRAX3887"     
#> [364] "WILDTRAX3933"      "WILDTRAX41"        "WILDTRAX410"      
#> [367] "WILDTRAX44"        "WILDTRAX446"       "WILDTRAX462"      
#> [370] "WILDTRAX5"         "WILDTRAX516"       "WILDTRAX517"      
#> [373] "WILDTRAX518"       "WILDTRAX519"       "WILDTRAX541"      
#> [376] "WILDTRAX543"       "WILDTRAX592"       "WILDTRAX60"       
#> [379] "WILDTRAX605"       "WILDTRAX609"       "WILDTRAX620"      
#> [382] "WILDTRAX659"       "WILDTRAX662"       "WILDTRAX672"      
#> [385] "WILDTRAX686"       "WILDTRAX715"       "WILDTRAX718"      
#> [388] "WILDTRAX719"       "WILDTRAX772"       "WILDTRAX804"      
#> [391] "WILDTRAX815"       "WILDTRAX828"       "WILDTRAX84"       
#> [394] "WILDTRAX85"        "WILDTRAX86"        "WILDTRAX870"      
#> [397] "WILDTRAX871"       "WILDTRAX872"       "WILDTRAX873"      
#> [400] "WILDTRAX874"       "WILDTRAX875"       "WILDTRAX876"      
#> [403] "WILDTRAX878"       "WILDTRAX879"       "WILDTRAX880"      
#> [406] "WILDTRAX881"       "WILDTRAX882"       "WILDTRAX883"      
#> [409] "WILDTRAX885"       "WILDTRAX887"       "WILDTRAX888"      
#> [412] "WILDTRAX889"       "WILDTRAX890"       "WILDTRAX911"      
#> [415] "WILDTRAX912"       "WILDTRAX924"       "WILDTRAX932"      
#> [418] "WILDTRAX934"       "WILDTRAX935"       "WILDTRAX948"      
#> [421] "WILDTRAX949"       "WILDTRAX950"       "WILDTRAX951"      
#> [424] "WILDTRAX952"       "WILDTRAX953"       "WILDTRAX954"      
#> [427] "WILDTRAX955"       "WILDTRAX956"       "WILDTRAX957"      
#> [430] "WILDTRAX958"       "WILDTRAX963"       "WILDTRAX964"      
#> [433] "WILDTRAX965"       "WILDTRAX966"       "WILDTRAX967"      
#> [436] "WILDTRAX973"       "WILDTRAX99"        "WILDTRAX990"      
#> [439] "WILDTRAX996"       "WILDTRAX997"       "WMMM"             
#> [442] "WPWI"             
nc_permissions(username = "sample")
#> [1] "SAMPLE1" "SAMPLE2"
```
