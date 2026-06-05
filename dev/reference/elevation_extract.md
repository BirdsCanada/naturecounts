# Extract Terrain Tiles Elevation Data

Extracts [Mapzen Terrain Tiles elevation
data](https://github.com/tilezen/joerd/tree/master/docs) from a
`terra SpatRaster`, as delivered by
[`elevation_download()`](https://birdscanada.github.io/naturecounts/dev/reference/elevation_download.md).

## Usage

``` r
elevation_extract(data, elevation_data, site_name = NULL)
```

## Arguments

- data:

  An `sf` 'POINT' or 'POLYGON' object, or `terra` 'points' or 'polygons'
  object.

- site_name:

  Character. Optional argument to provide the name of the column
  containing site names if not contained within the BMDE column
  `SurveyAreaIdentifier`. Can be left `NULL` and still function properly
  if originally specified in a call to
  [`data_fmt()`](https://birdscanada.github.io/naturecounts/dev/reference/data_fmt.md)
  or
  [`elevation_download()`](https://birdscanada.github.io/naturecounts/dev/reference/elevation_download.md).

- \`terra:

  SpatRaster\`. Terrain Tiles elevation data. We reccommend using
  [`elevation_download()`](https://birdscanada.github.io/naturecounts/dev/reference/elevation_download.md)
  to ensure that all data necessary to match your input data are
  captured. Direct output of
  [`elevation_download()`](https://birdscanada.github.io/naturecounts/dev/reference/elevation_download.md)
  can be supplied here.

## Value

For sf 'POINT' or terra 'points' input data, original data with numeric
column `elevation` appended containing the elevation value (metres above
sea level) at each point.

For sf 'POLYGON' or terra 'polygons' input data, original data with
numeric column `elevation` appended containing the mean elevation value
(metres above sea level) within each polygon.

## Details

Users should be conscious of the final spatial resolution of their
elevation data, as this varies by latitude and zoom level specified in
[`elevation_download()`](https://birdscanada.github.io/naturecounts/dev/reference/elevation_download.md).
This can be accessed using
[`terra::res()`](https://rspatial.github.io/terra/reference/dimensions.html).

## See also

[`terra::extract()`](https://rspatial.github.io/terra/reference/extract.html)
which is used to extract values from Terrain Tiles data for `sf` 'POINT'
and `terra` 'points' input data.

[`exactextractr::exact_extract()`](https://isciences.gitlab.io/exactextractr/reference/exact_extract.html)
which is used to extract values from Terrain Tiles data for sf 'POLYGON'
or terra 'polygons' input data.

## Examples

``` r
# Using the included, test data on black-capped chickadees
bcch # look at the data
#>     record_id  collection project_id protocol_id protocol_type species_id
#> 1   968039498 RCBIOTABASE       1030          50            10      14280
#> 2   968039557 RCBIOTABASE       1030          50            10      14280
#> 3   968039593 RCBIOTABASE       1030          50            10      14280
#> 4   968039612 RCBIOTABASE       1030          50            10      14280
#> 5   968039703 RCBIOTABASE       1030          50            10      14280
#> 6   968039959 RCBIOTABASE       1030          50            10      14280
#> 7   968039980 RCBIOTABASE       1030          50            10      14280
#> 8   968040040 RCBIOTABASE       1030          50            10      14280
#> 9   968040072 RCBIOTABASE       1030          50            10      14280
#> 10  968040107 RCBIOTABASE       1030          50            10      14280
#> 11  968040161 RCBIOTABASE       1030          50            10      14280
#> 12  968040202 RCBIOTABASE       1030          50            10      14280
#> 13  968040273 RCBIOTABASE       1030          50            10      14280
#> 14  968040333 RCBIOTABASE       1030          50            10      14280
#> 15  968040389 RCBIOTABASE       1030          50            10      14280
#> 16  968040404 RCBIOTABASE       1030          50            10      14280
#> 17  968040431 RCBIOTABASE       1030          50            10      14280
#> 18  968040448 RCBIOTABASE       1030          50            10      14280
#> 19  968040452 RCBIOTABASE       1030          50            10      14280
#> 20  968040462 RCBIOTABASE       1030          50            10      14280
#> 21  968040472 RCBIOTABASE       1030          50            10      14280
#> 22  968040483 RCBIOTABASE       1030          50            10      14280
#> 23  968040493 RCBIOTABASE       1030          50            10      14280
#> 24  968040503 RCBIOTABASE       1030          50            10      14280
#> 25  968040528 RCBIOTABASE       1030          50            10      14280
#> 26  968040570 RCBIOTABASE       1030          50            10      14280
#> 27  968040595 RCBIOTABASE       1030          50            10      14280
#> 28  968040638 RCBIOTABASE       1030          50            10      14280
#> 29  968040662 RCBIOTABASE       1030          50            10      14280
#> 30  968040720 RCBIOTABASE       1030          50            10      14280
#> 31  968040760 RCBIOTABASE       1030          50            10      14280
#> 32  968040956 RCBIOTABASE       1030          50            10      14280
#> 33  968041021 RCBIOTABASE       1030          50            10      14280
#> 34  968041070 RCBIOTABASE       1030          50            10      14280
#> 35  968041107 RCBIOTABASE       1030          50            10      14280
#> 36  968041171 RCBIOTABASE       1030          50            10      14280
#> 37  968041210 RCBIOTABASE       1030          50            10      14280
#> 38  968041237 RCBIOTABASE       1030          50            10      14280
#> 39  968041291 RCBIOTABASE       1030          50            10      14280
#> 40  968041431 RCBIOTABASE       1030          50            10      14280
#> 41  968041507 RCBIOTABASE       1030          50            10      14280
#> 42  968041531 RCBIOTABASE       1030          50            10      14280
#> 43  968041556 RCBIOTABASE       1030          50            10      14280
#> 44  968041579 RCBIOTABASE       1030          50            10      14280
#> 45  968041607 RCBIOTABASE       1030          50            10      14280
#> 46  968041633 RCBIOTABASE       1030          50            10      14280
#> 47  968041655 RCBIOTABASE       1030          50            10      14280
#> 48  968041672 RCBIOTABASE       1030          50            10      14280
#> 49  968041702 RCBIOTABASE       1030          50            10      14280
#> 50  968041778 RCBIOTABASE       1030          50            10      14280
#> 51  968041833 RCBIOTABASE       1030          50            10      14280
#> 52  968042171 RCBIOTABASE       1030          50            10      14280
#> 53  968042200 RCBIOTABASE       1030          50            10      14280
#> 54  968042238 RCBIOTABASE       1030          50            10      14280
#> 55  968042275 RCBIOTABASE       1030          50            10      14280
#> 56  968042326 RCBIOTABASE       1030          50            10      14280
#> 57  968042376 RCBIOTABASE       1030          50            10      14280
#> 58  968042414 RCBIOTABASE       1030          50            10      14280
#> 59  968042453 RCBIOTABASE       1030          50            10      14280
#> 60  968042500 RCBIOTABASE       1030          50            10      14280
#> 61  968042553 RCBIOTABASE       1030          50            10      14280
#> 62  968042597 RCBIOTABASE       1030          50            10      14280
#> 63  968042644 RCBIOTABASE       1030          50            10      14280
#> 64  968042691 RCBIOTABASE       1030          50            10      14280
#> 65  968042735 RCBIOTABASE       1030          50            10      14280
#> 66  968042786 RCBIOTABASE       1030          50            10      14280
#> 67  968042826 RCBIOTABASE       1030          50            10      14280
#> 68  968042872 RCBIOTABASE       1030          50            10      14280
#> 69  968042913 RCBIOTABASE       1030          50            10      14280
#> 70  968042964 RCBIOTABASE       1030          50            10      14280
#> 71  968043019 RCBIOTABASE       1030          50            10      14280
#> 72  968043078 RCBIOTABASE       1030          50            10      14280
#> 73  968043130 RCBIOTABASE       1030          50            10      14280
#> 74  968043182 RCBIOTABASE       1030          50            10      14280
#> 75  968043234 RCBIOTABASE       1030          50            10      14280
#> 76  968043272 RCBIOTABASE       1030          50            10      14280
#> 77  968043314 RCBIOTABASE       1030          50            10      14280
#> 78  968043360 RCBIOTABASE       1030          50            10      14280
#> 79  968043408 RCBIOTABASE       1030          50            10      14280
#> 80  968043456 RCBIOTABASE       1030          50            10      14280
#> 81  968043509 RCBIOTABASE       1030          50            10      14280
#> 82  968043557 RCBIOTABASE       1030          50            10      14280
#> 83  968043605 RCBIOTABASE       1030          50            10      14280
#> 84  968043647 RCBIOTABASE       1030          50            10      14280
#> 85  968043663 RCBIOTABASE       1030          50            10      14280
#> 86  968043688 RCBIOTABASE       1030          50            10      14280
#> 87  968043728 RCBIOTABASE       1030          50            10      14280
#> 88  968043793 RCBIOTABASE       1030          50            10      14280
#> 89  968043884 RCBIOTABASE       1030          50            10      14280
#> 90  968044027 RCBIOTABASE       1030          50            10      14280
#> 91  968044086 RCBIOTABASE       1030          50            10      14280
#> 92  968044162 RCBIOTABASE       1030          50            10      14280
#> 93  968044257 RCBIOTABASE       1030          50            10      14280
#> 94  968044288 RCBIOTABASE       1030          50            10      14280
#> 95  968044330 RCBIOTABASE       1030          50            10      14280
#> 96  968044371 RCBIOTABASE       1030          50            10      14280
#> 97  968044431 RCBIOTABASE       1030          50            10      14280
#> 98  968044582 RCBIOTABASE       1030          50            10      14280
#> 99  968044658 RCBIOTABASE       1030          50            10      14280
#> 100 968044684 RCBIOTABASE       1030          50            10      14280
#> 101 968044718 RCBIOTABASE       1030          50            10      14280
#> 102 968044749 RCBIOTABASE       1030          50            10      14280
#> 103 968044786 RCBIOTABASE       1030          50            10      14280
#> 104 968044819 RCBIOTABASE       1030          50            10      14280
#> 105 968044854 RCBIOTABASE       1030          50            10      14280
#> 106 968044884 RCBIOTABASE       1030          50            10      14280
#> 107 968044916 RCBIOTABASE       1030          50            10      14280
#> 108 968045050 RCBIOTABASE       1030          50            10      14280
#> 109 968045684 RCBIOTABASE       1030          50            10      14280
#> 110 968045722 RCBIOTABASE       1030          50            10      14280
#> 111 968045822 RCBIOTABASE       1030          50            10      14280
#> 112 968045880 RCBIOTABASE       1030          50            10      14280
#> 113 968045907 RCBIOTABASE       1030          50            10      14280
#> 114 968046045 RCBIOTABASE       1030          50            10      14280
#> 115 968046698 RCBIOTABASE       1030          50            10      14280
#> 116 968047152 RCBIOTABASE       1030          50            10      14280
#> 117 968047260 RCBIOTABASE       1030          50            10      14280
#> 118 968047299 RCBIOTABASE       1030          50            10      14280
#> 119 968047339 RCBIOTABASE       1030          50            10      14280
#> 120 968047348 RCBIOTABASE       1030          50            10      14280
#> 121 968047364 RCBIOTABASE       1030          50            10      14280
#> 122 968047377 RCBIOTABASE       1030          50            10      14280
#> 123 968047383 RCBIOTABASE       1030          50            10      14280
#> 124 968047394 RCBIOTABASE       1030          50            10      14280
#> 125 968047445 RCBIOTABASE       1030          50            10      14280
#> 126 968047459 RCBIOTABASE       1030          50            10      14280
#> 127 968047992 RCBIOTABASE       1030          50            10      14280
#> 128 968048007 RCBIOTABASE       1030          50            10      14280
#> 129 968048019 RCBIOTABASE       1030          50            10      14280
#> 130 968048047 RCBIOTABASE       1030          50            10      14280
#> 131 968048060 RCBIOTABASE       1030          50            10      14280
#> 132 968048072 RCBIOTABASE       1030          50            10      14280
#> 133 968048093 RCBIOTABASE       1030          50            10      14280
#> 134 968048102 RCBIOTABASE       1030          50            10      14280
#> 135 968048137 RCBIOTABASE       1030          50            10      14280
#> 136 968048155 RCBIOTABASE       1030          50            10      14280
#> 137 968048196 RCBIOTABASE       1030          50            10      14280
#> 138 968048252 RCBIOTABASE       1030          50            10      14280
#> 139 968048272 RCBIOTABASE       1030          50            10      14280
#> 140 968048293 RCBIOTABASE       1030          50            10      14280
#> 141 968048317 RCBIOTABASE       1030          50            10      14280
#> 142 968048361 RCBIOTABASE       1030          50            10      14280
#> 143 968048383 RCBIOTABASE       1030          50            10      14280
#> 144 968048398 RCBIOTABASE       1030          50            10      14280
#> 145 968048505 RCBIOTABASE       1030          50            10      14280
#> 146 968048619 RCBIOTABASE       1030          50            10      14280
#> 147 968049310 RCBIOTABASE       1030          50            10      14280
#> 148 968049321 RCBIOTABASE       1030          50            10      14280
#> 149 968049381 RCBIOTABASE       1030          50            10      14280
#> 150 968049429 RCBIOTABASE       1030          50            10      14280
#> 151 968049442 RCBIOTABASE       1030          50            10      14280
#> 152 968049490 RCBIOTABASE       1030          50            10      14280
#> 153 968049580 RCBIOTABASE       1030          50            10      14280
#> 154 968049618 RCBIOTABASE       1030          50            10      14280
#> 155 968049645 RCBIOTABASE       1030          50            10      14280
#> 156 968049678 RCBIOTABASE       1030          50            10      14280
#> 157 968049697 RCBIOTABASE       1030          50            10      14280
#> 158 968049802 RCBIOTABASE       1030          50            10      14280
#> 159 968049962 RCBIOTABASE       1030          50            10      14280
#> 160 968050016 RCBIOTABASE       1030          50            10      14280
#>     statprov_code country_code SiteCode latitude longitude bcr
#> 1              ON           CA       NA 45.51110 -77.50533  12
#> 2              ON           CA       NA 45.63436 -77.07484  12
#> 3              ON           CA       NA 45.82732 -77.12012  13
#> 4              ON           CA       NA 45.48730 -77.74651  12
#> 5              ON           CA       NA 45.61956 -77.23577  12
#> 6              ON           CA       NA 45.82851 -77.11430  13
#> 7              ON           CA       NA 45.77252 -76.91258  13
#> 8              ON           CA       NA 45.61788 -77.10342  12
#> 9              ON           CA       NA 45.83092 -77.13265  13
#> 10             ON           CA       NA 45.79141 -76.90451  13
#> 11             ON           CA       NA 45.81554 -77.14251  13
#> 12             ON           CA       NA 45.81554 -77.14251  13
#> 13             ON           CA       NA 45.88427 -77.24522  12
#> 14             ON           CA       NA 45.82732 -77.12012  13
#> 15             ON           CA       NA 45.79141 -76.90451  13
#> 16             ON           CA       NA 45.82165 -77.30006  12
#> 17             ON           CA       NA 45.88427 -77.24522  12
#> 18             ON           CA       NA 45.81219 -77.18879  12
#> 19             ON           CA       NA 45.82165 -77.30006  12
#> 20             ON           CA       NA 45.82165 -77.30006  12
#> 21             ON           CA       NA 45.82165 -77.30006  12
#> 22             ON           CA       NA 45.82165 -77.30006  12
#> 23             ON           CA       NA 45.81899 -77.22032  12
#> 24             ON           CA       NA 45.81899 -77.22032  12
#> 25             ON           CA       NA 45.81899 -77.22032  12
#> 26             ON           CA       NA 45.81899 -77.22032  12
#> 27             ON           CA       NA 45.79464 -76.91172  13
#> 28             ON           CA       NA 45.80918 -77.36667  12
#> 29             ON           CA       NA 45.81899 -77.22032  12
#> 30             ON           CA       NA 45.62016 -77.39027  12
#> 31             ON           CA       NA 45.88406 -77.24634  12
#> 32             ON           CA       NA 45.79464 -76.91172  13
#> 33             ON           CA       NA 45.80918 -77.36667  12
#> 34             ON           CA       NA 45.88633 -77.31328  12
#> 35             ON           CA       NA 45.79464 -76.91172  13
#> 36             ON           CA       NA 45.81522 -76.89571  13
#> 37             ON           CA       NA 45.79170 -76.90276  13
#> 38             ON           CA       NA 45.79464 -76.91172  13
#> 39             ON           CA       NA 45.60059 -77.10754  12
#> 40             ON           CA       NA 45.81553 -77.14251  13
#> 41             ON           CA       NA 45.81553 -77.14251  13
#> 42             ON           CA       NA 45.80918 -77.36667  12
#> 43             ON           CA       NA 45.81899 -77.22032  12
#> 44             ON           CA       NA 45.81899 -77.22032  12
#> 45             ON           CA       NA 45.81899 -77.22032  12
#> 46             ON           CA       NA 45.81899 -77.22032  12
#> 47             ON           CA       NA 45.81899 -77.22032  12
#> 48             ON           CA       NA 45.81899 -77.22032  12
#> 49             ON           CA       NA 45.81899 -77.22032  12
#> 50             ON           CA       NA 45.99577 -77.42108  12
#> 51             ON           CA       NA 45.81899 -77.22032  12
#> 52             ON           CA       NA 45.81553 -77.14251  13
#> 53             ON           CA       NA 45.81553 -77.14251  13
#> 54             ON           CA       NA 45.81553 -77.14251  13
#> 55             ON           CA       NA 45.81553 -77.14251  13
#> 56             ON           CA       NA 45.57893 -77.10279  12
#> 57             ON           CA       NA 45.81553 -77.14251  13
#> 58             ON           CA       NA 45.81553 -77.14251  13
#> 59             ON           CA       NA 45.81553 -77.14251  13
#> 60             ON           CA       NA 45.81553 -77.14251  13
#> 61             ON           CA       NA 45.81553 -77.14251  13
#> 62             ON           CA       NA 45.81553 -77.14251  13
#> 63             ON           CA       NA 45.81553 -77.14251  13
#> 64             ON           CA       NA 45.81553 -77.14251  13
#> 65             ON           CA       NA 45.81553 -77.14251  13
#> 66             ON           CA       NA 45.81553 -77.14251  13
#> 67             ON           CA       NA 45.81553 -77.14251  13
#> 68             ON           CA       NA 45.81553 -77.14251  13
#> 69             ON           CA       NA 45.81553 -77.14251  13
#> 70             ON           CA       NA 45.81553 -77.14251  13
#> 71             ON           CA       NA 45.81553 -77.14251  13
#> 72             ON           CA       NA 45.81553 -77.14251  13
#> 73             ON           CA       NA 45.81553 -77.14251  13
#> 74             ON           CA       NA 45.81553 -77.14251  13
#> 75             ON           CA       NA 45.81553 -77.14251  13
#> 76             ON           CA       NA 45.81553 -77.14251  13
#> 77             ON           CA       NA 45.81553 -77.14251  13
#> 78             ON           CA       NA 45.81553 -77.14251  13
#> 79             ON           CA       NA 45.81553 -77.14251  13
#> 80             ON           CA       NA 45.81553 -77.14251  13
#> 81             ON           CA       NA 45.81553 -77.14251  13
#> 82             ON           CA       NA 45.81553 -77.14251  13
#> 83             ON           CA       NA 45.81553 -77.14251  13
#> 84             ON           CA       NA 45.81899 -77.22032  12
#> 85             ON           CA       NA 45.61404 -77.11647  12
#> 86             ON           CA       NA 45.82732 -77.12013  13
#> 87             ON           CA       NA 45.79170 -76.90276  13
#> 88             ON           CA       NA 45.79170 -76.90276  13
#> 89             ON           CA       NA 45.79170 -76.90276  13
#> 90             ON           CA       NA 45.79170 -76.90276  13
#> 91             ON           CA       NA 45.79464 -76.91172  13
#> 92             ON           CA       NA 45.79170 -76.90276  13
#> 93             ON           CA       NA 45.79464 -76.91172  13
#> 94             ON           CA       NA 45.61908 -77.11236  12
#> 95             ON           CA       NA 45.79170 -76.90276  13
#> 96             ON           CA       NA 45.88406 -77.24634  12
#> 97             ON           CA       NA 45.79170 -76.90276  13
#> 98             ON           CA       NA 45.79170 -76.90276  13
#> 99             ON           CA       NA 45.55757 -77.41739  12
#> 100            ON           CA       NA 45.55757 -77.41739  12
#> 101            ON           CA       NA 45.55757 -77.41739  12
#> 102            ON           CA       NA 45.55757 -77.41739  12
#> 103            ON           CA       NA 45.55757 -77.41739  12
#> 104            ON           CA       NA 45.55757 -77.41739  12
#> 105            ON           CA       NA 45.55757 -77.41739  12
#> 106            ON           CA       NA 45.55757 -77.41739  12
#> 107            ON           CA       NA 45.55757 -77.41739  12
#> 108            ON           CA       NA 46.05060 -77.52416  12
#> 109            ON           CA       NA 45.88406 -77.24634  12
#> 110            ON           CA       NA 45.79170 -76.90276  13
#> 111            ON           CA       NA 45.79170 -76.90276  13
#> 112            ON           CA       NA 45.79170 -76.90276  13
#> 113            ON           CA       NA 45.88406 -77.24634  12
#> 114            ON           CA       NA 45.81899 -77.22032  12
#> 115            ON           CA       NA 45.81899 -77.22032  12
#> 116            ON           CA       NA 45.51110 -77.50533  12
#> 117            ON           CA       NA 45.81899 -77.22032  12
#> 118            ON           CA       NA 45.51110 -77.50533  12
#> 119            ON           CA       NA 45.81899 -77.22032  12
#> 120            ON           CA       NA 45.79087 -76.78371  13
#> 121            ON           CA       NA 45.81899 -77.22032  12
#> 122            ON           CA       NA 45.80918 -77.36667  12
#> 123            ON           CA       NA 45.81899 -77.22032  12
#> 124            ON           CA       NA 45.81899 -77.22032  12
#> 125            ON           CA       NA 45.88406 -77.24634  12
#> 126            ON           CA       NA 45.82165 -77.30006  12
#> 127            ON           CA       NA 45.81899 -77.22032  12
#> 128            ON           CA       NA 45.66671 -76.94713  13
#> 129            ON           CA       NA 45.80918 -77.36667  12
#> 130            ON           CA       NA 45.81899 -77.22032  12
#> 131            ON           CA       NA 45.81899 -77.22032  12
#> 132            ON           CA       NA 45.81899 -77.22032  12
#> 133            ON           CA       NA 45.81899 -77.22032  12
#> 134            ON           CA       NA 45.82165 -77.30006  12
#> 135            ON           CA       NA 45.81899 -77.22032  12
#> 136            ON           CA       NA 45.81899 -77.22032  12
#> 137            ON           CA       NA 45.81899 -77.22032  12
#> 138            ON           CA       NA 45.82116 -77.11359  13
#> 139            ON           CA       NA 45.81899 -77.22032  12
#> 140            ON           CA       NA 45.81899 -77.22032  12
#> 141            ON           CA       NA 45.43835 -76.33639  13
#> 142            ON           CA       NA 45.82116 -77.11359  13
#> 143            ON           CA       NA 45.63346 -77.06715  12
#> 144            ON           CA       NA 45.81899 -77.22032  12
#> 145            ON           CA       NA 46.08577 -77.46034  12
#> 146            ON           CA       NA 46.08577 -77.46034  12
#> 147            ON           CA       NA 45.81899 -77.22032  12
#> 148            ON           CA       NA 45.81899 -77.22032  12
#> 149            ON           CA       NA 45.63346 -77.06715  12
#> 150            ON           CA       NA 45.83865 -77.23486  12
#> 151            ON           CA       NA 45.82165 -77.30006  12
#> 152            ON           CA       NA 45.81289 -77.03902  13
#> 153            ON           CA       NA 45.88406 -77.24634  12
#> 154            ON           CA       NA 46.16771 -77.62982  12
#> 155            ON           CA       NA 45.61404 -77.11647  12
#> 156            ON           CA       NA 45.61404 -77.11647  12
#> 157            ON           CA       NA 45.63285 -77.06741  12
#> 158            ON           CA       NA 45.62641 -77.03477  12
#> 159            ON           CA       NA 45.81899 -77.22032  12
#> 160            ON           CA       NA 45.82165 -77.30006  12
#>     subnational2_code iba_site utm_square survey_year survey_month survey_week
#> 1            CA.ON.RE      N/A    18TUR04        2011            2           2
#> 2            CA.ON.RE      N/A    18TUR35        2010            7           1
#> 3            CA.ON.RE      N/A    18TUR37        2010            6           3
#> 4            CA.ON.RE      N/A    18TTR84        2011            5           1
#> 5            CA.ON.RE      N/A    18TUR25        2010            6           2
#> 6            CA.ON.RE      N/A    18TUR37        2011           10           1
#> 7            CA.ON.RE      N/A    18TUR57        2011           10           1
#> 8            CA.ON.RE      N/A    18TUR35        2011           10           4
#> 9            CA.ON.RE      N/A    18TUR37        2011           11           2
#> 10           CA.ON.RE      N/A    18TUR57        2002            4           3
#> 11           CA.ON.RE      N/A    18TUR37        2002           12           2
#> 12           CA.ON.RE      N/A    18TUR37        2009           12           3
#> 13           CA.ON.RE      N/A    18TUR28        2002            5           2
#> 14           CA.ON.RE      N/A    18TUR37        2002            8           3
#> 15           CA.ON.RE      N/A    18TUR57        2010            4           3
#> 16           CA.ON.RE      N/A    18TUR27        2011            2           3
#> 17           CA.ON.RE      N/A    18TUR28        2003            5           3
#> 18           CA.ON.RE      N/A    18TUR27        2011            2           3
#> 19           CA.ON.RE      N/A    18TUR27        2010            2           2
#> 20           CA.ON.RE      N/A    18TUR27        2009            2           2
#> 21           CA.ON.RE      N/A    18TUR27        2008            2           2
#> 22           CA.ON.RE      N/A    18TUR27        2007            2           3
#> 23           CA.ON.RE      N/A    18TUR27        2011           12           3
#> 24           CA.ON.RE      N/A    18TUR27        2011           12           4
#> 25           CA.ON.RE      N/A    18TUR27        2011           12           4
#> 26           CA.ON.RE      N/A    18TUR27        2012            4           3
#> 27           CA.ON.RE      N/A    18TUR57        2012            4           3
#> 28           CA.ON.RE      N/A    18TUR17        2011            5           3
#> 29           CA.ON.RE      N/A    18TUR27        2012            4           4
#> 30           CA.ON.RE      N/A    18TUR15        2011            6           4
#> 31           CA.ON.RE      N/A    18TUR28        2012            5           3
#> 32           CA.ON.RE      N/A    18TUR57        2012            7           1
#> 33           CA.ON.RE      N/A    18TUR17        2012            7           1
#> 34           CA.ON.RE      N/A    18TUR28        2012            7           2
#> 35           CA.ON.RE      N/A    18TUR57        2012            7           4
#> 36           CA.ON.RE      N/A    18TUR57        2012            9           4
#> 37           CA.ON.RE      N/A    18TUR57        2011            4           2
#> 38           CA.ON.RE      N/A    18TUR57        2012           10           1
#> 39           CA.ON.RE      N/A    18TUR35        2012           10           4
#> 40           CA.ON.RE      N/A    18TUR37        1978           12           2
#> 41           CA.ON.RE      N/A    18TUR37        1979           12           2
#> 42           CA.ON.RE      N/A    18TUR17        2012            2           3
#> 43           CA.ON.RE      N/A    18TUR27        2012            3           4
#> 44           CA.ON.RE      N/A    18TUR27        2012            4           2
#> 45           CA.ON.RE      N/A    18TUR27        2012            4           3
#> 46           CA.ON.RE      N/A    18TUR27        2012            4           3
#> 47           CA.ON.RE      N/A    18TUR27        2012            4           3
#> 48           CA.ON.RE      N/A    18TUR27        2012            4           4
#> 49           CA.ON.RE      N/A    18TUR27        2012            5           2
#> 50           CA.ON.RE      N/A    18TUR19        2012            7           1
#> 51           CA.ON.RE      N/A    18TUR27        2012            5           1
#> 52           CA.ON.RE      N/A    18TUR37        1980           12           3
#> 53           CA.ON.RE      N/A    18TUR37        1981           12           3
#> 54           CA.ON.RE      N/A    18TUR37        1982           12           3
#> 55           CA.ON.RE      N/A    18TUR37        1983           12           3
#> 56           CA.ON.RE      N/A    18TUR34        1984           12           2
#> 57           CA.ON.RE      N/A    18TUR37        1985           12           3
#> 58           CA.ON.RE      N/A    18TUR37        1986           12           3
#> 59           CA.ON.RE      N/A    18TUR37        1987           12           3
#> 60           CA.ON.RE      N/A    18TUR37        1988           12           3
#> 61           CA.ON.RE      N/A    18TUR37        1989           12           2
#> 62           CA.ON.RE      N/A    18TUR37        1990           12           2
#> 63           CA.ON.RE      N/A    18TUR37        1991           12           2
#> 64           CA.ON.RE      N/A    18TUR37        1992           12           3
#> 65           CA.ON.RE      N/A    18TUR37        1993           12           3
#> 66           CA.ON.RE      N/A    18TUR37        1994           12           3
#> 67           CA.ON.RE      N/A    18TUR37        1995           12           2
#> 68           CA.ON.RE      N/A    18TUR37        1996           12           3
#> 69           CA.ON.RE      N/A    18TUR37        1997           12           3
#> 70           CA.ON.RE      N/A    18TUR37        1998           12           3
#> 71           CA.ON.RE      N/A    18TUR37        1999           11           3
#> 72           CA.ON.RE      N/A    18TUR37        2000           12           2
#> 73           CA.ON.RE      N/A    18TUR37        2001           12           2
#> 74           CA.ON.RE      N/A    18TUR37        2002           12           2
#> 75           CA.ON.RE      N/A    18TUR37        2003           12           3
#> 76           CA.ON.RE      N/A    18TUR37        2004           12           3
#> 77           CA.ON.RE      N/A    18TUR37        2005           12           3
#> 78           CA.ON.RE      N/A    18TUR37        2006           12           3
#> 79           CA.ON.RE      N/A    18TUR37        2007           12           2
#> 80           CA.ON.RE      N/A    18TUR37        2008           12           3
#> 81           CA.ON.RE      N/A    18TUR37        2009           12           3
#> 82           CA.ON.RE      N/A    18TUR37        2010           12           3
#> 83           CA.ON.RE      N/A    18TUR37        2011           12           3
#> 84           CA.ON.RE      N/A    18TUR27        2012           11           3
#> 85           CA.ON.RE      N/A    18TUR35        2012           11           3
#> 86           CA.ON.RE      N/A    18TUR37        2002            8           3
#> 87           CA.ON.RE      N/A    18TUR57        2003            4           4
#> 88           CA.ON.RE      N/A    18TUR57        2004            4           3
#> 89           CA.ON.RE      N/A    18TUR57        2005            4           3
#> 90           CA.ON.RE      N/A    18TUR57        2007            4           3
#> 91           CA.ON.RE      N/A    18TUR57        2007            9           1
#> 92           CA.ON.RE      N/A    18TUR57        2008            4           3
#> 93           CA.ON.RE      N/A    18TUR57        2008           10           1
#> 94           CA.ON.RE      N/A    18TUR35        2008           10           4
#> 95           CA.ON.RE      N/A    18TUR57        2009            4           3
#> 96           CA.ON.RE      N/A    18TUR28        2009            5           1
#> 97           CA.ON.RE      N/A    18TUR57        2010            4           3
#> 98           CA.ON.RE      N/A    18TUR57        2012            4           3
#> 99           CA.ON.RE      N/A    18TUR14        1996           12           3
#> 100          CA.ON.RE      N/A    18TUR14        1997           12           3
#> 101          CA.ON.RE      N/A    18TUR14        1998           12           3
#> 102          CA.ON.RE      N/A    18TUR14        1999           12           3
#> 103          CA.ON.RE      N/A    18TUR14        2000           12           3
#> 104          CA.ON.RE      N/A    18TUR14        2001           12           3
#> 105          CA.ON.RE      N/A    18TUR14        2002           12           3
#> 106          CA.ON.RE      N/A    18TUR14        2003           12           4
#> 107          CA.ON.RE      N/A    18TUR14        2004           12           4
#> 108          CA.ON.RE      N/A    18TUS00        2012           12           1
#> 109          CA.ON.RE      N/A    18TUR28        2001            5           2
#> 110          CA.ON.RE      N/A    18TUR57        2001            3           3
#> 111          CA.ON.RE      N/A    18TUR57        2000            4           2
#> 112          CA.ON.RE      N/A    18TUR57        1999            4           3
#> 113          CA.ON.RE      N/A    18TUR28        1999            5           2
#> 114          CA.ON.RE      N/A    18TUR27        2013            2           1
#> 115          CA.ON.RE      N/A    18TUR27        2013            2           1
#> 116          CA.ON.RE      N/A    18TUR04        2012            5           3
#> 117          CA.ON.RE      N/A    18TUR27        2013            2           2
#> 118          CA.ON.RE      N/A    18TUR04        2012           12           4
#> 119          CA.ON.RE      N/A    18TUR27        2013            3           2
#> 120          CA.ON.RE      N/A    18TUR67        2013            3           2
#> 121          CA.ON.RE      N/A    18TUR27        2013            3           2
#> 122          CA.ON.RE      N/A    18TUR17        2013            3           2
#> 123          CA.ON.RE      N/A    18TUR27        2013            3           2
#> 124          CA.ON.RE      N/A    18TUR27        2013            3           2
#> 125          CA.ON.RE      N/A    18TUR28        1996            5           2
#> 126          CA.ON.RE      N/A    18TUR27        2013            3           3
#> 127          CA.ON.RE      N/A    18TUR27        2013            3           4
#> 128          CA.ON.RE      N/A    18TUR45        2013            3           4
#> 129          CA.ON.RE      N/A    18TUR17        2013            3           4
#> 130          CA.ON.RE      N/A    18TUR27        2013            3           4
#> 131          CA.ON.RE      N/A    18TUR27        2013            4           1
#> 132          CA.ON.RE      N/A    18TUR27        2013            4           1
#> 133          CA.ON.RE      N/A    18TUR27        2013            4           1
#> 134          CA.ON.RE      N/A    18TUR27        2013            2           4
#> 135          CA.ON.RE      N/A    18TUR27        2013            4           3
#> 136          CA.ON.RE      N/A    18TUR27        2013            4           3
#> 137          CA.ON.RE      N/A    18TUR27        2013            4           3
#> 138          CA.ON.RE      N/A    18TUR37        2013            3           4
#> 139          CA.ON.RE      N/A    18TUR27        2013            4           3
#> 140          CA.ON.RE      N/A    18TUR27        2013            5           1
#> 141          CA.ON.RE      N/A    18TUR93        2013            5           1
#> 142          CA.ON.RE      N/A    18TUR37        2013            5           1
#> 143          CA.ON.RE      N/A    18TUR35        2013            5           2
#> 144          CA.ON.RE      N/A    18TUR27        2013            2           3
#> 145          CA.ON.RE      N/A    18TUS00        2013            7           4
#> 146          CA.ON.RE      N/A    18TUS00        2013            7           4
#> 147          CA.ON.RE      N/A    18TUR27        2013            4           2
#> 148          CA.ON.RE      N/A    18TUR27        2013            4           2
#> 149          CA.ON.RE      N/A    18TUR35        2013            5           2
#> 150          CA.ON.RE      N/A    18TUR27        2014            2           3
#> 151          CA.ON.RE      N/A    18TUR27        2014            2           2
#> 152          CA.QC.PN      N/A    18TUR47        2014            5           2
#> 153          CA.ON.RE      N/A    18TUR28        2014            5           3
#> 154          CA.ON.RE      N/A    18TTS91        2014            9           3
#> 155          CA.ON.RE      N/A    18TUR35        2014            9           4
#> 156          CA.ON.RE      N/A    18TUR35        2014            9           4
#> 157          CA.ON.RE      N/A    18TUR35        2014            9           4
#> 158          CA.ON.RE      N/A    18TUR45        2014            8           3
#> 159          CA.ON.RE      N/A    18TUR27        2015            2           3
#> 160          CA.ON.RE      N/A    18TUR27        2017            1           4
#>     survey_day breeding_rank
#> 1           12            NA
#> 2            3            NA
#> 3           19            NA
#> 4            5            NA
#> 5           16            NA
#> 6            2            NA
#> 7            1            NA
#> 8           29            NA
#> 9           14            NA
#> 10          20            NA
#> 11          14            NA
#> 12          19            NA
#> 13          12            NA
#> 14          17            NA
#> 15          17            NA
#> 16          18            NA
#> 17          18            NA
#> 18          20            NA
#> 19          14            NA
#> 20          15            NA
#> 21          15            NA
#> 22          18            NA
#> 23          23            NA
#> 24          29            NA
#> 25          31            NA
#> 26          22            NA
#> 27          22            NA
#> 28          23            NA
#> 29          27            NA
#> 30          28            NA
#> 31          20            NA
#> 32           2            NA
#> 33           4            NA
#> 34          16            NA
#> 35          26            NA
#> 36          29            NA
#> 37          16            NA
#> 38           6            NA
#> 39          27            NA
#> 40          16            NA
#> 41          15            NA
#> 42          18            NA
#> 43          28            NA
#> 44          14            NA
#> 45          19            NA
#> 46          21            NA
#> 47          24            NA
#> 48          28            NA
#> 49          12            NA
#> 50           5            NA
#> 51           5            NA
#> 52          20            NA
#> 53          19            NA
#> 54          18            NA
#> 55          17            NA
#> 56          15            NA
#> 57          21            NA
#> 58          20            NA
#> 59          21            NA
#> 60          17            NA
#> 61          16            NA
#> 62          15            NA
#> 63          14            NA
#> 64          19            NA
#> 65          18            NA
#> 66          17            NA
#> 67          16            NA
#> 68          21            NA
#> 69          20            NA
#> 70          19            NA
#> 71          18            NA
#> 72          16            NA
#> 73          15            NA
#> 74          14            NA
#> 75          20            NA
#> 76          18            NA
#> 77          17            NA
#> 78          17            NA
#> 79          15            NA
#> 80          20            NA
#> 81          19            NA
#> 82          18            NA
#> 83          17            NA
#> 84          18            NA
#> 85          18            NA
#> 86          17            NA
#> 87          26            NA
#> 88          24            NA
#> 89          23            NA
#> 90          21            NA
#> 91           8            NA
#> 92          19            NA
#> 93           4            NA
#> 94          25            NA
#> 95          18            NA
#> 96           3            NA
#> 97          17            NA
#> 98          21            NA
#> 99          19            NA
#> 100         19            NA
#> 101         19            NA
#> 102         19            NA
#> 103         19            NA
#> 104         19            NA
#> 105         21            NA
#> 106         27            NA
#> 107         27            NA
#> 108          1            NA
#> 109         13            NA
#> 110         21            NA
#> 111         15            NA
#> 112         17            NA
#> 113         16            NA
#> 114          3            NA
#> 115          8            NA
#> 116         23            NA
#> 117          9            NA
#> 118         26            NA
#> 119         10            NA
#> 120         10            NA
#> 121         11            NA
#> 122         12            NA
#> 123         12            NA
#> 124         15            NA
#> 125         12            NA
#> 126         21            NA
#> 127         27            NA
#> 128         30            NA
#> 129         30            NA
#> 130         30            NA
#> 131          1            NA
#> 132          5            NA
#> 133          6            NA
#> 134         25            NA
#> 135         18            NA
#> 136         19            NA
#> 137         21            NA
#> 138         29            NA
#> 139         22            NA
#> 140          1            NA
#> 141          1            NA
#> 142          3            NA
#> 143         12            NA
#> 144         23            NA
#> 145         27            NA
#> 146         27            NA
#> 147         12            NA
#> 148         13            NA
#> 149         12            NA
#> 150         17            NA
#> 151         16            NA
#> 152         12            NA
#> 153         24            NA
#> 154         20            NA
#> 155         27            NA
#> 156         27            NA
#> 157         27            NA
#> 158         24            NA
#> 159         24            NA
#> 160         30            NA
#>                                  GlobalUniqueIdentifier    CatalogNumber
#> 1     URN:catalog:PEMBROKEFN:RCBIOTABASE:5565-1-14280-1   5565-1-14280-1
#> 2     URN:catalog:PEMBROKEFN:RCBIOTABASE:5650-1-14280-3   5650-1-14280-3
#> 3     URN:catalog:PEMBROKEFN:RCBIOTABASE:5022-1-14280-8   5022-1-14280-8
#> 4     URN:catalog:PEMBROKEFN:RCBIOTABASE:5936-1-14280-6   5936-1-14280-6
#> 5    URN:catalog:PEMBROKEFN:RCBIOTABASE:4976-1-14280-14  4976-1-14280-14
#> 6     URN:catalog:PEMBROKEFN:RCBIOTABASE:6797-1-14280-6   6797-1-14280-6
#> 7    URN:catalog:PEMBROKEFN:RCBIOTABASE:6798-1-14280-15  6798-1-14280-15
#> 8    URN:catalog:PEMBROKEFN:RCBIOTABASE:6842-1-14280-17  6842-1-14280-17
#> 9     URN:catalog:PEMBROKEFN:RCBIOTABASE:6894-1-14280-6   6894-1-14280-6
#> 10   URN:catalog:PEMBROKEFN:RCBIOTABASE:6956-1-14280-28  6956-1-14280-28
#> 11   URN:catalog:PEMBROKEFN:RCBIOTABASE:6957-1-14280-35  6957-1-14280-35
#> 12   URN:catalog:PEMBROKEFN:RCBIOTABASE:6958-1-14280-32  6958-1-14280-32
#> 13   URN:catalog:PEMBROKEFN:RCBIOTABASE:6961-1-14280-21  6961-1-14280-21
#> 14   URN:catalog:PEMBROKEFN:RCBIOTABASE:6964-1-14280-22  6964-1-14280-22
#> 15   URN:catalog:PEMBROKEFN:RCBIOTABASE:6972-1-14280-36  6972-1-14280-36
#> 16    URN:catalog:PEMBROKEFN:RCBIOTABASE:6973-1-14280-5   6973-1-14280-5
#> 17   URN:catalog:PEMBROKEFN:RCBIOTABASE:6979-1-14280-21  6979-1-14280-21
#> 18    URN:catalog:PEMBROKEFN:RCBIOTABASE:6980-1-14280-2   6980-1-14280-2
#> 19    URN:catalog:PEMBROKEFN:RCBIOTABASE:6983-1-14280-4   6983-1-14280-4
#> 20    URN:catalog:PEMBROKEFN:RCBIOTABASE:6986-1-14280-6   6986-1-14280-6
#> 21    URN:catalog:PEMBROKEFN:RCBIOTABASE:6988-1-14280-4   6988-1-14280-4
#> 22    URN:catalog:PEMBROKEFN:RCBIOTABASE:6989-1-14280-7   6989-1-14280-7
#> 23    URN:catalog:PEMBROKEFN:RCBIOTABASE:7044-1-14280-4   7044-1-14280-4
#> 24    URN:catalog:PEMBROKEFN:RCBIOTABASE:7051-1-14280-7   7051-1-14280-7
#> 25    URN:catalog:PEMBROKEFN:RCBIOTABASE:7100-1-14280-7   7100-1-14280-7
#> 26   URN:catalog:PEMBROKEFN:RCBIOTABASE:7543-1-14280-10  7543-1-14280-10
#> 27   URN:catalog:PEMBROKEFN:RCBIOTABASE:7544-1-14280-14  7544-1-14280-14
#> 28   URN:catalog:PEMBROKEFN:RCBIOTABASE:5927-1-14280-29  5927-1-14280-29
#> 29   URN:catalog:PEMBROKEFN:RCBIOTABASE:7607-1-14280-11  7607-1-14280-11
#> 30   URN:catalog:PEMBROKEFN:RCBIOTABASE:6207-1-14280-24  6207-1-14280-24
#> 31   URN:catalog:PEMBROKEFN:RCBIOTABASE:7783-1-14280-25  7783-1-14280-25
#> 32   URN:catalog:PEMBROKEFN:RCBIOTABASE:7991-1-14280-26  7991-1-14280-26
#> 33   URN:catalog:PEMBROKEFN:RCBIOTABASE:8020-1-14280-26  8020-1-14280-26
#> 34    URN:catalog:PEMBROKEFN:RCBIOTABASE:8068-1-14280-9   8068-1-14280-9
#> 35   URN:catalog:PEMBROKEFN:RCBIOTABASE:8221-1-14280-22  8221-1-14280-22
#> 36   URN:catalog:PEMBROKEFN:RCBIOTABASE:8652-1-14280-15  8652-1-14280-15
#> 37   URN:catalog:PEMBROKEFN:RCBIOTABASE:8727-1-14280-25  8727-1-14280-25
#> 38   URN:catalog:PEMBROKEFN:RCBIOTABASE:8755-1-14280-11  8755-1-14280-11
#> 39   URN:catalog:PEMBROKEFN:RCBIOTABASE:8826-1-14280-22  8826-1-14280-22
#> 40   URN:catalog:PEMBROKEFN:RCBIOTABASE:8928-1-14280-20  8928-1-14280-20
#> 41   URN:catalog:PEMBROKEFN:RCBIOTABASE:8930-1-14280-20  8930-1-14280-20
#> 42    URN:catalog:PEMBROKEFN:RCBIOTABASE:7212-1-14280-7   7212-1-14280-7
#> 43   URN:catalog:PEMBROKEFN:RCBIOTABASE:7418-1-14280-16  7418-1-14280-16
#> 44   URN:catalog:PEMBROKEFN:RCBIOTABASE:7484-1-14280-16  7484-1-14280-16
#> 45   URN:catalog:PEMBROKEFN:RCBIOTABASE:7514-1-14280-14  7514-1-14280-14
#> 46   URN:catalog:PEMBROKEFN:RCBIOTABASE:7526-1-14280-12  7526-1-14280-12
#> 47   URN:catalog:PEMBROKEFN:RCBIOTABASE:7568-1-14280-13  7568-1-14280-13
#> 48   URN:catalog:PEMBROKEFN:RCBIOTABASE:7606-1-14280-10  7606-1-14280-10
#> 49   URN:catalog:PEMBROKEFN:RCBIOTABASE:7744-1-14280-12  7744-1-14280-12
#> 50   URN:catalog:PEMBROKEFN:RCBIOTABASE:8028-1-14280-30  8028-1-14280-30
#> 51   URN:catalog:PEMBROKEFN:RCBIOTABASE:7706-1-14280-18  7706-1-14280-18
#> 52   URN:catalog:PEMBROKEFN:RCBIOTABASE:8948-1-14280-19  8948-1-14280-19
#> 53   URN:catalog:PEMBROKEFN:RCBIOTABASE:8949-1-14280-16  8949-1-14280-16
#> 54   URN:catalog:PEMBROKEFN:RCBIOTABASE:8950-1-14280-26  8950-1-14280-26
#> 55   URN:catalog:PEMBROKEFN:RCBIOTABASE:8951-1-14280-24  8951-1-14280-24
#> 56   URN:catalog:PEMBROKEFN:RCBIOTABASE:8952-1-14280-36  8952-1-14280-36
#> 57   URN:catalog:PEMBROKEFN:RCBIOTABASE:8954-1-14280-29  8954-1-14280-29
#> 58   URN:catalog:PEMBROKEFN:RCBIOTABASE:8955-1-14280-22  8955-1-14280-22
#> 59   URN:catalog:PEMBROKEFN:RCBIOTABASE:8956-1-14280-14  8956-1-14280-14
#> 60   URN:catalog:PEMBROKEFN:RCBIOTABASE:8957-1-14280-12  8957-1-14280-12
#> 61   URN:catalog:PEMBROKEFN:RCBIOTABASE:8958-1-14280-24  8958-1-14280-24
#> 62   URN:catalog:PEMBROKEFN:RCBIOTABASE:8959-1-14280-29  8959-1-14280-29
#> 63   URN:catalog:PEMBROKEFN:RCBIOTABASE:8960-1-14280-29  8960-1-14280-29
#> 64   URN:catalog:PEMBROKEFN:RCBIOTABASE:8962-1-14280-29  8962-1-14280-29
#> 65   URN:catalog:PEMBROKEFN:RCBIOTABASE:8963-1-14280-23  8963-1-14280-23
#> 66   URN:catalog:PEMBROKEFN:RCBIOTABASE:8964-1-14280-31  8964-1-14280-31
#> 67   URN:catalog:PEMBROKEFN:RCBIOTABASE:8965-1-14280-24  8965-1-14280-24
#> 68   URN:catalog:PEMBROKEFN:RCBIOTABASE:8966-1-14280-26  8966-1-14280-26
#> 69   URN:catalog:PEMBROKEFN:RCBIOTABASE:8967-1-14280-27  8967-1-14280-27
#> 70   URN:catalog:PEMBROKEFN:RCBIOTABASE:8968-1-14280-32  8968-1-14280-32
#> 71   URN:catalog:PEMBROKEFN:RCBIOTABASE:8969-1-14280-33  8969-1-14280-33
#> 72   URN:catalog:PEMBROKEFN:RCBIOTABASE:8974-1-14280-32  8974-1-14280-32
#> 73   URN:catalog:PEMBROKEFN:RCBIOTABASE:8975-1-14280-38  8975-1-14280-38
#> 74   URN:catalog:PEMBROKEFN:RCBIOTABASE:8976-1-14280-29  8976-1-14280-29
#> 75   URN:catalog:PEMBROKEFN:RCBIOTABASE:8977-1-14280-39  8977-1-14280-39
#> 76   URN:catalog:PEMBROKEFN:RCBIOTABASE:8980-1-14280-29  8980-1-14280-29
#> 77   URN:catalog:PEMBROKEFN:RCBIOTABASE:8981-1-14280-24  8981-1-14280-24
#> 78   URN:catalog:PEMBROKEFN:RCBIOTABASE:8984-1-14280-27  8984-1-14280-27
#> 79   URN:catalog:PEMBROKEFN:RCBIOTABASE:8985-1-14280-28  8985-1-14280-28
#> 80   URN:catalog:PEMBROKEFN:RCBIOTABASE:8987-1-14280-31  8987-1-14280-31
#> 81   URN:catalog:PEMBROKEFN:RCBIOTABASE:8988-1-14280-32  8988-1-14280-32
#> 82   URN:catalog:PEMBROKEFN:RCBIOTABASE:8989-1-14280-32  8989-1-14280-32
#> 83   URN:catalog:PEMBROKEFN:RCBIOTABASE:8990-1-14280-32  8990-1-14280-32
#> 84    URN:catalog:PEMBROKEFN:RCBIOTABASE:8996-1-14280-7   8996-1-14280-7
#> 85   URN:catalog:PEMBROKEFN:RCBIOTABASE:8998-1-14280-12  8998-1-14280-12
#> 86   URN:catalog:PEMBROKEFN:RCBIOTABASE:9000-1-14280-20  9000-1-14280-20
#> 87   URN:catalog:PEMBROKEFN:RCBIOTABASE:9002-1-14280-25  9002-1-14280-25
#> 88   URN:catalog:PEMBROKEFN:RCBIOTABASE:9017-1-14280-25  9017-1-14280-25
#> 89   URN:catalog:PEMBROKEFN:RCBIOTABASE:9023-1-14280-25  9023-1-14280-25
#> 90   URN:catalog:PEMBROKEFN:RCBIOTABASE:9030-1-14280-30  9030-1-14280-30
#> 91   URN:catalog:PEMBROKEFN:RCBIOTABASE:9032-1-14280-18  9032-1-14280-18
#> 92   URN:catalog:PEMBROKEFN:RCBIOTABASE:9037-1-14280-27  9037-1-14280-27
#> 93   URN:catalog:PEMBROKEFN:RCBIOTABASE:9040-1-14280-16  9040-1-14280-16
#> 94   URN:catalog:PEMBROKEFN:RCBIOTABASE:9041-1-14280-22  9041-1-14280-22
#> 95   URN:catalog:PEMBROKEFN:RCBIOTABASE:9042-1-14280-28  9042-1-14280-28
#> 96   URN:catalog:PEMBROKEFN:RCBIOTABASE:9043-1-14280-21  9043-1-14280-21
#> 97   URN:catalog:PEMBROKEFN:RCBIOTABASE:9045-1-14280-32  9045-1-14280-32
#> 98   URN:catalog:PEMBROKEFN:RCBIOTABASE:9055-1-14280-25  9055-1-14280-25
#> 99   URN:catalog:PEMBROKEFN:RCBIOTABASE:9104-1-14280-20  9104-1-14280-20
#> 100  URN:catalog:PEMBROKEFN:RCBIOTABASE:9105-1-14280-17  9105-1-14280-17
#> 101  URN:catalog:PEMBROKEFN:RCBIOTABASE:9106-1-14280-27  9106-1-14280-27
#> 102  URN:catalog:PEMBROKEFN:RCBIOTABASE:9107-1-14280-19  9107-1-14280-19
#> 103  URN:catalog:PEMBROKEFN:RCBIOTABASE:9108-1-14280-23  9108-1-14280-23
#> 104  URN:catalog:PEMBROKEFN:RCBIOTABASE:9109-1-14280-25  9109-1-14280-25
#> 105  URN:catalog:PEMBROKEFN:RCBIOTABASE:9111-1-14280-19  9111-1-14280-19
#> 106  URN:catalog:PEMBROKEFN:RCBIOTABASE:9112-1-14280-19  9112-1-14280-19
#> 107  URN:catalog:PEMBROKEFN:RCBIOTABASE:9113-1-14280-20  9113-1-14280-20
#> 108   URN:catalog:PEMBROKEFN:RCBIOTABASE:9153-1-14280-3   9153-1-14280-3
#> 109  URN:catalog:PEMBROKEFN:RCBIOTABASE:9734-1-14280-27  9734-1-14280-27
#> 110  URN:catalog:PEMBROKEFN:RCBIOTABASE:9735-1-14280-34  9735-1-14280-34
#> 111  URN:catalog:PEMBROKEFN:RCBIOTABASE:9738-1-14280-27  9738-1-14280-27
#> 112  URN:catalog:PEMBROKEFN:RCBIOTABASE:9739-1-14280-34  9739-1-14280-34
#> 113  URN:catalog:PEMBROKEFN:RCBIOTABASE:9740-1-14280-20  9740-1-14280-20
#> 114   URN:catalog:PEMBROKEFN:RCBIOTABASE:9779-1-14280-7   9779-1-14280-7
#> 115  URN:catalog:PEMBROKEFN:RCBIOTABASE:10085-1-14280-4  10085-1-14280-4
#> 116 URN:catalog:PEMBROKEFN:RCBIOTABASE:10286-1-14280-12 10286-1-14280-12
#> 117  URN:catalog:PEMBROKEFN:RCBIOTABASE:10329-1-14280-6  10329-1-14280-6
#> 118  URN:catalog:PEMBROKEFN:RCBIOTABASE:10372-1-14280-3  10372-1-14280-3
#> 119  URN:catalog:PEMBROKEFN:RCBIOTABASE:10549-1-14280-5  10549-1-14280-5
#> 120  URN:catalog:PEMBROKEFN:RCBIOTABASE:10550-1-14280-6  10550-1-14280-6
#> 121  URN:catalog:PEMBROKEFN:RCBIOTABASE:10554-1-14280-8  10554-1-14280-8
#> 122  URN:catalog:PEMBROKEFN:RCBIOTABASE:10556-1-14280-7  10556-1-14280-7
#> 123  URN:catalog:PEMBROKEFN:RCBIOTABASE:10558-1-14280-4  10558-1-14280-4
#> 124  URN:catalog:PEMBROKEFN:RCBIOTABASE:10564-1-14280-8  10564-1-14280-8
#> 125 URN:catalog:PEMBROKEFN:RCBIOTABASE:10587-1-14280-16 10587-1-14280-16
#> 126  URN:catalog:PEMBROKEFN:RCBIOTABASE:10690-1-14280-6  10690-1-14280-6
#> 127  URN:catalog:PEMBROKEFN:RCBIOTABASE:10837-1-14280-9  10837-1-14280-9
#> 128 URN:catalog:PEMBROKEFN:RCBIOTABASE:10847-1-14280-10 10847-1-14280-10
#> 129  URN:catalog:PEMBROKEFN:RCBIOTABASE:10848-1-14280-8  10848-1-14280-8
#> 130 URN:catalog:PEMBROKEFN:RCBIOTABASE:10850-1-14280-12 10850-1-14280-12
#> 131  URN:catalog:PEMBROKEFN:RCBIOTABASE:10861-1-14280-7  10861-1-14280-7
#> 132  URN:catalog:PEMBROKEFN:RCBIOTABASE:10899-1-14280-7  10899-1-14280-7
#> 133 URN:catalog:PEMBROKEFN:RCBIOTABASE:10911-1-14280-13 10911-1-14280-13
#> 134  URN:catalog:PEMBROKEFN:RCBIOTABASE:10428-1-14280-2  10428-1-14280-2
#> 135 URN:catalog:PEMBROKEFN:RCBIOTABASE:11048-1-14280-13 11048-1-14280-13
#> 136 URN:catalog:PEMBROKEFN:RCBIOTABASE:11059-1-14280-11 11059-1-14280-11
#> 137 URN:catalog:PEMBROKEFN:RCBIOTABASE:11069-1-14280-16 11069-1-14280-16
#> 138 URN:catalog:PEMBROKEFN:RCBIOTABASE:11075-1-14280-11 11075-1-14280-11
#> 139 URN:catalog:PEMBROKEFN:RCBIOTABASE:11085-1-14280-14 11085-1-14280-14
#> 140 URN:catalog:PEMBROKEFN:RCBIOTABASE:11162-1-14280-11 11162-1-14280-11
#> 141 URN:catalog:PEMBROKEFN:RCBIOTABASE:11163-1-14280-19 11163-1-14280-19
#> 142  URN:catalog:PEMBROKEFN:RCBIOTABASE:11211-1-14280-8  11211-1-14280-8
#> 143 URN:catalog:PEMBROKEFN:RCBIOTABASE:11278-1-14280-14 11278-1-14280-14
#> 144  URN:catalog:PEMBROKEFN:RCBIOTABASE:10419-1-14280-7  10419-1-14280-7
#> 145  URN:catalog:PEMBROKEFN:RCBIOTABASE:12284-1-14280-1  12284-1-14280-1
#> 146  URN:catalog:PEMBROKEFN:RCBIOTABASE:12297-1-14280-6  12297-1-14280-6
#> 147  URN:catalog:PEMBROKEFN:RCBIOTABASE:10946-1-14280-9  10946-1-14280-9
#> 148  URN:catalog:PEMBROKEFN:RCBIOTABASE:10947-1-14280-5  10947-1-14280-5
#> 149 URN:catalog:PEMBROKEFN:RCBIOTABASE:12794-1-14280-36 12794-1-14280-36
#> 150  URN:catalog:PEMBROKEFN:RCBIOTABASE:13476-1-14280-8  13476-1-14280-8
#> 151  URN:catalog:PEMBROKEFN:RCBIOTABASE:13477-1-14280-7  13477-1-14280-7
#> 152 URN:catalog:PEMBROKEFN:RCBIOTABASE:14023-1-14280-18 14023-1-14280-18
#> 153 URN:catalog:PEMBROKEFN:RCBIOTABASE:14101-1-14280-26 14101-1-14280-26
#> 154  URN:catalog:PEMBROKEFN:RCBIOTABASE:14960-1-14280-7  14960-1-14280-7
#> 155 URN:catalog:PEMBROKEFN:RCBIOTABASE:14998-1-14280-21 14998-1-14280-21
#> 156 URN:catalog:PEMBROKEFN:RCBIOTABASE:15000-1-14280-17 15000-1-14280-17
#> 157  URN:catalog:PEMBROKEFN:RCBIOTABASE:15001-1-14280-9  15001-1-14280-9
#> 158  URN:catalog:PEMBROKEFN:RCBIOTABASE:15494-1-14280-9  15494-1-14280-9
#> 159  URN:catalog:PEMBROKEFN:RCBIOTABASE:16023-1-14280-9  16023-1-14280-9
#> 160  URN:catalog:PEMBROKEFN:RCBIOTABASE:68353-1-14280-6  68353-1-14280-6
#>                                       Locality TimeCollected CollectorNumber
#> 1                   Morninglory Farm, Killaloe       11.7333         1031381
#> 2                          Shaw Woods FBMP-C *       8.33333         1031206
#> 3                              Kiwanas Walkway          13.5         1031138
#> 4                     Wiltom Drive, Barrys Bay             2         1031147
#> 5                            1311 Burchat Road          <NA>         1030660
#> 6                              Pembroke Marina       15.0833         1031370
#> 7                     Westmeath PP front field          16.5         1031370
#> 8                                  Lake Doré *       9.08333         1031370
#> 9                     Riverside Park, Pembroke         12.25         1031370
#> 10                 PAFN: A Trip to Bellows Bay          <NA>         1031138
#> 11              Christmas Bird Count, Pembroke          <NA>         1031136
#> 12              Christmas Bird Count, Pembroke          <NA>         1031355
#> 13      PAFN: Petawawa Terrace Provincial Park          <NA>         1031138
#> 14                       PAFN: Kiwanas Walkway          <NA>         1031138
#> 15                                 Bellows Bay          <NA>         1031131
#> 16                      905 Barron Canyon Road           8.5         1031206
#> 17      PAFN: Petawawa Terrace Provincial Park          <NA>         1031138
#> 18                             571 Russham Rd.          <NA>         1031206
#> 19                      905 Barron Canyon Road          <NA>         1031206
#> 20                      905 Barron Canyon Road          <NA>         1031206
#> 21                      905 Barron Canyon Road          <NA>         1031206
#> 22                      905 Barron Canyon Road          <NA>         1031206
#> 23                                        <NA>             9         1031370
#> 24                   Sufian St Laurentian View       9.16667         1031370
#> 25                   Sufian St Laurentian View           9.5         1031370
#> 26                   Sufian St Laurentian View             8         1031370
#> 27                         Westmeath Prov Park       15.1667         1031370
#> 28                          Barron Canyon Road            10         1031370
#> 29                   Sufian St Laurentian View             8         1031370
#> 30                           Deacon Escarpment             7         1031370
#> 31      PAFN: Petawawa Terrace Provincial Park       17.0833         1031370
#> 32                         Westmeath Prov Park          9.05         1031370
#> 33                          Barron Canyon Road             7         1031370
#> 34                                  Rantz Rd *         20.25         1031370
#> 35                         Westmeath Prov Park            11         1031370
#> 36       PAFN: Westmeath Provincial Park Walk*           8.5         1031370
#> 37                 PAFN: A Trip to Bellows Bay             9         1031131
#> 38                         Westmeath Prov Park            10         1031370
#> 39                        lake dore field trip             9         1031135
#> 40        PAFN: Christmas Bird Count, Pembroke          <NA>         1031361
#> 41        PAFN: Christmas Bird Count, Pembroke          <NA>         1031361
#> 42                          Barron Canyon Road             8         1031370
#> 43                   Sufian St Laurentian View             8         1031370
#> 44                                        <NA>       9.33333         1031370
#> 45                   Sufian St Laurentian View             8         1031370
#> 46                   Sufian St Laurentian View          7.75         1031370
#> 47                   Sufian St Laurentian View             8         1031370
#> 48                   Sufian St Laurentian View             8         1031370
#> 49                   Sufian St Laurentian View             8         1031370
#> 50                 Petawawa Rresearch Forest *           7.5         1031370
#> 51                                        <NA>           8.5         1031370
#> 52        PAFN: Christmas Bird Count, Pembroke          <NA>         1031361
#> 53        PAFN: Christmas Bird Count, Pembroke          <NA>         1031361
#> 54        PAFN: Christmas Bird Count, Pembroke          <NA>         1031361
#> 55        PAFN: Christmas Bird Count, Pembroke          <NA>         1031361
#> 56       PAFN: Christmas Bird Count, Eganville          <NA>         1031361
#> 57        PAFN: Christmas Bird Count, Pembroke          <NA>         1031361
#> 58        PAFN: Christmas Bird Count, Pembroke          <NA>         1031361
#> 59        PAFN: Christmas Bird Count, Pembroke          <NA>         1031361
#> 60                                        <NA>          <NA>         1031361
#> 61        PAFN: Christmas Bird Count, Pembroke          <NA>         1031361
#> 62        PAFN: Christmas Bird Count, Pembroke          <NA>         1031361
#> 63        PAFN: Christmas Bird Count, Pembroke          <NA>         1031361
#> 64        PAFN: Christmas Bird Count, Pembroke          <NA>         1031361
#> 65        PAFN: Christmas Bird Count, Pembroke          <NA>         1031361
#> 66        PAFN: Christmas Bird Count, Pembroke          <NA>         1031361
#> 67        PAFN: Christmas Bird Count, Pembroke          <NA>         1031361
#> 68        PAFN: Christmas Bird Count, Pembroke          <NA>         1031361
#> 69        PAFN: Christmas Bird Count, Pembroke          <NA>         1031361
#> 70        PAFN: Christmas Bird Count, Pembroke          <NA>         1031361
#> 71        PAFN: Christmas Bird Count, Pembroke          <NA>         1031361
#> 72        PAFN: Christmas Bird Count, Pembroke          <NA>         1031361
#> 73        PAFN: Christmas Bird Count, Pembroke          <NA>         1031361
#> 74        PAFN: Christmas Bird Count, Pembroke          <NA>         1031361
#> 75                                        <NA>          <NA>         1031361
#> 76        PAFN: Christmas Bird Count, Pembroke          <NA>         1031361
#> 77        PAFN: Christmas Bird Count, Pembroke          <NA>         1031361
#> 78        PAFN: Christmas Bird Count, Pembroke          <NA>         1031361
#> 79        PAFN: Christmas Bird Count, Pembroke          <NA>         1031361
#> 80        PAFN: Christmas Bird Count, Pembroke          <NA>         1031361
#> 81        PAFN: Christmas Bird Count, Pembroke          <NA>         1031361
#> 82        PAFN: Christmas Bird Count, Pembroke          <NA>         1031361
#> 83        PAFN: Christmas Bird Count, Pembroke          <NA>         1031361
#> 84                   Sufian St Laurentian View            13         1031370
#> 85                                   Lake Doré            16         1031370
#> 86  PAFN: Pembroke Marina/Waterfront Bird Walk          <NA>         1031361
#> 87                 PAFN: A Trip to Bellows Bay          <NA>         1031361
#> 88                 PAFN: A Trip to Bellows Bay          <NA>         1031361
#> 89                 PAFN: A Trip to Bellows Bay          <NA>         1031361
#> 90                 PAFN: A Trip to Bellows Bay          <NA>         1031361
#> 91                         Westmeath Prov Park          <NA>         1031361
#> 92                 PAFN: A Trip to Bellows Bay          <NA>         1031361
#> 93                         Westmeath Prov Park          <NA>         1031361
#> 94                       PAFN: Lake Doré Birds          <NA>         1031361
#> 95                 PAFN: A Trip to Bellows Bay          <NA>         1031361
#> 96      PAFN: Petawawa Terrace Provincial Park          <NA>         1031361
#> 97                 PAFN: A Trip to Bellows Bay          <NA>         1031361
#> 98                 PAFN: A Trip to Bellows Bay          <NA>         1031361
#> 99         PAFN: Killaloe Christmas Bird Count          <NA>         1031361
#> 100        PAFN: Killaloe Christmas Bird Count          <NA>         1031361
#> 101        PAFN: Killaloe Christmas Bird Count          <NA>         1031361
#> 102        PAFN: Killaloe Christmas Bird Count          <NA>         1031361
#> 103        PAFN: Killaloe Christmas Bird Count          <NA>         1031361
#> 104        PAFN: Killaloe Christmas Bird Count          <NA>         1031361
#> 105        PAFN: Killaloe Christmas Bird Count          <NA>         1031361
#> 106        PAFN: Killaloe Christmas Bird Count          <NA>         1031361
#> 107        PAFN: Killaloe Christmas Bird Count          <NA>         1031361
#> 108                     Wylie Rd, Deep River *            16         1031370
#> 109     PAFN: Petawawa Terrace Provincial Park          <NA>         1031361
#> 110                PAFN: A Trip to Bellows Bay          <NA>         1031361
#> 111                PAFN: A Trip to Bellows Bay          <NA>         1031361
#> 112                                       <NA>          <NA>         1031361
#> 113     PAFN: Petawawa Terrace Provincial Park          <NA>         1031361
#> 114                  Sufian St Laurentian View             8         1031370
#> 115                  Sufian St Laurentian View       8.41667         1031370
#> 116                 Morninglory Farm, Killaloe            12         1031381
#> 117                                       <NA>            11         1031370
#> 118                 Morninglory Farm, Killaloe             9         1031381
#> 119                  Sufian St Laurentian View             8         1031370
#> 120                                  LaPasse *            17         1031370
#> 121                                       <NA>             9         1031370
#> 122                         Barron Canyon Road       11.1667         1031370
#> 123                                       <NA>       8.83333         1031370
#> 124                  Sufian St Laurentian View             9         1031370
#> 125     PAFN: Petawawa Terrace Provincial Park          <NA>         1031361
#> 126                                       <NA>             9         1031206
#> 127                  Sufian St Laurentian View             8         1031370
#> 128                                       <NA>             8         1031370
#> 129                         Barron Canyon Road            10         1031370
#> 130                  Sufian St Laurentian View       12.0167         1031370
#> 131                  Sufian St Laurentian View           8.5         1031370
#> 132                                       <NA>          8.25         1031370
#> 133                  Sufian St Laurentian View           8.5         1031370
#> 134                     905 Barron Canyon Road            11         1031206
#> 135                  Sufian St Laurentian View          8.25         1031370
#> 136                  Sufian St Laurentian View       8.33333         1031370
#> 137                  Sufian St Laurentian View           8.5         1031370
#> 138                           Pansy Patch Park            11         1031138
#> 139                  Sufian St Laurentian View       8.16667         1031370
#> 140                  Sufian St Laurentian View       8.16667         1031370
#> 141                  Macnamara Trail, Arnprior          11.5         1031138
#> 142                           Pansy Patch Park            20         1031138
#> 143                                       <NA>            14         1031370
#> 144                  Sufian St Laurentian View           8.5         1031370
#> 145                         4SC Satellite Site       15.9667         1031381
#> 146                         4SC Satellite Site          <NA>         1031381
#> 147                  Sufian St Laurentian View       8.33333         1031370
#> 148                  Sufian St Laurentian View       10.0167         1031370
#> 149                           PAFN: Shaw Woods            14         1031598
#> 150                            601 Achray Road            14         1031206
#> 151                     905 Barron Canyon Road             8         1031206
#> 152                            Morrison Island       7.83333         1031138
#> 153     PAFN: Petawawa Terrace Provincial Park       17.0167         1031370
#> 154                 Reilly Bird Nature Reserve            10         1031370
#> 155                                  Lake Doré       8.33333         1031370
#> 156                                  Lake Doré         14.25         1031370
#> 157                       Shaw Woods deciduous            12         1031370
#> 158                           Connaught Trail           <NA>         1031206
#> 159                  Sufian St Laurentian View          8.25         1031370
#> 160                     905 Barron Canyon Road           9.5         1031206
#>     FieldNumber Remarks ProjectCode ProtocolType ProtocolCode
#> 1            NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 2            NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 3            NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 4            NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 5            NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 6            NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 7            NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 8            NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 9            NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 10           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 11           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 12           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 13           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 14           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 15           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 16           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 17           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 18           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 19           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 20           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 21           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 22           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 23           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 24           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 25           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 26           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 27           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 28           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 29           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 30           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 31           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 32           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 33           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 34           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 35           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 36           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 37           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 38           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 39           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 40           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 41           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 42           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 43           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 44           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 45           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 46           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 47           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 48           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 49           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 50           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 51           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 52           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 53           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 54           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 55           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 56           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 57           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 58           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 59           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 60           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 61           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 62           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 63           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 64           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 65           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 66           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 67           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 68           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 69           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 70           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 71           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 72           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 73           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 74           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 75           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 76           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 77           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 78           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 79           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 80           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 81           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 82           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 83           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 84           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 85           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 86           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 87           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 88           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 89           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 90           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 91           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 92           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 93           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 94           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 95           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 96           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 97           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 98           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 99           NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 100          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 101          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 102          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 103          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 104          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 105          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 106          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 107          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 108          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 109          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 110          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 111          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 112          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 113          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 114          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 115          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 116          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 117          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 118          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 119          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 120          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 121          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 122          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 123          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 124          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 125          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 126          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 127          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 128          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 129          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 130          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 131          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 132          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 133          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 134          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 135          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 136          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 137          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 138          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 139          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 140          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 141          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 142          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 143          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 144          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 145          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 146          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 147          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 148          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 149          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 150          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 151          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 152          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 153          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 154          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 155          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 156          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 157          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 158          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 159          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#> 160          NA      NA RCBIOTABASE    Checklist    CHECKLIST
#>                                 ProtocolURL SurveyAreaIdentifier
#> 1   http://www.birdscanada.org/birdmon/rcb/                   NA
#> 2   http://www.birdscanada.org/birdmon/rcb/                   NA
#> 3   http://www.birdscanada.org/birdmon/rcb/                   NA
#> 4   http://www.birdscanada.org/birdmon/rcb/                   NA
#> 5   http://www.birdscanada.org/birdmon/rcb/                   NA
#> 6   http://www.birdscanada.org/birdmon/rcb/                   NA
#> 7   http://www.birdscanada.org/birdmon/rcb/                   NA
#> 8   http://www.birdscanada.org/birdmon/rcb/                   NA
#> 9   http://www.birdscanada.org/birdmon/rcb/                   NA
#> 10  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 11  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 12  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 13  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 14  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 15  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 16  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 17  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 18  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 19  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 20  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 21  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 22  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 23  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 24  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 25  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 26  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 27  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 28  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 29  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 30  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 31  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 32  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 33  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 34  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 35  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 36  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 37  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 38  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 39  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 40  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 41  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 42  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 43  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 44  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 45  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 46  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 47  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 48  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 49  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 50  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 51  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 52  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 53  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 54  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 55  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 56  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 57  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 58  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 59  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 60  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 61  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 62  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 63  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 64  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 65  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 66  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 67  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 68  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 69  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 70  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 71  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 72  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 73  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 74  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 75  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 76  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 77  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 78  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 79  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 80  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 81  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 82  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 83  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 84  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 85  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 86  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 87  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 88  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 89  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 90  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 91  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 92  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 93  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 94  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 95  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 96  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 97  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 98  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 99  http://www.birdscanada.org/birdmon/rcb/                   NA
#> 100 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 101 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 102 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 103 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 104 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 105 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 106 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 107 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 108 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 109 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 110 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 111 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 112 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 113 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 114 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 115 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 116 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 117 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 118 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 119 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 120 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 121 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 122 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 123 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 124 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 125 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 126 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 127 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 128 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 129 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 130 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 131 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 132 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 133 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 134 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 135 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 136 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 137 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 138 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 139 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 140 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 141 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 142 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 143 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 144 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 145 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 146 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 147 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 148 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 149 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 150 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 151 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 152 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 153 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 154 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 155 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 156 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 157 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 158 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 159 http://www.birdscanada.org/birdmon/rcb/                   NA
#> 160 http://www.birdscanada.org/birdmon/rcb/                   NA
#>     SamplingEventIdentifier SamplingEventStructure RouteIdentifier
#> 1        RCBIOTABASE-5565-1                     NA              NA
#> 2        RCBIOTABASE-5650-1                     NA              NA
#> 3        RCBIOTABASE-5022-1                     NA              NA
#> 4        RCBIOTABASE-5936-1                     NA              NA
#> 5        RCBIOTABASE-4976-1                     NA              NA
#> 6        RCBIOTABASE-6797-1                     NA              NA
#> 7        RCBIOTABASE-6798-1                     NA              NA
#> 8        RCBIOTABASE-6842-1                     NA              NA
#> 9        RCBIOTABASE-6894-1                     NA              NA
#> 10       RCBIOTABASE-6956-1                     NA              NA
#> 11       RCBIOTABASE-6957-1                     NA              NA
#> 12       RCBIOTABASE-6958-1                     NA              NA
#> 13       RCBIOTABASE-6961-1                     NA              NA
#> 14       RCBIOTABASE-6964-1                     NA              NA
#> 15       RCBIOTABASE-6972-1                     NA              NA
#> 16       RCBIOTABASE-6973-1                     NA              NA
#> 17       RCBIOTABASE-6979-1                     NA              NA
#> 18       RCBIOTABASE-6980-1                     NA              NA
#> 19       RCBIOTABASE-6983-1                     NA              NA
#> 20       RCBIOTABASE-6986-1                     NA              NA
#> 21       RCBIOTABASE-6988-1                     NA              NA
#> 22       RCBIOTABASE-6989-1                     NA              NA
#> 23       RCBIOTABASE-7044-1                     NA              NA
#> 24       RCBIOTABASE-7051-1                     NA              NA
#> 25       RCBIOTABASE-7100-1                     NA              NA
#> 26       RCBIOTABASE-7543-1                     NA              NA
#> 27       RCBIOTABASE-7544-1                     NA              NA
#> 28       RCBIOTABASE-5927-1                     NA              NA
#> 29       RCBIOTABASE-7607-1                     NA              NA
#> 30       RCBIOTABASE-6207-1                     NA              NA
#> 31       RCBIOTABASE-7783-1                     NA              NA
#> 32       RCBIOTABASE-7991-1                     NA              NA
#> 33       RCBIOTABASE-8020-1                     NA              NA
#> 34       RCBIOTABASE-8068-1                     NA              NA
#> 35       RCBIOTABASE-8221-1                     NA              NA
#> 36       RCBIOTABASE-8652-1                     NA              NA
#> 37       RCBIOTABASE-8727-1                     NA              NA
#> 38       RCBIOTABASE-8755-1                     NA              NA
#> 39       RCBIOTABASE-8826-1                     NA              NA
#> 40       RCBIOTABASE-8928-1                     NA              NA
#> 41       RCBIOTABASE-8930-1                     NA              NA
#> 42       RCBIOTABASE-7212-1                     NA              NA
#> 43       RCBIOTABASE-7418-1                     NA              NA
#> 44       RCBIOTABASE-7484-1                     NA              NA
#> 45       RCBIOTABASE-7514-1                     NA              NA
#> 46       RCBIOTABASE-7526-1                     NA              NA
#> 47       RCBIOTABASE-7568-1                     NA              NA
#> 48       RCBIOTABASE-7606-1                     NA              NA
#> 49       RCBIOTABASE-7744-1                     NA              NA
#> 50       RCBIOTABASE-8028-1                     NA              NA
#> 51       RCBIOTABASE-7706-1                     NA              NA
#> 52       RCBIOTABASE-8948-1                     NA              NA
#> 53       RCBIOTABASE-8949-1                     NA              NA
#> 54       RCBIOTABASE-8950-1                     NA              NA
#> 55       RCBIOTABASE-8951-1                     NA              NA
#> 56       RCBIOTABASE-8952-1                     NA              NA
#> 57       RCBIOTABASE-8954-1                     NA              NA
#> 58       RCBIOTABASE-8955-1                     NA              NA
#> 59       RCBIOTABASE-8956-1                     NA              NA
#> 60       RCBIOTABASE-8957-1                     NA              NA
#> 61       RCBIOTABASE-8958-1                     NA              NA
#> 62       RCBIOTABASE-8959-1                     NA              NA
#> 63       RCBIOTABASE-8960-1                     NA              NA
#> 64       RCBIOTABASE-8962-1                     NA              NA
#> 65       RCBIOTABASE-8963-1                     NA              NA
#> 66       RCBIOTABASE-8964-1                     NA              NA
#> 67       RCBIOTABASE-8965-1                     NA              NA
#> 68       RCBIOTABASE-8966-1                     NA              NA
#> 69       RCBIOTABASE-8967-1                     NA              NA
#> 70       RCBIOTABASE-8968-1                     NA              NA
#> 71       RCBIOTABASE-8969-1                     NA              NA
#> 72       RCBIOTABASE-8974-1                     NA              NA
#> 73       RCBIOTABASE-8975-1                     NA              NA
#> 74       RCBIOTABASE-8976-1                     NA              NA
#> 75       RCBIOTABASE-8977-1                     NA              NA
#> 76       RCBIOTABASE-8980-1                     NA              NA
#> 77       RCBIOTABASE-8981-1                     NA              NA
#> 78       RCBIOTABASE-8984-1                     NA              NA
#> 79       RCBIOTABASE-8985-1                     NA              NA
#> 80       RCBIOTABASE-8987-1                     NA              NA
#> 81       RCBIOTABASE-8988-1                     NA              NA
#> 82       RCBIOTABASE-8989-1                     NA              NA
#> 83       RCBIOTABASE-8990-1                     NA              NA
#> 84       RCBIOTABASE-8996-1                     NA              NA
#> 85       RCBIOTABASE-8998-1                     NA              NA
#> 86       RCBIOTABASE-9000-1                     NA              NA
#> 87       RCBIOTABASE-9002-1                     NA              NA
#> 88       RCBIOTABASE-9017-1                     NA              NA
#> 89       RCBIOTABASE-9023-1                     NA              NA
#> 90       RCBIOTABASE-9030-1                     NA              NA
#> 91       RCBIOTABASE-9032-1                     NA              NA
#> 92       RCBIOTABASE-9037-1                     NA              NA
#> 93       RCBIOTABASE-9040-1                     NA              NA
#> 94       RCBIOTABASE-9041-1                     NA              NA
#> 95       RCBIOTABASE-9042-1                     NA              NA
#> 96       RCBIOTABASE-9043-1                     NA              NA
#> 97       RCBIOTABASE-9045-1                     NA              NA
#> 98       RCBIOTABASE-9055-1                     NA              NA
#> 99       RCBIOTABASE-9104-1                     NA              NA
#> 100      RCBIOTABASE-9105-1                     NA              NA
#> 101      RCBIOTABASE-9106-1                     NA              NA
#> 102      RCBIOTABASE-9107-1                     NA              NA
#> 103      RCBIOTABASE-9108-1                     NA              NA
#> 104      RCBIOTABASE-9109-1                     NA              NA
#> 105      RCBIOTABASE-9111-1                     NA              NA
#> 106      RCBIOTABASE-9112-1                     NA              NA
#> 107      RCBIOTABASE-9113-1                     NA              NA
#> 108      RCBIOTABASE-9153-1                     NA              NA
#> 109      RCBIOTABASE-9734-1                     NA              NA
#> 110      RCBIOTABASE-9735-1                     NA              NA
#> 111      RCBIOTABASE-9738-1                     NA              NA
#> 112      RCBIOTABASE-9739-1                     NA              NA
#> 113      RCBIOTABASE-9740-1                     NA              NA
#> 114      RCBIOTABASE-9779-1                     NA              NA
#> 115     RCBIOTABASE-10085-1                     NA              NA
#> 116     RCBIOTABASE-10286-1                     NA              NA
#> 117     RCBIOTABASE-10329-1                     NA              NA
#> 118     RCBIOTABASE-10372-1                     NA              NA
#> 119     RCBIOTABASE-10549-1                     NA              NA
#> 120     RCBIOTABASE-10550-1                     NA              NA
#> 121     RCBIOTABASE-10554-1                     NA              NA
#> 122     RCBIOTABASE-10556-1                     NA              NA
#> 123     RCBIOTABASE-10558-1                     NA              NA
#> 124     RCBIOTABASE-10564-1                     NA              NA
#> 125     RCBIOTABASE-10587-1                     NA              NA
#> 126     RCBIOTABASE-10690-1                     NA              NA
#> 127     RCBIOTABASE-10837-1                     NA              NA
#> 128     RCBIOTABASE-10847-1                     NA              NA
#> 129     RCBIOTABASE-10848-1                     NA              NA
#> 130     RCBIOTABASE-10850-1                     NA              NA
#> 131     RCBIOTABASE-10861-1                     NA              NA
#> 132     RCBIOTABASE-10899-1                     NA              NA
#> 133     RCBIOTABASE-10911-1                     NA              NA
#> 134     RCBIOTABASE-10428-1                     NA              NA
#> 135     RCBIOTABASE-11048-1                     NA              NA
#> 136     RCBIOTABASE-11059-1                     NA              NA
#> 137     RCBIOTABASE-11069-1                     NA              NA
#> 138     RCBIOTABASE-11075-1                     NA              NA
#> 139     RCBIOTABASE-11085-1                     NA              NA
#> 140     RCBIOTABASE-11162-1                     NA              NA
#> 141     RCBIOTABASE-11163-1                     NA              NA
#> 142     RCBIOTABASE-11211-1                     NA              NA
#> 143     RCBIOTABASE-11278-1                     NA              NA
#> 144     RCBIOTABASE-10419-1                     NA              NA
#> 145     RCBIOTABASE-12284-1                     NA              NA
#> 146     RCBIOTABASE-12297-1                     NA              NA
#> 147     RCBIOTABASE-10946-1                     NA              NA
#> 148     RCBIOTABASE-10947-1                     NA              NA
#> 149     RCBIOTABASE-12794-1                     NA              NA
#> 150     RCBIOTABASE-13476-1                     NA              NA
#> 151     RCBIOTABASE-13477-1                     NA              NA
#> 152     RCBIOTABASE-14023-1                     NA              NA
#> 153     RCBIOTABASE-14101-1                     NA              NA
#> 154     RCBIOTABASE-14960-1                     NA              NA
#> 155     RCBIOTABASE-14998-1                     NA              NA
#> 156     RCBIOTABASE-15000-1                     NA              NA
#> 157     RCBIOTABASE-15001-1                     NA              NA
#> 158     RCBIOTABASE-15494-1                     NA              NA
#> 159     RCBIOTABASE-16023-1                     NA              NA
#> 160     RCBIOTABASE-68353-1                     NA              NA
#>     TimeObservationsStarted TimeObservationsEnded DurationInHours
#> 1                   11.7333                 11.75            <NA>
#> 2                   8.33333                  <NA>        0.166667
#> 3                      13.5                  14.5               1
#> 4                         2                     4               2
#> 5                      <NA>                  <NA>            <NA>
#> 6                   15.0833                  5.75         2.66667
#> 7                      16.5                 17.75            1.25
#> 8                   9.08333                 10.75         1.66667
#> 9                     12.25               12.9167        0.333333
#> 10                     <NA>                  <NA>            <NA>
#> 11                     <NA>                  <NA>            <NA>
#> 12                     <NA>                  <NA>            <NA>
#> 13                     <NA>                  <NA>            <NA>
#> 14                     <NA>                  <NA>            <NA>
#> 15                     <NA>                  <NA>            <NA>
#> 16                      8.5                  <NA>            <NA>
#> 17                     <NA>                  <NA>            <NA>
#> 18                     <NA>                  <NA>            <NA>
#> 19                     <NA>                  <NA>            <NA>
#> 20                     <NA>                  <NA>            <NA>
#> 21                     <NA>                  <NA>            <NA>
#> 22                     <NA>                  <NA>            <NA>
#> 23                        9                    16               7
#> 24                  9.16667                  15.5         6.33333
#> 25                      9.5                    17       0.0333333
#> 26                        8                  20.5             1.5
#> 27                  15.1667               16.6667             1.5
#> 28                       10                     0               2
#> 29                        8                  20.5               1
#> 30                        7               8.83333         1.83333
#> 31                  17.0833               19.5833             2.5
#> 32                     9.05                 10.25             1.2
#> 33                        7                     9               2
#> 34                    20.25               21.3333         1.08333
#> 35                       11                  13.5             2.5
#> 36                      8.5                 11.75            3.25
#> 37                        9                    12               3
#> 38                       10                    11               1
#> 39                        9                    12               3
#> 40                     <NA>                  <NA>            <NA>
#> 41                     <NA>                  <NA>            <NA>
#> 42                        8                 10.75            2.75
#> 43                        8                    21               1
#> 44                  9.33333               20.3333               2
#> 45                        8                    21               1
#> 46                     7.75                 20.75             1.5
#> 47                        8                    20               1
#> 48                        8                  20.5               1
#> 49                        8                  20.5               2
#> 50                      7.5                    11             3.5
#> 51                      8.5                  20.5               2
#> 52                     <NA>                  <NA>            <NA>
#> 53                     <NA>                  <NA>            <NA>
#> 54                     <NA>                  <NA>            <NA>
#> 55                     <NA>                  <NA>            <NA>
#> 56                     <NA>                  <NA>            <NA>
#> 57                     <NA>                  <NA>            <NA>
#> 58                     <NA>                  <NA>            <NA>
#> 59                     <NA>                  <NA>            <NA>
#> 60                     <NA>                  <NA>            <NA>
#> 61                     <NA>                  <NA>            <NA>
#> 62                     <NA>                  <NA>            <NA>
#> 63                     <NA>                  <NA>            <NA>
#> 64                     <NA>                  <NA>            <NA>
#> 65                     <NA>                  <NA>            <NA>
#> 66                     <NA>                  <NA>            <NA>
#> 67                     <NA>                  <NA>            <NA>
#> 68                     <NA>                  <NA>            <NA>
#> 69                     <NA>                  <NA>            <NA>
#> 70                     <NA>                  <NA>            <NA>
#> 71                     <NA>                  <NA>            <NA>
#> 72                     <NA>                  <NA>            <NA>
#> 73                     <NA>                  <NA>            <NA>
#> 74                     <NA>                  <NA>            <NA>
#> 75                     <NA>                  <NA>            <NA>
#> 76                     <NA>                  <NA>            <NA>
#> 77                     <NA>                  <NA>            <NA>
#> 78                     <NA>                  <NA>            <NA>
#> 79                     <NA>                  <NA>            <NA>
#> 80                     <NA>                  <NA>            <NA>
#> 81                     <NA>                  <NA>            <NA>
#> 82                     <NA>                  <NA>            <NA>
#> 83                     <NA>                  <NA>            <NA>
#> 84                       13                  15.5             2.5
#> 85                       16               16.9667        0.966667
#> 86                     <NA>                  <NA>            <NA>
#> 87                     <NA>                  <NA>            <NA>
#> 88                     <NA>                  <NA>            <NA>
#> 89                     <NA>                  <NA>            <NA>
#> 90                     <NA>                  <NA>            <NA>
#> 91                     <NA>                  <NA>            <NA>
#> 92                     <NA>                  <NA>            <NA>
#> 93                     <NA>                  <NA>            <NA>
#> 94                     <NA>                  <NA>            <NA>
#> 95                     <NA>                  <NA>            <NA>
#> 96                     <NA>                  <NA>            <NA>
#> 97                     <NA>                  <NA>            <NA>
#> 98                     <NA>                  <NA>            <NA>
#> 99                     <NA>                  <NA>            <NA>
#> 100                    <NA>                  <NA>            <NA>
#> 101                    <NA>                  <NA>            <NA>
#> 102                    <NA>                  <NA>            <NA>
#> 103                    <NA>                  <NA>            <NA>
#> 104                    <NA>                  <NA>            <NA>
#> 105                    <NA>                  <NA>            <NA>
#> 106                    <NA>                  <NA>            <NA>
#> 107                    <NA>                  <NA>            <NA>
#> 108                      16               16.0833       0.0833333
#> 109                    <NA>                  <NA>            <NA>
#> 110                    <NA>                  <NA>            <NA>
#> 111                    <NA>                  <NA>            <NA>
#> 112                    <NA>                  <NA>            <NA>
#> 113                    <NA>                  <NA>            <NA>
#> 114                       8                  16.5               2
#> 115                 8.41667               8.43333       0.0166667
#> 116                      12                    19             2.5
#> 117                      11                    16               1
#> 118                       9                  <NA>            0.25
#> 119                       8                 16.25             0.5
#> 120                      17                    18               1
#> 121                       9                  18.5         1.01667
#> 122                 11.1667                  12.5         1.33333
#> 123                 8.83333               18.1667               1
#> 124                       9                 18.75            1.25
#> 125                    <NA>                  <NA>            <NA>
#> 126                       9                  <NA>            <NA>
#> 127                       8                   8.5             0.5
#> 128                       8                   8.5             0.5
#> 129                      10               11.3333         1.33333
#> 130                 12.0167                  19.5               1
#> 131                     8.5                  19.5               1
#> 132                    8.25                 19.75            0.75
#> 133                     8.5                  18.5             1.5
#> 134                      11                  <NA>            <NA>
#> 135                    8.25                  17.5               1
#> 136                 8.33333                 22.25               1
#> 137                     8.5               22.0167            1.25
#> 138                      11                  11.5             0.5
#> 139                 8.16667                 20.25         1.01667
#> 140                 8.16667                    21               1
#> 141                    11.5                    16             4.5
#> 142                      20                 20.75            <NA>
#> 143                      14                    17               3
#> 144                     8.5               5.83333             0.5
#> 145                 15.9667                    20         4.03333
#> 146                    <NA>                  <NA>            <NA>
#> 147                 8.33333               19.3333               2
#> 148                 10.0167                  20.5               2
#> 149                      14                    17            <NA>
#> 150                      14                  <NA>               2
#> 151                       8                  <NA>            <NA>
#> 152                 7.83333               9.66667         1.83333
#> 153                 17.0167               20.0167               3
#> 154                      10                 13.25            3.25
#> 155                 8.33333                    12         3.66667
#> 156                   14.25                 15.75             1.5
#> 157                      12                    14               2
#> 158                    <NA>                  <NA>            <NA>
#> 159                    8.25               17.5167             0.5
#> 160                     9.5                  <NA>             0.5
#>     TimeIntervalStarted TimeIntervalEnded TimeIntervalsAdditive
#> 1                    NA                NA                    NA
#> 2                    NA                NA                    NA
#> 3                    NA                NA                    NA
#> 4                    NA                NA                    NA
#> 5                    NA                NA                    NA
#> 6                    NA                NA                    NA
#> 7                    NA                NA                    NA
#> 8                    NA                NA                    NA
#> 9                    NA                NA                    NA
#> 10                   NA                NA                    NA
#> 11                   NA                NA                    NA
#> 12                   NA                NA                    NA
#> 13                   NA                NA                    NA
#> 14                   NA                NA                    NA
#> 15                   NA                NA                    NA
#> 16                   NA                NA                    NA
#> 17                   NA                NA                    NA
#> 18                   NA                NA                    NA
#> 19                   NA                NA                    NA
#> 20                   NA                NA                    NA
#> 21                   NA                NA                    NA
#> 22                   NA                NA                    NA
#> 23                   NA                NA                    NA
#> 24                   NA                NA                    NA
#> 25                   NA                NA                    NA
#> 26                   NA                NA                    NA
#> 27                   NA                NA                    NA
#> 28                   NA                NA                    NA
#> 29                   NA                NA                    NA
#> 30                   NA                NA                    NA
#> 31                   NA                NA                    NA
#> 32                   NA                NA                    NA
#> 33                   NA                NA                    NA
#> 34                   NA                NA                    NA
#> 35                   NA                NA                    NA
#> 36                   NA                NA                    NA
#> 37                   NA                NA                    NA
#> 38                   NA                NA                    NA
#> 39                   NA                NA                    NA
#> 40                   NA                NA                    NA
#> 41                   NA                NA                    NA
#> 42                   NA                NA                    NA
#> 43                   NA                NA                    NA
#> 44                   NA                NA                    NA
#> 45                   NA                NA                    NA
#> 46                   NA                NA                    NA
#> 47                   NA                NA                    NA
#> 48                   NA                NA                    NA
#> 49                   NA                NA                    NA
#> 50                   NA                NA                    NA
#> 51                   NA                NA                    NA
#> 52                   NA                NA                    NA
#> 53                   NA                NA                    NA
#> 54                   NA                NA                    NA
#> 55                   NA                NA                    NA
#> 56                   NA                NA                    NA
#> 57                   NA                NA                    NA
#> 58                   NA                NA                    NA
#> 59                   NA                NA                    NA
#> 60                   NA                NA                    NA
#> 61                   NA                NA                    NA
#> 62                   NA                NA                    NA
#> 63                   NA                NA                    NA
#> 64                   NA                NA                    NA
#> 65                   NA                NA                    NA
#> 66                   NA                NA                    NA
#> 67                   NA                NA                    NA
#> 68                   NA                NA                    NA
#> 69                   NA                NA                    NA
#> 70                   NA                NA                    NA
#> 71                   NA                NA                    NA
#> 72                   NA                NA                    NA
#> 73                   NA                NA                    NA
#> 74                   NA                NA                    NA
#> 75                   NA                NA                    NA
#> 76                   NA                NA                    NA
#> 77                   NA                NA                    NA
#> 78                   NA                NA                    NA
#> 79                   NA                NA                    NA
#> 80                   NA                NA                    NA
#> 81                   NA                NA                    NA
#> 82                   NA                NA                    NA
#> 83                   NA                NA                    NA
#> 84                   NA                NA                    NA
#> 85                   NA                NA                    NA
#> 86                   NA                NA                    NA
#> 87                   NA                NA                    NA
#> 88                   NA                NA                    NA
#> 89                   NA                NA                    NA
#> 90                   NA                NA                    NA
#> 91                   NA                NA                    NA
#> 92                   NA                NA                    NA
#> 93                   NA                NA                    NA
#> 94                   NA                NA                    NA
#> 95                   NA                NA                    NA
#> 96                   NA                NA                    NA
#> 97                   NA                NA                    NA
#> 98                   NA                NA                    NA
#> 99                   NA                NA                    NA
#> 100                  NA                NA                    NA
#> 101                  NA                NA                    NA
#> 102                  NA                NA                    NA
#> 103                  NA                NA                    NA
#> 104                  NA                NA                    NA
#> 105                  NA                NA                    NA
#> 106                  NA                NA                    NA
#> 107                  NA                NA                    NA
#> 108                  NA                NA                    NA
#> 109                  NA                NA                    NA
#> 110                  NA                NA                    NA
#> 111                  NA                NA                    NA
#> 112                  NA                NA                    NA
#> 113                  NA                NA                    NA
#> 114                  NA                NA                    NA
#> 115                  NA                NA                    NA
#> 116                  NA                NA                    NA
#> 117                  NA                NA                    NA
#> 118                  NA                NA                    NA
#> 119                  NA                NA                    NA
#> 120                  NA                NA                    NA
#> 121                  NA                NA                    NA
#> 122                  NA                NA                    NA
#> 123                  NA                NA                    NA
#> 124                  NA                NA                    NA
#> 125                  NA                NA                    NA
#> 126                  NA                NA                    NA
#> 127                  NA                NA                    NA
#> 128                  NA                NA                    NA
#> 129                  NA                NA                    NA
#> 130                  NA                NA                    NA
#> 131                  NA                NA                    NA
#> 132                  NA                NA                    NA
#> 133                  NA                NA                    NA
#> 134                  NA                NA                    NA
#> 135                  NA                NA                    NA
#> 136                  NA                NA                    NA
#> 137                  NA                NA                    NA
#> 138                  NA                NA                    NA
#> 139                  NA                NA                    NA
#> 140                  NA                NA                    NA
#> 141                  NA                NA                    NA
#> 142                  NA                NA                    NA
#> 143                  NA                NA                    NA
#> 144                  NA                NA                    NA
#> 145                  NA                NA                    NA
#> 146                  NA                NA                    NA
#> 147                  NA                NA                    NA
#> 148                  NA                NA                    NA
#> 149                  NA                NA                    NA
#> 150                  NA                NA                    NA
#> 151                  NA                NA                    NA
#> 152                  NA                NA                    NA
#> 153                  NA                NA                    NA
#> 154                  NA                NA                    NA
#> 155                  NA                NA                    NA
#> 156                  NA                NA                    NA
#> 157                  NA                NA                    NA
#> 158                  NA                NA                    NA
#> 159                  NA                NA                    NA
#> 160                  NA                NA                    NA
#>     NumberOfObservers NoObservations ObservationCount ObservationDescriptor
#> 1                  NA             NA                1                    NA
#> 2                  NA             NA                1                    NA
#> 3                  NA             NA                1                    NA
#> 4                  NA             NA                2                    NA
#> 5                  NA             NA                2                    NA
#> 6                  NA             NA                3                    NA
#> 7                  NA             NA                4                    NA
#> 8                  NA             NA                6                    NA
#> 9                  NA             NA                3                    NA
#> 10                 NA             NA                1                    NA
#> 11                 NA             NA              925                    NA
#> 12                 NA             NA                1                    NA
#> 13                 NA             NA                1                    NA
#> 14                 NA             NA                1                    NA
#> 15                 NA             NA                1                    NA
#> 16                 NA             NA                3                    NA
#> 17                 NA             NA                1                    NA
#> 18                 NA             NA                4                    NA
#> 19                 NA             NA                6                    NA
#> 20                 NA             NA                8                    NA
#> 21                 NA             NA                5                    NA
#> 22                 NA             NA                4                    NA
#> 23                 NA             NA                5                    NA
#> 24                 NA             NA                6                    NA
#> 25                 NA             NA                8                    NA
#> 26                 NA             NA                4                    NA
#> 27                 NA             NA                6                    NA
#> 28                 NA             NA                2                    NA
#> 29                 NA             NA                4                    NA
#> 30                 NA             NA                2                    NA
#> 31                 NA             NA                3                    NA
#> 32                 NA             NA                4                    NA
#> 33                 NA             NA                6                    NA
#> 34                 NA             NA                2                    NA
#> 35                 NA             NA                4                    NA
#> 36                 NA             NA                4                    NA
#> 37                 NA             NA                1                    NA
#> 38                 NA             NA                4                    NA
#> 39                 NA             NA                1                    NA
#> 40                 NA             NA              183                    NA
#> 41                 NA             NA              158                    NA
#> 42                 NA             NA               12                    NA
#> 43                 NA             NA                6                    NA
#> 44                 NA             NA                4                    NA
#> 45                 NA             NA                4                    NA
#> 46                 NA             NA                4                    NA
#> 47                 NA             NA                4                    NA
#> 48                 NA             NA                3                    NA
#> 49                 NA             NA                2                    NA
#> 50                 NA             NA               13                    NA
#> 51                 NA             NA                4                    NA
#> 52                 NA             NA              214                    NA
#> 53                 NA             NA              237                    NA
#> 54                 NA             NA              318                    NA
#> 55                 NA             NA              472                    NA
#> 56                 NA             NA              462                    NA
#> 57                 NA             NA              457                    NA
#> 58                 NA             NA              595                    NA
#> 59                 NA             NA              643                    NA
#> 60                 NA             NA              828                    NA
#> 61                 NA             NA              602                    NA
#> 62                 NA             NA              756                    NA
#> 63                 NA             NA              779                    NA
#> 64                 NA             NA              740                    NA
#> 65                 NA             NA              721                    NA
#> 66                 NA             NA              599                    NA
#> 67                 NA             NA              693                    NA
#> 68                 NA             NA              686                    NA
#> 69                 NA             NA              632                    NA
#> 70                 NA             NA              733                    NA
#> 71                 NA             NA              782                    NA
#> 72                 NA             NA              721                    NA
#> 73                 NA             NA              897                    NA
#> 74                 NA             NA              925                    NA
#> 75                 NA             NA             1120                    NA
#> 76                 NA             NA              878                    NA
#> 77                 NA             NA              898                    NA
#> 78                 NA             NA              843                    NA
#> 79                 NA             NA              789                    NA
#> 80                 NA             NA              907                    NA
#> 81                 NA             NA              978                    NA
#> 82                 NA             NA              787                    NA
#> 83                 NA             NA              741                    NA
#> 84                 NA             NA                7                    NA
#> 85                 NA             NA                4                    NA
#> 86                 NA             NA                1                    NA
#> 87                 NA             NA                1                    NA
#> 88                 NA             NA                1                    NA
#> 89                 NA             NA                1                    NA
#> 90                 NA             NA                1                    NA
#> 91                 NA             NA                1                    NA
#> 92                 NA             NA                1                    NA
#> 93                 NA             NA                1                    NA
#> 94                 NA             NA                1                    NA
#> 95                 NA             NA                1                    NA
#> 96                 NA             NA                1                    NA
#> 97                 NA             NA                1                    NA
#> 98                 NA             NA                1                    NA
#> 99                 NA             NA              299                    NA
#> 100                NA             NA              393                    NA
#> 101                NA             NA              321                    NA
#> 102                NA             NA              300                    NA
#> 103                NA             NA              445                    NA
#> 104                NA             NA              444                    NA
#> 105                NA             NA              466                    NA
#> 106                NA             NA              720                    NA
#> 107                NA             NA              826                    NA
#> 108                NA             NA                8                    NA
#> 109                NA             NA                1                    NA
#> 110                NA             NA                1                    NA
#> 111                NA             NA                1                    NA
#> 112                NA             NA                1                    NA
#> 113                NA             NA                1                    NA
#> 114                NA             NA                7                    NA
#> 115                NA             NA                3                    NA
#> 116                NA             NA                4                    NA
#> 117                NA             NA                6                    NA
#> 118                NA             NA                4                    NA
#> 119                NA             NA                7                    NA
#> 120                NA             NA                2                    NA
#> 121                NA             NA                7                    NA
#> 122                NA             NA                6                    NA
#> 123                NA             NA                7                    NA
#> 124                NA             NA                7                    NA
#> 125                NA             NA                1                    NA
#> 126                NA             NA                2                    NA
#> 127                NA             NA                7                    NA
#> 128                NA             NA                2                    NA
#> 129                NA             NA                4                    NA
#> 130                NA             NA                6                    NA
#> 131                NA             NA                5                    NA
#> 132                NA             NA                7                    NA
#> 133                NA             NA                7                    NA
#> 134                NA             NA                1                    NA
#> 135                NA             NA                6                    NA
#> 136                NA             NA                6                    NA
#> 137                NA             NA                6                    NA
#> 138                NA             NA                1                    NA
#> 139                NA             NA                5                    NA
#> 140                NA             NA                3                    NA
#> 141                NA             NA                1                    NA
#> 142                NA             NA                1                    NA
#> 143                NA             NA                1                    NA
#> 144                NA             NA                7                    NA
#> 145                NA             NA                9                    NA
#> 146                NA             NA                1                    NA
#> 147                NA             NA                6                    NA
#> 148                NA             NA                6                    NA
#> 149                NA             NA                1                    NA
#> 150                NA             NA                3                    NA
#> 151                NA             NA                2                    NA
#> 152                NA             NA                1                    NA
#> 153                NA             NA                2                    NA
#> 154                NA             NA                6                    NA
#> 155                NA             NA                4                    NA
#> 156                NA             NA                2                    NA
#> 157                NA             NA                3                    NA
#> 158                NA             NA                1                    NA
#> 159                NA             NA                6                    NA
#> 160                NA             NA                2                    NA
#>     ObservationCount2 ObservationDescriptor2 ObservationCount3
#> 1                  NA                     NA                NA
#> 2                  NA                     NA                NA
#> 3                  NA                     NA                NA
#> 4                  NA                     NA                NA
#> 5                  NA                     NA                NA
#> 6                  NA                     NA                NA
#> 7                  NA                     NA                NA
#> 8                  NA                     NA                NA
#> 9                  NA                     NA                NA
#> 10                 NA                     NA                NA
#> 11                 NA                     NA                NA
#> 12                 NA                     NA                NA
#> 13                 NA                     NA                NA
#> 14                 NA                     NA                NA
#> 15                 NA                     NA                NA
#> 16                 NA                     NA                NA
#> 17                 NA                     NA                NA
#> 18                 NA                     NA                NA
#> 19                 NA                     NA                NA
#> 20                 NA                     NA                NA
#> 21                 NA                     NA                NA
#> 22                 NA                     NA                NA
#> 23                 NA                     NA                NA
#> 24                 NA                     NA                NA
#> 25                 NA                     NA                NA
#> 26                 NA                     NA                NA
#> 27                 NA                     NA                NA
#> 28                 NA                     NA                NA
#> 29                 NA                     NA                NA
#> 30                 NA                     NA                NA
#> 31                 NA                     NA                NA
#> 32                 NA                     NA                NA
#> 33                 NA                     NA                NA
#> 34                 NA                     NA                NA
#> 35                 NA                     NA                NA
#> 36                 NA                     NA                NA
#> 37                 NA                     NA                NA
#> 38                 NA                     NA                NA
#> 39                 NA                     NA                NA
#> 40                 NA                     NA                NA
#> 41                 NA                     NA                NA
#> 42                 NA                     NA                NA
#> 43                 NA                     NA                NA
#> 44                 NA                     NA                NA
#> 45                 NA                     NA                NA
#> 46                 NA                     NA                NA
#> 47                 NA                     NA                NA
#> 48                 NA                     NA                NA
#> 49                 NA                     NA                NA
#> 50                 NA                     NA                NA
#> 51                 NA                     NA                NA
#> 52                 NA                     NA                NA
#> 53                 NA                     NA                NA
#> 54                 NA                     NA                NA
#> 55                 NA                     NA                NA
#> 56                 NA                     NA                NA
#> 57                 NA                     NA                NA
#> 58                 NA                     NA                NA
#> 59                 NA                     NA                NA
#> 60                 NA                     NA                NA
#> 61                 NA                     NA                NA
#> 62                 NA                     NA                NA
#> 63                 NA                     NA                NA
#> 64                 NA                     NA                NA
#> 65                 NA                     NA                NA
#> 66                 NA                     NA                NA
#> 67                 NA                     NA                NA
#> 68                 NA                     NA                NA
#> 69                 NA                     NA                NA
#> 70                 NA                     NA                NA
#> 71                 NA                     NA                NA
#> 72                 NA                     NA                NA
#> 73                 NA                     NA                NA
#> 74                 NA                     NA                NA
#> 75                 NA                     NA                NA
#> 76                 NA                     NA                NA
#> 77                 NA                     NA                NA
#> 78                 NA                     NA                NA
#> 79                 NA                     NA                NA
#> 80                 NA                     NA                NA
#> 81                 NA                     NA                NA
#> 82                 NA                     NA                NA
#> 83                 NA                     NA                NA
#> 84                 NA                     NA                NA
#> 85                 NA                     NA                NA
#> 86                 NA                     NA                NA
#> 87                 NA                     NA                NA
#> 88                 NA                     NA                NA
#> 89                 NA                     NA                NA
#> 90                 NA                     NA                NA
#> 91                 NA                     NA                NA
#> 92                 NA                     NA                NA
#> 93                 NA                     NA                NA
#> 94                 NA                     NA                NA
#> 95                 NA                     NA                NA
#> 96                 NA                     NA                NA
#> 97                 NA                     NA                NA
#> 98                 NA                     NA                NA
#> 99                 NA                     NA                NA
#> 100                NA                     NA                NA
#> 101                NA                     NA                NA
#> 102                NA                     NA                NA
#> 103                NA                     NA                NA
#> 104                NA                     NA                NA
#> 105                NA                     NA                NA
#> 106                NA                     NA                NA
#> 107                NA                     NA                NA
#> 108                NA                     NA                NA
#> 109                NA                     NA                NA
#> 110                NA                     NA                NA
#> 111                NA                     NA                NA
#> 112                NA                     NA                NA
#> 113                NA                     NA                NA
#> 114                NA                     NA                NA
#> 115                NA                     NA                NA
#> 116                NA                     NA                NA
#> 117                NA                     NA                NA
#> 118                NA                     NA                NA
#> 119                NA                     NA                NA
#> 120                NA                     NA                NA
#> 121                NA                     NA                NA
#> 122                NA                     NA                NA
#> 123                NA                     NA                NA
#> 124                NA                     NA                NA
#> 125                NA                     NA                NA
#> 126                NA                     NA                NA
#> 127                NA                     NA                NA
#> 128                NA                     NA                NA
#> 129                NA                     NA                NA
#> 130                NA                     NA                NA
#> 131                NA                     NA                NA
#> 132                NA                     NA                NA
#> 133                NA                     NA                NA
#> 134                NA                     NA                NA
#> 135                NA                     NA                NA
#> 136                NA                     NA                NA
#> 137                NA                     NA                NA
#> 138                NA                     NA                NA
#> 139                NA                     NA                NA
#> 140                NA                     NA                NA
#> 141                NA                     NA                NA
#> 142                NA                     NA                NA
#> 143                NA                     NA                NA
#> 144                NA                     NA                NA
#> 145                NA                     NA                NA
#> 146                NA                     NA                NA
#> 147                NA                     NA                NA
#> 148                NA                     NA                NA
#> 149                NA                     NA                NA
#> 150                NA                     NA                NA
#> 151                NA                     NA                NA
#> 152                NA                     NA                NA
#> 153                NA                     NA                NA
#> 154                NA                     NA                NA
#> 155                NA                     NA                NA
#> 156                NA                     NA                NA
#> 157                NA                     NA                NA
#> 158                NA                     NA                NA
#> 159                NA                     NA                NA
#> 160                NA                     NA                NA
#>     ObservationDescriptor3 ObservationCount4 ObservationDescriptor4
#> 1                       NA                NA                     NA
#> 2                       NA                NA                     NA
#> 3                       NA                NA                     NA
#> 4                       NA                NA                     NA
#> 5                       NA                NA                     NA
#> 6                       NA                NA                     NA
#> 7                       NA                NA                     NA
#> 8                       NA                NA                     NA
#> 9                       NA                NA                     NA
#> 10                      NA                NA                     NA
#> 11                      NA                NA                     NA
#> 12                      NA                NA                     NA
#> 13                      NA                NA                     NA
#> 14                      NA                NA                     NA
#> 15                      NA                NA                     NA
#> 16                      NA                NA                     NA
#> 17                      NA                NA                     NA
#> 18                      NA                NA                     NA
#> 19                      NA                NA                     NA
#> 20                      NA                NA                     NA
#> 21                      NA                NA                     NA
#> 22                      NA                NA                     NA
#> 23                      NA                NA                     NA
#> 24                      NA                NA                     NA
#> 25                      NA                NA                     NA
#> 26                      NA                NA                     NA
#> 27                      NA                NA                     NA
#> 28                      NA                NA                     NA
#> 29                      NA                NA                     NA
#> 30                      NA                NA                     NA
#> 31                      NA                NA                     NA
#> 32                      NA                NA                     NA
#> 33                      NA                NA                     NA
#> 34                      NA                NA                     NA
#> 35                      NA                NA                     NA
#> 36                      NA                NA                     NA
#> 37                      NA                NA                     NA
#> 38                      NA                NA                     NA
#> 39                      NA                NA                     NA
#> 40                      NA                NA                     NA
#> 41                      NA                NA                     NA
#> 42                      NA                NA                     NA
#> 43                      NA                NA                     NA
#> 44                      NA                NA                     NA
#> 45                      NA                NA                     NA
#> 46                      NA                NA                     NA
#> 47                      NA                NA                     NA
#> 48                      NA                NA                     NA
#> 49                      NA                NA                     NA
#> 50                      NA                NA                     NA
#> 51                      NA                NA                     NA
#> 52                      NA                NA                     NA
#> 53                      NA                NA                     NA
#> 54                      NA                NA                     NA
#> 55                      NA                NA                     NA
#> 56                      NA                NA                     NA
#> 57                      NA                NA                     NA
#> 58                      NA                NA                     NA
#> 59                      NA                NA                     NA
#> 60                      NA                NA                     NA
#> 61                      NA                NA                     NA
#> 62                      NA                NA                     NA
#> 63                      NA                NA                     NA
#> 64                      NA                NA                     NA
#> 65                      NA                NA                     NA
#> 66                      NA                NA                     NA
#> 67                      NA                NA                     NA
#> 68                      NA                NA                     NA
#> 69                      NA                NA                     NA
#> 70                      NA                NA                     NA
#> 71                      NA                NA                     NA
#> 72                      NA                NA                     NA
#> 73                      NA                NA                     NA
#> 74                      NA                NA                     NA
#> 75                      NA                NA                     NA
#> 76                      NA                NA                     NA
#> 77                      NA                NA                     NA
#> 78                      NA                NA                     NA
#> 79                      NA                NA                     NA
#> 80                      NA                NA                     NA
#> 81                      NA                NA                     NA
#> 82                      NA                NA                     NA
#> 83                      NA                NA                     NA
#> 84                      NA                NA                     NA
#> 85                      NA                NA                     NA
#> 86                      NA                NA                     NA
#> 87                      NA                NA                     NA
#> 88                      NA                NA                     NA
#> 89                      NA                NA                     NA
#> 90                      NA                NA                     NA
#> 91                      NA                NA                     NA
#> 92                      NA                NA                     NA
#> 93                      NA                NA                     NA
#> 94                      NA                NA                     NA
#> 95                      NA                NA                     NA
#> 96                      NA                NA                     NA
#> 97                      NA                NA                     NA
#> 98                      NA                NA                     NA
#> 99                      NA                NA                     NA
#> 100                     NA                NA                     NA
#> 101                     NA                NA                     NA
#> 102                     NA                NA                     NA
#> 103                     NA                NA                     NA
#> 104                     NA                NA                     NA
#> 105                     NA                NA                     NA
#> 106                     NA                NA                     NA
#> 107                     NA                NA                     NA
#> 108                     NA                NA                     NA
#> 109                     NA                NA                     NA
#> 110                     NA                NA                     NA
#> 111                     NA                NA                     NA
#> 112                     NA                NA                     NA
#> 113                     NA                NA                     NA
#> 114                     NA                NA                     NA
#> 115                     NA                NA                     NA
#> 116                     NA                NA                     NA
#> 117                     NA                NA                     NA
#> 118                     NA                NA                     NA
#> 119                     NA                NA                     NA
#> 120                     NA                NA                     NA
#> 121                     NA                NA                     NA
#> 122                     NA                NA                     NA
#> 123                     NA                NA                     NA
#> 124                     NA                NA                     NA
#> 125                     NA                NA                     NA
#> 126                     NA                NA                     NA
#> 127                     NA                NA                     NA
#> 128                     NA                NA                     NA
#> 129                     NA                NA                     NA
#> 130                     NA                NA                     NA
#> 131                     NA                NA                     NA
#> 132                     NA                NA                     NA
#> 133                     NA                NA                     NA
#> 134                     NA                NA                     NA
#> 135                     NA                NA                     NA
#> 136                     NA                NA                     NA
#> 137                     NA                NA                     NA
#> 138                     NA                NA                     NA
#> 139                     NA                NA                     NA
#> 140                     NA                NA                     NA
#> 141                     NA                NA                     NA
#> 142                     NA                NA                     NA
#> 143                     NA                NA                     NA
#> 144                     NA                NA                     NA
#> 145                     NA                NA                     NA
#> 146                     NA                NA                     NA
#> 147                     NA                NA                     NA
#> 148                     NA                NA                     NA
#> 149                     NA                NA                     NA
#> 150                     NA                NA                     NA
#> 151                     NA                NA                     NA
#> 152                     NA                NA                     NA
#> 153                     NA                NA                     NA
#> 154                     NA                NA                     NA
#> 155                     NA                NA                     NA
#> 156                     NA                NA                     NA
#> 157                     NA                NA                     NA
#> 158                     NA                NA                     NA
#> 159                     NA                NA                     NA
#> 160                     NA                NA                     NA
#>     ObservationCount5 ObservationDescriptor5 ObservationCount6
#> 1                  NA                     NA                NA
#> 2                  NA                     NA                NA
#> 3                  NA                     NA                NA
#> 4                  NA                     NA                NA
#> 5                  NA                     NA                NA
#> 6                  NA                     NA                NA
#> 7                  NA                     NA                NA
#> 8                  NA                     NA                NA
#> 9                  NA                     NA                NA
#> 10                 NA                     NA                NA
#> 11                 NA                     NA                NA
#> 12                 NA                     NA                NA
#> 13                 NA                     NA                NA
#> 14                 NA                     NA                NA
#> 15                 NA                     NA                NA
#> 16                 NA                     NA                NA
#> 17                 NA                     NA                NA
#> 18                 NA                     NA                NA
#> 19                 NA                     NA                NA
#> 20                 NA                     NA                NA
#> 21                 NA                     NA                NA
#> 22                 NA                     NA                NA
#> 23                 NA                     NA                NA
#> 24                 NA                     NA                NA
#> 25                 NA                     NA                NA
#> 26                 NA                     NA                NA
#> 27                 NA                     NA                NA
#> 28                 NA                     NA                NA
#> 29                 NA                     NA                NA
#> 30                 NA                     NA                NA
#> 31                 NA                     NA                NA
#> 32                 NA                     NA                NA
#> 33                 NA                     NA                NA
#> 34                 NA                     NA                NA
#> 35                 NA                     NA                NA
#> 36                 NA                     NA                NA
#> 37                 NA                     NA                NA
#> 38                 NA                     NA                NA
#> 39                 NA                     NA                NA
#> 40                 NA                     NA                NA
#> 41                 NA                     NA                NA
#> 42                 NA                     NA                NA
#> 43                 NA                     NA                NA
#> 44                 NA                     NA                NA
#> 45                 NA                     NA                NA
#> 46                 NA                     NA                NA
#> 47                 NA                     NA                NA
#> 48                 NA                     NA                NA
#> 49                 NA                     NA                NA
#> 50                 NA                     NA                NA
#> 51                 NA                     NA                NA
#> 52                 NA                     NA                NA
#> 53                 NA                     NA                NA
#> 54                 NA                     NA                NA
#> 55                 NA                     NA                NA
#> 56                 NA                     NA                NA
#> 57                 NA                     NA                NA
#> 58                 NA                     NA                NA
#> 59                 NA                     NA                NA
#> 60                 NA                     NA                NA
#> 61                 NA                     NA                NA
#> 62                 NA                     NA                NA
#> 63                 NA                     NA                NA
#> 64                 NA                     NA                NA
#> 65                 NA                     NA                NA
#> 66                 NA                     NA                NA
#> 67                 NA                     NA                NA
#> 68                 NA                     NA                NA
#> 69                 NA                     NA                NA
#> 70                 NA                     NA                NA
#> 71                 NA                     NA                NA
#> 72                 NA                     NA                NA
#> 73                 NA                     NA                NA
#> 74                 NA                     NA                NA
#> 75                 NA                     NA                NA
#> 76                 NA                     NA                NA
#> 77                 NA                     NA                NA
#> 78                 NA                     NA                NA
#> 79                 NA                     NA                NA
#> 80                 NA                     NA                NA
#> 81                 NA                     NA                NA
#> 82                 NA                     NA                NA
#> 83                 NA                     NA                NA
#> 84                 NA                     NA                NA
#> 85                 NA                     NA                NA
#> 86                 NA                     NA                NA
#> 87                 NA                     NA                NA
#> 88                 NA                     NA                NA
#> 89                 NA                     NA                NA
#> 90                 NA                     NA                NA
#> 91                 NA                     NA                NA
#> 92                 NA                     NA                NA
#> 93                 NA                     NA                NA
#> 94                 NA                     NA                NA
#> 95                 NA                     NA                NA
#> 96                 NA                     NA                NA
#> 97                 NA                     NA                NA
#> 98                 NA                     NA                NA
#> 99                 NA                     NA                NA
#> 100                NA                     NA                NA
#> 101                NA                     NA                NA
#> 102                NA                     NA                NA
#> 103                NA                     NA                NA
#> 104                NA                     NA                NA
#> 105                NA                     NA                NA
#> 106                NA                     NA                NA
#> 107                NA                     NA                NA
#> 108                NA                     NA                NA
#> 109                NA                     NA                NA
#> 110                NA                     NA                NA
#> 111                NA                     NA                NA
#> 112                NA                     NA                NA
#> 113                NA                     NA                NA
#> 114                NA                     NA                NA
#> 115                NA                     NA                NA
#> 116                NA                     NA                NA
#> 117                NA                     NA                NA
#> 118                NA                     NA                NA
#> 119                NA                     NA                NA
#> 120                NA                     NA                NA
#> 121                NA                     NA                NA
#> 122                NA                     NA                NA
#> 123                NA                     NA                NA
#> 124                NA                     NA                NA
#> 125                NA                     NA                NA
#> 126                NA                     NA                NA
#> 127                NA                     NA                NA
#> 128                NA                     NA                NA
#> 129                NA                     NA                NA
#> 130                NA                     NA                NA
#> 131                NA                     NA                NA
#> 132                NA                     NA                NA
#> 133                NA                     NA                NA
#> 134                NA                     NA                NA
#> 135                NA                     NA                NA
#> 136                NA                     NA                NA
#> 137                NA                     NA                NA
#> 138                NA                     NA                NA
#> 139                NA                     NA                NA
#> 140                NA                     NA                NA
#> 141                NA                     NA                NA
#> 142                NA                     NA                NA
#> 143                NA                     NA                NA
#> 144                NA                     NA                NA
#> 145                NA                     NA                NA
#> 146                NA                     NA                NA
#> 147                NA                     NA                NA
#> 148                NA                     NA                NA
#> 149                NA                     NA                NA
#> 150                NA                     NA                NA
#> 151                NA                     NA                NA
#> 152                NA                     NA                NA
#> 153                NA                     NA                NA
#> 154                NA                     NA                NA
#> 155                NA                     NA                NA
#> 156                NA                     NA                NA
#> 157                NA                     NA                NA
#> 158                NA                     NA                NA
#> 159                NA                     NA                NA
#> 160                NA                     NA                NA
#>     ObservationDescriptor6 AllIndividualsReported AllSpeciesReported
#> 1                       NA                    Yes                Yes
#> 2                       NA                    Yes                Yes
#> 3                       NA                    Yes                Yes
#> 4                       NA                    Yes                Yes
#> 5                       NA                    Yes                Yes
#> 6                       NA                    Yes                Yes
#> 7                       NA                    Yes                Yes
#> 8                       NA                    Yes                Yes
#> 9                       NA                    Yes                Yes
#> 10                      NA                    Yes                Yes
#> 11                      NA                    Yes                Yes
#> 12                      NA                    Yes                Yes
#> 13                      NA                    Yes                Yes
#> 14                      NA                    Yes                Yes
#> 15                      NA                    Yes                Yes
#> 16                      NA                    Yes                Yes
#> 17                      NA                    Yes                Yes
#> 18                      NA                    Yes                Yes
#> 19                      NA                    Yes                Yes
#> 20                      NA                    Yes                Yes
#> 21                      NA                    Yes                Yes
#> 22                      NA                    Yes                Yes
#> 23                      NA                    Yes                Yes
#> 24                      NA                    Yes                Yes
#> 25                      NA                    Yes                Yes
#> 26                      NA                    Yes                Yes
#> 27                      NA                    Yes                Yes
#> 28                      NA                    Yes                Yes
#> 29                      NA                    Yes                Yes
#> 30                      NA                    Yes                Yes
#> 31                      NA                    Yes                Yes
#> 32                      NA                    Yes                Yes
#> 33                      NA                    Yes                Yes
#> 34                      NA                    Yes                Yes
#> 35                      NA                    Yes                Yes
#> 36                      NA                    Yes                Yes
#> 37                      NA                    Yes                Yes
#> 38                      NA                    Yes                Yes
#> 39                      NA                    Yes                Yes
#> 40                      NA                    Yes                Yes
#> 41                      NA                    Yes                Yes
#> 42                      NA                    Yes                Yes
#> 43                      NA                    Yes                Yes
#> 44                      NA                    Yes                Yes
#> 45                      NA                    Yes                Yes
#> 46                      NA                    Yes                Yes
#> 47                      NA                    Yes                Yes
#> 48                      NA                    Yes                Yes
#> 49                      NA                    Yes                Yes
#> 50                      NA                    Yes                Yes
#> 51                      NA                    Yes                Yes
#> 52                      NA                    Yes                Yes
#> 53                      NA                    Yes                Yes
#> 54                      NA                    Yes                Yes
#> 55                      NA                    Yes                Yes
#> 56                      NA                    Yes                Yes
#> 57                      NA                    Yes                Yes
#> 58                      NA                    Yes                Yes
#> 59                      NA                    Yes                Yes
#> 60                      NA                    Yes                Yes
#> 61                      NA                    Yes                Yes
#> 62                      NA                    Yes                Yes
#> 63                      NA                    Yes                Yes
#> 64                      NA                    Yes                Yes
#> 65                      NA                    Yes                Yes
#> 66                      NA                    Yes                Yes
#> 67                      NA                    Yes                Yes
#> 68                      NA                    Yes                Yes
#> 69                      NA                    Yes                Yes
#> 70                      NA                    Yes                Yes
#> 71                      NA                    Yes                Yes
#> 72                      NA                    Yes                Yes
#> 73                      NA                    Yes                Yes
#> 74                      NA                    Yes                Yes
#> 75                      NA                    Yes                Yes
#> 76                      NA                    Yes                Yes
#> 77                      NA                    Yes                Yes
#> 78                      NA                    Yes                Yes
#> 79                      NA                    Yes                Yes
#> 80                      NA                    Yes                Yes
#> 81                      NA                    Yes                Yes
#> 82                      NA                    Yes                Yes
#> 83                      NA                    Yes                Yes
#> 84                      NA                    Yes                Yes
#> 85                      NA                    Yes                Yes
#> 86                      NA                    Yes                Yes
#> 87                      NA                    Yes                Yes
#> 88                      NA                    Yes                Yes
#> 89                      NA                    Yes                Yes
#> 90                      NA                    Yes                Yes
#> 91                      NA                    Yes                Yes
#> 92                      NA                    Yes                Yes
#> 93                      NA                    Yes                Yes
#> 94                      NA                    Yes                Yes
#> 95                      NA                    Yes                Yes
#> 96                      NA                    Yes                Yes
#> 97                      NA                    Yes                Yes
#> 98                      NA                    Yes                Yes
#> 99                      NA                    Yes                Yes
#> 100                     NA                    Yes                Yes
#> 101                     NA                    Yes                Yes
#> 102                     NA                    Yes                Yes
#> 103                     NA                    Yes                Yes
#> 104                     NA                    Yes                Yes
#> 105                     NA                    Yes                Yes
#> 106                     NA                    Yes                Yes
#> 107                     NA                    Yes                Yes
#> 108                     NA                    Yes                Yes
#> 109                     NA                    Yes                Yes
#> 110                     NA                    Yes                Yes
#> 111                     NA                    Yes                Yes
#> 112                     NA                    Yes                Yes
#> 113                     NA                    Yes                Yes
#> 114                     NA                    Yes                Yes
#> 115                     NA                    Yes                Yes
#> 116                     NA                    Yes                Yes
#> 117                     NA                    Yes                Yes
#> 118                     NA                    Yes                Yes
#> 119                     NA                    Yes                Yes
#> 120                     NA                    Yes                Yes
#> 121                     NA                    Yes                Yes
#> 122                     NA                    Yes                Yes
#> 123                     NA                    Yes                Yes
#> 124                     NA                    Yes                Yes
#> 125                     NA                    Yes                Yes
#> 126                     NA                    Yes                Yes
#> 127                     NA                    Yes                Yes
#> 128                     NA                    Yes                Yes
#> 129                     NA                    Yes                Yes
#> 130                     NA                    Yes                Yes
#> 131                     NA                    Yes                Yes
#> 132                     NA                    Yes                Yes
#> 133                     NA                    Yes                Yes
#> 134                     NA                    Yes                Yes
#> 135                     NA                    Yes                Yes
#> 136                     NA                    Yes                Yes
#> 137                     NA                    Yes                Yes
#> 138                     NA                    Yes                Yes
#> 139                     NA                    Yes                Yes
#> 140                     NA                    Yes                Yes
#> 141                     NA                    Yes                Yes
#> 142                     NA                    Yes                Yes
#> 143                     NA                    Yes                Yes
#> 144                     NA                    Yes                Yes
#> 145                     NA                    Yes                Yes
#> 146                     NA                    Yes                Yes
#> 147                     NA                    Yes                Yes
#> 148                     NA                    Yes                Yes
#> 149                     NA                    Yes                Yes
#> 150                     NA                    Yes                Yes
#> 151                     NA                    Yes                Yes
#> 152                     NA                    Yes                Yes
#> 153                     NA                    Yes                Yes
#> 154                     NA                    Yes                Yes
#> 155                     NA                    Yes                Yes
#> 156                     NA                    Yes                Yes
#> 157                     NA                    Yes                Yes
#> 158                     NA                    Yes                Yes
#> 159                     NA                    Yes                Yes
#> 160                     NA                    Yes                Yes

# Convert to sf POINT object
bcch <- sf::st_as_sf(
  bcch,
  coords = c("longitude", "latitude"),
  crs = 4326
)

# Load Terrain Tiles data
elev <- elevation_download(data = bcch)
#> [Elevation Download] downloading data.
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'rast': The elevatr package requires longitude in a range from -180 to 180.

# Extract Terrain Tiles data
output <- elevation_extract(data = bcch, elevation_data = elev)
#> Error in elevation_extract(data = bcch, elevation_data = elev): object 'covariates' not found
```
