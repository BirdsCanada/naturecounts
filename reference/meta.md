# Metadata

These functions return metadata codes, names, descriptions, and
information associated with the data downloaded from NatureCounts.

## Usage

``` r
meta_country_codes()

meta_statprov_codes()

meta_subnational2_codes()

meta_iba_codes()

meta_bcr_codes()

meta_utm_squares()

meta_species_authority()

meta_species_codes()

meta_species_taxonomy()

meta_collections()

meta_breeding_codes()

meta_project_protocols()

meta_projects()

meta_protocol_types()

meta_bmde_versions()

meta_bmde_fields(version = "minimum")
```

## Arguments

- version:

  Character. BMDE version for which to return fields. NULL returns all
  versions

## Value

Data frame

## Details

Some of these metadata are stored locally and can be updated with the
[`nc_metadata()`](https://birdscanada.github.io/naturecounts/reference/nc_metadata.md)
function. Others are downloaded as requested.

**Metadata stored locally** - use
[`nc_metadata()`](https://birdscanada.github.io/naturecounts/reference/nc_metadata.md)
to update

- `meta_country_codes()`

- `meta_statprov_codes()`

- `meta_subnational2_codes()`

- `meta_iba_codes()`

- `meta_bcr_codes()`

- `meta_utm_squares()` - use `nc_metadata(utm = TRUE)` to update (big
  update)

- `meta_species_authority()`

- `meta_species_codes()`

- `meta_species_taxonomy()`

**Metadata always fetched from NatureCounts**

- `meta_collections()()`

- `meta_breeding_codes()`

- `meta_project_protocols()`

- `meta_projects()`

- `meta_protocol_types()`

- `meta_bmde_versions()`

## Functions

- `meta_country_codes()`: Country codes

- `meta_statprov_codes()`: State/Province codes

- `meta_subnational2_codes()`: Subnational2 codes

- `meta_iba_codes()`: Important Bird Area (IBA) codes

- `meta_bcr_codes()`: Bird Conservation Region (BCR) codes

- `meta_utm_squares()`: UTM Square codes

- `meta_species_authority()`: Species taxonomic authorities

- `meta_species_codes()`: Alpha-numeric codes for avian species

- `meta_species_taxonomy()`: Codes and taxonomic information for all
  species

- `meta_collections()`: Collections names and descriptions

- `meta_breeding_codes()`: Breeding codes and descriptions

- `meta_project_protocols()`: Project protocols

- `meta_projects()`: Projects ids, names, websites, and descriptions

- `meta_protocol_types()`: Protocol types and descriptions

- `meta_bmde_versions()`: Names and descriptions of the available
  versions of BMDE (Bird Monitoring Data Exchange). These refer to sets
  of fields/columns which can be downloaded for a given group of data.
  See
  [`nc_data_dl()`](https://birdscanada.github.io/naturecounts/reference/nc_data_dl.md)
  for more details.

- `meta_bmde_fields()`: Fields/columns associated with a particular BMDE
  (Bird Monitoring Data Exchange) version. See `meta_bmde_versions()`
  for the different versions available, `meta_collections()` for which
  version is used by which project, and
  [`nc_data_dl()`](https://birdscanada.github.io/naturecounts/reference/nc_data_dl.md)
  for more details on downloading data with a given set of
  fields/columns.

## Examples

``` r
# Return fields/columns in the 'minimum' version
meta_bmde_fields()
#> Metadata hasn't been updated in >4 weeks, consider using `nc_metadata()` to update local copies.
#> # A tibble: 37 × 3
#>    field_order version      local_name            
#>          <int> <chr>        <chr>                 
#>  1           1 BMDE2.00-min GlobalUniqueIdentifier
#>  2           2 BMDE2.00-min CatalogNumber         
#>  3           3 BMDE2.00-min Locality              
#>  4           4 BMDE2.00-min TimeCollected         
#>  5           5 BMDE2.00-min CollectorNumber       
#>  6           6 BMDE2.00-min FieldNumber           
#>  7           7 BMDE2.00-min Remarks               
#>  8           8 BMDE2.00-min ProjectCode           
#>  9           9 BMDE2.00-min ProtocolType          
#> 10          10 BMDE2.00-min ProtocolCode          
#> # ℹ 27 more rows

# Retrun fields/columns in the 'core' version
meta_bmde_fields(version = "core")
#> Metadata hasn't been updated in >4 weeks, consider using `nc_metadata()` to update local copies.
#> # A tibble: 169 × 3
#>    field_order version  local_name            
#>          <int> <chr>    <chr>                 
#>  1           1 BMDE2.00 GlobalUniqueIdentifier
#>  2           2 BMDE2.00 DateLastModified      
#>  3           3 BMDE2.00 BasisOfRecord         
#>  4           4 BMDE2.00 InstitutionCode       
#>  5           5 BMDE2.00 CollectionCode        
#>  6           6 BMDE2.00 CatalogNumber         
#>  7           7 BMDE2.00 ScientificName        
#>  8           8 BMDE2.00 HigherTaxon           
#>  9           9 BMDE2.00 Kingdom               
#> 10          10 BMDE2.00 Phylum                
#> # ℹ 159 more rows

# Return all possible fields
meta_bmde_fields(version = "extended")
#> Metadata hasn't been updated in >4 weeks, consider using `nc_metadata()` to update local copies.
#> # A tibble: 265 × 3
#>    field_order version      local_name            
#>          <int> <chr>        <chr>                 
#>  1           1 BMDE2.00-ext GlobalUniqueIdentifier
#>  2           2 BMDE2.00-ext DateLastModified      
#>  3           3 BMDE2.00-ext BasisOfRecord         
#>  4           4 BMDE2.00-ext InstitutionID         
#>  5           5 BMDE2.00-ext InstitutionCode       
#>  6           6 BMDE2.00-ext CollectionCode        
#>  7           7 BMDE2.00-ext CatalogNumber         
#>  8           8 BMDE2.00-ext ScientificName        
#>  9           9 BMDE2.00-ext HigherTaxon           
#> 10          10 BMDE2.00-ext Kingdom               
#> # ℹ 255 more rows
```
