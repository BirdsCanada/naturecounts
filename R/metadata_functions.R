#' Metadata
#'
#' These functions return metadata codes, names, descriptions, and information
#' associated with the data downloaded from NatureCounts.
#'
#' @details Some of these metadata are stored locally and can be updated with
#'   the [nc_metadata()] function. Others are downloaded as requested.
#'
#' **Metadata stored locally** - use `nc_metadata()` to update
#' - `meta_country_codes()`
#' - `meta_statprov_codes()`
#' - `meta_subnational2_codes()`
#' - `meta_iba_codes()`
#' - `meta_bcr_codes()`
#' - `meta_utm_squares()` - use `nc_metadata(utm = TRUE)` to update (big update)
#' - `meta_species_authority()`
#' - `meta_species_codes()`
#' - `meta_species_taxonomy()`
#'
#' **Metadata always fetched from NatureCounts**
#' - `meta_collections()()`
#' - `meta_breeding_codes()`
#' - `meta_project_protocols()`
#' - `meta_projects()`
#' - `meta_protocol_types()`
#' - `meta_bmde_versions()`
#'
#'
#' @return Data frame
#'
#' @name meta

NULL

#' @describeIn meta Country codes
#' @export
meta_country_codes <- function() {
  metadata_read("country_codes")
}

#' @describeIn meta State/Province codes
#' @export
meta_statprov_codes <- function() {
  metadata_read("statprov_codes")
}

#' @describeIn meta Subnational2 codes
#' @export
meta_subnational2_codes <- function() {
  metadata_read("subnational2_codes")
}

#' @describeIn meta Important Bird Area (IBA) codes
#' @export
meta_iba_codes <- function() {
  metadata_read("iba_codes")
}

#' @describeIn meta Bird Conservation Region (BCR) codes
#' @export
meta_bcr_codes <- function() {
  metadata_read("bcr_codes")
}

#' @describeIn meta UTM Square codes
#' @export
meta_utm_squares <- function() {
  have_pkg_check("sf")
  metadata_read("utm_squares")
}

#' @describeIn meta Species taxonomic authorities
#' @export
meta_species_authority <- function() {
  metadata_read("species_authority")
}

#' @describeIn meta Alpha-numeric codes for avian species
#' @export
meta_species_codes <- function() {
  metadata_read("species_codes")
}

#' @describeIn meta Codes and taxonomic information for all species
#' @export
meta_species_taxonomy <- function() {
  metadata_read("species_taxonomy")
}

#' @describeIn meta Collections names and descriptions
#' @export
meta_collections <- function() {
  srv_query(api$collections, timeout = 30) %>%
    parse_results()
}

#' @describeIn meta Breeding codes and descriptions
#' @export
meta_breeding_codes <- function() {
  srv_query(api$breeding_codes, timeout = 30) %>%
    parse_results()
}

#' @describeIn meta Project protocols
#' @export
meta_project_protocols <- function() {
  srv_query(api$project_protocols, timeout = 30) %>%
    parse_results()
}

#' @describeIn meta Projects ids, names, websites, and descriptions
#' @export
meta_projects <- function() {
  p1 <- srv_query(api$projects, timeout = 30) %>%
    parse_results()
  p2 <- srv_query(api$projects_meta, timeout = 30) %>%
    parse_results()
  dplyr::left_join(
    p1,
    p2,
    by = c("project_id", "project_code", "project_name", "project_name_fr")
  )
}

#' @describeIn meta Protocol types and descriptions
#' @export
meta_protocol_types <- function() {
  srv_query(api$protocol_types, timeout = 30) %>%
    parse_results()
}

#' @describeIn meta Names and descriptions of the available versions of BMDE
#'   (Bird Monitoring Data Exchange). These refer to sets of fields/columns
#'   which can be downloaded for a given group of data. See [nc_data_dl()] for
#'   more details.
#' @export
meta_bmde_versions <- function() {
  srv_query(api$bmde_versions, timeout = 30) %>%
    parse_results()
}

#' @describeIn meta Fields/columns associated with a particular BMDE (Bird
#'   Monitoring Data Exchange) version. See [meta_bmde_versions()] for the
#'   different versions available, [meta_collections()] for which version is
#'   used by which project, and [nc_data_dl()] for more details on downloading
#'   data with a given set of fields/columns.
#'
#' @param version Character. BMDE version for which to return fields. NULL
#'   returns all versions
#'
#' @examples
#' # Return fields/columns in the 'minimum' version
#' meta_bmde_fields()
#'
#' # Retrun fields/columns in the 'core' version
#' meta_bmde_fields(version = "core")
#'
#' # Return all possible fields
#' meta_bmde_fields(version = "extended")
#' @export
meta_bmde_fields <- function(version = "minimum") {
  # Check version
  f <- metadata_read("bmde_fields")
  if (!is.null(version)) {
    version <- fields_set_check(version)
    f <- dplyr::filter(f, .data$version == !!version)
  }
  f
}

#' Metadata for data sources for the covariate download and extraction functions.
#' 
#' @returns `data.frame` containing information on the resolution and source for 
#' the covariate data available through `naturecounts`.

nc_covariate_table <- function() {
  cov.table <- data.frame(
    covariate_name = c(
      "modis_lctype1",
      "modis_lctype2",
      "modis_lctype3",
      "modis_lctype4",
      "modis_lctype5",
      "modis_snow",
      "modis_ndvi",
      "modis_evi",
      "elevation",
      "worldclim_tavg",
      "worldclim_tmax",
      "worldclim_tmin",
      "worldclim_prec",
      "worldclim_srad",
      "worldclim_wind",
      "worldclim_vapr",
      "scanfi_biomass",
      "scanfi_closure",
      "scanfi_height",
      "scanfi_nfilc",
      "scanfi_balsamfir",
      "scanfi_blackspruce",
      "scanfi_douglasfir",
      "scanfi_jackpine",
      "scanfi_lodgepolepine",
      "scanfi_ponderosapine",
      "scanfi_tamarack",
      "scanfi_whiteredpine",
      "scanfi_broadleaf",
      "scanfi_otherconifer",
      "daymet_dayl",
      "daymet_prcp",
      "dayment_srad",
      "daymet_swe",
      "daymet_tmax",
      "daymet_tmin",
      "daymet_vp"
    ),
    covariate_source = c(
      "MODIS Land Cover - IGBP global vegetation classification scheme",
      "MODIS Land Cover - University of Maryland (UMD) scheme",
      "MODIS Land Cover - MODIS-derived LAI/fPAR scheme",
      "MODIS Land Cover - MODIS-derived Net Primary Production scheme",
      "MODIS Land Cover - Plant Functional Type (PFT) scheme",
      "MODIS Snow Cover",
      "MODIS Vegetation Indices - Normalized Difference Vegetation Index",
      "MODIS Vegetation Indices - Enhanced Vegetation Index",
      "AWS Terrain Tiles Elevation (m)",
      "WorldClim - Monthly Average Temperature (degC), 1970-2000",
      "WorldClim - Monthly Maximum Temperature (degC), 1970-2000",
      "WorldClim - Monthly Minimum Temperature (degC), 1970-2000",
      "WorldClim - Monthly Precipitation (mm), 1970-2000",
      "WorldClim - Monthly Solar Radiation (kJ/m^2/day), 1970-2000",
      "WorldClim - Monthly Average Wind Speed (m/s), 1970-2000",
      "WorldClim - Monthly Average Water Vapor Pressure (kPa), 1970-2000",
      "SCANFI - Biomass (tons/ha)",
      "SCANFI - Crown closure (% covered by tree canopy)",
      "SCANFI - Height (m)",
      "SCANFI - NFI land cover class",
      "SCANFI - Balsam Fir cover proportion of total crown cover",
      "SCANFI - Black Spruce cover proportion of total crown cover",
      "SCANFI - Douglas Fir cover proportion of total crown cover",
      "SCANFI - Jack Pine cover proportion of total crown cover",
      "SCANFI - Lodgepole Pine cover proportion of total crown cover",
      "SCANFI - Ponderosa Pine cover proportion of total crown cover",
      "SCANFI - Tamarack cover proportion of total crown cover",
      "SCANFI - White and Red Pine cover proportion of total crown cover",
      "SCANFI - Broadleaf tree species cover proportion of total crown cover",
      "SCANFI - Other Conifer Species cover proportion of total crown cover",
      "Daymet - Daylength (s/day)",
      "Daymet - Precipitation (mm/day)",
      "Daymet - Shortwave radiation (W/m^2)",
      "Daymet - Snow water equivalent (kg/m^2)",
      "Daymet - Maximum air temperature (degrees C)",
      "Daymet - Minimum air temperature (degrees C)",
      "Daymet - Water vapor pressure (Pa)"
    ),
    covariate_source_specific = c(
      rep("MCD12Q1", times = 5),
      "MOD10A1",
      rep("MOD13A1", times = 2),
      NA,
      rep("WorldClim Ver. 2.1", times = 7),
      rep("SCANFI Ver. 1.2", times = 14),
      rep("DAYMET Ver. 004", times = 7)
    ),
    temporal_resolution = c(
      rep("Annual", times = 5),
      "Daily",
      rep("16-Day", times = 2),
      rep("Static", times = 22),
      rep("Daily", times = 7)
    ),
    spatial_resolution = c(
      rep("500 m", times = 8),
      "~600-800m",
      rep("~1 km^2", times = 7),
      rep("30 m", times = 14),
      rep("1 km", times = 7)
    ),
    via = c(
      rep("luna", times = 8),
      "elevatr",
      rep("geodata", times = 7),
      rep("Direct Download", times = 14),
      rep("appeears", times = 7)
    ),
    documentation = c(
      rep("https://doi.org/10.5067/MODIS/MCD12Q1.061", times = 5),
      "http://doi.org/10.5067/MODIS/MOD10A1.061",
      rep("https://doi.org/10.5067/MODIS/MOD13A1.061", times = 2),
      "https://github.com/USEPA/elevatr",
      rep("https://worldclim.org/data/worldclim21.html", times = 7),
      rep(
        "https://doi.org/10.23687/18e6a919-53fd-41ce-b4e2-44a9707c52dc",
        times = 14
      ),
      rep("https://doi.org/10.3334/ORNLDAAC/1840", times = 7)
    )
  )
  
  return(cov.table)
}
