#' Metadata
#'
#' These functions return metadata codes, names, descriptions, and information
#' associated with the data downloaded from NatureCounts.
#'
#' @details Some of these metadata are stored locally and can be updated with
#'   the [nc_metadata()] function. Others are downloaded as requested.
#'
#' @return Data frame
#'
#' @name meta

NULL

#' @describeIn meta Country codes
#' @export
meta_country_codes <- function() {metadata_read("country_codes")}

#' @describeIn meta State/Province codes
#' @export
meta_statprov_codes <- function() {metadata_read("statprov_codes")}

#' @describeIn meta Subnational2 codes
#' @export
meta_subnational2_codes <- function() {metadata_read("subnational2_codes")}

#' @describeIn meta Important Bird Area (IBA) codes
#' @export
meta_iba_codes <- function() {metadata_read("iba_codes")}

#' @describeIn meta Bird Conservation Region (BCR) codes
#' @export
meta_bcr_codes <- function() {metadata_read("bcr_codes")}

#' @describeIn meta UTM Square codes
#' @export
meta_utm_squares <- function() {metadata_read("utm_squares")}

#' @describeIn meta Species taxonomic authorities
#' @export
meta_species_authority <- function() {metadata_read("species_authority")}

#' @describeIn meta Alpha-numeric codes for avian species
#' @export
meta_species_codes <- function() {metadata_read("species_codes")}

#' @describeIn meta Codes and taxonomic information for all species
#' @export
meta_species_taxonomy <- function() {metadata_read("species_taxonomy")}

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
  dplyr::left_join(p1, p2, by = c("project_id", "project_code",
                                  "project_name", "project_name_fr"))
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
  if(!is.null(version)) {
    version <- fields_set_check(version)
    f <- dplyr::filter(f, .data$version == !!version)
  }
  f
}