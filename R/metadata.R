#' Update NatureCounts metadata files
#'
#' Updates the local copies of meta data used by the package.
#'
#' @param verbose Logical. Show progress messages?
#'
#' @examples
#'
#' \donttest{
#' nc_metadata()
#' }
#' @export

nc_metadata <- function(verbose = TRUE) {
  nc_metadata_generic(system.file("extdata", package = "naturecounts"),
                      verbose = verbose)
}

metadata_save <- function(data, path, name = deparse(substitute(data))) {
  saveRDS(data, file.path(path, paste0(name, ".rds")), compress = TRUE)
}

metadata_read <- function(name) {
  readRDS(system.file("extdata", paste0(name, ".rds"),
                      package = "naturecounts"))
}


nc_metadata_generic <- function(path = "./inst/extdata", verbose = TRUE) {

  # Species authorities
  message("Updating species authority...")
  species_authority <- srv_query(api$species_authority) %>%
    parse_results(results = FALSE) %>%
    dplyr::select("authority", dplyr::everything())
  metadata_save(species_authority, path)

  # Get species codes
  message("Updating species codes...")
  species_codes <- species_authority$authority %>%
    lapply(., FUN = function(x) srv_query(api$species_codes,
                                          query = list(authority = x))) %>%
    lapply(parse_results, results = FALSE) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(species_id2 = dplyr::if_else(is.na(.data$species_id2),
                                               .data$species_id,
                                               .data$species_id2)) %>%
    tidyr::spread("authority", "species_code") %>%
    as.data.frame()
  metadata_save(species_codes, path)

  message("Updating species taxonomy...")
  species_taxonomy <- srv_query(api$species_taxonomy) %>%
    parse_results(results = FALSE)
  metadata_save(species_taxonomy, path)

  # Get country codes
  message("Updating country codes...")
  country_codes <- srv_query(api$country_codes) %>%
    parse_results(results = FALSE) %>%
    dplyr::arrange(.data$country_code)
  metadata_save(country_codes, path)

  # Get province/state codes
  message("Updating state/province codes...")
  statprov_codes <- srv_query(api$statprov_codes) %>%
    parse_results(results = FALSE) %>%
    dplyr::select("country_code", "statprov_code", dplyr::everything()) %>%
    dplyr::arrange(.data$country_code, .data$statprov_code)
  metadata_save(statprov_codes, path)

  # Get subnational codes
  message("Updating subnational codes...")
  subnat_codes <- srv_query(api$subnat_codes) %>%
    parse_results(results = FALSE) %>%
    dplyr::select("country_code", "statprov_code", subnat_code = "subnat2_code",
                  subnat_name = "subnat2_name", dplyr::everything()) %>%
    dplyr::arrange(.data$country_code, .data$statprov_code, .data$subnat_code)
  metadata_save(subnat_codes, path)

  # Update metadata version
  message("Metadata version updated from ",
          metadata_version(), " to ", metadata_version <- "2019.1")
  metadata_save(metadata_version, path)
}


country_codes <- function() metadata_read("country_codes")
statprov_codes <- function() metadata_read("statprov_codes")
subnat_codes <- function() metadata_read("subnat_codes")
species_authority <- function() metadata_read("species_authority")
species_codes <- function() metadata_read("species_codes")
species_taxonomy <- function() metadata_read("species_taxonomy")
metadata_version <- function() metadata_read("metadata_version")
