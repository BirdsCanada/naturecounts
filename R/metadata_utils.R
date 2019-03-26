#' Update NatureCounts metadata files
#'
#' Updates the local copies of meta data used by the package.
#'
#' @param force Logical. Force update even if the remote version matches local?
#' @param utm Logical. Update [meta_utm_squares()] as well? **WARNING**: This is a
#'   large and time consuming download!
#' @param verbose Logical. Show progress messages?
#'
#' @examples
#' \donttest{nc_metadata()}
#'
#' @export

nc_metadata <- function(force = FALSE, utm = FALSE, verbose = TRUE) {
  nc_metadata_internal(system.file("extdata", package = "naturecounts"),
                       force = force, utm = utm, verbose = verbose)
}


#' Fetch API metadata version
#'
#' Returns the current version of the metadata on the API
#'
#' @keywords internal
metadata_v_remote <- function() {
 srv_query(api$version) %>%
    unlist()
}

metadata_save <- function(data, path, name = deparse(substitute(data)),
                          compress = TRUE) {
  save(data, file = file.path(path, paste0(name, ".rds")), compress = compress)
}

metadata_read <- function(name) {
  data <- NULL # load(f) reads data into envir as 'data', use this to avoid NOTE
  f <- system.file("extdata", paste0(name, ".rds"), package = "naturecounts")
  if(!file.exists(f)) stop("Could not find metadata file '", name, "'",
                           call. = FALSE)
  load(f)
  data
}


nc_metadata_internal <- function(path = "./inst/extdata", force = TRUE,
                                 utm = FALSE, verbose = TRUE) {

  # Check if update necessary
  # (either no version file, force = TRUE, or out of date)

  if(all(class(try(metadata_v_local(), silent = TRUE)) != "try-error") &&
    !force &&
    metadata_v_local() == metadata_v_remote()) {

    message("Local metadata already up-to-date with server")

  } else {

    # Species authorities
    message("Updating species authority...")
    species_authority <- srv_query(api$species_authority) %>%
      parse_results(results = FALSE) %>%
      dplyr::select("authority", dplyr::everything())
    metadata_save(species_authority, path)

    # Get species codes
    message("Updating species codes...")
    species_codes <- srv_query(api$species_codes) %>%
      parse_results(results = FALSE) %>%
      dplyr::mutate(species_id2 = dplyr::if_else(is.na(.data$species_id2),
                                                 .data$species_id,
                                                 .data$species_id2))
    metadata_save(species_codes, path = path)

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
    subnational2_codes <- srv_query(api$subnational2_codes) %>%
      parse_results(results = FALSE) %>%
      dplyr::select("country_code", "statprov_code",
                    subnational2_code = "subnat2_code",
                    subnational2_name = "subnat2_name", dplyr::everything()) %>%
      dplyr::arrange(.data$country_code, .data$statprov_code,
                     .data$subnational2_code)
    metadata_save(subnational2_codes, path)

    # Get IBA codes
    message("Updating IBA codes...")
    iba_codes <- srv_query(api$iba_codes) %>%
      parse_results(results = FALSE) %>%
      dplyr::rename_all(tolower) %>%
      dplyr::select("iba_site", dplyr::everything())
    metadata_save(iba_codes, path)

    # Get BCA codes
    message("Updating BCR codes...")
    bcr_codes <- srv_query(api$bcr_codes) %>%
      parse_results(results = FALSE) %>%
      dplyr::rename_all(tolower)
    metadata_save(bcr_codes, path)

    if(utm) {
      if(!requireNamespace("sf", quietly = TRUE)) {
        stop("The sf package is required to use and process utm_squares. ",
             "It can be installed with \"install.packages('sf')\"",
             call. = FALSE)
      }

      # Get UTM square codes
      message("Updating UTM squares codes...")
      utm_squares <- statprov_codes()$statprov_code %>%
        lapply(., function(x) {
          message("  Getting ", x)
          srv_query(api$utm_squares, query = list('statprov' = x))
        }) %>%
        lapply(parse_results, results = FALSE) %>%
        dplyr::bind_rows() %>%
        dplyr::rename("geometry" = "square_wkt") %>%
        sf::st_as_sf(., wkt = "geometry", crs = 3347)
      metadata_save(utm_squares, path, compress = "xz")
    }

    # Update metadata version
    message("Metadata version updated to ", metadata_v_remote())
    metadata_save(metadata_v_remote(), name = "metadata_v_local", path = path)
  }
}

metadata_v_local <- function() {metadata_read("metadata_v_local")}
