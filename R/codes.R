#' Find country, state/province, subnational2, IBA, or BCR codes
#'
#' Search for the correct codes to identify countries, states/provinces,
#' subnational2 areas, Important Bird Areas (IBA), or Bird Conservation Regions
#' (BCR). These are then used in the \code{\link{nc_data_dl}()} and
#' \code{\link{nc_count}()} functions.
#'
#' @param name Character. The location name to search for
#' @param type Character. One of "country", "statprov", "subnational2", "iba",
#'   or "bcr". The type of information to return.
#'
#' @return A data frame with the relevant codes and other information
#'
#' @examples
#'
#' region_search("Mexico", type = "country")   # MX
#'
#' region_search("Yucatan", type = "statprov") # Yucatán
#' region_search("Alberta", type = "statprov") # AB
#'
#' region_search("Edmonton", type = "subnational2") # CA.AB.11
#' region_search("Brandon", type = "subnational2")  # CA.MB.07
#'
#' region_search("hays reservoir", type = "iba") # AB075
#' region_search("rainforest", type = "bcr")     # 5
#'
#'
#' # Show all codes
#' region_search(type = "country")
#' region_search(type = "statprov")
#' region_search(type = "subnational2")
#' region_search(type = "iba")
#' region_search(type = "bcr")
#'
#' \donttest{
#' # Using the codes
#' nc_count(region = list(statprov = "AB"), years = 2010)
#' }
#'
#' @export

region_search <- function(name = NULL, type = "country"){
  if(!type %in% c("country", "statprov", "subnational2", "iba", "bcr")) {
    stop("'type' must be one of 'country', 'statprov', 'subnational2', ",
         "'iba', or 'bcr'", call. = FALSE)
  }

  df <- eval(parse(text = paste0("meta_", type, "_codes()")))
  columns <- stringr::str_subset(names(df), "name")

  if(!is.null(name)) df <- codes_search(name, df = df,
                                        code_column = paste0(type, "_code"),
                                        columns = columns)
  df
}


#' Find species codes
#'
#' Find species id codes by searching for scientific, English and French species
#' names.
#'
#' @param name Character. The species name to search for
#' @param show Character. Either "all" or "names" (default). Whether to return
#'   all taxonomic information or only a subset with species names
#' @param authority Character. If not NULL (default), return the alphanumeric
#'   code associated with avian species for this taxonomic authority.
#'
#' @return Data frame of species ids and taxonomic information
#'
#' @examples
#'
#' # Show all ids
#' species_search()
#'
#' species_search("chickadee")
#' species_search("black-capped chickadee")
#'
#' # Add alphanumeric code for BSCDATA authority
#' species_search("black-capped chickadee", authority = "BSCDATA")
#'
#' # Show all taxonomic information
#' species_search("black-capped chickadee", show = "all")
#'
#' # Using the codes
#' nc_count(species = 14280)
#'
#' @export
species_search <- function(name = NULL, show = "names", authority = NULL) {

  # Argument checks
  if(!show %in% c("all", "names")) {
    stop("'show' must be either 'all' or 'names'", call.= FALSE)
  }

  ids <- meta_species_taxonomy()

  if(!is.null(name)){
    search_columns <- c("scientific_name", "english_name", "french_name")
    ids <- codes_search(name, df = ids,
                        code_column = "species_id",
                        columns = search_columns)
  }

  # No rows?
  if(nrow(ids) == 0) {
    stop("No species matched your description. Either try again, or consider ",
         "searching through the species_taxonomy data frame by hand",
         call. = FALSE)
  }

  if(show == "names") {
    ids <- dplyr::select(ids, "species_id",
                         dplyr::one_of("scientific_name", "english_name",
                                       "french_name", "taxon_group"))
  }

  if(!is.null(authority)) {
    authority_check(authority)
    ids <- dplyr::left_join(ids, dplyr::select(meta_species_codes(), "species_id2",
                                               dplyr::one_of(authority)),
                            by = c("species_id" = "species_id2"))
  }

  ids
}

#' Search for bird species id codes by alphanumeric codes
#'
#' This is an advanced function for returning all Bird-related species id codes
#' based on the various alphanumeric codes used by different authorities.
#'
#' @param code Vector. Character or numeric code indicating a species for a
#'   given authority.
#' @param authority Character. The authority to compare codes against (defaults
#'   to "BSCDATA")
#' @param results Character. "all" returns codes for all related species
#'   (including subspecies and main species). "exact" returns only the code for
#'   exact species indicated by the code.
#'
#' @return A data frame of numeric species id codes and names
#'
#' @examples
#'
#' # Show all ids
#' species_code_search()
#'
#' # Get all species ids for house finches
#' species_code_search("HOFI")
#'
#' # Get all species ids for Dark-eyed Juncos
#' species_code_search("DEJU")
#'
#' # Get all species ids related to Yellow-rumped Warbler (Myrtle)
#' # NOTE! This includes Audubon's and the main, Yellow-rumped Warbler species
#' species_code_search("MYWA")
#'
#' # Get ONLY specific id related to Yellow-rumped Warbler (Myrtle)
#' species_code_search("MYWA", results = "exact")
#'
#' # Use the Christmas Bird Count authority
#' species_code_search(11609, authority = "CBC")
#'
#' # Look in more than one authority (note that the code only needs to match on
#' # of the authorities)
#' species_code_search("MYWA", authority = c("BCMA", "CBC"))
#'
#' @export

species_code_search <- function(code = NULL, authority = "BSCDATA",
                                results = "all") {

  # Argument checks
  authority_check(authority)

  if(!results %in% c("all", "exact")) {
    stop("'results' must be 'all' or 'exact'", call. = FALSE)
  }

  # Where to search
  code_column <- dplyr::if_else(results == "exact", "species_id2", "species_id")

  # Return matching rows
  ids <- meta_species_codes() %>%
    tidyr::gather("authority", "code",
                  -.data$species_id, -.data$rank, -.data$species_id2) %>%
    dplyr::filter(.data$authority %in% !!authority,
                  !is.na(.data$code)) %>%
    tidyr::spread("authority", "code") %>%
    dplyr::select(-rank) %>%
    dplyr::distinct()

  if(!is.null(code)) ids <- codes_search(code, df = ids,
                                         code_column = code_column,
                                         columns = authority)

  # No rows?
  if(nrow(ids) == 0) {
    stop("No species matched your description. Either try again, or consider ",
         "searching through the species_codes data frame by hand",
         call. = FALSE)
  }

  # We're interested in all/exact species_id2s
  ids <- dplyr::select(ids, species_id = "species_id2", authority) %>%
    dplyr::distinct()


  # Add in common/scientific names for reference
  dplyr::left_join(ids,
                   dplyr::select(meta_species_taxonomy(), "species_id",
                                 "scientific_name", "english_name",
                                 "french_name"),
                   by = "species_id")
}


#' Generic function to match code ids
#'
#' Returns a subset of the data frame where the specified columns match the
#' provided description.
#'
#' @param desc Character. Description to match in the codes
#' @param df Data frame. Data to look in
#' @param code_column Character. Column name which contains the codes
#' @param columns Character vector. Column name(s) which contain the values to
#'   match against the description.
#'
#' @return data frame of matching rows
#'
#' @keywords internal

codes_search <- function(desc, df, code_column, columns) {

  desc <- stringi::stri_trans_general(desc, "Latin-ASCII") %>%
    stringr::str_replace("-|_", " ")
  desc <- paste0("(", paste0(desc, collapse = ")|("), ")")

  codes <- df %>%
    tidyr::gather("cols", "name", dplyr::one_of(columns)) %>%
    dplyr::mutate(name = stringi::stri_trans_general(.data$name, "Latin-ASCII"),
                  name = stringr::str_replace(.data$name, "-|_", " ")) %>%
    dplyr::filter(
      stringr::str_detect(.data$name,
                          stringr::regex(desc, ignore_case = TRUE))) %>%
    dplyr::pull(!!code_column) %>%
    unique()

  dplyr::filter(df, !!rlang::sym(code_column) %in% codes)
}