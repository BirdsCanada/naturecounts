#' Find country, state/province, subnational2, IBA, or BCR codes
#'
#' Search for the correct codes to identify countries, states/provinces,
#' subnational2 areas, Important Bird Areas (IBA), or Bird Conservation Regions
#' (BCR). These are then used in the \code{\link{nc_data_dl}()} and
#' \code{\link{nc_count}()} functions.
#'
#' `region_search()` is deprecated in favour of `search_region()`
#'
#' @param name Character. The location name to search for
#' @param type Character. One of "country", "statprov", "subnational2", "iba",
#'   or "bcr". The type of information to return.
#'
#' @return A data frame with the relevant codes and other information
#'
#' @examples
#'
#' search_region("Mexico", type = "country")   # MX
#'
#' search_region("Yucatan", type = "statprov") # Yucatán
#' search_region("Alberta", type = "statprov") # AB
#'
#' search_region("Edmonton", type = "subnational2") # CA.AB.11
#' search_region("Brandon", type = "subnational2")  # CA.MB.07
#'
#' search_region("hays reservoir", type = "iba") # AB075
#' search_region("rainforest", type = "bcr")     # 5
#'
#'
#' # Show all codes
#' search_region(type = "country")
#' search_region(type = "statprov")
#' search_region(type = "subnational2")
#' search_region(type = "iba")
#' search_region(type = "bcr")
#'
#' \donttest{
#' # Using the codes
#' nc_count(region = list(statprov = "AB"), years = 2010)
#' }
#'
#' @aliases region_search
#' @export

search_region <- function(name = NULL, type = "country"){
  if(!type %in% c("country", "statprov", "subnational2", "iba", "bcr")) {
    stop("'type' must be one of 'country', 'statprov', 'subnational2', ",
         "'iba', or 'bcr'", call. = FALSE)
  }


  df <- eval(parse(text = paste0("meta_", type, "_codes()")))
  columns <- stringr::str_subset(names(df), "name")

  if(!is.null(name)) {
    df <- search_codes(name, df = df,
                       code_column = keys[[paste0(type, "_codes")]],
                       columns = columns)
  }
    df
}


#' Find species codes
#'
#' Find species id codes by searching for scientific, English and French species
#' names.
#'
#' `species_search()` is deprecated in favour of `search_species()`
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
#' search_species()
#'
#' search_species("chickadee")
#' search_species("black-capped chickadee")
#'
#' # Add alphanumeric code for BSCDATA authority
#' search_species("black-capped chickadee", authority = "BSCDATA")
#'
#' # Show all taxonomic information
#' search_species("black-capped chickadee", show = "all")
#'
#' # Using the codes
#' nc_count(species = 14280)
#'
#' @aliases species_search
#'
#' @export
search_species <- function(name = NULL, show = "names", authority = NULL) {

  # Argument checks
  if(!show %in% c("all", "names")) {
    stop("'show' must be either 'all' or 'names'", call.= FALSE)
  }

  ids <- meta_species_taxonomy()

  if(!is.null(name)){
    search_columns <- c("scientific_name", "english_name", "french_name")
    ids <- search_codes(name, df = ids,
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
                         tidyselect::any_of(c("scientific_name", "english_name",
                                              "french_name", "taxon_group")))
  }

  if(!is.null(authority)) {
    authority_check(authority)
    auth <- meta_species_codes() %>%
      dplyr::filter(.data$authority == authority) %>%
      dplyr::select("species_id2", !!authority := "species_code")

    ids <- dplyr::left_join(ids, auth, by = c("species_id" = "species_id2"))
  }

  ids
}

#' Search for bird species id codes by alphanumeric codes
#'
#' This is an advanced function for returning all Bird-related species id codes
#' based on the various alphanumeric codes used by different authorities.
#'
#' `species_code_search()` is deprecated in favour of `search_species_code()`
#'
#' @param code Vector. Character or numeric code indicating a species for a
#'   given authority.
#' @param authority Character. The authority to compare codes against (defaults
#'   to "BSCDATA")
#' @param results Character. "all" returns codes for all related species
#'   (including subspecies and main species). "exact" returns only the code for
#'   exact species indicated by the code.
#'
#' @details Species ids returned reflect both species and sub-species levels.
#'
#' @return A data frame of numeric species id codes and names
#'
#' @examples
#'
#' # Show all ids
#' search_species_code()
#'
#' # Get all species ids for house finches
#' search_species_code("HOFI")
#'
#' # Get all species ids for Dark-eyed Juncos
#' search_species_code("DEJU")
#'
#' # Get all species ids related to Yellow-rumped Warbler (Myrtle)
#' # NOTE! This includes Audubon's and the main, Yellow-rumped Warbler species
#' search_species_code("MYWA")
#'
#' # Get ONLY specific id related to Yellow-rumped Warbler (Myrtle)
#' search_species_code("MYWA", results = "exact")
#'
#' # Use the Christmas Bird Count authority
#' search_species_code(11609, authority = "CBC")
#'
#' # Look in more than one authority (note that the code only needs to match on
#' # of the authorities)
#' search_species_code("MYWA", authority = c("BCMA", "CBC"))
#'
#' @aliases species_code_search
#'
#' @export

search_species_code <- function(code = NULL, authority = "BSCDATA",
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
    dplyr::select("species_id", "species_code",
                  "authority", "species_id2", "rank") %>%
    dplyr::filter(.data$authority %in% !!authority) %>%
    tidyr::spread("authority", "species_code") %>%
    dplyr::select(-"rank") %>%
    dplyr::distinct()

  if(!is.null(code)) ids <- search_codes(code, df = ids,
                                         code_column = code_column,
                                         columns = authority)

  # No rows?
  if(nrow(ids) == 0) {
    stop("No species matched your description. Either try again, or consider ",
         "searching through the species_codes data frame by hand",
         call. = FALSE)
  }

  # We're interested in all/exact species_id2s
  ids <- dplyr::select(ids, species_id = "species_id2",
                       tidyselect::all_of(authority)) %>%
    dplyr::distinct()

  # Add in common/scientific names for reference
  dplyr::left_join(ids,
                   dplyr::select(meta_species_taxonomy(),
                                 "species_id",
                                 tidyselect::any_of(c("scientific_name",
                                                      "english_name",
                                                      "french_name"))),
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

search_codes <- function(desc, df, code_column, columns) {

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

#' @export
region_search <- function(name = NULL, type = "country"){
  nc_deprecate("search_region")
  search_region(name, type)
}

#' @export
species_search <- function(name = NULL, show = "names", authority = NULL) {
  nc_deprecate("search_species")
  search_species(name, show, authority)
}

#' @export
species_code_search <- function(code = NULL, authority = "BSCDATA",
                                results = "all") {
  nc_deprecate("search_species_code")
  search_species_code(code, authority, results)
}

