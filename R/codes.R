
#' Find country, state/province, or sub-national codes
#'
#' Search for the correct codes to identify countries, states/provinces or
#' sub-national areas. These are then used in the \code{\link{nc_data_dl}()} and
#' \code{\link{nc_count}()} functions.
#'
#' @param name Character. The location name to search for
#' @param type Character. One of "country", "statprov", or "subnat". The type
#'   of code to return.
#'
#' @return A data frame with the relevant codes and other information
#'
#' @examples
#'
#' location_search("Mexico", type = "country")   # MX
#'
#' location_search("Yucatan", type = "statprov") # Yucatán
#' location_search("Alberta", type = "statprov") # AB
#'
#' location_search("Edmonton", type = "subnat") # CA.AB.11
#' location_search("Brandon", type = "subnat")  # CA.MB.07
#'
#' \donttest{
#' # Using the codes
#' nc_count(statprov = "AB", startyear = 2010)
#' }
#'
#' @export

location_search <- function(name, type = "country"){
  if(!type %in% c("country", "statprov", "subnat")) {
    stop("'type' must be one of 'country', 'statprov', or 'subnat'.",
         call. = FALSE)
  }

  df <- eval(parse(text = paste0(type, "_codes()")))
  columns <- stringr::str_subset(names(df), "name")

  codes_search(name, df = df, code_column = paste0(type, "_code"),
               columns = columns)
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

  search_columns <- c("scientific_name", "english_name", "french_name")
  ids <- codes_search(name, df = species_taxonomy(), code_column = "species_id",
                      columns = search_columns)

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
    check_authority(authority)
    ids <- dplyr::left_join(ids, dplyr::select(species_codes(), "species_id2",
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
  check_authority(authority)

  if(!results %in% c("all", "exact")) {
    stop("'results' must be 'all' or 'exact'", call. = FALSE)
  }

  # Where to search
  code_column <- dplyr::if_else(results == "exact", "species_id2", "species_id")

  # Return matching rows
  df <- species_codes() %>%
    tidyr::gather("authority", "code",
                  -.data$species_id, -.data$rank, -.data$species_id2) %>%
    dplyr::filter(.data$authority %in% !!authority,
                  !is.na(.data$code)) %>%
    tidyr::spread("authority", "code") %>%
    dplyr::select(-rank) %>%
    dplyr::distinct()

  ids <- codes_search(code, df = df, code_column = code_column,
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
                   dplyr::select(species_taxonomy(), "species_id",
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



codes_check <- function(desc) {
  # If nothing, return as is
  if(is.null(desc)) return(desc)

  # Get type of code
  type <- deparse(substitute(desc))
  m <- ifelse(type == "species", "numeric", "character")

  # Get code data frames
  if(type == "species") {
    df <- species_taxonomy()
  } else df <- eval(parse(text = paste0(type, "_codes()")))

  vapply(desc, codes_check_each, type = type, df = df,
         FUN.VALUE = vector(length = 1, mode = m),
         USE.NAMES = FALSE)
}

codes_check_each <- function(desc, type, df) {
  # If a two character code, conver to upper, just in case
  if(type != "species" & stringr::str_detect(desc, "^[:alpha:]{2}$")) {
    desc <- toupper(desc)
    code_column <- paste0(type, "_code")
  } else {
    code_column <- paste0(type, "_id")
  }

  # Convert to numeric (if possible)
  desc <- as_numeric(desc)

  # If code, check that correct
  if((type == "species" & is.numeric(desc)) |
     (type != "species" & stringr::str_detect(desc, "^[:upper:]{2}$"))) {

    if(!desc %in% dplyr::pull(df, code_column)) {
      stop("'", type, "' code not found, see the ", type, "_codes",
           " data frame for valid codes", call. = FALSE)
    }
    return(desc)

  # If species, and not numeric, stop
  } else if(type == "species" & !is.numeric(desc)) {
    stop("'species' code must be a numeric code ",
         "(see documentation)", call. = FALSE)
  }

  # Otherwise try to convert
  codes_convert(desc, type)
}


codes_convert <- function(desc, type) {
  c <- location_search(desc, type) %>%
    dplyr::select(dplyr::contains("_code"),
                  dplyr::contains("_name")) %>%
    dplyr::distinct()

  if(nrow(c) == 0) {
    stop("Unable to match '", desc, "' to any codes in the ",
         paste0(type, "_codes"), " data frame", call. = FALSE)
  } else if(nrow(c) > 1) {
    stop("Matched '", desc, "' to ", nrow(c), " codes: \n",
         capture_df(c), call. = FALSE)
  }
  dplyr::pull(c, paste0(type, "_code"))
}

