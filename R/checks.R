collections_check <- function(c) {
 if(!is.null(c) && !is.character(c)) {
   stop("'collections' must be either NULL (for all collections) ",
        "or a character vector of collection names.", call. = FALSE)
 }
}

authority_check <- function(a) {
  if(!all(a %in% species_authority()$authority)) {
    stop("'authority' must be one or more of the authorities ",
         "specified in the species_authority data frame.", call. = FALSE)
  }
}

fields_set_check <- function(fields_set) {
  v <- bmde_versions()

  if(length(fields_set) > 1 || !fields_set %in% c(v$version, "custom")) {
    if(!fields_set %in% v$shorthand) {
      stop("'field_set' must be either a 'version' or a 'shorthand' code ",
           "returned by bmde_version(), or 'custom'", call. = FALSE)
    }
    if(fields_set != "custom") {
      fields_set <- dplyr::filter(v, shorthand == fields_set) %>%
        dplyr::pull(version)
    }
  }
  fields_set
}


fields_check <- function(fields) {
  if(is.null(fields)) {
    stop("For a custom 'fields_set', specify 'fields'",
         call. = FALSE)
  } else {
    # Can't really check, because don't have list of internal fields
    # col <- nc_collections() %>%
    #   dplyr::filter(bmdr_code %in% collections) %>%
    #   dplyr::pull(bmde_version) %>%
    #   unique()
    # f <- vapply(col, FUN = function(x) list(bmde_fields(version = x)$field_name),
    #             FUN.VALUE = list("A")) %>%
    #   unlist() %>%
    #   unique()
    # if(!all(fields %in% f)) {
    #   problem <- fields[!fields %in% f]
  }
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

