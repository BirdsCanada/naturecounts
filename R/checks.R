year_check <- function(y) {
  y <- as_numeric(y)
  if(!is.numeric(y) || y > lubridate::year(Sys.Date()) || y < 1900) {
    stop("Years must be numbers between 1900 and ",
         lubridate::year(Sys.Date()), call. = FALSE)
  }
  y
}

doy_check <- function(s) {

  stp <- FALSE

  if(stringr::str_detect(s, "^[:digit:]+$")) s <- as_numeric(s)
  if(is.numeric(s)) {
    if(s < 0 | s > 366) stp <- TRUE
    if(round(s) != s) stp <- TRUE
  } else {
    s <- suppressWarnings(lubridate::ymd_hms(s, truncated = 4)) %>%
      lubridate::yday()
    if(is.na(s)) stp <- TRUE
  }
  if(stp) stop("Day of year must be either a date (YM or YMD), ",
               "or a whole number (1-366)", call. = FALSE)
  s
}

collections_check <- function(c) {
  if(!is.character(c)) {
    stop("'collections' must be either NULL (for all collections) ",
         "or a character vector of collection names.", call. = FALSE)
  }
  c
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
      fields_set <- dplyr::filter(v, .data$shorthand == fields_set) %>%
        dplyr::pull(version)
    }
  }
  fields_set
}


fields_check <- function(fields) {
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
  fields
}

codes_check <- function(desc, type = NULL) {

  # Get type of code
  if(is.null(type)) type <- deparse(substitute(desc))
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


filter_check <- function(f) {
  for(i in 1:length(f)) {
    n <- names(f[i])
    if(n == "fields_set") f[[i]] <- fields_set_check(f[[i]])
    if(n == "fields") f[[i]] <- fields_check(f[[i]])
    if(stringr::str_detect(n, "collection")) f[[i]] <- collections_check(f[[i]])
    if(stringr::str_detect(n, "_year")) f[[i]] <- year_check(f[[i]])
    if(stringr::str_detect(n, "_doy")) f[[i]] <- doy_check(f[[i]])
    if(stringr::str_detect(n, "country|statprov|species")) {
      f[[i]] <- codes_check(f[[i]], type = n)
    }
  }
  f
}

redundancy_check <- function(f) {

  f <- f[!vapply(f, is.null, logical(1))]

  # Detect too many parameters
  if(length(f[!names(f) %in% c("fields", "fields_set")]) > 3) {
  stop("Only 3 arguments from `collections`, `species`, `years`, `doy`, ",
       "`region`, and `site_type` are permited. Remove ", length(f) - 3,
       " arguments and try again.", call. = FALSE)
  }

  # Detect redundancy in regions
  if(!is.null(f$region)) {

    if(!is.list(f$region) || is.null(names(f$region))) {
      stop("'regions' must be a named list", call. = FALSE)
    }

    region <- f$region
    if(length(region) > 1) {
      # If country/statprov/subnational2, take the smallest unit
      if(all(names(region) %in% c("country", "statprov", "subnational2"))) {
        locs <- factor(names(region),
                       levels = c("country", "statprov", "subnational2")) %>%
          sort()
        message("'country', 'statprov' and 'subnational2' are redundant, ",
                "keeping only '", locs[length(locs)], "'")
        locs <- locs[-length(locs)]
        f$region[as.character(locs)] <- list(NULL)

      } else {
        stop("'region' can only be one of 'country', 'statprov', ",
             "'subnational2', 'bcr', 'iba', 'utm_square', or 'bbox'")
      }
    }
  }

  # Only use 'fields' if 'fields_set' == "custom" | MUST have 'fields' if custom
  if(!is.null(f[['fields']]) &&
     (is.null(f[['fields_set']]) | f[['fields_set']] != "custom")) {
    message("Ignoring 'fields' argument because 'fields_set' is not 'custom'")
    f['fields'] <- list(NULL)
  } else if(!is.null(f[['fields_set']]) && f[['fields_set']] == "custom" &&
            is.null(f[['fields']])) {
    stop("Must specify 'fields' if using a custom 'field_set'", call. = FALSE)
  }
  f
}
