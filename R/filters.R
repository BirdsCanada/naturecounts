#' Create filter list
#'
#' Creates a filter list from package variables and matches them to api query
#' names stored in the internal `queries` data. This is created in
#' "./data-raw/data_creation.R". Also checks parameters for incorrect types and
#' redundancy
#'
#' @param verbose Logical. Display progress messages?
#' @param ... The parameters (package names) to create the filter list with
#'
#' @return A list of api-named filter parameters
#'
#' @keywords internal

filter_create <- function(verbose, ...) {
  f <- list(...)

  # Check parameters redundancy
  f <- filter_redundancy(f)

  if(length(f) == 0) return(f)

  # Unpack arguments
  f <- filter_unpack(f)

  # Check parameters validity
  f <- filter_check(f)

  # Check names
  if(any(!names(f) %in% queries$package_name)) {
    stop("Unknown parameters", call. = FALSE)
  }

  # Report filters
  if(verbose) {
    f_msg <- filter_to_str(f)
    message("Using filters: ", f_msg)
  }

  # Replace names with API names
  names(f) <- queries$api_name[match(names(f), queries$package_name)]
  f
}

filter_to_str <- function(f) {
  names(f) <- stringr::str_remove(names(f), "filter_")
  f_msg <- lapply(f, function(x) paste0(x, collapse = ", "))
  f_msg <- paste0("(", f_msg, ")")
  paste0(paste0(names(f), " ", f_msg), collapse = "; ")
}


filter_unpack <- function(f) {
  f$years <- filter_dup(f$years)
  f$doy <- filter_dup(f$doy)

  if(!is.null(f$years)) names(f$years) <- c("start_year", "end_year")
  if(!is.null(f$doy)) names(f$doy) <- c("start_doy", "end_doy")
  for(i in c("years", "doy")){
    f <- append(f, unlist(f[[i]]))
    f <- f[names(f) != i]
  }

  if(!is.null(f$region)) {
    if(names(f$region) == "bbox") {
      bbox <- unlist(f$region[1])
      names(bbox) <- c("bbox_left", "bbox_bottom", "bbox_right", "bbox_top")
      f <- append(f, bbox)
    } else f[[names(f$region)]] <- f$region[[1]]
    f <- f[names(f) != "region"]
  }

  # Remove missing parameters now that all are named
  f[which(is.na(f))] <- NULL
  f
}

filter_dup <- function(i) if(length(i) == 1) i <- rep(i, 2) else i

filter_check <- function(f) {

  for(i in 1:length(f)) {
    n <- names(f[i])
    if(n == "fields_set") f[[i]] <- fields_set_check(f[[i]])
    if(n == "fields") f[[i]] <- fields_check(f[[i]])
    if(n == "utm_squares") f[[i]] <- utm_check(f[[i]])
    if(n == "iba") f[[i]] <- iba_check(f[[i]])
    if(n == "bcr") f[[i]] <- bcr_check(f[[i]])
    if(n == "site_type") f[[i]] <- site_type_check(f[[i]])
    if(stringr::str_detect(n, "collection")) f[[i]] <- collections_check(f[[i]])
    if(stringr::str_detect(n, "_year")) f[[i]] <- year_check(f[[i]])
    if(stringr::str_detect(n, "_doy")) f[[i]] <- doy_check(f[[i]])
    if(stringr::str_detect(n, "bbox")) f[[i]] <- bbox_check(f[[i]], type = n)
    if(stringr::str_detect(n, "country|statprov|species")) {
      f[[i]] <- codes_check(f[[i]], type = n)
    }
  }
  f
}

filter_redundancy <- function(f) {

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
