#' Query NatureCounts server for data
#'
#' @param path character. Path to the table
#' @param query list. Queries to pass
#' @param api_url character. Base URL for API
#' @param verbose logical. Whether or not to return verbose Curl messages
#'
#' @return A data frame
#'
#' @keywords internal

srv_query <- function(path, query = NULL, filter = NULL,
                      token = NULL,
                      api_url = NULL,
                      verbose = FALSE) {

  # Set Curl configuration
  httr::set_config(httr::content_type_json())
  httr::set_config(httr::accept_json())
  httr::set_config(httr::timeout(300))
  if(verbose) httr::set_config(httr::verbose())

  # Build API path
  if(is.null(api_url)) api_url <- api$api
  url <- file.path(api_url, path)

  # Add token to query
  if(!is.null(token)) query <- append(query, list(token = pass_token(token)))

  # Add filter to query
  if(!is.null(filter)) {
    filter <- filter_json(filter)
    query <- append(query, list(filter = filter))
  }

  # Send request (retry up to three times)
  resp <- httr::RETRY("GET", url, query = query, ua)

  # Check for http errors
  if(httr::status_code(resp) == 403) {
    stop("Invalid token, no access", call. = FALSE)
  } else {
    httr::stop_for_status(resp, "access NatureCounts server")
  }

  # Parse response
  parsed <- resp %>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON(simplifyVector = TRUE)

  # Check for server errors
  srv_error(parsed, url, query)

  # Reset Curl settings
  httr::reset_config()

  parsed
}

srv_error <- function(parsed, url, query) {
  if(any(stringr::str_detect(names(parsed), "Error"))) {
    q <- dplyr::if_else(
      !is.null(query),
      "\n Query: ", paste0(paste0("'", names(query), ": ", query, "'"),
                          collapse = "; "),
      "")
    stop("NatureCounts API request returned an error ",
         "\n'", parsed$Error_msg, "'",
         "\n API: ", url,
         q,
         call. = FALSE)
  }
}


pass_token <- function(token) {
  if(is.null(token)) return(token) else return(I(token))
}

#' Convert filter parameters to JSON
#'
#' Converts filter parameters to JSON, first unboxing the parameters which need
#' to be unboxed. The list of parameters needing to be unbox is stored in the
#' internal data `queries` which is created by "./data-raw/data_creation.R".
#'
#' @param f List. Filter parameters
#'
#' @return A JSON object
#'
#' @keywords internal

filter_json <- function(f) {
  # Which queries need to be unboxed?
  ubox <- queries$api_name[queries$unbox]

  f[names(f) %in% ubox] <- lapply(f[names(f) %in% ubox], jsonlite::unbox)
  jsonlite::toJSON(f, null = "null")
}

#' Create filter list
#'
#' Creates a filter list from package variables and matches them to api query
#' names stored in the internal `queries` data. This is created in
#' "./data-raw/data_creation.R". Also checks parameters for incorrect types and
#' redundancy
#'
#' @param ... The parameters (package-named) to create the filter list with
#'
#' @return A list of api-named filter parameters
#'
#' @keywords internal

filter_create <- function(...) {
  f <- list(...)

  # Check parameters redundancy
  f <- redundancy_check(f)

  # Unpack arguments
  f <- filter_unpack(f)

  # Check parameters validity
  f <- filter_check(f)

  # Replace names with API names
  names(f) <- queries$api_name[match(names(f), queries$package_name)]
  f
}

filter_unpack <- function(f) {
  f$years <- filter_dup(f$years)
  f$doy <- filter_dup(f$doy)

  if(!is.null(f$years)) names(f$years) <- c("start_year", "end_year")
  if(!is.null(f$doy)) names(f$doy) <- c("start_doy", "end_doy")
  if(!is.null(f$region)) f$region <- unlist(f$region)

  for(i in c("years", "doy", "region")){
    f <- append(f, unlist(f[[i]]))
    f <- f[names(f) != i]
  }

  # Remove missing parameters now that all are named
  f[which(is.na(f))] <- NULL
  f
}

filter_dup <- function(i) if(length(i) == 1) i <- rep(i, 2) else i