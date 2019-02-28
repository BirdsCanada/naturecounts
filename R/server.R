#' Query NatureCounts server for data
#'
#' @param data_type character. Base data type, "metadata" or "data"
#' @param table character. Table to access.
#' @param query list. Queries to pass.
#' @param api character. Base URL for API
#' @param verbose logical. Whether or not to return verbose Curl messages
#'
#' @return A data frame
#'
#' @keywords internal

srv_query <- function(data_type, table, query = NULL, filter = NULL,
                      token = NULL,
                      api = "https://sandbox.birdscanada.org/api",
                      verbose = FALSE) {

  # Set Curl configuration
  httr::set_config(httr::content_type_json())
  httr::set_config(httr::accept_json())
  httr::set_config(httr::timeout(300))
  if(verbose) httr::set_config(httr::verbose())

  # Build API path
  url <- file.path(api, data_type, table)

  # Add token to query
  if(!is.null(token)) query <- append(query, list(token = pass_token(token)))

  # Add filter to query
  if(!is.null(filter)) {
    filter <- filter_json(filter)
    query <- append(query, list(filter = filter))
  }

  # Send request
  resp <- httr::GET(url, query = query, ua)

  # Check for http errors
  if(httr::status_code(resp) == 403) {
    stop("Invalid token, no access", call. = FALSE)
  } else {
    httr::stop_for_status(resp)
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


filter_json <- function(f) {
  ubox <- c("minlat", "maxlat", "minlong", "maxlong", "startyear",
            "endyear", "startday", "endday", "collection", "request_id",
            "utmsquare")

  f[names(f) %in% ubox] <- lapply(f[names(f) %in% ubox], jsonlite::unbox)
  jsonlite::toJSON(f, null = "null")
}

pass_token <- function(token) {
  if(is.null(token)) return(token) else return(I(token))
}