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
                      token = NULL, api_url = NULL, timeout = 300,
                      verbose = FALSE) {

  # Set Curl configuration
  httr::set_config(httr::content_type_json())
  httr::set_config(httr::accept_json())
  if(verbose) httr::set_config(httr::verbose())

  # Build API path
  if(is.null(api_url)) api_url <- api$api
  url <- file.path(api_url, path)

  # Add token to query
  if(!is.null(token)) query <- append(query, list(token = pass_token(token)))

  # Add filter to query
  if(!is.null(filter)) {
    filter <- to_json(filter)
    query <- append(query, list(filter = filter))
  }

  # Send request (try twice if first fails, unless it was a forced failure)
  resp <- try(httr::POST(url, body = query, encode = "form",
                         ua, httr::timeout(timeout)),
            silent = TRUE)
  if(class(resp) == "try-error") {
    if(stringr::str_detect(resp, "aborted by an application callback")){
      stop(resp, call. = FALSE)
    } else {
      #message("Ooops, error on first try, retrying...\nError: ",
      #        as.character(resp))
      resp <- httr::POST(url, body = query, encode = "form",
                         ua, httr::timeout(timeout))
    }
  }

  # Parse response
  parsed <- resp %>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON(simplifyVector = TRUE)

  # Check for server errors
  srv_error(parsed, url, filter)

  # Check for http errors
  if(httr::status_code(resp) == 403) {
    stop("Invalid authorization, no access", call. = FALSE)
  } else {
    httr::stop_for_status(resp, "access NatureCounts server")
  }

  # Reset Curl settings
  httr::reset_config()

  parsed
}

srv_error <- function(parsed, url, filter) {
  if(any(stringr::str_detect(names(parsed), "rror"))) {
    if(!is.null(filter)) {
      f <- jsonlite::fromJSON(filter, simplifyVector = TRUE)
      f <- paste0("\n Query: ",
                  paste0(paste0("'", names(f), ": ",
                                f, "'"),
                         collapse = "; "))
      } else f <- ""
    e <- paste0(parsed$errorMsgs, collapse = "; ")
    stop("NatureCounts API request returned an error ",
         "\n Message: '", e, "'",
         "\n API: ", url,
         f,
         call. = FALSE)
  }
}

#' Fetch authorization token
#'
#' For a given username, check to see if we already have a token in the storage
#' environment `srv_auth_env`, otherwise prompt safely for password and fetch a
#' token from the NatureCounts server.
#'
#' @param username Character vector. Username for <http://naturecounts.ca>. If
#'   provided, the user will be prompted for a password. If left NULL, only
#'   public collections will be returned.
#'
#' @return Token character string
#'
#' @keywords internal

srv_auth <- function(username) {

  if(is.null(username)) {
    # Username supplied?
    token <- NULL

  } else if(exists(username, envir = srv_auth_env)) {
    # See if username associated with token in storage
    token <- get(username, envir = srv_auth_env)

  } else {
    # Otherwise prompt for password
    p <- askpass::askpass(prompt = paste0("Please enter password for ",
                                          "NatureCounts user '", username, "'"))

    if(is.null(p)) stop("Password required for user ", username, call. = FALSE)

    # Fetch token from server
    token <- srv_query(path = api$auth,
                       query = list(username = username, password = p))$token

    # Save token to storage
    assign(username, token, envir = srv_auth_env)
  }

  token
}

# Environment for password storage
srv_auth_env <- new.env()



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

to_json <- function(f) {
  # Which queries need to be unboxed?
  ubox <- queries$api_name[queries$unbox]

  f[names(f) %in% ubox] <- lapply(f[names(f) %in% ubox], jsonlite::unbox)
  jsonlite::toJSON(f, null = "null")
}