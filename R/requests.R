#' Check on status of data requests
#'
#' List pending or completed data requests for a given user.
#'
#' @param type Character One of "web", "api", or "all" specifying which types of
#'   request to return (defaults to "web").
#' @inheritParams args
#' @inheritSection args Access and `request_id`s
#'
#' @return data frame
#'
#' @examples
#' nc_requests(username = "sample")
#' nc_requests(request_id = 152446, username = "sample")
#'
#' @export
nc_requests <- function(request_id = NULL, type = "web", username) {

  # Username check and Authorization
  token <- srv_auth(username)

  # Check type
  if(!type %in% c("web", "api", "all")) {
    stop("'type' must be one of 'web', 'api', or 'all'", call. = FALSE)
  }

  # Get list
  r <- nc_requests_internal(request_id, token)
  if(type != "all") r <- dplyr::filter(r, .data$request_origin == type)
  r
}

nc_requests_internal <- function(request_id = NULL, token) {
  req <- srv_query(api$list_requests, query = c(requestId = request_id),
                   token = token)
  if(length(req$requests) > 0 ){
    req <- parse_request(req$requests)
  } else req <- NULL
  req
}
