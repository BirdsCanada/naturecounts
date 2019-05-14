#' Check on status of data requests
#'
#' List pending or completed data requests for a given user.
#'
#' @inheritParams args
#' @inheritSection args `request_id`'s
#'
#' @return data frame
#'
#' @examples
#' nc_requests(username = "sample")
#' nc_requests(username = "sample", request_id = 152446)
#'
#' @export
nc_requests <- function(request_id = NULL, username) {

  # Username check and Authorization
  token <- srv_auth(username)

  # Get list
  nc_requests_internal(request_id, token)
}

nc_requests_internal <- function(request_id, token) {
  req <- srv_query(api$list_requests, query = c(requestId = request_id),
                   token = token)
  if(length(req$requests) > 0 ){
    req <- parse_request(req$requests)
  } else req <- NULL
  req
}
