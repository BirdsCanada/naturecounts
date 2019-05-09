#' Check on status of data requests
#'
#' List pending or completed data requests for a given user.
#'
#' @details There are two types of data requests: ones made through the api
#'   (i.e. `naturecounts` R package) and those made through the online [Web
#'   Request Form](https://www.birdscanada.org/birdmon/default/searchquery.jsp).
#'
#' Requests made through this package via the [nc_data_dl()] function only
#' return data the user has access to. If data was successfully downloaded, the
#' user can see the request details with this function.
#'
#' Requests made through the online [Web Request
#' Form](https://www.birdscanada.org/birdmon/default/searchquery.jsp) may
#' include collections to which the user does not have access. This function
#' will list the requests along with the approval status (either 'approved' or
#' 'pending')
#'
#' @param request_id Numeric. Specific request id to return status of.
#' @inheritParams args
#'
#' @return data frame
#'
#' @examples
#'
#' nc_requests(username = "sample")
#'
#' @export
nc_requests <- function(request_id = NULL, username) {

  # Username check and Authorization
  token <- srv_auth(username)
  #token <- "pk9u44v1n52dr88kvqfr0r99q2"
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
