#' Custom table queries
#'
#' Generate custom table queries with the table name and filter arguments.
#'
#' @param table Character. Table to query (see details)
#' @param ... Name/value pairs for custom queries/filters (see details)
#'
#' @inheritParams args
#'
#' @details
#'
#'   `nc_query_table(username = "sample")` for available options
#'
#' @return data.frame()
#'
#' @examples
#'
#' # What tables are available? What 'filters' do they take? Are any 'required'?
#'
#' nc_query_table(username = "sample")
#'
#' # Query the bmdefilter_bad_dates table
#'
#' d <- nc_query_table(table = "bmde_filter_bad_dates", username = "sample")
#' head(d)
#'
#' # Filter our query
#' d <- nc_query_table(table = "bmde_filter_bad_dates",
#'                     SiteCode = "DMBO", username = "sample")
#' d
#'
#' # Filter our query
#' d <- nc_query_table(table = "bmde_filter_bad_dates",
#'                     species_id = c(7680, 9750), username = "sample")
#' d
#'
#' @export

nc_query_table <- function(table = NULL, ..., username = NULL, timeout = 120,
                           verbose = FALSE) {

  # Grab filters
  if(length(list(...)) > 0) {
    filter <- list(...) %>%
      purrr::map(~{if(length(.) == 1) jsonlite::unbox(.) else .})
  } else {
    filter <- NULL
  }


  # Username check and Authorization
  token <- srv_auth(username)

  # No table
  if(is.null(table)) table <- "api_tables"

  # Make query
  request <- srv_query(file.path(api$query, table),
                       filter = filter,
                       token = token,
                       timeout = timeout) %>%
    as.data.frame()

  if(table == "api_tables") {
    request <- request %>%
      dplyr::filter(table_name != "api_tables") %>%
      dplyr::select(table_name, filters, required)
  }

  request
}