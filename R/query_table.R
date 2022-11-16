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
#'                     species_id = 15770, username = "sample")
#'                     
#' # Want more than one species? Either filter after, or combine two queries
#' 
#' # Filter after
#' library(dplyr)
#' d <- nc_query_table(table = "bmde_filter_bad_dates", username = "sample")
#' d <- filter(d, species_id %in% c(15770, 9750))
#' 
#' # Combine two queries
#' d1 <- nc_query_table(table = "bmde_filter_bad_dates",
#'                      species_id = 15770, username = "sample")
#' d2 <- nc_query_table(table = "bmde_filter_bad_dates",
#'                      species_id = 9750, username = "sample")
#' d <- rbind(d1, d2)
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

  # Check for multiple arguments to a filter
  f <- vapply(filter, length, FUN.VALUE = 1)
  if(any(f > 1)) {
    problems <- paste0(names(f)[f > 1], collapse = ", ")
    stop("Multiple options applied to a single filter (", problems, ").\n",
         "If you need more than one, download the entire table and filter ",
         "later with something\nlike `dplyr::filter()`.", call. = FALSE)
  }

  # Username check and Authorization
  token <- srv_auth(username)

  # No table
  if(is.null(table)) table <- "api_tables"

  # Make query
  request <- srv_query(api$query,
                       query = list("table" = table),
                       filter = filter,
                       token = token,
                       timeout = timeout,
                       verbose = verbose) %>%
    as.data.frame()

  if(table == "api_tables") {
    request <- request %>%
      dplyr::filter(.data$table_name != "api_tables") %>%
      dplyr::select("table_name", "filters", "required")
  }

  request
}