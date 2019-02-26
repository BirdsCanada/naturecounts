#' Download NatureCounts data records
#'
#' Download data records from various collections filtered by date and location
#' (if provided). An authorization token is required.
#'
#' @param collections Character vector. The collection codes from which to
#'   download data. "all" downloads data from all available collections
#' @param species Character vector. Filter data to specific species (NOT
#'   IMPLEMENTED)
#' @param startyear Numeric. The starting year of data to download
#' @param endyear Numeric. The ending year of data to download
#' @param location NOT IMPLEMENTED
#' @param country Character vector. Filter data to specific countries codes
#' @param statprov Character vector. Filter data to specific states/province
#'   codes
#' @param token Character vector. Authorization token.
#' @param sql_db Character vector. Name and location of SQLite database to
#'   either create or add to.
#' @param format Logical. Format downloaded data?
#' @param verbose Logical. Display progress messages?
#'
#' @return Data frame
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # Christmas Bird Count data for Manitoba since 2018
#'
#' nc_data_dl(collections = "CBC", statprov = "MB",
#'            startyear = "2018", token = YOUR_TOKEN)
#' }
#'

nc_data_dl <- function(collections, species = NULL,
                       startyear = NULL, endyear = NULL,
                       location = NULL, country = NULL, statprov = NULL,
                       token, sql_db = NULL, format = TRUE, verbose = TRUE) {

  if(missing(collections)) stop("You must specify collections from which to ",
                                "download the data.", call. = FALSE)

  if(missing(token)) stop("An authorization token is required to download data.",
                          call. = FALSE)

  if(verbose) message("Collecting available records...")
  records <- nc_count(collections = collections, country = country,
                      statprov = statprov, startyear = startyear,
                      endyear = endyear, token = token)

  # Get/Create database or dataframe
  if(!is.null(sql_db)) {
    con <- db_connect(sql_db)
  } else {
    all <- data.frame()
  }

  n <- 5000   # max number of records to parse

  if(verbose) message("Downloading records for each collection:")
  for(c in 1:nrow(records)) {
    if(verbose) message("  ", records$collection[c])
    d <- data.frame()
    nmax <- 0
    repeat {
      f <- list(collection = records$collection[c],
                startyear = startyear, endyear = endyear,
                country = country, statprov = statprov,
                beginRecord = nmax, numRecords = n)

      if(verbose) message("    Records ", nmax)
      d1 <- srv_query("data", table = "get_data",
                      query = list(token = pass_token(token)),
                      filter = f) %>%
        parse_results()

      # Save the data
      if(!is.null(sql_db)) {
        db_insert(con, "naturecounts", d1)
      } else d <- dplyr::bind_rows(d, d1)

      # End of collection
      if(nrow(d1) == 0 | nrow(d1) < n) {
        break
      }

      nmax <- max(d1$record_id)
    }

    # No data for this collection
    if(nrow(d) == 0) {
      if(verbose) message("    No data for ", records$collection[c],
                          " with these filters")
    }

    # Add collection to rest of data
    if(is.null(sql_db)) all <- dplyr::bind_rows(all, d)
  }

  if(is.null(sql_db)) {
    if(format) all <- nc_format(all)
    return(all)
  } else {
    return(con)
  }
}

#' Download information about NatureCounts collections
#'
#' Download the number of records available for different collections filtered
#' by location (if provided). If authorization is provided, the collections are
#' filtered to only those available to the user. Otherwise all collections are
#' returned.
#'
#' @param collections Character vector. Filter to specific collections
#' @param country Character vector. Filter to specific country codes
#' @param statprov Character vector. Filter to specific state/province codes
#' @param startyear NOT IMPLEMENTED
#' @param endyear NOT IMPLEMENTED
#' @param token Character. Authorization token
#'
#' @return Data frame
#'
#' @export
#'
#' @examples
#'
#' # Count records available in the Christmas Bird Count and Breeding Bird
#' # Survey collections (regardless of permissions)
#'
#' nc_count(c("CBC", "BBS"))
#'
#' # Count records with data for Manitoba, Canada (regardless of permissions)
#'
#' nc_count(country = "CA", statprov = "MB")
#'
#' # Count records for all collections you have access to
#'
#' \dontrun{
#' nc_count(token = YOUR_TOKEN)
#' }
#'
nc_count <- function(collections = NULL, country = NULL, statprov = NULL,
                     startyear = NULL, endyear = NULL, token = NULL) {

  cnts <- data.frame()
  cnts <- srv_query("data", "list_collections",
                    query = list(token = pass_token(token)),
                    filter = list(collections = collections,
                                  country = country, statprov = statprov)) %>%
    parse_results() %>%
    dplyr::arrange(collection)

  if(!is.null(token)) {
    cnts <- srv_query("data", "list_permissions",
                      query = list(token = pass_token(token))) %>%
      parse_results() %>%
      dplyr::rename(collection = "collection_code") %>%
      dplyr::semi_join(cnts, ., by = "collection")
  }
  cnts
}



