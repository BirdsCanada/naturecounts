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
                       token) {

  if(missing(collections)) stop("You must specify collections from which to ",
                                "download the data.", call. = FALSE)

  if(missing(token)) stop("An authorization token is required to download data.",
                          call. = FALSE)

  message("Collecting available records...")
  records <- nc_count(collections = collections, country = country,
                      statprov = statprov, startyear = startyear,
                      endyear = endyear, token = token)


  n <- 5000   # max number of records to parse
  all <- data.frame()

  message("Downloading records for each collection:")
  for(c in 1:nrow(records)) {
    message("  ", records$collection[c])
    d <- data.frame()
    nmax <- 0
    while(nmax < records$nrecords[c]) {
      message("    Records ", nmax + 1, " to ", n + nmax)
      f <- list(collection = records$collection[c],
                startyear = startyear, endyear = endyear,
                country = country, statprov = statprov,
                beginRecord = nmax + 1, numRecords = n)

      d <- srv_query("data", table = "get_data",
                     query = list(token = pass_token(token)),
                     filter = f) %>%
        parse_results() %>%
        dplyr::bind_rows(d)

      nmax <- nrow(d)

      if(nmax %% n != 0) break # if not a multiplier of download size, then end of records available
      if(nmax == 0) {
        d <- data.frame()
        message("    No data for ", records$collection[c], " with these filters")
        break
      }
    }

    all <- dplyr::bind_rows(all, d)
  }
  all
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



