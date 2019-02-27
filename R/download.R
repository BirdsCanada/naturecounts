#' Download NatureCounts data records
#'
#' Download data records from various collections filtered by date and location
#' (if provided). An authorization token is required.
#'
#' @param collections Character vector. The collection codes from which to
#'   download data. "all" downloads data from all available collections
#' @param species Character vector. Filter data to specific species
#' @param start_date Character. The starting date of data to download. See
#'   details for format.
#' @param end_date Character. The end date of data to download. See details for
#'   format.
#' @param location NOT IMPLEMENTED
#' @param country Character vector. Filter data to specific countries codes
#' @param statprov Character vector. Filter data to specific states/province
#'   codes
#' @param token Character vector. Authorization token, otherwise only public
#'   data is accessible.
#' @param sql_db Character vector. Name and location of SQLite database to
#'   either create or add to.
#' @param format Logical. Format downloaded data?
#' @param verbose Logical. Display progress messages?
#'
#' @details The format of start/end dates is fairly flexible and can be anything
#'   recognized by \code{\link[lubridate]{lubridate-package}}'s
#'   \code{\link[lubridate]{ymd}()} function. However, it must have the
#'   order of year, month, day. Month and day are option: It can be year and
#'   month (e.g., \code{"2000 May"}), or simply year (e.g., \code{2000}).
#'
#' @return Data frame
#'
#' @export
#'
#' @examples
#'
#' \donttest{
#' # All observations part of the RCBIOTABASE collection
#' rcbio <- nc_data_dl(collection = "RCBIOTABASE")
#' }
#'
#' # Observations of black-capped chickadees from RCBIOTABASE collection in 2010
#' bcch <- nc_data_dl(collection = "RCBIOTABASE", species = 7590,
#'                    start_date = 2010, end_date = 2010)
#'

nc_data_dl <- function(collections, species = NULL,
                       start_date = NULL, end_date = NULL,
                       location = NULL, country = NULL, statprov = NULL,
                       token = NULL, sql_db = NULL, format = TRUE,
                       verbose = TRUE) {

  if(missing(collections)) stop("You must specify collections from which to ",
                                "download the data.", call. = FALSE)

  # Format dates
  start_date <- parse_date(start_date)
  end_date <- parse_date(end_date)

  startyear <- parse_year(start_date)
  endyear <- parse_year(end_date)

  startday <- NULL
  endday <- NULL

  if(verbose) message("Collecting available records...")
  records <- nc_count(collections = collections, country = country,
                      statprov = statprov, startyear = startyear,
                      endyear = endyear, species = species,
                      token = token)

  if(is.null(sql_db) && sum(records$nrecords) > 1000000) {
    message("\nThis is a very large download. Consider using ",
            "a SQLite data base (see the sql_db argument), to prevent ",
            "memory overload or losing your data due to a loss of ",
            "internet connection during the download.")
  }

  if(verbose) {
    message(paste0(capture.output(records), collapse = "\n"))
  }

  # Get/Create database or dataframe
  if(!is.null(sql_db)) {
    con <- db_connect(sql_db)
  } else {
    all <- data.frame()
  }

  n <- 5000   # max number of records to parse

  f <- list(collection = records$collection[1],
            startyear = startyear, endyear = endyear,
           # startday = startday, endday = endday,
            country = country, statprov = statprov,
            species = species,
            beginRecord = 0, numRecords = n)

  if(verbose) message("\nDownloading records for each collection:")
  for(c in 1:nrow(records)) {
    if(verbose) message("  ", records$collection[c])
    d <- data.frame()
    nmax <- 0
    f$collection <- records$collection[c]
    total <- records$nrecords[c]

    repeat {
      if(verbose){
        from <- nrow(d) + 1
        to <- as.integer(total - nrow(d))
        to <- dplyr::if_else(to > n, as.integer(nrow(d) + n), to)
        message("    Records ", from, " to ", to, " / ", total)
      }

      f$beginRecord <- nmax

      d1 <- srv_query("data", table = "get_data",
                      token = token,
                      query = NULL,
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
#' @param startyear Numeric. Filter to this start year
#' @param endyear Numeric. Filter to this end year
#' @param species Numeric vector. Filter to these species codes.
#' @param show Character. Either "all" or "available". "all" returns counts from
#'   all data sources. "available" only returns counts for data available
#'   (public or accessible with the token provided).
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
                     species = NULL, startyear = NULL, endyear = NULL,
                     show = "available", token = NULL) {

  if(!show %in% c("available", "all")) {
    stop("show must either be 'all' or 'available'", call. = FALSE)
  }

  cnts <- srv_query("data", "list_collections",
                    token = token,
                    filter = list(collections = collections,
                                  species = species,
                                  startyear = startyear,
                                  endyear = endyear,
                                  country = country,
                                  statprov = statprov)) %>%
    parse_results() %>%
    dplyr::arrange(collection)

  if(nrow(cnts) == 0) stop("No records for these filters")

  if(show == "available") {
    cnts <- srv_query("data", "list_permissions",
                      token = token) %>%
      parse_results() %>%
      dplyr::semi_join(cnts, ., by = "collection")
  }
  cnts
}


