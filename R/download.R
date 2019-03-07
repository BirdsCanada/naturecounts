#' Download NatureCounts data records
#'
#' Download data records from various collections filtered by date and location
#' (if provided). An authorization token is required.
#'
#' @param collections Character vector. The collection codes from which to
#'   download data. NULL (default) downloads data from all available collections
#' @param species Numeric vector. Numeric species ids (see details)
#' @param start_date Character. The starting date of data to download. See
#'   details for format
#' @param end_date Character. The end date of data to download. See details for
#'   format
#' @param location NOT IMPLEMENTED
#' @param country Character vector. Filter data to specific countries codes
#' @param statprov Character vector. Filter data to specific states/province
#'   codes
#' @param token Character vector. Authorization token, otherwise only public
#'   data is accessible.
#' @param sql_db Character vector. Name and location of SQLite database to
#'   either create or add to.
#' @param verbose Logical. Display progress messages?
#'
#' @details Numeric species id codes can determined from the functions
#'   \code{\link{species_search}()} or \code{\link{species_code_search}()}, or
#'   by browsing the metadata \code{\link{species_codes}} or
#'   \code{\link{species_taxonomy}}.
#'
#'   The format of start/end dates is fairly flexible and can be anything
#'   recognized by \code{\link[lubridate]{lubridate-package}}'s
#'   \code{\link[lubridate]{ymd}()} function. However, it must have the
#'   order of year, month, day. Month and day are option: It can be year and
#'   month (e.g., \code{"2000 May"}), or simply year (e.g., \code{2000}).
#'
#' @return Data frame
#'
#' @examples
#'
#' \donttest{
#' # All observations part of the RCBIOTABASE collection
#' rcbio <- nc_data_dl(collection = "RCBIOTABASE")
#' }
#'
#' # Observations of black-capped chickadees from RCBIOTABASE collection in 2010
#' species_search("black-capped chickadee") # Find the species_id
#' bcch <- nc_data_dl(collection = "RCBIOTABASE", species = 14280,
#'                    start_date = 2010, end_date = 2010)
#'
#' # All moose observations with public access
#' species_search("moose")
#' moose <- nc_data_dl(species = 133990)
#'
#' @export

nc_data_dl <- function(collections = NULL, species = NULL,
                       start_date = NULL, end_date = NULL,
                       location = NULL, country = NULL, statprov = NULL,
                       token = NULL, sql_db = NULL,
                       verbose = TRUE) {

  check_collections(collections)

  # Format dates
  start_date <- parse_date(start_date)
  end_date <- parse_date(end_date)

  startyear <- parse_year(start_date)
  endyear <- parse_year(end_date)

  startday <- NULL
  endday <- NULL

  # Check/Convert character to codes
  species <- codes_check(species)
  country <- codes_check(country)
  statprov <- codes_check(statprov)

  if(verbose) message("Collecting available records...")
  records <- nc_count(collections = collections, country = country,
                      statprov = statprov, startyear = startyear,
                      endyear = endyear, species = species,
                      token = token)

  # If there are no records to download, see why not and report that to the user
  if(nrow(records) == 0) {

    # Is it because they don't have permission?
    no_access <- collections[!collections %in%
                               nc_permissions(token = token)$collection]

    if(length(no_access) == 0) {
      stop("These collections have no data that match these filters", call. = FALSE)
    } else {
      stop("You do not have permission to access these collections (",
           paste0(no_access, collapse = ", "), ")", call. = FALSE)
    }
  } else if(nrow(records) != length(collections)){
    # What about if not all the collections they want are available?
    missing <- collections[!collections %in% records$collection]
    message("Not all collections have data that match these filters (",
            paste0(missing, collapse = ", "), ")")
  }

  if(is.null(sql_db) && sum(records$nrecords) > 1000000) {
    message("\nThis is a very large download. Consider using ",
            "a SQLite database (see the sql_db argument), to prevent ",
            "memory overload or losing your data due to a loss of ",
            "internet connection during the download.")
  }

  if(verbose) message(capture_df(records))

  # Get/Create database or dataframe
  if(!is.null(sql_db)) {
    df_db <- db_connect(sql_db)
  } else {
    df_db <- data.frame()
  }

  # Query Information
  query <- list(lastRecord = 0, numRecords = 5000, requestId = NULL)

  # Filter information
  filter <- list(collection = records$collection[1],
                 startyear = startyear, endyear = endyear,
                 # startday = startday, endday = endday,
                 country = country, statprov = statprov,
                 species = species)

  if(verbose) message("\nDownloading records for each collection:")
  for(c in 1:nrow(records)) {

    # Get data for whole collection
    d <- nc_coll_dl(coll = records[c, ], query, filter, token, df_db, verbose)

    # Add collection to rest of data (if df, not db)
    if(is.data.frame(df_db)) df_db <- dplyr::bind_rows(df_db, d)

  }

  df_db
}

#' Download all records for a single collection
#'
#' This internal function queries and downloads data for a single collection
#'
#'
#' @param coll List. Data frame returned by nc_count() for collection in
#'   question
#' @param query List. Queries for server
#' @param filter List. Filter queries for server
#' @param token Character. Authorization token
#' @param df_db Data frame/SQLite database connection. Data source
#' @param verbose Logical. Display progress messages?
#'
#' @return An updated df_db (data.frame), or the database connection (update on
#'   harddrive)
#'
#' @keywords internal

nc_coll_dl <- function(coll, query, filter, token, df_db, verbose) {

  if(verbose) message("  ", coll$collection)
  if(verbose) progress_query(0, coll$nrecords, query$numRecords)

  # Update filter
  filter$collection <- coll$collection

  # Request
  r <- nc_single_dl(query, filter, token)
  query$requestId <- r$requestId

  # Save the data
  df_db <- nc_data_save(r$results, df_db)

  # Loop while we still have data to download
  coll$progress <- nrow(r$results)

  repeat {

    # Are we done?
    if(is.null(r$requestId)) break

    # Update our position
    query$lastRecord <- max(r$results$record_id)

    # Track download progress
    if(verbose) progress_query(coll$progress, coll$nrecords, query$numRecords)

    # Request
    r <- nc_single_dl(query, filter, token)

    # Save the data
    df_db <- nc_data_save(r$results, df_db)

    # Track progress
    coll$progress <- coll$progress + nrow(r$results)
  }

  df_db
}

#' Download single set of records for a single collection
#'
#' @param query List. Queries for server
#' @param filter List. Filter queries for server
#' @param token Character. Authorization token
#'
#' @return the request (a list with 'result' and 'requestId')
#'
#' @keywords internal

nc_single_dl <- function(query, filter, token){

  request <- srv_query("data", table = "get_data",
                       query = query,
                       filter = filter,
                       token = token)

  # Parse the data
  request$results <- parse_results(request)

  request
}


#' Save/Return the data to data frame or databse
#'
#' Either save data to database on disk, or bind them into an existing data
#' frame.
#'
#' @param data Data frame. Data to be saved
#' @param df_db Data frame/SQLite database connection. Where data should be
#'   saved
#' @param table Character. If df_db is a database connnection, the database
#'   table to save to
#'
#' @return Either a data frame or a SQLite database connection
#' @keywords internal

nc_data_save <- function(data, df_db, table = "naturecounts") {
  if(!is.data.frame(df_db)) {
    db_insert(df_db, "naturecounts", data)
  } else {
    df_db <- dplyr::bind_rows(df_db, data)
  }
  df_db
}



#' Download information about NatureCounts collections
#'
#' Download the number of records available for different collections filtered
#' by location (if provided). If authorization is provided, the collections are
#' filtered to only those available to the user. Otherwise all collections are
#' returned.
#'
#' @param collections Character vector. Filter to specific collections
#' @param species Numeric vector. Numeric species ids (see details)
#' @param country Character vector. Filter to specific country codes
#' @param statprov Character vector. Filter to specific state/province codes
#' @param startyear Numeric. Filter to this start year
#' @param endyear Numeric. Filter to this end year
#' @param show Character. Either "all" or "available". "all" returns counts from
#'   all data sources. "available" only returns counts for data available
#'   (public or accessible with the token provided).
#' @param token Character. Authorization token
#'
#' @return Data frame
#'
#' @details Numeric species id codes can determined from the functions
#'   \code{\link{species_search}()} or \code{\link{species_code_search}()}, or
#'   by browsing the metadata \code{\link{species_codes}} or
#'   \code{\link{species_taxonomy}}.
#'
#' @examples
#'
#' # Count all publicly available records:
#' \donttest{
#' nc_count()
#' }
#'
#' # Count publicly available records for Manitoba, Canada
#' \donttest{
#' nc_count(statprov = "MB")
#' }
#'
#' # Count all records for all collections you have access to
#' \dontrun{
#' nc_count(token = YOUR_TOKEN)
#' }
#'
#' # Count all public records with barred owls in Ontario
#' species_search("barred owl")
#' nc_count(species = 7590, statprov = "ON")
#'
#' # Count records available in the Christmas Bird Count and Breeding Bird
#' # Survey collections (regardless of permissions)
#' \donttest{
#' nc_count(c("CBC", "BBS"), show = "all")
#' }
#'
#' @export

nc_count <- function(collections = NULL, species = NULL, country = NULL,
                     statprov = NULL, startyear = NULL, endyear = NULL,
                     show = "available", token = NULL) {

  check_collections(collections)
  if(!show %in% c("available", "all")) {
    stop("show must either be 'all' or 'available'", call. = FALSE)
  }

  # Convert character to codes
  species <- codes_check(species)
  country <- codes_check(country)
  statprov <- codes_check(statprov)

  # Get counts
  cnts <- srv_query("data", "list_collections",
                    token = token,
                    filter = list(collections = collections,
                                  species = species,
                                  startyear = startyear,
                                  endyear = endyear,
                                  country = country,
                                  statprov = statprov)) %>%
    parse_results() %>%
    dplyr::arrange(.data$collection)

  if(show == "available" && nrow(cnts) > 0) {
    cnts <- srv_query("data", "list_permissions",
                      token = token) %>%
      parse_results() %>%
      dplyr::semi_join(cnts, ., by = "collection")
  }

  cnts
}

nc_permissions <- function(token = NULL) {
  srv_query("data", "list_permissions", token = token) %>%
    parse_results()
}


