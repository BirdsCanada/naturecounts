#' Download NatureCounts data records
#'
#' Download data records from various collections filtered by various options.
#' In order to ease the load on the server, note that only **three** of
#' `collections`/`project_ids`, `species`, `years`, `doy`, `region`, and
#' `site_type` can be used in any one request. See the vignette for filtering
#' your data after download for more options:
#' `vignette("filtering_data", package = "naturecounts")`.
#'
#' @param fields_set Character. Set of fields/columns to download. See details.
#' @param fields Character vector. If `fields_set = custom`, which
#'   fields/columns to download. See details
#' @param sql_db Character vector. Name and location of SQLite database to
#'   either create or add to
#' @param warn Logical. Interactive warning if request more than 1,000,000
#'   records to download.
#'
#' @inheritParams args
#' @inheritSection args NatureCounts account
#' @inheritSection args Species ids (`species`)
#' @inheritSection args Day of Year (`doy`)
#' @inheritSection args Regions (`region`)
#' @inheritSection args Data Fields/Columns (`fields_set` and `fields`)
#' @inheritSection args `request_id`'s
#'
#' @return Data frame
#'
#' @examples
#' # All observations part of the RCBIOTABASE collection
#' rcbio <- nc_data_dl(collections = "RCBIOTABASE", username = "sample")
#'
#' \donttest{# All observations part of project_id 1042 accessible by "sample"
#' p1042 <- nc_data_dl(project_ids = 1042, username = "sample")}
#'
#' # Black-capped chickadees from RCBIOTABASE collection in 2010
#' species_search("black-capped chickadee") # Find the species_id
#' bcch <- nc_data_dl(collection = "RCBIOTABASE", species = 14280,
#'                    years = 2010, username = "sample")
#'
#' # All bcch observations since 2015 accessible to user "sample"
#' bcch <- nc_data_dl(species = 14280, years = c(2015, NA), username = "sample")
#'
#' bcch <- nc_data_dl(species = 14280, doy = c(200, 300), username = "sample")
#'
#' bcch <- nc_data_dl(species = 14280, username = "sample",
#'                    region = list(bbox = c(left = -145, bottom = 45,
#'                                           right = -100, top = 60)))
#'
#' # All moose observations with public access
#' species_search("moose")
#' moose <- nc_data_dl(species = 133990, username = "sample")
#'
#' # Different fields/columns
#' moose <- nc_data_dl(species = 133990, fields_set = "core",
#'                     username = "sample")
#'
#' moose <- nc_data_dl(species = 133990, fields_set = "custom",
#'                     fields = c("Locality", "AllSpeciesReported"),
#'                     username = "sample")
#'
#' \dontrun{
#' # All collections by request id
#' my_data <- nc_data_dl(request_id = 000000, username = "USER")
#'
#' # Specific collection by request id
#' my_data <- nc_data_dl(collections = "ABATLAS1",
#'                       request_id = 000000, username = "USER")
#' }
#'
#' @export

nc_data_dl <- function(collections = NULL, project_ids = NULL,
                       species = NULL, years = NULL,
                       doy = NULL, region = NULL, site_type = NULL,
                       fields_set = "minimum", fields = NULL,
                       username, request_id = NULL, sql_db = NULL,
                       warn = TRUE, verbose = TRUE) {

  # Username check and Authorization
  token <- srv_auth(username)

  # Check/convert project_ids to collections (collections checked in filter)
  collections <- projects_check(project_ids, collections)

  # If request_id provided, check, and ignore other filter values
  if(!is.null(request_id)) {

    if(any(!is.null(c(species, years, doy, region, site_type)))) {
      message("Donwloading previously logged request_id ",
              "(ignoring filters 'species', 'years', 'doy', ",
              "'region', and 'site_type')")
      species <- years <- doy <- region <- site_type <- NULL
    }

    requests <- nc_requests_internal(request_id, token)

    if(!is.null(collections)) {
      if(any(!collections %in% requests$collection)) {
        stop("Some 'collections' were not included in the original request and ",
             "cannot be downloaded with this 'request_id'", call. = FALSE)
      }
      requests <- dplyr::filter(requests, collection %in% collections)
    } else {
      collections <- requests$collection
    }
  }

  # Assemble and check filter parameters
  filter <- filter_create(verbose = verbose,
                          collections = collections, species = species,
                          years = years, doy = doy, region = region,
                          site_type = site_type,
                          fields_set = fields_set, fields = fields)

  # Get available records
  if(verbose) message("Collecting available records...")

  if(!is.null(request_id)) {
    records <- dplyr::select(requests, collection, nrecords)
  } else {
    records <- nc_count_internal(filter = filter, token = token)
    request_id <- records$requestId
    records <- records$results
  }

  # If there are no records to download, see why not and report that to the user
  if(nrow(records) == 0) {

    # Is it because they don't have permission?
    if(!is.null(collections)) {
      no_access <- collections[!collections %in%
                                 nc_permissions(token = token)$collection]
    } else no_access <- c()

    if(length(no_access) == 0) {
      stop("These collections have no data that match these filters",
           call. = FALSE)
    } else {
      stop("You do not have permission to access these collections (",
           paste0(no_access, collapse = ", "), ")", call. = FALSE)
    }
  } else if(!is.null(collections) && nrow(records) != length(collections)){
    # What about if not all the collections they want are available?
    missing <- collections[!collections %in% records$collection]
    message("Not all collections have data that match these filters (",
            paste0(missing, collapse = ", "), ")")
  }

  if(verbose) message(capture_df(records),
                      "\nTotal records: ",
                      format(sum(records$nrecords), scientific = FALSE,
                             big.mark = ","))

  if(warn == TRUE && sum(records$nrecords) > 1000000) {
    msg <- "This is a large download (> 1,000,000 records). "
    if(is.null(sql_db)) msg <- paste0(msg,
                                      "Consider using a SQLite data ",
                                      "base with 'sql_db'. ")
    msg <- paste0(msg,
                  "\nAre you sure you wish to proceed? ",
                  "(To always proceed use 'warn = FALSE')")

    choice <- utils::menu(choices = c("Yes", "No"), title = msg)
    if(choice == 2) return(message(""))
  }

  # Get/Create database or dataframe
  if(!is.null(sql_db)) {
    df_db <- db_connect(sql_db, verbose = verbose)
  } else {
    df_db <- data.frame()
  }

  # Query Information
  query <- list(lastRecord = 0, numRecords = 5000, requestId = request_id)

  if(verbose) message("\nDownloading records for each collection:")
  for(c in 1:nrow(records)) {

    # Get data for whole collection
    df_db <- nc_coll_dl(coll = records[c, ], query, filter, token, df_db, verbose)
  }

  # Clear the web request id
  if(username != "sample") srv_query(api$release_request_id, query = query['requestId'], token = token)

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

  # Save the data
  df_db <- nc_data_save(r$results, df_db)

  # Loop while we still have data to download
  coll$progress <- nrow(r$results)

  repeat {
    # Are we done? (return less than asked)
    if(nrow(r$results) < query$numRecords) break

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

  request <- srv_query(api$data,
                       query = query,
                       filter = filter,
                       token = token,
                       timeout = 60)

  # Parse the data
  request$results <- parse_results(request, results = TRUE)

  # Make sure all have equal row counts
  rows <- unique(vapply(request$results, FUN = length, FUN.VALUE = c(11)))

  if(length(rows) > 1) stop("Requested data has unequal row counts",
                            call. = FALSE)

  request
}


#' Save/Return the data to data frame or database
#'
#' Either save data to database on disk, or bind them into an existing data
#' frame.
#'
#' @param data Data frame. Data to be saved
#' @param df_db Data frame/SQLite database connection. Where data should be
#'   saved
#' @param table Character. If df_db is a database connection, the database
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
#' @param show Character. Either "all" or "available". "all" returns counts from
#'   all data sources. "available" only returns counts for data available for
#'   the username provided. If no username is provided, defaults to "all".
#'
#' @inheritParams args
#' @inheritSection args NatureCounts account
#' @inheritSection args Species ids (`species`)
#' @inheritSection args Day of Year (`doy`)
#' @inheritSection args Regions (`region`)
#'
#' @return Data frame
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
#' nc_count(region = list(statprov = "MB"))
#' }
#'
#' # Count all records for all collections user "sample" has access to
#' \dontrun{
#' nc_count(username = "sample")
#' }
#'
#' # Count records with barred owls in Ontario
#' species_search("barred owl")
#' nc_count(species = 7590, region = list(statprov = "ON"), username = "sample")
#'
#' # Count all records available in the Christmas Bird Count and Breeding Bird
#' # Survey collections (regardless of user permissions)
#' \donttest{
#' nc_count(collections = c("CBC", "BBS"), show = "all", username = "sample")
#' }
#'
#' @export

nc_count <- function(collections = NULL, project_ids = NULL, species = NULL,
                     years = NULL, doy = NULL, region = NULL, site_type = NULL,
                     show = "available", username = NULL, verbose = TRUE) {

  if(!show %in% c("available", "all")) {
    stop("show must either be 'all' or 'available'", call. = FALSE)
  }

  if(is.null(username) && show == "available") {
    show <- "all"
    message("Without a username, using 'show = \"all\"'")
  }

  # Username check and Authorization
  token <- srv_auth(username)

  # Check/convert project_ids to collections
  collections <- projects_check(project_ids, collections)

  # Assemble and check filter parameters
  filter <- filter_create(verbose = verbose,
                          collections = collections, species = species,
                          years = years, doy = doy, region = region,
                          site_type = site_type)

  # Get counts
  cnts <- nc_count_internal(filter, token, show)

  cnts[['results']]
}

nc_count_internal <- function(filter, token, show = "available") {
  cnts <- srv_query(api$collections_count, token = token, filter = filter,
                    timeout = 60)

  requestId <- cnts$requestId

  cnts <- cnts %>%
    parse_results(results = TRUE) %>%
    dplyr::arrange(.data$collection)

  if(show == "available" && nrow(cnts) > 0) {
    cnts <- dplyr::filter(cnts, .data$access == "yes")
  }
  list(results = cnts, requestId = requestId)
}


nc_permissions <- function(token = NULL) {
  srv_query(api$permissions, token = token, timeout = 30) %>%
    parse_results(results = TRUE)
}