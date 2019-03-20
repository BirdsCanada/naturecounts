#' Specify common parameters for `nc_data_dl` and `nc_count`
#'
#' @param collections Character vector. The collection codes from which to
#'   download data. NULL (default) downloads data from all available collections
#' @param species Numeric vector. Numeric species ids (see details)
#' @param years Numeric vector. The start/end years of data to download. Can use
#'   NA for either start or end, or a single value to return data from a single
#'   year.
#' @param doy Character/Numeric vector. The start/end day-of-year to download
#'   (1-366 or dates that can be converted to day of year). Can use NA for
#'   either start or end
#' @param region List. Named list with *one* of the following options:
#'   `country`, `statprov`, `subnational2`, `bcr`, `iba`, `utm_squares`, `bbox`.
#'   See details
#' @param site_type Character vector. The type of site to return (e.g., `IBA`).
#' @param token Character vector. Authorization token, otherwise only public
#'   data is accessible
#'
#' @section Species ids (`species`):
#'   Numeric species id codes can determined from the functions
#'   \code{\link{species_search}()} or \code{\link{species_code_search}()}. See
#'   also the vignette for more information
#'   `vignette("species-codes", package = "naturecounts")`.
#'
#' @section Day of Year (`doy`):
#'   The format for day of year (`doy`) is fairly flexible and can be anything
#'   recognized by \code{\link[lubridate]{lubridate-package}}'s
#'   \code{\link[lubridate]{ymd}()} function. However, it must have the order of
#'   year, month, day. Note that year is ignored when converting to day of year,
#'   except that it will result in a 1 day offset for leap years.
#'
#' @section Regions (`region`):
#'   Regions are defined by codes reflecting the country, state/province,
#'   subnational (level 2), Important Bird Areas (IBA), and Bird Conservation
#'   Regions (BCR) (see [region_search()] for codes). They can also be defined
#'   by providing specific UTM squares to download or a bounding box area which
#'   specifyings the min/max longitude and min/max latitude (`bbox`). See the
#'   article on regional filtering for more detail:
#'   <http://BirdStudiesCanada.github.io/naturecounts/articles/region.html>
#'
#' @name args
NULL

#' Download NatureCounts data records
#'
#' Download data records from various collections filtered by various options.
#' In order to ease the load on the server, note that only **three** of
#' `collections`, `species`, `years`, `doy`, `region`, and `site_type` can be
#' used in any one request. See the vignette for filtering your data after
#' download for more options:
#' `vignette("filtering_data", package = "naturecounts")`.
#'
#' @param fields_set Charcter. Set of fields/columns to download. See details.
#' @param fields Character vector. If `fields_set = custom`, which
#'   fields/columns to download. See details
#' @param sql_db Character vector. Name and location of SQLite database to
#'   either create or add to
#' @param verbose Logical. Display progress messages?
#'
#' @inheritParams args
#' @inheritSection args Species ids (`species`)
#' @inheritSection args Day of Year (`doy`)
#' @inheritSection args Regions (`region`)
#'
#' @section Data Fields/Columns:
#'   By default data is downloaded with the `minimum` set of fields/columns.
#'   However, for more advanced applications, users may wish to specify which
#'   fields/columns to return. The Bird Monitoring Data Exchange (BMDE) schema
#'   keeps track of variables used to augment observation data. There are
#'   different versions reflecting different collections of variables which can
#'   be specified for download in one of four ways:
#'
#'   1. `fields_set` can be a specific shorthand reflecting a BMDE version:
#'   `core`, `extended` or `minimum` (default). See [meta_bmde_versions()] to see
#'   which BMDE version the shorthand refers to.
#'   2. `fields_set` can be `default` which uses the default BMDE version for a
#'   particular collection (note that if you download more than one collection,
#'   the field sets will expand to cover all fields/columns in the combined
#'   collections)
#'   3. `fields_set` can be the exact BMDE version. See [meta_bmde_versions()]
#'   for options.
#'   4. `fields_set` can be `custom` and the `fields` argument can be a
#'   character vector specifying the exact fields/columns to return. See
#'   [meta_bmde_fields()]) for potential `fields` values.
#'
#'   Note that in all cases there are a set of fields/columns that are *always*
#'   returned, no matter what `fields_set` is used.
#'
#' @return Data frame
#'
#' @examples
#' \donttest{# All observations part of the RCBIOTABASE collection
#' rcbio <- nc_data_dl(collection = "RCBIOTABASE")}
#'
#' # Observations of black-capped chickadees from RCBIOTABASE collection in 2010
#' species_search("black-capped chickadee") # Find the species_id
#' bcch <- nc_data_dl(collection = "RCBIOTABASE", species = 14280,
#'                    years = 2010)
#'
#' # All public bcch observations since 2015
#' bcch <- nc_data_dl(species = 14280, years = c(2015, NA))
#'
#' bcch <- nc_data_dl(species = 14280, doy = c(200, 300))
#'
#' bcch <- nc_data_dl(species = 14280,
#'                    region = list(bbox = c(left = -145, bottom = 45,
#'                                           right = -100, top = 60)))
#'
#' # All moose observations with public access
#' species_search("moose")
#' moose <- nc_data_dl(species = 133990)
#'
#' # Different fields/columns
#' moose <- nc_data_dl(species = 133990, fields_set = "core")
#'
#' moose <- nc_data_dl(species = 133990, fields_set = "custom",
#'                     fields = c("Locality", "AllSpeciesReported"))
#'
#' @export

nc_data_dl <- function(collections = NULL, species = NULL, years = NULL,
                       doy = NULL, region = NULL, site_type = NULL,
                       fields_set = "minimum", fields = NULL,
                       token = NULL, sql_db = NULL,
                       verbose = TRUE) {

  # Assemble and check filter parameters
  filter <- filter_create(collections = collections, species = species,
                          years = years, doy = doy, region = region,
                          fields_set = fields_set, fields = fields)

  # Get available records
  if(verbose) message("Collecting available records...")
  records <- nc_count_internal(filter = filter, token = token)

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

    # Are we done? (return less than asked)
    if(nrow(r$results) <= query$numRecords) break

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
                       token = token)

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
#'   all data sources. "available" only returns counts for data available
#'   (public or accessible with the token provided).
#'
#' @inheritParams args
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
#' # Count all records for all collections you have access to
#' \dontrun{
#' nc_count(token = YOUR_TOKEN)
#' }
#'
#' # Count all public records with barred owls in Ontario
#' species_search("barred owl")
#' nc_count(species = 7590, region = list(statprov = "ON"))
#'
#' # Count records available in the Christmas Bird Count and Breeding Bird
#' # Survey collections (regardless of permissions)
#' \donttest{
#' nc_count(collections = c("CBC", "BBS"), show = "all")
#' }
#'
#' @export

nc_count <- function(collections = NULL, species = NULL, years = NULL,
                     doy = NULL, region = NULL, site_type = NULL,
                     show = "available", token = NULL) {

  if(!show %in% c("available", "all")) {
    stop("show must either be 'all' or 'available'", call. = FALSE)
  }

  # Filter
  filter <- filter_create(collections = collections, species = species,
                          years = years, doy = doy, region = region)

  # Get counts
  cnts <- nc_count_internal(filter, token, show)

  cnts
}

nc_count_internal <- function(filter, token, show = "available") {

  cnts <- srv_query(api$collections_count, token = token, filter = filter) %>%
    parse_results(results = TRUE) %>%
    dplyr::arrange(.data$collection)

  if(show == "available" && nrow(cnts) > 0) {
    cnts <- srv_query(api$permissions, token = token) %>%
      parse_results(results = TRUE) %>%
      dplyr::semi_join(cnts, ., by = "collection")
  }
  cnts
}


nc_permissions <- function(token = NULL) {
  srv_query(api$permissions, token = token) %>%
    parse_results(results = TRUE)
}