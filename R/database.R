#' Connect to or create a SQLite database
#'
#' Connect to an existing database, or, if the database doesn't exist, create a
#' new one and fill with the appropriate table (internally stored empty df
#' called nc_dbs).
#'
#' @param name Character. The file path and name (no extension) of the data base
#'   to create. By default the data base is created in the current directory and
#'   named "naturecounts_DATE.nc".
#'
#' @return A RSQLite connection
#'
#' @keywords internal
#'
#' @examples
#'
#' \donttest{
#'   db_connect()
#' }

db_connect <- function(name = paste0("./naturecounts_", Sys.Date())) {

  con <- DBI::dbConnect(RSQLite::SQLite(),
                        dbname = paste0(name, ".nc"))

  t <- DBI::dbListTables(con)

  if("naturecounts" %in% t) {
    # Check version
    db_check_version(con)
  } else {
    # Create tables
    dplyr::copy_to(con, nc_dbs[["2018-02-22"]],
                   name = "naturecounts", temporary = FALSE)
    dplyr::copy_to(con, data.frame(version = "2018-02-22"),
                   name = "version", temporary = FALSE)
  }

  con
}

db_check_version <- function(con) {

  if("version" %in% DBI::dbListTables(con)) {
    v <- dplyr::tbl(con, "version") %>%
      dplyr::pull(version) %>%
      lubridate::as_date(version) %>%
      max()

    if(v < max_version) stop("You are using an out-of-date version of ",
                             "the naturecounts database.", call. = FALSE)
  } else {
    stop("There is no version information for this database.", call. = FALSE)
  }
}

#' Add/replace records in a db table from a data frame
#'
#' @param con DBI database connection
#' @param table Character. Name of table to add/replace records into
#' @param df Data frame. Data from which to write data
#'
#' @keywords internal
#'
#' @references Adapted from \code{motus::dbInsertOrReplace()} originally written by
#' John Brzustowski for the \code{motus} package.

db_insert <- function(con, table, df) {

  if (nrow(df) == 0) return()

  # Create temporary table
  temp_name <- basename(tempfile())
  temp <- dplyr::copy_to(con, df, temp_name)

  # Replace records
  rs <- DBI::dbSendStatement(con,
                             paste0("REPLACE into ", table,
                                    " select * from ", temp_name))
  DBI::dbClearResult(rs)

  # Remove table
  DBI::dbRemoveTable(con, temp_name)
}