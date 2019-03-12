#' Connect to or create a SQLite database
#'
#' Connect to an existing database, or, if the database doesn't exist, create a
#' new one and fill with the appropriate table (internally stored empty df
#' called nc_dbs).
#'
#' @param name Character. The file path and name (no extension) of the database
#'   to create. By default the database is created in the current directory and
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
    db_create(con)
  }

  con
}

db_create <- function(con) {
  # Download and copy empty naturecounts table
  d <- nc_data_dl(collections = "RCBIOTABASE", species = 14280,
                  start_date = 2010, end_date = 2019, verbose = FALSE)[0, ]
  dplyr::copy_to(con, d, name = "naturecounts", temporary = FALSE,
                 unique_indexes = list("record_id"))

  # Copy metadata
  dplyr::copy_to(con, country_codes(), temporary = FALSE)
  dplyr::copy_to(con, statprov_codes(), temporary = FALSE)
  dplyr::copy_to(con, subnat_codes(), temporary = FALSE)
  dplyr::copy_to(con, species_authority(), temporary = FALSE)
  dplyr::copy_to(con, species_codes(), temporary = FALSE)
  dplyr::copy_to(con, species_taxonomy(), temporary = FALSE)

  # Create versions table with current versions
  v <- data.frame(Rpackage = as.character(utils::packageVersion("naturecounts")),
                  metadata = metadata_v_local())
  dplyr::copy_to(con, v, name = "versions", temporary = FALSE)

  # Create empty request table with filters and dates
  dplyr::copy_to(con,
                 data.frame(date = NA, species = NA, start_date = NA,
                            end_date = NA, country = NA, statprov = NA)[0,],
                 name = "download_request", temporary = FALSE)
}

db_check_version <- function(con) {

  if("versions" %in% DBI::dbListTables(con)) {
    v <- dplyr::tbl(con, "versions") %>%
      dplyr::collect()

    if(numeric_version(v$Rpackage) < utils::packageVersion("naturecounts")) {
      stop("Your NatureCounts database is out of date. ",
           "You will need to re-download your data.\n",
           "(created with package v", v$Rpackage, ", current is v",
           utils::packageVersion("naturecounts"), ")",
           call. = FALSE)
    }
  } else {
    stop("There is no version information for this database. ",
         "Are you sure this is a NatureCounts database?", call. = FALSE)
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

  # Compare columns
  col_df <- names(df)
  col_db <- DBI::dbListFields(con, table)

  # Fill new cols in df with NA
  col_new <- col_db[!col_db %in% col_df]
  df[, col_new] <- NA

  # Add cols missing from db
  col_missing <- col_df[!col_df %in% col_db]
  col_missing <- sql_class(df[, col_missing])

  for(n in names(col_missing)) {
    DBI::dbExecute(con, paste("ALTER TABLE", table, "ADD COLUMN",
                              n, col_missing[n]))
  }

  # Arrange column order to match db
  df <- df[, DBI::dbListFields(con, table)]

  # Create temporary table in the data frame
  temp_name <- basename(tempfile())
  temp <- dplyr::copy_to(con, df, temp_name)

  # Replace records
  rs <- DBI::dbExecute(con,
                       paste0("REPLACE into ", table,
                              " select * from ", temp_name))

  # Remove table
  DBI::dbRemoveTable(con, temp_name)
}


sql_class <- function(df) {
  x <- vapply(df, class, FUN.VALUE = "text")
  dplyr::if_else(x %in% c("double", "integer", "numeric"), "NUMERIC", "TEXT") %>%
    rlang::set_names(names(x))
}