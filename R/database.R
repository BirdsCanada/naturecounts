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

db_connect <- function(name = paste0("./naturecounts_", Sys.Date()),
                       verbose = TRUE) {

  con <- DBI::dbConnect(RSQLite::SQLite(),
                        dbname = paste0(name, ".nc"))

  t <- DBI::dbListTables(con)

  if("naturecounts" %in% t) {
    if(verbose) message("\nDatabase '", name,
                        ".nc' already exists, connecting to it...")
    # Check version
    db_check_version(con)
  } else {
    # Create tables
    if(verbose) message("\nDatabase '", name,
                        ".nc' does not exist, creating it...")
    db_create(con)
  }

  con
}

db_create_primary <- function(con, df, primary_key) {
  table <- stringr::str_remove_all(deparse(substitute(df)), "\\(|\\)") %>%
    stringr::str_remove("meta_")

  qry <- paste0("CREATE TABLE ", table," ([",
                paste0(names(df), collapse = "], ["), "]")
  if(all(!is.na(primary_key))) {
    qry <- paste0(qry, ", PRIMARY KEY([",
                  paste0(primary_key, collapse = "], ["), "]));")
  } else qry <- paste0(qry, ");")

  DBI::dbExecute(con, qry)
  db_insert(con, table, df)
}

# This function is used internally to create a database for use by the db_create function
db_create_empty <- function(con) {
  # Download and copy empty naturecounts table
  naturecounts <- nc_data_dl(collections = "SAMPLE1", species = 14280,
                             username = "sample",
                             info = "nc: create database",
                             verbose = FALSE)[0, ]
  db_create_primary(con, naturecounts, primary_key = keys$data)
}

db_create <- function(con) {

  db_create_empty(con)

  # Copy metadata tables (13/17)
  db_create_primary(con, meta_country_codes(), primary_key = keys$country_codes)
  db_create_primary(con, meta_statprov_codes(), primary_key = keys$statprov_codes)
  db_create_primary(con, meta_subnational2_codes(), primary_key = keys$subnational2_codes)
  db_create_primary(con, meta_iba_codes(), primary_key = keys$iba_codes)
  db_create_primary(con, meta_bcr_codes(), primary_key = keys$bcr_codes)
  db_create_primary(con, meta_species_authority(), primary_key = keys$species_authority)
  db_create_primary(con, meta_species_codes(), primary_key = keys$species_codes)
  db_create_primary(con, meta_species_taxonomy(), primary_key = keys$species_taxonomy)
  db_create_primary(con, meta_collections(), primary_key = keys$collections)
  db_create_primary(con, meta_projects(), primary_key = keys$projects)
  db_create_primary(con, meta_breeding_codes(), primary_key = keys$breeding_codes)
  db_create_primary(con, meta_project_protocols(), primary_key = keys$project_protocols)
  db_create_primary(con, meta_protocol_types(), primary_key = keys$protocol_types)

  # No utm_squares, bmde_version, bmde_fields
  # projects_meta included in projects

  # Create versions table with current versions
  v <- data.frame(Rpackage = as.character(utils::packageVersion("naturecounts")),
                  metadata = metadata_v_local())
  dplyr::copy_to(con, v, name = "versions", temporary = FALSE)

}

db_check_version <- function(con) {

  if("versions" %in% DBI::dbListTables(con)) {
    v <- dplyr::tbl(con, "versions") %>%
      dplyr::collect()

    if(numeric_version(v$Rpackage) < utils::packageVersion("naturecounts")) {
      warning("Your NatureCounts database is out of date. ",
              "It is highly recommended that you re-download your data.\n",
              "(database created with package v", v$Rpackage, 
              ", but current package version is v",
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
  col_missing <- sql_class(df[col_missing])

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