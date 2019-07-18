#' Add date and day-of-year field/columns to data
#'
#' Creates and adds columns `date` and `doy` (day-of-year) to the data source
#' (either data frame or database table `naturecounts`).
#'
#' @param df_db Either data frame or a connection to database with
#'   `naturecounts` table. Must have fields/columns of `survey_year`,
#'   `survey_month`, `survey_day`
#' @param overwrite Logical. Overwrite existing columns `date` and/or `doy`?
#'
#' @return If `df_db`was a data frame, return a data frame with new columns
#'   `date` and `doy`. Otherwise return database connection.
#'
#' @examples
#' bcch_with_dates <- format_dates(bcch)
#'
#' @export
format_dates <- function(df_db, overwrite = FALSE) {
  r <- df_db

  if(!any(c("SQLiteConnection", "data.frame") %in% class(df_db))) {
    stop("'df_db' must either be a data frame or ",
         "a connection to a SQLite database",
         call. = FALSE)
  }


  if("data.frame" %in% class(df_db)) r <- format_dates_df(df_db, overwrite)
  if("SQLiteConnection" %in% class(df_db)) format_dates_db(df_db, overwrite)

  r
}

format_dates_df <- function(df, overwrite) {
  if(!overwrite) {
    if("date" %in% names(df)) stop("'date' column already exists, and ",
                                   "'overwrite' is FALSE", call. = FALSE)
    if("doy" %in% names(df)) stop("'doy' column already exists, and ",
                                  "'overwrite' is FALSE", call. = FALSE)
  }

  if(!all(c("survey_year", "survey_month", "survey_day") %in% names(df))) {
    stop("Missing column 'survey_year', 'survey_month', and/or 'survey_day'",
         call. = FALSE)
  }
  dplyr::mutate(df,
                date = lubridate::ymd(paste(survey_year,
                                            survey_month,
                                            survey_day)),
                doy = lubridate::yday(date))
}

format_dates_db <- function(db, overwrite) {

  col_db <- DBI::dbListFields(db, "naturecounts")

  # Add columns if don't already exist
  if(!"date" %in% col_db) {
    DBI::dbExecute(db, "ALTER TABLE naturecounts ADD COLUMN date TEXT;")
  } else if(!overwrite) {
    stop("'date' field already exists, and ",
         "'overwrite' is FALSE", call. = FALSE)
  }

  if(!"doy" %in% col_db) {
    DBI::dbExecute(db, "ALTER TABLE naturecounts ADD COLUMN doy NUMERIC;")
  } else if(!overwrite) {
    stop("'doy' field already exists, and ",
         "'overwrite' is FALSE", call. = FALSE)
  }

  # Format to date
  DBI::dbExecute(db, paste0("UPDATE naturecounts SET ",
                            "date = survey_year || '-' || ",
                            "PRINTF('%02d', survey_month) || '-' ",
                            "|| PRINTF('%02d', survey_day);"))

  # Add Day of Year
  DBI::dbExecute(db, paste0("UPDATE naturecounts SET ",
                            "doy = strftime('%j', date);"))

  db
}



#' Zero-fill data
#'
#' Zero-fill the species presence data by adding zero observation counts
#' (absences) data to an existing naturecounts dataset.
#'
#' @param df_db Either data frame or a connection to database with
#'   `naturecounts` table (a data frame is returned).
#' @param by Character vector. By default, "SamplingEventIdentifier" or a vector
#'   of specific column names to fill by (see details)
#' @param species Character vector. Either "all", for species in the data, or a
#'   vector of species ID codes to fill in.
#' @param fill Character. The column name to fill in. Defaults to
#'   "ObservationCount".
#' @param extra_species Character vector. Extra columns/fields uniquely
#'   associated with `species_id` to keep in the data (all columns not in `by`,
#'   `species`, `fill`, or `extra_species` will be omitted from the result).
#' @param warn Logical. If TRUE, stop zero-filling if >100 species and >1000
#'   unique sampling events. If FALSE, ignore and proceed.
#' @inheritParams args
#'
#' @details `by` refers to the combination of columns which are used to detect
#'   missing values. By default `SamplingEventIdentifier` is used. Otherwise
#'   users can specify their own combination of columns.
#'
#'   If `species` is supplied, all records will be used to determine observation
#'   events, but only records (zero-filled or otherwise) which correspond to a
#'   species in `species` will be returned (all others will be omitted). Note
#'   that records where `species_id` is NA (generally for 0 counts for
#'   presence/absence), will be converted to a list of 0's for the individual
#'   species.
#'
#' @return Data frame
#'
#' @examples
#' # Download data (with "core" fields to include 'CommonName')
#' rc <- nc_data_dl(collection = "RCBIOTABASE", fields_set = "core",
#'                username = "sample")
#'
#' # Remove casual observations (i.e. 'AllSpeciesReported' = "No")
#' library(dplyr) # For filter function
#' rc <- filter(rc, AllSpeciesReported == "Yes")
#'
#' # Zero fill by all species present
#' rc_all_zeros <- format_zero_fill(rc)
#'
#' # Zero fill only for Canada Goose
#' rc_goose <- format_zero_fill(rc, species = "230")
#'
#' # Keep species-specific variables
#' rc_goose <- format_zero_fill(rc, species = "230", extra_species = "CommonName")
#'
#' # Keep sampling-event-specific variables (keep 'SamplingEventIdentifier')
#' rc_goose <- format_zero_fill(rc, by = c("SamplingEventIdentifier",
#'                                         "latitude", "longitude"))
#'
#' @export
format_zero_fill <- function(df_db, by = "SamplingEventIdentifier",
                             species = "all", fill = "ObservationCount",
                             extra_species = NULL,
                             warn = TRUE, verbose = TRUE) {

  # SQLite Connections must become dataframes
  if(any(class(df_db) == "SQLiteConnection")) {

    if(verbose) message(" - Cannot work directly on SQLite database connections, ",
                        "collecting data into a data frame...")

    if(!"naturecounts" %in% DBI::dbListTables(df_db)) {
      stop("If 'df_db' is a SQLite database, it must have a 'naturecounts' ",
           "table", call. = FALSE)
    }

    df <- dplyr::tbl(df_db, "naturecounts") %>%
      dplyr::collect()

  } else {
    df <- df_db
  }

  # Species ids present?
  if(!"species_id" %in% names(df)) {
    stop("Column 'species_id' must be present", call. = FALSE)
  }

  # fill columns present?
  if(length(fill) > 1) stop("'fill' can only be one column", .call = FALSE)
  if(!fill %in% names(df)) {
    stop("'fill' column ('", fill, "') is missing from the data", call. = FALSE)
  }
  # All species reported?
  if(!"AllSpeciesReported" %in% names(df) ||
     any(is.na(df$AllSpeciesReported)) ||
     any(df$AllSpeciesReported != "Yes")) {
    stop("Column 'AllSpeciesReported' must be present and 'Yes'", call. = FALSE)
  }

  # Select grouping columns
  if(any(!by %in% names(df))) {
    stop("'by' columns must be present in the ",
         "data (missing: ",
         paste0(by[!by %in% names(df)], collapse = ", "), ")", call. = FALSE)
  }

  if("species_id" %in% by) {
    stop("The column 'species_id' cannot be in 'by'", call. = FALSE)
  }

  # Keep extra columns completely associated with 'by'
  if(!is.null(extra_species)) {

    if(any(!extra_species %in% names(df))) {
      stop("Some 'extra_species' are not in the data (",
           paste0(extra_species[!extra_species %in% names(df)], collapse = ", "),
           ")", call. = FALSE)
    }

    extra_keep <- find_unique(df, "species_id", extra_species)

    if(!all(extra_species %in% extra_keep)) {
      if(verbose) {
        message(" - Ignoring 'extra_species' columns (",
                paste0(extra_species[!extra_species %in% extra_keep], collapse = ", "),
                ") not uniquely ",
                "associated with the 'species_id' column")
      }
    }
    extra_species <- dplyr::select(df, "species_id", extra_keep) %>%
      dplyr::distinct()
  }

  # Select species ids
  if(species == "all") {
    species <- unique(df$species_id)
  } else {
    species <- codes_check(species)
  }

  # Check how many species/events there are
  if(warn && length(species) > 1000 && nrow(unique(df[by])) > 5000) {
    stop("You are trying to zero-fill over 1000 species with over 5000 ",
         "sampling events. This could take a while! ",
         "To ignore this warning and proceed, set 'warn = FALSE'",
         call. = FALSE)
  }

  # Check if more than one observation per unique column set
  if(verbose &&
     any(dplyr::count(df, !!!rlang::syms(c(by, "species_id")))$n > 1)) {
    message(" - Consider summarizing multiple observations per set of 'by' ",
            "before zero-filling to increase speed")
  }

  # Check for missing by values
  if(verbose && any(is.na(df[by]))) {
    message(" - There are missing values in 'by'. ",
            "These are classified as a single event")
  }

  # Convert fill column to numeric
  if(!is.numeric(df[[fill]])) {
    orig <- class(df[[fill]])
    df[[fill]] <- as_numeric(df[[fill]])
    if(!is.numeric(df[[fill]])) {
      stop("'fill' column cannot be converted to numeric", call. = FALSE)
    }
    if(verbose) message(" - Converted 'fill' column (", fill, ") from ",
                        orig, " to numeric")
  }
  fill <- as.list(rlang::set_names(0, fill))

  df_filled <- df %>%
    dplyr::select(by, "species_id", names(fill)) %>%
    dplyr::group_by(!!!rlang::syms(by)) %>%
    tidyr::complete(species_id = species, fill = fill)

  if(!is.null(extra_species)) {
    df_filled <- dplyr::left_join(df_filled, extra_species, by = "species_id")
  }

  as.data.frame(df_filled)
}


# Grab extra columns also unique to 'by'
find_unique <- function(df, by, extra){
  extra[sapply(extra, FUN = function(x) {
    nrow(unique(cbind(df[by], df[x]))) == nrow(unique(df[by]))
  })]
}
