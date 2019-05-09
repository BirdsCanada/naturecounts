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
#' @param df_db Either data frame or a connection to database with
#'   `naturecounts` table
#' @param by Character vector. Either "event" or a vector of specific column
#'   names to fill by (see details)
#' @param species Character vector. Either "all", for species in the data, or a
#'   vector of species ID codes to fill in.
#' @param fill Character. The column name to fill in. Defaults to
#'   "ObservationCount".
#' @param extra_cols Character vector. Extra columns/fields uniquely associated
#'   with `by` to keep in the data (all columns not in `by`, `species`, `fill`,
#'   or `extra_cols` will be omitted from the result.
#' @inheritParams args
#'
#' @details `by` refers to the combination of columns which are used to detect
#'   missing values. When `by = "event"`, `project_id`, `collection`, `date`,
#'   and `SamplingEventIdentifier` are used. Otherwise
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
#' # Download data
#' rc <- nc_data_dl(collection = "RCBIOTABASE", username = "sample")
#'
#' # Remove casual observations (i.e. 'AllSpeciesReported' = "No")
#' library(dplyr) # For filter function
#' rc <- filter(rc, AllSpeciesReported == "Yes")
#'
#' # Add dates
#' rc_dates <- format_dates(rc)
#'
#' # Zero fill by all species present
#' rc_all_zeros <- format_zero_fill(rc_dates)
#'
#' # Zero fill only for Canada Goose
#' rc_goose <- format_zero_fill(rc_dates, species = "230")
#'
#' @export
format_zero_fill <- function(df_db, by = "event", species = "all",
                             fill = "ObservationCount", extra_cols = NULL,
                             verbose = TRUE) {

  # SQLite Connections must become dataframes
  if(class(df_db) == "SQLiteConnection") {

    if(verbose) message("Cannot work directly on SQLite database connections, ",
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

  # All species reported?
  if(!"AllSpeciesReported" %in% names(df)) {
    stop("Column 'AllSpeciesReported' must be present", call. = FALSE)
  } else if(any(is.na(df$AllSpeciesReported) || df$AllSpeciesReported != "Yes")) {
    stop("Cannot zero-fill if not all species were reported ",
         "(column 'AllSpeciesReported' needs to be 'Yes')", call. = FALSE)
  }

  # Select grouping columns
  if(length(by) == 1) {

    by_cols <- c("project_id", "collection", "date")

    if(by == "event") {
      by <- c(by_cols, "SamplingEventIdentifier")
    }

    if(any(!by %in% names(df))) {
      if(!"date" %in% names(df)) {
        date_msg <- " See 'format_date()' to add the 'date' column."
      } else date_msg <- ""
      stop("To zero-fill by event, columns ", paste0(by, collapse = ", "),
           " must be present in the data.", date_msg, call. = FALSE)
    }

  } else {
    if(any(!by %in% names(df))) {
      stop("To zero-fill by specific columns, they must be present in the ",
           "data (some are missing: ",
           paste0(by[!by %in% names(df)], collapse = ", "), ")", call. = FALSE)
    }
    if("species_id" %in% by) {
      stop("The column 'species_id' cannot be in 'by'", call. = FALSE)
    }
  }

  # Keep extra columns completely associated with 'by'
  if(!is.null(extra_cols)) {

    if("species_id" %in% extra_cols) {
      stop("'species_id' cannot be in 'extra_cols'", call. = FALSE)
    }

    if(any(!extra_cols %in% names(df))) {
      stop("Some 'extra_cols' are not in the data (",
           paste0(extra_cols[!extra_cols %in% names(df)], collapse = ", "),
           ")", call. = FALSE)
    }

    extra_keep <- find_unique(df, by, extra_cols)

    if(!all(extra_cols %in% extra_keep)) {
      if(verbose) {
        message("Ignoring 'extra_cols' columns not uniquely ",
                "associated with the fill columns (",
                paste0(extra_cols[!extra_cols %in% extra_keep], collapse = ", "),
                ")")
      }
    }

    by <- c(by, extra_keep)
    by <- by[by != "species_id"]
  }

  # Check if any by columns all NA
  na <- vector()
  for(i in by) na <- c(na, length(na.omit(df[[i]])))
  if(any(na == 0)) stop("At least one column in 'by' has no non-NA values (",
                        paste0(by[na == 0], collapse = ", "), ")", call. = FALSE)

  # Check if more than one observation per unique column set
  if(any(dplyr::count(df, !!!rlang::syms(c(by, "species_id")))$n > 1)) {
   warning("There is more than one observation per species per set of ",
           "'by' columns.\nConsider filtering your data or adding more ",
           "identifier columns to 'by'",
           call. = FALSE)
  }

  # Select species ids
  if(species == "all") {
    species <- unique(df$species_id)
  } else {
    species <- codes_check(species)
  }

  # Convert fill column to numeric
  if(!is.numeric(df[[fill]])) {
    orig <- class(df[[fill]])
    df[[fill]] <- as_numeric(df[[fill]])
    if(!is.numeric(df[[fill]])) {
      stop("'fill' column cannot be converted to numeric", call. = FALSE)
    }
    if(verbose) message("Converted 'fill' column (", fill, ") from ",
                        orig, " to numeric")
  }
  fill <- as.list(rlang::set_names(0, fill))

  df %>%
    dplyr::select(by, "species_id", names(fill)) %>%
    dplyr::mutate(species_id = factor(species_id, levels = species)) %>%
    dplyr::group_by(!!!rlang::syms(by)) %>%
    tidyr::complete(species_id, fill = fill) %>%
    dplyr::mutate(species_id = as.integer(as.character(species_id))) %>%
    dplyr::filter(!is.na(species_id)) %>%
    as.data.frame()
}


# Grab extra columns also unique to 'by'
find_unique <- function(df, by, extra){
  extra <- names(df)[names(df) %in% extra]
  extra[sapply(extra, FUN = function(x) {
    nrow(unique(cbind(df[, by], df[, x]))) == nrow(unique(df[, by]))
  })]
}
