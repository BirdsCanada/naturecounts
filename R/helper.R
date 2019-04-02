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