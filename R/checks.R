username_check <- function(u) {
  if(missing(u)) stop("A 'username' is needed to access the server\n  ",
                      "(consider username = 'sample' for testing)",
                      call. = FALSE)
}

year_check <- function(y) {
  y <- as_numeric(y)
  if(!is.numeric(y) || y > lubridate::year(Sys.Date()) || y < 1900) {
    stop("Years must be numbers between 1900 and ",
         lubridate::year(Sys.Date()), call. = FALSE)
  }
  y
}

doy_check <- function(s) {

  stp <- FALSE

  if(stringr::str_detect(s, "^[:digit:]+$")) s <- as_numeric(s)
  if(is.numeric(s)) {
    if(s < 0 | s > 366) stp <- TRUE
    if(round(s) != s) stp <- TRUE
  } else {
    s <- suppressWarnings(lubridate::ymd_hms(s, truncated = 4)) %>%
      lubridate::yday()
    if(is.na(s)) stp <- TRUE
  }
  if(stp) stop("Day of year must be either a date (YM or YMD), ",
               "or a whole number (1-366)", call. = FALSE)
  s
}

utm_check <- function(u) {
  u_all <- meta_utm_squares()
  n <- u %in% u_all$utm_square
  if(sum(!n) > 0) {
    bad <- u[!n]
    mx <- dplyr::if_else(length(bad) <= 3, length(bad), 3L)
    bad <- paste0("'", paste0(bad[1:mx], collapse = "', '"), "'")
    if(mx < sum(!n)) bad <- paste0(bad, ", ...")
    stop("Invalid 'utm_squares' provided (", bad, ")", call. = FALSE)
  }
  u
}

bbox_check <- function(b, type) {
 if(type %in% c("bbox_left", "bbox_right")) {
   if(abs(b) > 180) stop("Bounding box longitude ('left'/'right' bound) ",
                         "must be a number between -180 and 180", call. = FALSE)
 } else if (type %in% c("bbox_top", "bbox_bottom")) {
   if(abs(b) > 90) stop("Bounding box latitude ('top'/'bottom' bound) ",
                        "must be a number between -90 and 90", call. = FALSE)
 } else {
   stop("Invalid name for bbox coordinate", call. = FALSE)
 }
  b
}

iba_check <- function(i) {
  i <- toupper(i)
  w <- i[!i %in% meta_iba_codes()$iba_site]

  if(length(w) > 0) {
    stop("Some arguments in 'iba' are invalid (", paste0(w, collapse = ","),
         "). See 'iba_site' in 'meta_iba_codes()' for valid options",
         call. = FALSE)
  }
  paste0("IBA.", i) # To match API expectations
}

bcr_check <- function(b) {

  b <- as_numeric(b)

  w <- b[!b %in% meta_bcr_codes()$bcr]

  if(length(w) > 0) {
    stop("Some arguments in 'bcr' are invalid (", paste0(w, collapse = ","),
         "). See 'bcr' in 'meta_bcr_codes()' for valid options",
         call. = FALSE)
  }

  b <- sprintf("%02d", b) # Convert to dual digit
  paste0("BCR.", b) # To match API expectations
}

site_type_check <- function(site_type) {
  if(toupper(site_type) != "IBA") {
    stop("'site_type' must be either NULL or 'IBA'", call. = FALSE)
  }
  toupper(site_type)
}

projects_check <- function(project_ids, collections = NULL) {
  if(!is.null(project_ids)) {
    c1 <- meta_collections() %>%
      dplyr::filter(.data$project_id %in% project_ids)

    if(nrow(c1) == 0) {
      stop("'project_ids' must be either NULL or a vector of valid ",
           "'project_id's specified in 'meta_collections()'", call. = FALSE)
    }
    c1 <- unique(c(c1$collection, collections))
  } else c1 <- collections
  c1
}

collections_check <- function(c, token = NULL) {
  c1 <- nc_permissions_internal(token) %>%
    dplyr::pull("collection")

  if(!is.null(token)) {
    c_req <- nc_requests_internal(token = token)
    if(!is.null(c_req)) {
      c1 <- c_req %>%
        dplyr::pull("collection") %>%
        append(c1) %>%
        unique()
    }
  }
  c1 <- c[!c %in% c1]

  if(!is.null(c) && (!is.character(c) || length(c1) > 0)) {
    msg <- paste0("'collections' must be either NULL (return all collections) ",
                  "or a \ncharacter vector of valid 'collection' names to which ",
                  "'username' has access.\nSee 'vignette(\"Data Access\", ",
                  "package = \"naturecounts\")' for details.")
    if(any(c1 %in% meta_collections()$collection)) {
      msg <- paste0(msg, "\nNo access to collection(s): ",
                    paste0(c1[c1 %in% meta_collections()$collection],
                           collapse = ", "))
    }
    if(any(!c1 %in% meta_collections()$collection)) {
      msg <- paste0(msg, "\nNo access or no collection: ",
                    paste0(c1[!c1 %in% meta_collections()$collection],
                           collapse = ", "))
    }

    stop(msg, call. = FALSE)
  }
  c
}

authority_check <- function(a) {
  if(!all(a %in% meta_species_authority()$authority)) {
    stop("'authority' must be one or more of the authorities ",
         "specified in 'meta_species_authority()'.", call. = FALSE)
  }
}

fields_set_check <- function(fields_set) {
  v <- meta_bmde_versions()

  if(length(fields_set) > 1 || !fields_set %in% c(v$version, "custom")) {
    if(!fields_set %in% v$shorthand) {
      stop("'field_set' must be either a 'version' or a 'shorthand' code ",
           "returned by 'meta_bmde_version()', or 'custom'", call. = FALSE)
    }
    if(fields_set != "custom") {
      fields_set <- dplyr::filter(v, .data$shorthand == fields_set) %>%
        dplyr::pull(version)
    }
  }
  fields_set
}


fields_check <- function(fields) {
  f <- meta_bmde_fields(version = NULL) %>%
    dplyr::pull(.data$local_name) %>%
    unique()

  w <- fields[!fields %in% f]
  if(length(w) > 0) {
    stop("Some arguments in 'fields' are invalid (", paste0(w, collapse = ","),
    "). See 'meta_bmde_fields()' for valid options", call. = FALSE)
  }
  fields
}

info_check <- function(info) {
  if(is.null(info) || !is.character(info)) {
    stop("'info' is required text if not using a 'request_id'", call. = FALSE)
  }
}

codes_check <- function(desc, type = NULL) {

  # Get type of code
  if(is.null(type)) type <- deparse(substitute(desc))
  m <- ifelse(type == "species", "numeric", "character")

  # Get code data frames
  if(type == "species") {
    df <- meta_species_taxonomy()
  } else df <- eval(parse(text = paste0("meta_", type, "_codes()")))

  vapply(desc, codes_check_each, type = type, df = df,
         FUN.VALUE = vector(length = 1, mode = m),
         USE.NAMES = FALSE)
}

codes_check_each <- function(desc, type, df) {
  # If a two character code, conver to upper, just in case
  if(type != "species" & stringr::str_detect(desc, "^[:alpha:]{2}$")) {
    desc <- toupper(desc)
    code_column <- paste0(type, "_code")
  } else {
    code_column <- paste0(type, "_id")
  }

  # Convert to numeric (if possible)
  desc <- as_numeric(desc)

  # If code, check that correct
  if((type == "species" & is.numeric(desc)) |
     (type != "species" & stringr::str_detect(desc, "^[:upper:]{2}$"))) {

    if(!desc %in% dplyr::pull(df, code_column)) {
      stop("'", type, "' code not found, see meta_", type, "_codes() ",
           "for valid codes", call. = FALSE)
    }
    return(desc)

    # If species, and not numeric, stop
  } else if(type == "species" & !is.numeric(desc)) {
    stop("'species' code must be a numeric code ",
         "(see documentation)", call. = FALSE)
  }

  # Otherwise try to convert
  codes_convert(desc, type)
}


codes_convert <- function(desc, type) {
  c <- search_region(desc, type) %>%
    dplyr::select(dplyr::contains("_code"),
                  dplyr::contains("_name")) %>%
    dplyr::distinct()

  if(nrow(c) == 0) {
    stop("Unable to match '", desc, "' to any codes in ",
         paste0("meta_", type, "_codes()"), call. = FALSE)
  } else if(nrow(c) > 1) {
    stop("Matched '", desc, "' to ", nrow(c), " codes: \n",
         capture_df(c), call. = FALSE)
  }
  dplyr::pull(c, paste0(type, "_code"))
}

have_pkg_check <- function(pkg) {
  # TODO: remove suppression when rnaturalearth resolved
  if(!suppressPackageStartupMessages(requireNamespace(pkg, quietly = TRUE))) {
    stop("This function requires the '", pkg, "' package.", 
         "Please install with `install.packages(\"", pkg, "\")` first", call. = FALSE)
  } else if(pkg == "sf" && utils::packageVersion("sf") < "1.0-9") {
    stop("This function requires 'sf' version 1.0-9 or higher.",
         "Please update with `install.packages(\"sf\")` first", call. = FALSE)
  }
}


df_db_check <- function(df_db, collect = TRUE, verbose = TRUE) {
  
  if(!inherits(df_db, c("data.frame", "SQLiteConnection", "tbl_sql"))) {
    stop("'df_db' must be either a data frame, a connection to an SQLite ",
         "database, or a connection to an SQLite table", call. = FALSE)
  }
  
  # Collect as required
  if(collect && inherits(df_db, c("SQLiteConnection", "tbl_sql"))) {
    
    if(verbose) message(" - Cannot work directly on SQLite database connections, ",
                        "collecting data into a data frame...")
    
    if(inherits(df_db, "SQLiteConnection")) {
      
      if(!"naturecounts" %in% DBI::dbListTables(df_db)) {
        stop("If 'df_db' is a SQLite database, it must have a 'naturecounts' ",
             "table", call. = FALSE)
      }
      df_db <- dplyr::tbl(df_db, "naturecounts")
    }
    
    df_db <- dplyr::collect(df_db)
  }
  
  df_db
}