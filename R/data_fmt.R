#' Format Data for Covariate Download and Extraction
#'
#' This function accepts a variety of input data and conforms it to a
#' standardized format for use in the various covariate download and extraction
#' functions available in the naturecounts R package. Users are not required to
#' use this function before using covariate download and extraction functions,
#' but may avoid some finnicky work by doing so.
#'
#' @param data Data frame, `sf` 'POINT' or 'POLYGON' object, or `terra` 'points'
#'   or 'polygons' object containing observations associated with coordinate and
#'   date data.
#' @param site_name Character. Optional argument to provide the name of the
#'   column containing site names if not contained within the BMDE column
#'   `SurveyAreaIdentifier`.
#' @param coord_lon Character. Optional argument to provide the name of the
#'   column containing year data if not contained within the BMDE column
#'   `longitude`.
#' @param coord_lat Character. Optional argument to provide the name of the
#'   column containing year data if not contained within the BMDE column
#'   `latitude`.
#' @param date_year Character. Optional argument to provide the name of the
#'   column containing year data if not contained within the BMDE column
#'   `survey_year`.
#' @param date_month Character. Optional argument to provide the name of the
#'   column containing month data if not contained within the BMDE column
#'   `survey_month`.
#' @param date_day Character. Optional argument to provide the name of the
#'   column containing day of month data if not contained within the BMDE column
#'   `survey_day`.
#' @param date_lubridate Character. Optional argument to provide the name of a
#'   column containing date data in `lubridate` formats.
#' @param date_ordinal Character. Optional argument to provide the name of a
#'   column containing date data in ordinal format.
#' @param crs Character. Optional argument to provide the coordinate reference
#'   system of the provided data. Only required when providing a data frame
#'   containing data not using the typical GPS latitude/longitude
#'   [WGS84](https://epsg.io/4326) (`EPSG:4326`) coordinate reference system, or
#'   `sf`/ `terra` objects without coordinate reference systems embedded.
#'
#' @returns If `data.frame`, `sf` "POINT", or `terra` "points" data provided,
#'   `sf` "POINT" object. If `sf` "POLYGON" or `terra` "polygons" data provided,
#'   `sf` "POLYGON" object. Returned object contains a row for each unique
#'   site-date combination in the provided data, and is provided in the [NAD 1983
#'   Albers Canada](https://epsg.io/102001) (`EPSG:102001`) coordinate reference system with the
#'   following columns.
#'    - SurveyAreaIdentifier - character. Site names, or if missing in original
#'   data, filled site names for use in later functions.
#'    - latitude - numeric. Y-coordinate in NAD 1983 Albers Canada
#'   (`EPSG:102001`) coordinate reference system.
#'    - longitude - numeric. X-coordinate in NAD 1983 Albers Canada
#'   (`EPSG:102001`) coordinate reference system.
#'    - survey_year - numeric. Observation year.
#'    - survey_month - numeric. Observation month.
#'    - survey_day - numeric. Observation day (of month).
#'    - geometry - `sf` geometry column.
#'
#' @examples
#'
#' # Using the included, test data on black-capped chickadees
#' bcch # look at the data
#'
#' # Format
#' output <- data_fmt(bcch)
#'
#' @seealso [sf::st_as_sf()] and [terra::vect()] which this function wraps.
#'
#'   [data_buff()] to buffer data points by a specified distance to measure
#'   covariates at desired spatial scales.
#'
#' @export

data_fmt <- function(
    data,
    site_name = NULL, # optional argument to provide column name containing site
    # names. Default is assumed to be the BMDE column 'SurveyAreaIdentifier'.
    coord_lon = NULL, # as in cosewic_ranges
    coord_lat = NULL, # as in cosewic_ranges
    date_year = NULL, # optional argument to provide column name containing year
    # data. Default is assumed to be the BMDE column 'survey_year'.
    date_month = NULL, # optional argument to provide column name containing month
    # data. Default is assumed to be the BMDE column 'survey_month'.
    date_day = NULL, # optional argument to provide column name containing day
    # data. Default is assumed to be the BMDE column 'survey_day'.
    date_lubridate = NULL, # optional argument to provide column name containing
    # 'lubridate' date objects.
    date_ordinal = NULL, # optional argument to provide column name containing
    # ordinal dates.
    crs = NULL # optional argument to provide a Coordinate Reference System for
    # provided data.
) {
  message("[Data Formatting] beginning formatting.")
  
  # Check packages
  
  have_pkg_check(c(
    "sf",
    "terra",
    "tidyterra"
  ))
  
  # Check data type - we need either a dataframe, sf points object, sf polygon,
  # or terra SpatVector.
  
  input_fmt <- covariate_fmt_check(data)
  
  # Deal with alternate CRS's
  
  # Check that 'crs' argument has been provided.
  if (!is.null(crs)) {
    # Check if input is an sf object.
    if (input_fmt$type == "sf") {
      # Check if provided sf object has a CRS. If missing, set to provided CRS.
      # Warn.
      if (is.na(sf::st_crs(data))) {
        warning(
          "[Data Formatting] the CRS of the provided sf object is missing, it",
          " will be set to the alternate CRS specified in the 'crs' argument.",
          call. = FALSE
        )
        
        suppressWarnings(sf::st_crs(data) <- crs)
        
        # If sf object still is missing CRS, suggests that provided CRS is
        # invalid. Return error.
        if (is.na(sf::st_crs(data))) {
          stop(
            "[Data Formatting] the provided CRS is invalid. CRS must be a",
            " valid proj4string character, a valid epsg integer value, or a list",
            " containing named elements proj4string (character) and/or epsg",
            " (integer).",
            call. = FALSE
          )
        }
      } else {
        # If sf object has a CRS and the 'crs' argument has been provided, use
        # the CRS included in the sf object. Warn.
        warning(
          "[Data Formatting] the sf object provided has a specified CRS and a",
          " CRS has been provided using the 'crs' argument. The CRS of the sf",
          " object will be used.",
          call. = FALSE
        )
        
        crs <- sf::st_crs(data)
      }
    }
    
    # Check if input is a terra SpatVector.
    if (input_fmt$type == "terra") {
      # Check if provided terra object has a CRS. If missing, set to provided
      # CRS. Warn.
      if (terra::crs(data) == "") {
        warning(
          "[Data Formatting] the CRS of the provided terra object is missing,",
          " it will be set to the alternate CRS specified in the 'crs'",
          " argument.",
          call. = FALSE
        )
        
        # Convert terra warnings associated with invalid CRS inputs into errors.
        tryCatch(
          terra::crs(data) <- crs,
          warning = function(w) {
            if (
              "[crs<-] Cannot set SRS to vector: empty srs" %in%
              conditionMessage(w) |
              paste0(
                "PROJ: proj_create_from_database: crs not found:",
                " EPSG:234634 (GDAL error 1)"
              ) %in%
              conditionMessage(w)
            ) {
              stop(
                "[Data Formatting] the provided CRS is invalid. CRS",
                " must be a character string in WKT (e.g. 'EPSG:4326') or",
                " PROJ-string format (e.g. '+proj=utm +zone=12').",
                call. = FALSE
              )
            } else {
              warning(conditionMessage(w), call. = FALSE)
            }
          },
          error = function(e) {
            if (
              conditionMessage(e) ==
              paste0(
                "[crs] I do not know what",
                " to do with this argument",
                " (expected a character",
                " string)"
              )
            ) {
              stop(
                "[Data Formatting] the provided CRS is invalid. CRS",
                " must be a character string in WKT (e.g. 'EPSG:4326') or",
                " PROJ-string format (e.g. '+proj=utm +zone=12').",
                call. = FALSE
              )
            } else {
              stop(conditionMessage(e), call. = FALSE)
            }
          }
        )
      } else {
        # If terra object has a CRS and the 'crs' argument has been provided,
        # use the CRS included in the terra object. Warn.
        warning(
          "[Data Formatting] the terra object provided has a specified CRS and",
          " a CRS has been provided using the 'crs' argument. The CRS of the",
          " terra object will be used.",
          call. = FALSE
        )
        
        crs <- terra::crs(data)
      }
    }
    
    # If provided data is a data.frame, make sure we have the names of columns
    # pointing us to associated coordinate data. If not, return error.
    if (
      input_fmt$type == "data.frame" & (is.null(coord_lon) | is.null(coord_lat))
    ) {
      stop(
        "[Data Formatting] alternate CRS provided, but without specified",
        " column for one or more coordinate. Use the 'coord_lon' argument to",
        " give the name of column containing the X-coordinate, and the",
        " 'coord_lat' argument to give the name of the column containing the",
        " Y-coordinate.",
        call. = FALSE
      )
    }
  }
  
  # If no 'crs' argument is provided, and provided sf object lacks a CRS,
  # return error.
  if (is.null(crs) & input_fmt$type == "sf") {
    if (is.na(sf::st_crs(data))) {
      stop(
        "[Data Formatting] provided sf object lacks a CRS. Please specify",
        " using the 'crs' argument or provide an sf object with a CRS.",
        call. = FALSE
      )
    }
  }
  
  # If no 'crs' argument is provided, and provided terra object lacks a CRS,
  # return error.
  if (is.null(crs) & input_fmt$type == "terra") {
    if (terra::crs(data) == "") {
      stop(
        "[Data Formatting] provided terra object lacks a CRS. Please specify",
        " using the 'crs' argument or provide a terra object with a CRS.",
        call. = FALSE
      )
    }
  }
  
  # If no 'crs' argument is provided, and provided data is a dataframe, assume
  # it is the default NatureCounts format which uses lat/lon and use EPSG:4326.
  # Warn.
  if (is.null(crs) & input_fmt$type == "data.frame") {
    warning(
      "[Data Formatting] as the 'crs' argument is not specified, data CRS is",
      " assumed to be EPSG:4326.",
      call. = FALSE
    )
    
    crs <- 4326
  }
  
  # If spatial object is provided and the 'coord_lon'/'coord_lat' arguments
  # have been provided, use the coordinate data included in the spatial object.
  # Warn.
  if (
    input_fmt$type %in%
    c("sf", "terra") &
    (!is.null(coord_lon) | !is.null(coord_lat))
  ) {
    warning(
      "[Data Formatting] sf or terra object provided as well as a lat/lon",
      " column name. lat/lon will be derived from the spatial data within the",
      " sf/terra object and specified lat/lon column will be ignored.",
      call. = FALSE
    )
    
    coord_lon <- NULL
    coord_lat <- NULL
  }
  
  # Check that all specified column names are present in the data.
  
  # Gather all potentially specified columns.
  specified_cols <- c(
    site_name,
    coord_lon,
    coord_lat,
    date_year,
    date_month,
    date_day,
    date_lubridate,
    date_ordinal
  )
  
  # Remove any that haven't been specified.
  specified_cols <- specified_cols[!is.null(specified_cols)]
  
  data_cols <- names(data)
  
  # Compare to columns present in data. Return error if any specified columns
  # are not present.
  if (!(all(specified_cols %in% data_cols))) {
    stop(
      "[Data Formatting] some specified columns missing from the data: ",
      stringr::str_flatten_comma(specified_cols[
        !(specified_cols %in% data_cols)
      ]),
      ". Use arguments to specify alternate column names if using data that",
      " diverges from NatureCounts default column names.",
      call. = FALSE
    )
  }
  
  # Conform specified columns to naturecounts default column names. Calls to
  # st_sf() needed to avoid sf specific issue with attributes.
  if (!is.null(site_name)) {
    if (input_fmt$type == "sf") {
      data <- sf::st_sf(data)
    }
    data <- dplyr::rename(data, "SurveyAreaIdentifier" = !!site_name)
  }
  
  data$SurveyAreaIdentifier <- as.character(data$SurveyAreaIdentifier)
  
  if (input_fmt$type == "data.frame") {
    if (!is.null(coord_lon)) {
      # Edge case: there is a col called longitude that isn't coord_lon.
      # Remove.
      if ("longitude" %in% names(data) & !(coord_lon == "longitude")) {
        data <- dplyr::select(data, -"longitude")
      }
      
      if (input_fmt$type == "sf") {
        data <- sf::st_sf(data)
      }
      
      data <- dplyr::rename(data, "longitude" = !!coord_lon)
    }
    
    data$longitude <- as.numeric(data$longitude)
    
    if (!is.null(coord_lat)) {
      # Edge case: there is a col called latitude that isn't coord_lat. Remove.
      if ("latitude" %in% names(data) & !(coord_lat == "latitude")) {
        data <- dplyr::select(data, -"latitude")
      }
      
      if (input_fmt$type == "sf") {
        data <- sf::st_sf(data)
      }
      
      data <- dplyr::rename(data, "latitude" = !!coord_lat)
    }
    
    data$latitude <- as.numeric(data$latitude)
  }
  
  if (!is.null(date_year)) {
    if (input_fmt$type == "sf") {
      data <- sf::st_sf(data)
    }
    
    data <- dplyr::rename(data, "survey_year" = !!date_year)
  }
  
  if (!is.null(date_month)) {
    if (input_fmt$type == "sf") {
      data <- sf::st_sf(data)
    }
    
    data <- dplyr::rename(data, "survey_month" = !!date_month)
  }
  
  # Use month_check() to validate month data. 'if' wrapper needed to handle
  # cases where no month column was provided, and a lubridate or ordinal date
  # column was provided instead.
  if ("survey_month" %in% names(data)) {
    month_corr <- c()
    
    for (i in 1:length(data$survey_month)) {
      month_corr[i] <- month_check(data$survey_month[i])
    }
    
    data$survey_month <- month_corr
  }
  
  if (!is.null(date_day)) {
    if (input_fmt$type == "sf") {
      data <- sf::st_sf(data)
    }
    
    data <- dplyr::rename(data, "survey_day" = !!date_day)
  }
  
  # Use dom_check() to validate day data. 'if' wrapper needed to handle cases
  # where no month column was provided, and a lubridate or ordinal date column
  # was provided instead.
  if ("survey_day" %in% names(data)) {
    for (i in data$survey_day) {
      dom_check(i)
    }
  }
  
  # If a date in lubridate or ordinal format is provided, make year, month and
  # day columns.
  if (!is.null(date_lubridate)) {
    # Standardize date column name
    data <- dplyr::rename(data, "date" = !!date_lubridate)
    
    # Check that provided lubridate data is a date object. If not, return error.
    if (!lubridate::is.Date(data$date)) {
      stop(
        "[Data Formatting] column ",
        date_lubridate,
        " expected to be in `Date` format, but is not.",
        call. = FALSE
      )
    }
    
    # Check that provided lubridate data is an instant rather than a duration
    # object. If not, return error.
    if (!lubridate::is.instant(data$date)) {
      stop(
        "[Data Formatting] column ",
        date_lubridate,
        " expected to be a single instant in time, but is not.",
        call. = FALSE
      )
    }
    
    # Check that all dates are either the current date or in the past. If not,
    # return error.
    if (!all(data$date <= as.Date(Sys.Date()))) {
      stop(
        "[Data Formatting] some dates are in the future! Covariate data only",
        " available for data in the past.",
        call. = FALSE
      )
    }
    
    # If lubridate column provided alongside other specified date column
    # options, use data from lubridate columns. Warn.
    if (
      !is.null(date_year) |
      !is.null(date_month) |
      !is.null(date_day) |
      !is.null(date_ordinal)
    ) {
      date_cols <- c(
        date_lubridate,
        date_year,
        date_month,
        date_day,
        date_ordinal
      )
      date_cols <- date_cols[!is.null(date_cols)]
      
      warning(
        paste0(
          "[Data Formatting] multiple date column options provided including ",
          stringr::str_flatten_comma(date_cols),
          ". The data in ",
          date_lubridate,
          " will be used."
        ),
        call. = FALSE
      )
    }
    
    # Extract year/month/day columns from lubridate date.
    data$survey_year <- lubridate::year(data$date)
    
    data$survey_month <- lubridate::month(data$date)
    
    data$survey_day <- lubridate::day(data$date)
    
    # In case ordinal data has also been provided, set to NULL so dates aren't
    # recalculated using ordinal data.
    date_year <- NULL
    
    date_ordinal <- NULL
  }
  
  # If a date in ordinal format is provided (and a date in lubridate format is
  # not provided, see above), make year, month and day columns.
  if (!is.null(date_ordinal)) {
    # Standardize ordinal date column name
    data <- dplyr::rename(data, "doy" = !!date_ordinal)
    
    # Check that year data has been provided alongside ordinal day data as this
    # is needed to convert to calendar date. If not, return error.
    if (!("survey_year" %in% names(data))) {
      stop(
        "[Data Formatting] if providing an ordinal date, year data must",
        " accompany it. Please provide a column with associated year data",
        " using the `date_year` argument.",
        call. = FALSE
      )
    }
    
    # Use doy_check() to validate ordinal date data.
    for (i in data$doy) {
      doy_check(i)
    }
    
    # If month or day data has also been provided, warn that ordinal date data
    # will supersede it.
    if (!is.null(date_month) | !is.null(date_day)) {
      warning(
        "[Data Formatting] dates derived from ordinal dates will supersede",
        " provided month and/or day data.",
        call. = FALSE
      )
    }
    
    # If ordinal date is numeric, add it to the first day of the associated
    # year to get the calendar date.
    if (is.numeric(data$doy)) {
      data$date <- as.Date(paste0(data$survey_year, "-01-01")) + data$doy - 1
    }
    
    # If ordinal date has been provided as a date object (likely due to
    # misunderstanding of the meaning of ordinal date) convert it to ordinal
    # date and add it to the first day of the associated calendar year.
    if (lubridate::is.Date(data$doy)) {
      data$date <- as.Date(paste0(data$survey_year, "-01-01")) +
        lubridate::yday(data$doy) -
        1
    }
    
    # Extract month and day data from ordinal-derived date column
    data$survey_month <- lubridate::month(data$date)
    
    data$survey_day <- lubridate::day(data$date)
  }
  
  # Ensure date columns are numeric.
  data$survey_year <- as.numeric(data$survey_year)
  
  data$survey_month <- as.numeric(data$survey_month)
  
  data$survey_day <- as.numeric(data$survey_day)
  
  # If data is a dataframe, ensure that there are no rows missing coordinate
  # data as this would prevent conversion into an sf object. Warn.
  if (input_fmt$type == "data.frame") {
    if (NA %in% unique(data$latitude) | NA %in% unique(data$longitude)) {
      warning(
        "[Data Formatting] some rows missing coordinate data will be dropped.",
        call. = FALSE
      )
      
      data <- dplyr::filter(data, !(is.na(.data$latitude) | is.na(.data$longitude)))
    }
  }
  
  # Handle missing SurveyAreaIdentifiers and ensure coordinates are present in
  # the data for later use in nc_covariates_merge().
  if (TRUE %in% is.na(data$SurveyAreaIdentifier)) {
    # For dataframe objects create an object containing all X/Y coordinates
    # that do not have an associated SurveyAreaIdentifier.
    if (input_fmt$type == "data.frame") {
      missing_sitecode <- data %>%
        dplyr::select("SurveyAreaIdentifier", "latitude", "longitude") %>%
        dplyr::filter(is.na(.data$SurveyAreaIdentifier)) %>%
        dplyr::distinct()
    }
    
    # For sf objects, create an object containing all X/Y coordinates (derived
    # from geometries) that do not have an associated SurveyAreaIdentifier.
    # Also append coordinates to original data object for later joining.
    if (input_fmt$type == "sf") {
      missing_sitecode <- data %>%
        dplyr::select("SurveyAreaIdentifier", "geometry")
      
      # For polygons, use the centroid as the X/Y coordinates.
      if (input_fmt$geometry == "POLYGON") {
        missing_sitecode <- suppressWarnings(sf::st_centroid(missing_sitecode))
      }
      
      # Extract coordinates and bind to data. Drop geometry and get all
      # unique coordinate combinations with missing SurveyAreaIdentifiers.
      missing_sitecode <- cbind(
        missing_sitecode,
        sf::st_coordinates(missing_sitecode)
      ) %>%
        dplyr::rename("longitude" = "X", "latitude" = "Y") %>%
        sf::st_drop_geometry() %>%
        dplyr::filter(is.na(.data$SurveyAreaIdentifier)) %>%
        dplyr::distinct()
      
      # Edge case: there is a col called X. This does not lead to the removal
      # of this column in final data when merged using nc_covariates_merge().
      if ("X" %in% names(data)) {
        data <- dplyr::select(data, -"X")
      }
      
      # Edge case: there is a col called Y. This does not lead to the removal
      # of this column in final data when merged using nc_covariates_merge().
      if ("Y" %in% names(data)) {
        data <- dplyr::select(data, -"Y")
      }
      
      # Edge case: there is a col called longitude. This does not lead to the
      # removal of this column in final data when merged using
      # nc_covariates_merge().
      if ("longitude" %in% names(data)) {
        data <- dplyr::select(data, -"longitude")
      }
      
      # Edge case: there is a col called latitude. This does not lead to the
      # removal of this column in final data when merged using
      # nc_covariates_merge().
      if ("latitude" %in% names(data)) {
        data <- dplyr::select(data, -"latitude")
      }
      
      # Append coordinates (from centroids if polygons) to provided data object.
      if (input_fmt$geometry == "POLYGON") {
        data <- cbind(
          data,
          sf::st_coordinates(suppressWarnings(sf::st_centroid(data)))
        ) %>%
          dplyr::rename("longitude" = "X", "latitude" = "Y")
      } else {
        data <- cbind(data, sf::st_coordinates(data)) %>%
          dplyr::rename("longitude" = "X", "latitude" = "Y")
      }
    }
    
    # For terra objects, create an object containing all X/Y coordinates
    # (derived from geometries) that do not have an associated
    # SurveyAreaIdentifier. Also append coordinates to original data object for
    # later joining.
    if (input_fmt$type == "terra") {
      missing_sitecode <- data %>%
        tidyterra::select("SurveyAreaIdentifier")
      
      # For polygons, use the centroid as the X/Y coordinates.
      if (input_fmt$geometry == "polygons") {
        missing_sitecode <- terra::centroids(missing_sitecode)
      }
      
      # Extract coordinates and bind to data. Drop geometry and get all
      # unique coordinate combinations with missing SurveyAreaIdentifiers.
      missing_sitecode <- cbind(
        missing_sitecode,
        terra::crds(missing_sitecode)
      ) %>%
        tidyterra::rename("longitude" = "x", "latitude" = "y") %>%
        terra::as.data.frame() %>%
        dplyr::filter(is.na(.data$SurveyAreaIdentifier)) %>%
        dplyr::distinct()
      
      # Edge case: there is a col called x. This does not lead to the removal
      # of this column in final data when merged using nc_covariates_merge().
      if ("x" %in% names(data)) {
        data <- tidyterra::select(data, -"x")
      }
      
      # Edge case: there is a col called y. This does not lead to the removal
      # of this column in final data when merged using nc_covariates_merge().
      if ("y" %in% names(data)) {
        data <- tidyterra::select(data, -"y")
      }
      
      # Edge case: there is a col called longitude. This does not lead to the
      # removal of this column in final data when merged using
      # nc_covariates_merge().
      if ("longitude" %in% names(data)) {
        data <- dplyr::select(data, -"longitude")
      }
      
      # Edge case: there is a col called latitude. This does not lead to the
      # removal of this column in final data when merged using
      # nc_covariates_merge().
      if ("latitude" %in% names(data)) {
        data <- dplyr::select(data, -"latitude")
      }
      
      # Append coordinates (from centroids if polygons) to provided data object.
      if (input_fmt$geometry == "polygons") {
        data <- cbind(data, terra::crds(terra::centroids(data))) %>%
          dplyr::rename("longitude" = "x", "latitude" = "y")
      } else {
        data <- cbind(data, terra::crds(data)) %>%
          dplyr::rename("longitude" = "x", "latitude" = "y")
      }
    }
    
    # Create a dummy SurveyAreaIdentifier for all unique coordinate combinations
    # which are missing an associated SurveyAreaIdentifier.
    for (i in 1:nrow(missing_sitecode)) {
      missing_sitecode$SurveyAreaIdentifier[i] <- paste0("FilledSurveyArea", i)
    }
    
    # Use coordinates to join dummy SurveyAreaIdentifiers to original data.
    for (i in missing_sitecode$latitude) {
      for (j in missing_sitecode$longitude[missing_sitecode$latitude == i]) {
        data$SurveyAreaIdentifier[
          data$latitude == i & data$longitude == j
        ] <- missing_sitecode$SurveyAreaIdentifier[
          missing_sitecode$latitude == i & missing_sitecode$longitude == j
        ]
      }
    }
  } else {
    # In case all SurveyAreaIdentifiers are present, append coordinates to
    # spatial data objects for later use in nc_covariates_merge().
    if (input_fmt$type == "sf") {
      # Edge case: there is a col called X. This does not lead to the removal
      # of this column in final data when merged using nc_covariates_merge().
      if ("X" %in% names(data)) {
        data <- dplyr::select(data, -"X")
      }
      
      # Edge case: there is a col called Y. This does not lead to the removal
      # of this column in final data when merged using nc_covariates_merge().
      if ("Y" %in% names(data)) {
        data <- dplyr::select(data, -"Y")
      }
      
      # Edge case: there is a col called longitude. This does not lead to the
      # removal of this column in final data when merged using
      # nc_covariates_merge().
      if ("longitude" %in% names(data)) {
        data <- dplyr::select(data, -"longitude")
      }
      
      # Edge case: there is a col called latitude. This does not lead to the
      # removal of this column in final data when merged using
      # nc_covariates_merge().
      if ("latitude" %in% names(data)) {
        data <- dplyr::select(data, -"latitude")
      }
      
      # Append coordinates (from centroids if polygons) to provided data object.
      if (input_fmt$geometry == "POLYGON") {
        data <- cbind(
          data,
          sf::st_coordinates(suppressWarnings(sf::st_centroid(data)))
        ) %>%
          dplyr::rename("longitude" = "X", "latitude" = "Y")
      } else {
        data <- cbind(data, sf::st_coordinates(data)) %>%
          dplyr::rename("longitude" = "X", "latitude" = "Y")
      }
    }
    
    if (input_fmt$type == "terra") {
      # Edge case: there is a col called x. This does not lead to the removal
      # of this column in final data when merged using nc_covariates_merge().
      if ("x" %in% names(data)) {
        data <- tidyterra::select(data, -"x")
      }
      
      # Edge case: there is a col called y. This does not lead to the removal
      # of this column in final data when merged using nc_covariates_merge().
      if ("y" %in% names(data)) {
        data <- tidyterra::select(data, -"y")
      }
      
      # Edge case: there is a col called longitude. This does not lead to the
      # removal of this column in final data when merged using
      # nc_covariates_merge().
      if ("longitude" %in% names(data)) {
        data <- tidyterra::select(data, -"longitude")
      }
      
      # Edge case: there is a col called latitude. This does not lead to the
      # removal of this column in final data when merged using
      # nc_covariates_merge().
      if ("latitude" %in% names(data)) {
        data <- tidyterra::select(data, -"latitude")
      }
      
      # Append coordinates (from centroids if polygons) to provided data object.
      if (input_fmt$geometry == "polygons") {
        data <- cbind(data, terra::crds(terra::centroids(data))) %>%
          dplyr::rename("longitude" = "x", "latitude" = "y")
      } else {
        data <- cbind(data, terra::crds(data)) %>%
          dplyr::rename("longitude" = "x", "latitude" = "y")
      }
    }
  }
  
  if (!is.null(date_ordinal)) {
    names(data)[names(data) == "doy"] <- date_ordinal
  }
  
  if (!is.null(date_lubridate)) {
    names(data)[names(data) == "date"] <- date_lubridate
  }
  
  # Create base list of columns to preserve.
  keep_cols <- c(
    "SurveyAreaIdentifier",
    "latitude",
    "longitude",
    "survey_year",
    "survey_month",
    "survey_day"
  )
  
  # If ordinal date provided, preserve it for later use in
  # nc_covariates_merge().
  if (!is.null(date_ordinal)) {
    keep_cols <- c(keep_cols[1:4], date_ordinal, keep_cols[5:6])
  }
  
  # If lubridate date provided, preserve it for later use in
  # nc_covariates_merge().
  if (!is.null(date_lubridate)) {
    keep_cols <- c(keep_cols[1:3], date_lubridate, keep_cols[4:6])
  }
  
  # For dataframe objects, convert to spatial features object.
  if (input_fmt$type == "data.frame") {
    # Get all distinct combinations of kept columns, convert to sf object.
    suppressWarnings(
      data <- dplyr::select(data, tidyselect::all_of(keep_cols)) %>%
        dplyr::distinct() %>%
        sf::st_as_sf(
          coords = c("longitude", "latitude"),
          crs = crs,
          remove = FALSE
        )
    )
    
    # If created spatial object CRS is missing, provided CRS was invalid.
    # Return error.
    if (is.na(sf::st_crs(data))) {
      stop(
        "[Data Formatting] the provided CRS is invalid. CRS must be a valid",
        " proj4string character, a valid epsg integer value, or a list",
        " containing named elements proj4string (character) and/or epsg",
        " (integer).",
        call. = FALSE
      )
    }
    
    # Convert to CRS with metres as a base unit to facilitate buffering.
    data <- sf::st_transform(data, "ESRI:102001")
  }
  
  # For sf objects, keep all distinct combinations of kept columns.
  if (input_fmt$type == "sf") {
    # Ensure geometry column is retained.
    keep_cols <- c(keep_cols, "geometry")
    
    # Convert to CRS with metres as a base unit to facilitate buffering.
    data <- dplyr::select(data, tidyselect::all_of(keep_cols)) %>%
      dplyr::distinct() %>%
      sf::st_transform("ESRI:102001")
  }
  
  # For terra objects, keep all distinct combinations of kept columns and
  # convert to CRS with metres as a base unit to facilitate buffering.
  if (input_fmt$type == "terra") {
    data <- tidyterra::select(data, tidyselect::all_of(keep_cols)) %>%
      tidyterra::distinct() %>%
      terra::project("ESRI:102001")
  }
  
  # Store specified column names and crs as attributes so that they don't need
  # to be specified any time associated functions are called.
  if (!is.null(site_name)) {
    names(data)[names(data) == "SurveyAreaIdentifier"] <- site_name
    attr(data, "site_name") <- site_name
  }
  
  if (!is.null(coord_lon)) {
    names(data)[names(data) == "longitude"] <- coord_lon
    attr(data, "coord_lon") <- coord_lon
  }
  
  if (!is.null(coord_lat)) {
    names(data)[names(data) == "latitude"] <- coord_lat
    attr(data, "coord_lat") <- coord_lat
  }
  
  if (!is.null(date_year)) {
    names(data)[names(data) == "survey_year"] <- date_year
    attr(data, "date_year") <- date_year
  }
  
  if (!is.null(date_month)) {
    names(data)[names(data) == "survey_month"] <- date_month
    attr(data, "date_month") <- date_month
  }
  
  if (!is.null(date_day)) {
    names(data)[names(data) == "survey_day"] <- date_day
    attr(data, "date_day") <- date_day
  }
  
  if (!is.null(date_ordinal)) {
    attr(data, "date_ordinal") <- date_ordinal
  }
  
  if (!is.null(date_lubridate)) {
    attr(data, "date_lubridate") <- date_lubridate
  }
  
  if (!is.null(crs)) {
    attr(data, "crs") <- crs
  }
  
  # Return formatted data.
  return(data)
}