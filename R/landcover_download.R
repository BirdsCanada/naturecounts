#' Download MODIS Landcover Data
#'
#' Downloads [annual landcover data](https://doi.org/10.5067/MODIS/MCD12Q1.061)
#' derived from imagery from the MODIS Terra and Aqua satellites at
#' approximately 500 m spatial resolution. This data is retreived via the NASA
#' EarthData Archive, requiring an EarthData account to be made. This can be
#' done at the following link: [register for an EarthData
#' account](https://urs.earthdata.nasa.gov/users/new).
#'
#' All five classification schemes available through [landcover_extract()] are
#' downloaded by this function without need for extra specification.
#'
#' Downloads are facilitated by a call to [luna::getNASA()].
#'
#' @param data An `sf` 'POINT' or 'POLYGON' object, or `terra` 'points' or
#'   'polygons' object.
#' @param ed_email Character. The email address associated with your EarthData
#'   account.
#' @param ed_transfer Logical. Should data be downloaded from EarthData?
#'   If FALSE, a vector containing the names of the files that would be
#'   downloaded is returned.
#' @param site_name Character. Optional argument to provide the name of the
#'   column containing site names if not contained within the BMDE column
#'   `SurveyAreaIdentifier`. Can be left NULL and still function properly if
#'   originally specified in a call to [data_fmt()].
#' @param date_year Character. Optional argument to provide the name of the
#'   column containing year data if not contained within the BMDE column
#'   `survey_year`. Can be left NULL and still function properly if originally
#'   specified in a call to [data_fmt()].
#' @param dl_path Character. Optional argument to provide path to download data
#'   to. By default, data is downloaded to a subfolder `modis/` in the working
#'   directory.
#'
#' @returns If `ed_transfer = TRUE`, character vector containing
#'   file-paths to downloaded MODIS landcover files. If `ed_transfer =
#'   FALSE`, character vector containing filenames of MODIS landcover files that
#'   would be downloaded.
#'
#' @examples
#'
#' # Using the included, test data on black-capped chickadees
#' bcch # look at the data
#'
#' # Convert to sf POINT object
#' bcch <- sf::st_as_sf(
#'   bcch,
#'   coords = c("longitude", "latitude"),
#'   crs = 4326
#' )
#'
#' # Get file names that would be downloaded.
#' output <- landcover_download(
#'   data = bcch,
#'   ed_transfer = FALSE
#' )
#'
#' # Enter EarthData email
#' ed_email <- "your EarthData email"
#'
#' # Download MODIS data
#' #output <- landcover_download(
#' #  data = bcch,
#' #  ed_email = ed_email
#' )
#'
#' @seealso [luna::getNASA()] which this function wraps.
#'
#' @export

# Function for downloading MODIS MCD12Q1 data from NASA EarthData. Wrapper for
# luna::getNASA().
landcover_download <- function(
  data,
  ed_email = NULL, # users' EarthData account email address.
  ed_transfer = TRUE, # Should data be downloaded from EarthData?
  site_name = NULL, # optional argument to provide column name containing site
  # names. Default is assumed to be the BMDE column 'SurveyAreaIdentifier'. Can
  # be left NULL and still function properly if originally specified in a call
  # to data_fmt().
  date_year = NULL, # optional argument to provide column name containing year
  # data. Default is assumed to be the BMDE column 'survey_year'. Can
  # be left NULL and still function properly if originally specified in a call
  # to data_fmt().
  dl_path = NULL # optional argument to provide path to download data to. By
  # default, data is downloaded to a subfolder 'modis/' in the working
  # directory.
) {
  # Check packages
  have_pkg_check(c(
    "sf",
    "terra",
    "luna"
  ))

  # Check that an EarthData account email has been provided. If not, return
  # error.
  if (missing(ed_email) & ed_transfer == TRUE) {
    stop(
      "[MODIS Landcover Download] MODIS Landcover data requested but Earthdata",
      " system login information not supplied. Please register at",
      " https://urs.earthdata.nasa.gov/users/new and supply using `ed_email`",
      " argument.",
      call. = FALSE
    )
  }
  
  # Check that ed_transfer is logical.
  if (!is.logical(ed_transfer)) {
    stop("[MODIS Landcover Download] ed_transfer must be TRUE or FALSE.",
         call. = FALSE)
  }

  # Check whether an EarthData password exists in the environment (is specified
  # earlier in the nc_covariates() workflow), and if not, request using
  # askpass::askpass().
  if (is.null(parent.frame()$ed_password) & ed_transfer == TRUE) {
    ed_password <- askpass::askpass(
      prompt = paste0(
        "Please enter password for ",
        "EarthData user '",
        ed_email,
        "'."
      )
    )
  } else if (ed_transfer == TRUE) {
    ed_password <- parent.frame()$ed_password
  }

  # Check data is in the desired format.
  input_fmt <- covariate_fmt_check(data)

  # If not an sf or terra object, return error and point towards data_fmt().
  if (input_fmt$type == "data.frame") {
    stop(
      "[MODIS Landcover Download] downloading requires an sf or terra object",
      " as input in this workflow. Consider using `data_fmt` to conform data",
      " first.",
      call. = FALSE
    )
  }

  # Check whether information on alternate column names has been stored
  # in the attributes by data_fmt(). However, prioritize alternate column names
  # specified in the current call.
  if (is.null(site_name) & !is.null(attr(data, "site_name"))) {
    site_name <- attr(data, "site_name")
  }

  if (is.null(date_year) & !is.null(attr(data, "date_year"))) {
    date_year <- attr(data, "date_year")
  }

  # Check that all specified column names are present in the data.

  # Gather all potentially specified columns.
  specified_cols <- c(site_name, date_year)

  # Remove any that haven't been specified.
  specified_cols <- specified_cols[!is.null(specified_cols)]

  data_cols <- names(data)

  # Compare to columns present in data. Return error if any specified columns
  # are not present. 'if' wrapper needed for when alternate column names exist
  # in the attributes of the data, but conversion of those columns to
  # standardized names has already taken place in data_fmt().
  if (
    !(all(specified_cols %in% data_cols)) &
      (!("SurveyAreaIdentifier" %in% data_cols) |
        !("survey_year" %in% data_cols))
  ) {
    stop(
      "[MODIS Landcover Download] some specified columns missing from the",
      " data: ",
      stringr::str_flatten_comma(specified_cols[
        !(specified_cols %in% data_cols)
      ]),
      ". Use arguments to specify alternate column names if using data that",
      " diverges from naturecounts default column names.",
      call. = FALSE
    )
  }

  # Conform specified columns to naturecounts default column names. Calls to
  # st_sf() needed to avoid sf specific issue with attributes.
  if (!is.null(site_name) & !("SurveyAreaIdentifier") %in% data_cols) {
    if (input_fmt$type == "sf") {
      data <- sf::st_sf(data)
    }

    data <- dplyr::rename(data, "SurveyAreaIdentifier" = !!site_name)
  }

  data$SurveyAreaIdentifier <- as.character(data$SurveyAreaIdentifier)

  if (!is.null(date_year) & !("survey_year") %in% data_cols) {
    if (input_fmt$type == "sf") {
      data <- sf::st_sf(data)
    }

    data <- dplyr::rename(data, "survey_year" = !!date_year)
  }

  data$survey_year <- as.numeric(data$survey_year)

  # Create area of interest polygon from provided sf object.
  if (input_fmt$type == "sf") {
    # Store original CRS so data can be returned as provided.
    orig_crs <- terra::crs(data)

    # Convert to CRS used in this workflow if not already in that CRS, create
    # bounding box polygon with generous buffer to ensure data isn't missed.
    if (!(orig_crs == terra::crs("ESRI:102001"))) {
      study_area <- sf::st_bbox(data) %>%
        sf::st_as_sfc() %>%
        sf::st_transform("ESRI:102001") %>%
        sf::st_buffer(20000) %>% # Arbitrarily high number selected (20km).
        # Maybe unnecessary, could reduce download size.
        terra::vect()
    } else {
      study_area <- sf::st_bbox(data) %>%
        sf::st_as_sfc() %>%
        sf::st_buffer(20000) %>% # Arbitrarily high number selected (20km).
        # Maybe unnecessary, could reduce download size.
        terra::vect()
    }
  }

  # Create area of interest polygon from provided terra object.
  if (input_fmt$type == "terra") {
    # Store original CRS so data can be returned as provided.
    orig_crs <- terra::crs(data)

    # Convert to CRS used in this workflow if not already in that CRS, create
    # bounding box polygon with generous buffer to ensure data isn't missed.
    if (!(orig_crs == terra::crs("ESRI:102001"))) {
      study_area <- terra::ext(data) %>%
        terra::vect(crs = orig_crs) %>%
        terra::project("ESRI:102001") %>%
        terra::buffer(20000) # Arbitrarily high number selected (20km).
      # Maybe unnecessary, could reduce download size.
    } else {
      study_area <- terra::ext(data) %>%
        terra::vect(crs = orig_crs) %>%
        terra::buffer(20000) # Arbitrarily high number selected (20km).
      # Maybe unnecessary, could reduce download size.
    }

    # Convert to sf object for use in workflow.
    data <- sf::st_as_sf(data) # Maybe down the line write full process out
    # in terra for terra data?
  }

  # Create download path if it doesn't already exist.
  if (is.null(dl_path) & !dir.exists("./modis/MCD12Q1")) {
    dir.create("./modis/MCD12Q1", recursive = TRUE)
  }

  if (!is.null(dl_path) & !dir.exists(paste0(dl_path, "/modis/MCD12Q1"))) {
    dir.create(paste0(dl_path, "/modis/MCD12Q1"), recursive = TRUE)
  }

  if (ed_transfer == TRUE) {
    message("[MODIS Landcover Download] downloading data.")
  } else {
    message("[MODIS Landcover Download] fetching data filenames.")
  }

  # Call to API using luna::getNASA()

  if (ed_transfer == FALSE) {
    modis_files <- luna::getNASA(
      product = "MCD12Q1",
      start = paste0(min(data$survey_year), "-01-01"), # Starting year
      end = paste0(max(data$survey_year), "-12-31"), # End year
      aoi = terra::ext(terra::project(study_area, "epsg:4326")),
      download = FALSE
    )
  } else {
    modis_files <- luna::getNASA(
      product = "MCD12Q1",
      start = paste0(min(data$survey_year), "-01-01"), # Starting year
      end = paste0(max(data$survey_year), "-12-31"), # End year
      aoi = terra::ext(terra::project(study_area, "epsg:4326")),
      download = TRUE,
      overwrite = FALSE,
      path = ifelse(
        is.null(dl_path),
        "./modis/MCD12Q1",
        paste0(dl_path, "/modis/MCD12Q1")
      ),
      username = ed_email,
      password = ed_password
    )
  }

  # Return character vector of filepaths to downloaded files.
  return(modis_files)
}
