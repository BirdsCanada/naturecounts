#' Download MODIS NDVI/EVI Data
#'
#' Downloads [16-day NDVI/EVI data](https://doi.org/10.5067/MODIS/MOD13A1.061)
#' derived from imagery from the MODIS Terra and Aqua satellites at
#' approximately 500 m spatial resolution. This data is retreived via the NASA
#' EarthData Archive, requiring an EarthData account to be made. This can be
#' done at the following link: [register for an EarthData
#' account](https://urs.earthdata.nasa.gov/users/new).
#'
#' Both NDVI and EVI data are downloaded in a single file, and can be accessed
#' specifically by specifying `modis_ndvi` and/or `modis_evi` to the `covariates`
#' argument in a call to `vegetation_extract()`.
#'
#' Downloads are facilitated by a call to [luna::getNASA()].
#'
#' @param data An `sf` 'POINT' or 'POLYGON' object, or `terra` 'points' or
#'   'polygons' object.
#' @param ed_email Character. The email address associated with your EarthData
#'   account.
#' @param ed_transfer Logical. Should data be downloaded from EarthData?
#'   If `FALSE`, a vector containing the names of the files that would be
#'   downloaded is returned.
#' @param site_name Character. Optional argument to provide the name of the
#'   column containing site names if not contained within the BMDE column
#'   `SurveyAreaIdentifier`. Can be left `NULL` and still function properly if
#'   originally specified in a call to [data_fmt()].
#' @param date_year Character. Optional argument to provide the name of the
#'   column containing year data if not contained within the BMDE column
#'   `survey_year`. Can be left `NULL` and still function properly if originally
#'   specified in a call to [data_fmt()].
#' @param date_month Character. Optional argument to provide the name of the
#'   column containing month data if not contained within the BMDE column
#'   `survey_month`. Can be left `NULL` and still function properly if originally
#'   specified in a call to [data_fmt()].
#' @param date_day Character. Optional argument to provide the name of the
#'   column containing day-of-month (i.e., a number from 1 to 31) data if not
#'   contained within the BMDE column `survey_day`. Can be left `NULL` and still
#'   function properly if originally specified in a call to [data_fmt()].
#' @param dl_path Character. Optional argument to provide path to download data
#'   to. By default, data is downloaded to a subfolder `modis/` in the working
#'   directory.
#' @param progress Logical. Should progress bars for downloads be displayed?
#'
#' @returns If `ed_transfer = TRUE`, character vector containing
#'   file-paths to downloaded MODIS landcover files. If `ed_transfer =
#'   FALSE`, character vector containing filenames of MODIS landcover files that
#'   would be downloaded.
#'
#' @examplesIf interactive()
#'
#' # Using the included, test data on black-capped chickadees
#' bcch # look at the data
#'
#' # Grab one year to reduce number of files to download
#' bcch <- dplyr::filter(bcch, survey_year == 2010)
#'
#' # Convert to sf POINT object
#' bcch <- sf::st_as_sf(
#'   bcch,
#'   coords = c("longitude", "latitude"),
#'   crs = 4326
#' )
#'
#' # Get file names that would be downloaded.
#' output <- vegetation_download(
#'   data = bcch,
#'   ed_transfer = FALSE
#' )
#'
#' # Enter EarthData email
#' ed_email <- "your EarthData email"
#'
#' # Download MODIS data
#' #output <- vegetation_download(
#' #  data = bcch,
#' #  ed_email = ed_email
#' #)
#'
#' @seealso [luna::getNASA()] which this function wraps. [vegetation_extract()]
#' which can be used to extract data from downloaded vegetation data files.
#'
#' @export

# Function for downloading MODIS MOD13A1 data from NASA EarthData. Wrapper for
# luna::getNASA().
vegetation_download <- function(
  data,
  ed_email = NULL, # users' EarthData account email address.
  ed_transfer = TRUE, # should data be downloaded from EarthData?
  site_name = NULL, # optional argument to provide column name containing site
  # names. Default is assumed to be the BMDE column 'SurveyAreaIdentifier'. Can
  # be left NULL and still function properly if originally specified in a call
  # to data_fmt().
  date_year = NULL, # optional argument to provide column name containing year
  # data. Default is assumed to be the BMDE column 'survey_year'. Can
  # be left NULL and still function properly if originally specified in a call
  # to data_fmt().
  date_month = NULL, # optional argument to provide column name containing month
  # data. Default is assumed to be the BMDE column 'survey_month'
  date_day = NULL, # optional argument to provide column name containing day
  # data. Default is assumed to be the BMDE column 'survey_day'.
  dl_path = NULL, # optional argument to provide path to download data to. By
  # default, data is downloaded to a subfolder 'modis/' in the working
  # directory.
  progress = TRUE # Should progress bars for downloads be displayed?
) {
  # Check packages
  have_pkg_check(c(
    "sf",
    "terra",
    "luna"
  ))

  # Check that ed_transfer is logical.
  if (!is.logical(ed_transfer)) {
    stop(
      "[MODIS NDVI/EVI Download] ed_transfer must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  # Check that an EarthData account email has been provided. If not, return
  # error.
  if (missing(ed_email) & ed_transfer == TRUE) {
    stop(
      "[MODIS NDVI/EVI Download] MODIS data requested but Earthdata system",
      " login information not supplied. Please register at",
      " https://urs.earthdata.nasa.gov/users/new and supply using `ed_email`",
      " argument.",
      call. = FALSE
    )
  }

  # Check whether user password is stored in .Renviron
  if (ed_transfer == TRUE) {
    ed_password <- Sys.getenv("EarthData_password")

    # If not available in .Renviron, check whether an EarthData password exists
    # in the environment (is specified earlier in the nc_covariates() workflow),
    # and if not, request using askpass::askpass().
    if (ed_password == "") {
      if (is.null(parent.frame()$ed_password)) {
        ed_password <- askpass::askpass(
          prompt = paste0(
            "Please enter password for ",
            "EarthData user '",
            ed_email,
            "'."
          )
        )

        if (is.null(ed_password)) {
          stop(
            "[MODIS NDVI/EVI Download] EarthData password could not be found",
            ". Please add line 'EarthData_password = yourpassword' to your",
            " .Renviron file. This can be accessed using usethis::edit_r_environ().",
            call. = FALSE
          )
        }
      } else {
        ed_password <- parent.frame()$ed_password
      }
    }
  }

  # Check data is in the desired format.
  input_fmt <- covariate_fmt_check(data)

  # If not an sf or terra object, return error and point towards data_fmt().
  if (input_fmt$type == "data.frame") {
    stop(
      "[MODIS NDVI/EVI Download] downloading requires an sf or terra object as input in this workflow. Consider using `data_fmt` to conform data first.",
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

  if (is.null(date_month) & !is.null(attr(data, "date_month"))) {
    date_month <- attr(data, "date_month")
  }

  if (is.null(date_day) & !is.null(attr(data, "date_day"))) {
    date_day <- attr(data, "date_day")
  }

  # Check that all specified column names are present in the data.

  # Gather all potentially specified columns.
  specified_cols <- c(site_name, date_year, date_month, date_day)

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
        !("survey_year" %in% data_cols) |
        !("survey_month" %in% data_cols) |
        !("survey_day" %in% data_cols))
  ) {
    stop(
      "[MODIS NDVI/EVI Download] some specified columns missing from the data: ",
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
  if (!is.null(site_name) & !("SurveyAreaIdentifier" %in% data_cols)) {
    if (input_fmt$type == "sf") {
      data <- sf::st_sf(data)
    }

    data <- dplyr::rename(data, "SurveyAreaIdentifier" = !!site_name)
  }

  data$SurveyAreaIdentifier <- as.character(data$SurveyAreaIdentifier)

  if (!is.null(date_year) & !("survey_year" %in% data_cols)) {
    if (input_fmt$type == "sf") {
      data <- sf::st_sf(data)
    }

    data <- dplyr::rename(data, "survey_year" = !!date_year)
  }

  data$survey_year <- as.numeric(data$survey_year)

  if (!is.null(date_month)) {
    if (input_fmt$type == "sf") {
      data <- sf::st_sf(data)
    }

    data <- dplyr::rename(data, "survey_month" = !!date_month)
  }

  # Use month_check() to validate month data.
  month_corr <- c()

  for (i in 1:length(data$survey_month)) {
    month_corr[i] <- month_check(data$survey_month[i])
  }

  data$survey_month <- month_corr

  if (!is.null(date_day)) {
    if (input_fmt$type == "sf") {
      data <- sf::st_sf(data)
    }

    data <- dplyr::rename(data, "survey_day" = !!date_day)
  }

  # Use dom_check() to validate day data.
  for (i in data$survey_day) {
    dom_check(i)
  }

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
        sf::st_buffer(20000) %>% # # Arbitrarily high number selected (20km).
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
    data <- sf::st_as_sf(data) # Maybe down the line write full process out in
    # terra for terra data.
  }

  # Remove any observations missing year, month, or day data. Warn.
  if (
    TRUE %in%
      is.na(data$survey_year) |
      TRUE %in% is.na(data$survey_month) |
      TRUE %in% is.na(data$survey_day)
  ) {
    warning(
      "[MODIS NDVI/EVI Download] missing date data detected. Complete year,",
      " month, and day data is needed for data download. Observations missing",
      " date data will be dropped.",
      call. = FALSE
    )

    data <- data %>%
      dplyr::filter(
        !is.na(.data$survey_year),
        !is.na(.data$survey_month),
        !is.na(.data$survey_day)
      )
  }

  # Create download path if it doesn't already exist.
  if (is.null(dl_path) & !dir.exists("./modis/MOD13A1")) {
    dir.create("./modis/MOD13A1", recursive = TRUE)
  }

  if (!is.null(dl_path) & !dir.exists(paste0(dl_path, "/modis/MOD13A1"))) {
    dir.create(paste0(dl_path, "/modis/MOD13A1"), recursive = TRUE)
  }

  # In first iteration of loop, fetch number of files to download to warn
  # user as the 16-day resolution of this data can result in large file batches
  # by setting download = FALSE in luna::getNASA().
  for (i in unique(c(FALSE, ed_transfer))) {
    if (i == TRUE) {
      if (length(modis_files > 0)) {
        message("[MODIS NDVI/EVI Download] downloading data.")
      }
    } else if (i == FALSE & ed_transfer == FALSE) {
      message("[MODIS NDVI/EVI Download] fetching data filenames.")
    }

    dates <- sort(unique(paste0(
      data$survey_year,
      "-",
      data$survey_month,
      "-",
      data$survey_day
    )))

    # Open vector to store filenames.
    modis_files <- c()
    warning_dates <- c()

    for (j in dates) {
      if (i == FALSE) {
        tryCatch(
          tmp <- luna::getNASA(
            product = "MOD13A1",
            start = j,
            end = j,
            aoi = terra::project(study_area, "epsg:4326"),
            download = i,
            overwrite = FALSE,
            path = ifelse(
              is.null(dl_path),
              "./modis/MOD13A1",
              paste0(dl_path, "/modis/MOD13A1")
            ),
            verbose = progress
          ),
          warning = function(w) {
            if (
              conditionMessage(w) ==
                "No downloadable granules found for product='MOD13A1'. Did you mean: MOD13A2, MOD13A3, MOD13A4N, MOD13C1, MOD13C2? Use `getProducts('MOD13')` to list related products."
            ) {
              warning_dates <<- unique(c(warning_dates, j))
            } else {
              warning(conditionMessage(w))
            }
          }
        )
      } else {
        tryCatch(
          tmp <- luna::getNASA(
            product = "MOD13A1",
            start = j,
            end = j,
            aoi = terra::project(study_area, "epsg:4326"),
            download = i,
            overwrite = FALSE,
            path = ifelse(
              is.null(dl_path),
              "./modis/MOD13A1",
              paste0(dl_path, "/modis/MOD13A1")
            ),
            username = ed_email,
            password = ed_password,
            verbose = progress
          ),
          warning = function(w) {
            if (
              conditionMessage(w) ==
                "No downloadable granules found for product='MOD13A1'. Did you mean: MOD13A2, MOD13A3, MOD13A4N, MOD13C1, MOD13C2? Use `getProducts('MOD13')` to list related products."
            ) {
              warning_dates <<- unique(c(warning_dates, j))
            } else {
              warning(conditionMessage(w))
            }
          }
        )
      }

      if (!(j %in% warning_dates)) {
        modis_files <- unique(c(modis_files, tmp))
      }
    }

    # Warn of any out of range dates on first iteration only.
    if (i == FALSE & length(warning_dates > 0)) {
      warning(
        "Observation on date(s) ",
        stringr::str_flatten_comma(warning_dates),
        " could not be matched to a MODIS vegetation data file. Are they",
        " outside of the temporal coverage of the data (i.e., before 2000 or in the current year)?",
        call. = FALSE
      )
    }

    # On first iteration (if downloading files) send message about expected
    # number of files to download.
    if (i == FALSE & ed_transfer == TRUE & length(modis_files > 0)) {
      message(paste0(
        "[MODIS NDVI/EVI Download] data products are at a 16 day resolution, resulting in ",
        length(modis_files),
        " files to download for your data. This may take some time."
      ))
    }
  }

  # Return character vector of filepaths to downloaded files (if ed_transfer = TRUE)
  # or character vector of filenames (if ed_transfer = FALSE).
  return(modis_files)
}
