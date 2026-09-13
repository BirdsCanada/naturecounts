#' Submit Request for Daymet Data.
#'
#' Submits requests for all available variables from the
#' [Daymet](https://daymet.ornl.gov/) at the spatial extent of provided input
#' observation data. All variables are available at a daily
#' resolution since 1980 in North America and Hawaii, and since 1950 in Puerto
#' Rico, and at a ~ 1 km spatial resolution. Requests are submitted to the NASA
#' AppEEARS service, requiring an EarthData account to be made. This can be
#' done at the following link: [register for an EarthData
#' account](https://urs.earthdata.nasa.gov/users/new).
#'
#' One (or multiple) Daymet variable(s) can be requested by specifying the following
#' values to the `covariates` argument:
#' - Day length (s/day): `daymet_dayl`
#' - Precipitation (mm/day): `daymet_prcp`
#' - Shortwave radiation (W/m^2): `dayment_srad`
#' - Snow water equivalent (kg/m^2): `daymet_swe`
#' - Maximum air temperature (°C): `daymet_tmax`
#' - Minimum air temperature (°C): `daymet_tmin`
#' - Water vapor pressure (Pa): `daymet_vp`
#'
#' Due to API limitations, one request will be submitted for each day in `data`.
#' Unfortunately, AppEEARS automatically sends an email upon request receipt
#' and completion for each request, so for users with many observation dates,
#' we recommend considering setting rules in their email
#' client for handling these emails (address <appeears-noreply@nasa.gov>).
#'
#' To preserve request information in the event the R session ends, users can
#' choose to set `save = TRUE` and have request information saved externally
#' in a `.rds` file. Users can then provide the path to this file to
#' [daymet_check()], [daymet_download()] or [daymet_extract()] or read it
#' back into the R environment using [base::readRDS()].
#'
#' Once requests are submitted, users can use [daymet_check()] to check the
#' status of their requests. In the author's experience, requests take from
#' 1-24 hrs to process. Once requests are complete, downloads can be
#' executed with [daymet_download()].
#'
#' Requests are facilitated by a call to [appeears::rs_request()].
#'
#' @param data A `data.frame`, `sf` 'POINT' or 'POLYGON' object, or `terra` 'points'
#'   or 'polygons' object containing columns with the year, month, and day an
#'   observation was made either named the BMDE defaults `survey_year`, `survey_month`
#'   , and `survey_day` respectively or another name specified in arguments
#'  `date_year`, `date_month`, and/or `date_day`.
#' @param covariates Character, vector if multiple Daymet data types desired. By
#'   default, downloads Daymet precipitation data.
#' @param ed_username Character. The username associated with your EarthData account.
#' @param request_name Character. Optional argument to provide informative name
#'   for the AppEEARS request. This can make file management more intuitive for
#'   the user.
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
#'   `survey_year`.
#' @param dl_path Character. Optional argument to provide path to save request
#'   information to. By default, data is downloaded to a subfolder `daymet/` in
#'   the working directory.
#' @param save Logical. Should Daymet request ID information be saved externally
#'   in a .rds file?
#' @param verbose Logical. Should messages be displayed?
#'
#' @returns A `data.frame` with three columns: 1) `request_name` containing
#'   AppEEARS request names, 2) `request_id` containing AppEEARS request IDs,
#'   and 3) `date` containing the date for which the associated request is
#'   downloading data for.
#'
#' @examplesIf interactive()
#' # Convert included test data on black-capped chickadees to sf POINT object
#' bcch <- sf::st_as_sf(
#'   bcch,
#'   coords = c("longitude", "latitude"),
#'   crs = 4326
#' )
#'
#' # Grab data from a single year
#' bcch <- bcch[bcch$survey_year == 2011,]
#'
#' # Enter EarthData username
#' ed_username <- "your EarthData username"
#'
#' # Submit Daymet requests
#' requests <- daymet_request(data = bcch,
#'                            covariates = "daymet_prcp",
#'                            ed_username = ed_username)
#'
#'
#' @seealso [daymet_download()] to execute downloads once requests have been
#' submitted and are complete.
#'
#' [daymet_check()] to check the status of existing
#' requests.
#'
#' [daymet_extract()] which can be used to extract data
#' from downloaded Daymet files.
#'
#' @export

# Function to request data from Daymet. Wrapper for appeears::request_rs().
daymet_request <- function(
  data,
  covariates = "daymet_prcp", # Other options listed in nc_covariate_table().
  ed_username, # users' EarthData account username NOT EMAIL.
  request_name = NULL,
  site_name = NULL, # optional argument to provide column name containing site
  # names. Default is assumed to be the BMDE column 'SurveyAreaIdentifier'. Can
  # be left NULL and still function properly if originally specified in a call
  # to data_fmt().
  date_year = NULL, # optional argument to provide column name containing year
  # data. Default is assumed to be the BMDE column 'survey_year'. Can
  # be left NULL and still function properly if originally specified in a call
  # to data_fmt().
  date_month = NULL, # optional argument to provide column name containing month
  # data. Default is assumed to be the BMDE column 'survey_month'. Can
  # be left NULL and still function properly if originally specified in a call
  # to data_fmt().
  date_day = NULL, # optional argument to provide column name containing day
  # data. Default is assumed to be the BMDE column 'survey_day'. Can
  # be left NULL and still function properly if originally specified in a call
  # to data_fmt().
  dl_path = NULL, # optional argument to provide path to save request information
  # to. By  default, data is downloaded to a subfolder 'daymet/' in the working
  # directory.
  save = FALSE,
  verbose = TRUE
) {
  # Check packages
  have_pkg_check(c(
    "sf",
    "terra",
    "appeears"
  ))

  # Check that an EarthData account username has been provided. If not, return
  # error.
  if (missing(ed_username)) {
    stop(
      "[Daymet Request] Daymet data requested but Earthdata system login",
      " information not supplied. NOTE: downloading Daymet data requires your",
      " EarthData username, not email. Please register at",
      " https://urs.earthdata.nasa.gov/users/new and supply using",
      " the `ed_username` parameter.",
      call. = FALSE
    )
  }

  # Check whether user password is stored in .Renviron
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
          ed_username,
          "'."
        )
      )

      if (is.null(ed_password)) {
        stop(
          "[Daymet Request] EarthData password could not be found",
          ". Please add line 'EarthData_password = yourpassword' to your",
          " .Renviron file. This can be accessed using usethis::edit_r_environ().",
          call. = FALSE
        )
      }
    } else {
      ed_password <- parent.frame()$ed_password
    }
  }

  # Check data is in the desired format.
  input_fmt <- covariate_fmt_check(data)

  # If not an sf or terra object, return error and point towards data_fmt().
  if (input_fmt$type == "data.frame") {
    stop(
      "[Daymet Request] downloading requires an sf or terra object as input",
      " in this workflow. Consider using `data_fmt` to conform data first.",
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
      "[Daymet Request] some specified columns missing from the data: ",
      stringr::str_flatten_comma(specified_cols[
        !(specified_cols %in% data_cols)
      ]),
      ". Use arguments to specify alternate column names if using data that diverges from naturecounts default column names.",
      call. = FALSE
    )
  }

  # Create SurveyAreaIdentifiers if none exist and no site name specified.
  if (is.null(site_name) & !("SurveyAreaIdentifier" %in% data_cols)) {
    data <- create_SAI(data = data, input_fmt = input_fmt)
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

  # Check that SurveyAreaIdentifier does not contain NAs. Create dummy
  # SurveyAreaIdentifiers if so.
  if (TRUE %in% is.na(data$SurveyAreaIdentifier)) {
    # Store original SurveyAreaIdentifiers
    SAI_storage <- data$SurveyAreaIdentifier

    # Create dummy SurveyAreaIdentifiers
    data <- create_SAI(data = data, input_fmt = input_fmt)
  }

  if (!is.null(date_year) & !("survey_year" %in% data_cols)) {
    if (input_fmt$type == "sf") {
      data <- sf::st_sf(data)
    }

    data <- dplyr::rename(data, "survey_year" = !!date_year)
  }

  data$survey_year <- as.numeric(data$survey_year)

  if (!is.null(date_month) & !("survey_month" %in% data_cols)) {
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

  data$survey_month <- as.numeric(data$survey_month)

  if (!is.null(date_day) & !("survey_day" %in% data_cols)) {
    if (input_fmt$type == "sf") {
      data <- sf::st_sf(data)
    }

    data <- dplyr::rename(data, "survey_day" = !!date_day)
  }

  # Use dom_check() to validate day data.
  for (i in data$survey_day) {
    dom_check(i)
  }

  data$survey_day <- as.numeric(data$survey_day)

  # Create area of interest polygon from provided sf object.
  if (input_fmt$type == "sf") {
    # Check whether sf object is buffered or not to determine extraction
    # procedure down the line.
    buffered <- ifelse(input_fmt$geometry == "POINT", FALSE, TRUE)

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
    # Check whether terra object is buffered or not to determine extraction
    # procedure down the line.
    buffered <- ifelse(input_fmt$geometry == "points", FALSE, TRUE)

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
    data <- sf::st_as_sf(data)
  }

  # Create download path if it doesn't already exist.
  if (is.null(dl_path) & !dir.exists("./daymet")) {
    dir.create("./daymet", recursive = TRUE)
  }

  if (!is.null(dl_path) & !dir.exists(paste0(dl_path, "/daymet"))) {
    dir.create(paste0(dl_path, "/daymet"), recursive = TRUE)
  }

  # Set EarthData username and password in user Keyring.
  options(keyring_backend = "file")

  if (verbose == FALSE) {
    suppressMessages(appeears::rs_set_key(
      user = ed_username,
      password = ed_password
    ))
  } else {
    appeears::rs_set_key(user = ed_username, password = ed_password)
  }

  # Autheniticate with AppEEARS.
  token <- appeears::rs_login(user = ed_username)

  # Create index from each requested Daymet covariate.
  daymet_vars <- gsub(
    pattern = "daymet_",
    replacement = "",
    grep("daymet_", covariates, value = TRUE)
  )

  # Build a request for each surveyed year to be submitted to AppEEARS. This
  # request will download data for every day in the input data.

  call_date <- gsub(
    pattern = " ",
    replacement = "_",
    gsub(
      pattern = "\\..*",
      replacement = "",
      gsub(pattern = ":", replacement = "", as.character(Sys.time()))
    )
  )

  dates <- sort(unique(paste0(
    data$survey_year,
    "-",
    ifelse(
      nchar(data$survey_month) == 1,
      paste0(0, data$survey_month),
      data$survey_month
    ),
    "-",
    ifelse(
      nchar(data$survey_day) == 1,
      paste0(0, data$survey_day),
      data$survey_day
    )
  )))

  tasks <- list()

  for (i in dates) {
    tasks[[i]] <- data.frame(
      task = ifelse(
        is.null(request_name),
        paste0("naturecounts_rq_", call_date, "_", i),
        paste0(request_name, "_", call_date, "_", i)
      ),
      subtask = "subtask",
      latitude = mean(sf::st_coordinates(data %>% sf::st_transform(4326))[,
        "Y"
      ]),
      longitude = mean(sf::st_coordinates(data %>% sf::st_transform(4326))[,
        "X"
      ]),
      start = i,
      end = i,
      product = "DAYMET.004",
      layer = daymet_vars
    )
  }

  # Final build and submission.
  if (verbose) {
    for (i in dates) {
      task <- appeears::rs_build_task(
        df = tasks[[i]],
        roi = sf::st_as_sf(study_area),
        format = "geotiff"
      )

      appeears::rs_request(
        request = task,
        user = ed_username,
        transfer = FALSE,
        verbose = verbose
      )
    }
  } else {
    for (i in dates) {
      task <- appeears::rs_build_task(
        df = tasks[[i]],
        roi = sf::st_as_sf(study_area),
        format = "geotiff"
      )

      suppressMessages(appeears::rs_request(
        request = task,
        user = ed_username,
        transfer = FALSE,
        verbose = verbose
      ))
    }
  }

  # Open vector to store request IDs.
  task_ids <- c()

  # Grab request IDs.
  tasklist <- appeears::rs_list_task(user = ed_username)

  for (i in dates) {
    task_ids <- c(
      task_ids,
      tasklist$task_id[
        tasklist$task_name ==
          ifelse(
            is.null(request_name),
            paste0("naturecounts_rq_", call_date, "_", i),
            paste0(request_name, "_", call_date, "_", i)
          )
      ]
    )
  }

  task_ids <- data.frame(
    request_name = `if`(
      is.null(request_name),
      paste0("naturecounts_rq_", call_date, "_", dates),
      paste0(request_name, "_", call_date, "_", dates)
    ),
    request_id = task_ids,
    date = dates
  )

  # Save externally in case user ends R session.
  if (save) {
    path <- ifelse(
      is.null(dl_path),
      "./daymet/",
      paste0(
        dl_path,
        "/daymet/"
      )
    )

    filename <- paste0(
      path,
      ifelse(
        is.null(request_name),
        paste0("daymet_reqs_", call_date, ".RDS"),
        paste0(request_name, ".RDS")
      )
    )

    saveRDS(
      task_ids,
      file = filename
    )
  }

  # End AppEEARS session.
  if (verbose == FALSE) {
    suppressMessages(appeears::rs_logout(token))
  } else {
    appeears::rs_logout(token)
  }

  # Send detailed message instructing user on next steps.
  if (verbose) {
    if (save) {
      message(
        "[Daymet Request] requests have been placed with AppEEARS for the data",
        " you've requested. Look to your email for confirmation that these have",
        " been completed. We have saved the request data in an external object",
        " at ",
        filename,
        " and as the output of this daymet_request() call. Please",
        " run daymet_download() to execute downloads once you have received",
        " confirmation that these requests are approved at your EarthData email."
      )
    } else {
      message(
        "[Daymet Request] requests have been placed with AppEEARS for the data",
        " you've requested. Look to your email for confirmation that these have",
        " been completed. We have saved the request data as the output of this",
        " daymet_request() call. Please run daymet_download() to execute",
        " downloads once you have received confirmation that these requests",
        " are approved at your EarthData email."
      )
    }
  }
  return(task_ids)
}
