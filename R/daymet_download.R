#' Download Data from Daymet.
#'
#' Downloads all available variables from the [Daymet](https://daymet.ornl.gov/)
#' from request data either fetched via [daymet_request()] or supplied by the
#' user. All variables are available at a daily resolution since 1980 in
#' North America and Hawaii, and since 1950 in Puerto
#' Rico, and at a ~ 1 km spatial resolution. This data is retreived via the NASA
#' AppEEARS service, requiring an EarthData account to be made. This can be
#' done at the following link: [register for an EarthData
#' account](https://urs.earthdata.nasa.gov/users/new). Users should be aware
#' that since these data are at a daily resolution a large number of files will
#' be downloaded for datasets with many dates.
#'
#' One (or multiple) Daymet variable(s) can be downloaded by specifying the following
#' values to the `covariates` argument in [daymet_request()]:
#' - Day length (s/day): `daymet_dayl`
#' - Precipitation (mm/day): `daymet_prcp`
#' - Shortwave radiation (W/m^2): `dayment_srad`
#' - Snow water equivalent (kg/m^2): `daymet_swe`
#' - Maximum air temperature (°C): `daymet_tmax`
#' - Minimum air temperature (°C): `daymet_tmin`
#' - Water vapor pressure (Pa): `daymet_vp`
#'
#' Downloads are facilitated by a call to [appeears::rs_transfer()].
#'
#' @param daymet_reqs `data.frame`. A `data.frame` with columns 1)
#'   `request_name` containing AppEEARS request names, 2) `request_id`
#'   containing AppEEARS request IDs, and optionally 3) `date` containing the
#'   date for which the associated request is downloading data for, or a
#'   filepath to a `.rds` file containing such data. The direct output of
#'   [daymet_request()] can be supplied here.
#' @param ed_username Character. The username associated with your EarthData account.
#' @param dl_path Character. Optional argument to provide path to download data
#'   to. By default, data is downloaded to a subfolder `scanfi/` in the working
#'   directory.
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
#'                             covariates = "daymet_prcp",
#'                             ed_username = ed_username)
#' # Once email is received confirming that request has been processed, execute
#' # download!
#' downloaded <- daymet_download(daymet_reqs = requests
#'                               covariates = "daymet_prcp",
#'                               ed_username = ed_username)
#'
#'
#' @seealso [daymet_request()] which can be used to submit requests for Dayment
#' data. [daymet_extract()] which can be used to extract data from downloaded
#' Daymet files.
#'
#' @export

# Function to download data from Daymet. Wrapper for appeears::request_rs().
daymet_download <- function(
  daymet_reqs,
  ed_username, # users' EarthData account username NOT EMAIL.
  dl_path = NULL, # optional argument to provide path to download data to. By
  # default, data is downloaded to a subfolder 'worldclim/' in the working
  # directory.
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
      "[Daymet Download] Daymet data requested but Earthdata system login",
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
          ed_email,
          "'."
        )
      )

      if (is.null(ed_password)) {
        stop(
          "[Daymet Download] EarthData password could not be found",
          ". Please add line 'EarthData_password = yourpassword' to your",
          " .Renviron file. This can be accessed using usethis::edit_r_environ().",
          call. = FALSE
        )
      }
    } else {
      ed_password <- parent.frame()$ed_password
    }
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

  # Check whether daymet_reqs is a filepath to a .RDS file.
  if (inherits(daymet_reqs, "character")) {
    if (all(file.exists(daymet_reqs))) {
      daymet_reqs <- readRDS(daymet_reqs)
    } else {
      stop(
        "[Daymet Download] daymet_reqs in an unexpected format. Please",
        " provide either a data.frame with a column for the AppEEARS",
        " request name called request_name and a column for the AppEEARS",
        " request ID called request_id, or a filepath to a .rds file",
        " created by daymet_request() containing such data.",
        call. = FALSE
      )
    }
  }

  if (!inherits(daymet_reqs, "data.frame")) {
    stop(
      "[Daymet Download] daymet_reqs in an unexpected format. Please",
      " provide either a data.frame with a column for the AppEEARS",
      " request name called request_name and a column for the AppEEARS",
      " request ID called request_id, or a filepath to a .rds file",
      " created by daymet_request() containing such data.",
      call. = FALSE
    )
  }

  if (!(all(c("request_name", "request_id") %in% names(daymet_reqs)))) {
    stop(
      "[Daymet Download] daymet_reqs in an unexpected format. Please",
      " provide either a data.frame with a column for the AppEEARS",
      " request name called request_name and a column for the AppEEARS",
      " request ID called request_id, or a filepath to a .rds file",
      " created by daymet_request() containing such data.",
      call. = FALSE
    )
  }

  # Check that daymet_reqs contains valid request IDs within the user's AppEEARS
  # task ids
  task_list <- appeears::rs_list_task(user = ed_username)

  if (
    FALSE %in%
      (daymet_reqs$request_id %in%
        task_list$task_id[
          lubridate::as_datetime(task_list$expires_on) >
            lubridate::as_datetime(Sys.time())
        ])
  ) {
    stop(
      "[Daymet Download] request ID(s) ",
      stringr::str_flatten_comma(daymet_reqs$request_name[
        daymet_reqs$request_id %in%
          task_list$task_id[
            lubridate::as_datetime(task_list$expires_on) >
              lubridate::as_datetime(Sys.time())
          ] ==
          FALSE
      ]),
      " provided in daymet_reqs are not",
      " registered under EarthData user ",
      ed_username,
      ". Are they more than a month old (i.e., expired), or entered incorrectly?",
      call. = FALSE
    )
  }

  # Check status of requests
  incomplete_tasks <- c()

  for (i in 1:nrow(daymet_reqs)) {
    status <- task_list$status[task_list$task_id == daymet_reqs$request_id[i]]

    if (status %in% c("pending", "queued", "processing")) {
      incomplete_tasks <- c(incomplete_tasks, daymet_reqs$request_name[i])
    }
  }

  if (length(incomplete_tasks) > 0) {
    # End AppEEARS session.
    if (verbose == FALSE) {
      suppressMessages(appeears::rs_logout(token))
    } else {
      appeears::rs_logout(token)
    }

    stop(
      "[Daymet Download] some supplied Daymet requests are incomplete.",
      " Please wait for confirmation at the email address associated with",
      " your EarthData account '",
      ed_username,
      "' or use daymet_check() to confirm that requests with the following",
      " request IDs are complete: ",
      stringr::str_flatten_comma(incomplete_tasks),
      ".",
      call. = FALSE
    )
  }

  # Create download path if it doesn't already exist.
  if (is.null(dl_path) & !dir.exists("./daymet")) {
    dir.create("./daymet", recursive = TRUE)
  }

  if (!is.null(dl_path) & !dir.exists(paste0(dl_path, "/daymet"))) {
    dir.create(paste0(dl_path, "/daymet"), recursive = TRUE)
  }

  # Loop through each year and download respective request if not already
  # downloaded.
  for (i in 1:nrow(daymet_reqs)) {
    if (
      !dir.exists(ifelse(
        is.null(dl_path),
        paste0("./daymet/", daymet_reqs$request_name[i]),
        paste0(dl_path, "/daymet/", daymet_reqs$request_name[i])
      ))
    ) {
      dir.create(ifelse(
        is.null(dl_path),
        paste0("./daymet/", daymet_reqs$request_name[i]),
        paste0(dl_path, "/daymet/", daymet_reqs$request_name[i])
      ))

      if (verbose) {
        message(paste0(
          "[Daymet Download] downloading Daymet data from request ",
          i
        ))
      }

      appeears::rs_transfer(
        task_id = daymet_reqs$request_id[i],
        user = ed_username,
        path = ifelse(
          is.null(dl_path),
          paste0("./daymet/", daymet_reqs$request_name[i]),
          paste0(dl_path, "/daymet/", daymet_reqs$request_name[i])
        ),
        verbose = verbose
      )

      if (verbose) {
        message(paste0(
          "[Daymet Download] Daymet data for request",
          i,
          " downloaded."
        ))
      }
    }
  }

  # End AppEEARS session.
  if (verbose == FALSE) {
    suppressMessages(appeears::rs_logout(token))
  } else {
    appeears::rs_logout(token)
  }

  daymet_reqs$success <- NA

  for (i in daymet_reqs$request_name) {
    path <- ifelse(
      is.null(dl_path),
      paste0("./daymet/", i),
      paste0(dl_path, "/daymet/", i)
    )

    if (file.exists(paste0(path, "/Daymet-004-Statistics.csv"))) {
      daymet_stats <- readr::read_csv(
        paste0(path, "/Daymet-004-Statistics.csv"),
        show_col_types = FALSE
      )

      tifs_exist <- c()

      for (j in unique(daymet_stats$Dataset)) {
        for (k in unique(daymet_stats$Date[daymet_stats$Dataset == j])) {
          filename <- gsub(
            pattern = "DAYMET_",
            replacement = "DAYMET.",
            daymet_stats$`File Name`[
              daymet_stats$Date == k &
                daymet_stats$Dataset == j
            ]
          )

          if (file.exists(paste0(path, "/", filename, ".tif"))) {
            tifs_exist <- c(tifs_exist, TRUE)
          } else {
            tifs_exist <- c(tifs_exist, FALSE)
          }
        }
      }

      if (all(tifs_exist)) {
        daymet_reqs$success[daymet_reqs$request_name == i] <- TRUE
      } else {
        daymet_reqs$success[daymet_reqs$request_name == i] <- FALSE
      }
    } else {
      daymet_reqs$success[daymet_reqs$request_name == i] <- FALSE
    }
  }

  # Return request ID object.
  return(daymet_reqs)
}
