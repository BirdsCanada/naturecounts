#' Check the status of Daymet Data Requests
#'
#' Returns status information on requests submitted through [daymet_request()].
#' Requests are submitted to the NASA AppEEARS service, requiring an EarthData
#' account to be made. This can be done at the following link: [register for an
#' EarthData account](https://urs.earthdata.nasa.gov/users/new).
#'
#' Status checks are facilitated by a call to [appeears::rs_list_task()].
#'
#' @inheritParams daymet_request
#' @param daymet_reqs `data.frame`. A `data.frame` with columns 1)
#'   `request_name` containing AppEEARS request names, 2) `request_id`
#'   containing AppEEARS request IDs, and optionally 3) `date` containing the
#'   date for which the associated request is downloading data for, or a
#'   filepath to a `.rds` file containing such data. The direct output of
#'   [daymet_request()] can be supplied here.
#'
#' @returns A `data.frame` containing request status information.
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
#' # Check status
#' status_check <- daymet_check(daymet_reqs = requests,
#'                              ed_username = ed_username)
#'
#'
#' @seealso [daymet_request()] which can be used to submit requests for Dayment
#' data. [daymet_download()] to execute downloads once requests have been
#' submitted and are complete. [daymet_extract()] which can be used to extract data
#' from downloaded Daymet files.
#'
#' @export

daymet_check <- function(
  daymet_reqs,
  ed_username,
  verbose = TRUE
) {
  have_pkg_check(c(
    "appeears"
  ))

  # Check that an EarthData account username has been provided. If not, return
  # error.
  if (missing(ed_username)) {
    stop(
      "[Daymet Request Checking] Daymet data requested but Earthdata system login",
      " information not supplied. NOTE: checking Daymet data requires your",
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
          "[Daymet Request Checking] EarthData password could not be found",
          ". Please add line 'EarthData_password = yourpassword' to your",
          " .Renviron file. This can be accessed using usethis::edit_r_environ().",
          call. = FALSE
        )
      }
    } else {
      ed_password <- parent.frame()$ed_password
    }
  }

  # Check whether daymet_reqs is a filepath to a .RDS file.
  if (inherits(daymet_reqs, "character")) {
    if (all(file.exists(daymet_reqs))) {
      daymet_reqs <- readRDS(daymet_reqs)
    } else {
      stop(
        "[Daymet Request Checking] daymet_reqs in an unexpected format. Please",
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
      "[Daymet Request Checking] daymet_reqs in an unexpected format. Please",
      " provide either a data.frame with a column for the AppEEARS",
      " request name called request_name and a column for the AppEEARS",
      " request ID called request_id, or a filepath to a .rds file",
      " created by daymet_request() containing such data.",
      call. = FALSE
    )
  }

  if (!(all(c("request_name", "request_id") %in% names(daymet_reqs)))) {
    stop(
      "[Daymet Request Checking] daymet_reqs in an unexpected format. Please",
      " provide either a data.frame with a column for the AppEEARS",
      " request name called request_name and a column for the AppEEARS",
      " request ID called request_id, or a filepath to a .rds file",
      " created by daymet_request() containing such data.",
      call. = FALSE
    )
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

  # Pull AppEEARS task list
  task_list <- appeears::rs_list_task(user = ed_username)

  # Check that daymet_reqs contains valid request IDs within the user's AppEEARS
  # task ids
  if (
    FALSE %in%
      (daymet_reqs$request_id %in%
        task_list$task_id[
          lubridate::as_datetime(task_list$expires_on) >
            lubridate::as_datetime(Sys.time())
        ])
  ) {
    stop(
      "[Daymet Request Checking] request(s) ",
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

  # Filter to those named in daymet_reqs
  task_list <- task_list[task_list$task_name %in% daymet_reqs$request_name, ]

  # Grab key columns and make pretty
  output <- task_list[, c("task_name", "task_id", "status", "expires_on")]

  output$date <- substr(
    output$task_name,
    start = nchar(output$task_name) - 9,
    stop = nchar(output$task_name)
  )

  output <- output[, c("task_name", "task_id", "date", "status", "expires_on")]

  output <- dplyr::arrange(output, .data$date)

  output <- dplyr::rename(
    output,
    "request_name" = "task_name",
    "request_id" = "task_id"
  )

  # End AppEEARS session.
  if (verbose == FALSE) {
    suppressMessages(appeears::rs_logout(token))
  } else {
    appeears::rs_logout(token)
  }

  return(output)
}
