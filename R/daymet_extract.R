#' Extract Data from Daymet.
#'
#' Extracts all available variables from [Daymet](https://daymet.ornl.gov/) and
#' matches them to input observation data. All variables are available at a daily
#' resolution since 1980 in North America and Hawaii, and since 1950 in Puerto
#' Rico, and at a ~ 1 km spatial resolution. This data can be requested from the
#' NASA AppEEARS service using [daymet_request()] and downloaded using
#' [daymet_download()].
#'
#' One (or multiple) Daymet variable(s) can be extracted by specifying the following
#' values to the `covariates` argument. The appropriate variables must be available
#' in the AppEEARS request supplied to `daymet_reqs`. Requests can be submitted
#' via [daymet_request()] and downloaded via [daymet_download()]:
#' - Day length (s/day): `daymet_dayl`
#' - Precipitation (mm/day): `daymet_prcp`
#' - Shortwave radiation (W/m^2): `dayment_srad`
#' - Snow water equivalent (kg/m^2): `daymet_swe`
#' - Maximum air temperature (°C): `daymet_tmax`
#' - Minimum air temperature (°C): `daymet_tmin`
#' - Water vapor pressure (Pa): `daymet_vp`
#'
#' By default, for `sf` 'POLYGON' or `terra` 'polygons' input data the mean Daymet
#' variable value will be returned. Other summary statistics can be extracted
#' by specifying the `fun` argument, which is passed to
#' [exactextractr::exact_extract()]. The available summary statistics are:
#'
#'  * `min` - the minimum non-`NA` value in any raster cell wholly or
#'            partially covered by the polygon
#'  * `max` - the maximum non-`NA` value in any raster cell wholly or
#'            partially covered by the polygon
#'  * `count` - the sum of fractions of raster cells with non-`NA`
#'              values covered by the polygon
#'  * `sum`   - the sum of non-`NA` raster cell values, multiplied by
#'              the fraction of the cell that is covered by the polygon
#'  * `mean` - the mean cell value, weighted by the fraction of each cell
#'             that is covered by the polygon
#'  * `median` - the median cell value, weighted by the fraction of each cell
#'               that is covered by the polygon
#'  * `quantile` - arbitrary quantile(s) of cell values, specified in
#'                 `quantiles`, weighted by the fraction of each cell that is
#'                  covered by the polygon
#'  * `mode` - the most common cell value, weighted by the fraction of
#'             each cell that is covered by the polygon. Where multiple
#'             values occupy the same maximum number of weighted cells,
#'             the largest value will be returned.
#'  * `majority` - synonym for `mode`
#'  * `minority` - the least common cell value, weighted by the fraction
#'                 of each cell that is covered by the polygon. Where
#'                 multiple values occupy the same minimum number of
#'                 weighted cells, the smallest value will be returned.
#'  * `variety` - the number of distinct values in cells that are wholly or
#'                partially covered by the polygon.
#'  * `variance` - the population variance of cell values, weighted by the
#'                 fraction of each cell that is covered by the polygon.
#'  * `stdev` - the population standard deviation of cell values, weighted by
#'              the fraction of each cell that is covered by the polygon.
#'  * `coefficient_of_variation` - the population coefficient of variation of
#'                                 cell values, weighted by the fraction of each
#'                                 cell that is covered by the polygon.
#'  * `weighted_mean` - the mean cell value, weighted by the product of
#'                      the fraction of each cell covered by the polygon
#'                      and the value of a second weighting raster provided
#'                      as `weights`
#'  * `weighted_sum` - the sum of defined raster cell values, multiplied by
#'                     the fraction of each cell that is covered by the polygon
#'                     and the value of a second weighting raster provided
#'                     as `weights`
#'  * `weighted_stdev` - the population standard deviation of cell values,
#'                       weighted by the product of the fraction of each cell
#'                       covered by the polygon and the value of a second
#'                       weighting raster provided as `weights`
#'  * `weighted_variance` - the population variance of cell values, weighted by
#'                          the product of the fraction of each cell covered by
#'                          the polygon and the value of a second weighting
#'                          raster provided as `weights`
#'  * `frac` - returns one column for each possible value of `x`, with the
#'             the fraction of defined raster cells that are equal to that
#'             value.
#'  * `weighted_frac` - returns one column for each possible value of `x`,
#'                      with the fraction of defined cells that are equal
#'                      to that value, weighted by `weights.
#'
#' User defined functions can also be passed to `fun`, but these must return a
#' single value. More information can be found in the documentation for
#' [exactextractr::exact_extract()].
#'
#' @param data A `data.frame`, `sf` 'POINT' or 'POLYGON' object, or `terra` 'points'
#'   or 'polygons' object containing columns with the year, month, and day an
#'   observation was made either named the BMDE defaults `survey_year`, `survey_month`
#'   , and `survey_day` respectively or another name specified in arguments
#'  `date_year`, `date_month`, and/or `date_day`.
#' @param daymet_reqs `data.frame`. A `data.frame` with columns 1)
#'   `request_name` containing AppEEARS request names, 2) `request_id`
#'   containing AppEEARS request IDs, and optionally 3) `date` containing the
#'   date for which the associated request is downloading data for, or a
#'   filepath to a `.rds` file containing such data. The direct output of
#'   [daymet_request()] can be supplied here.
#' @param covariates Character, vector if multiple Daymet data types desired. By
#'   default, extracts Daymet precipitation data.
#' @param site_name Character. Optional argument to provide the name of the
#'   column containing site names if not contained within the BMDE column
#'   `SurveyAreaIdentifier`. Can be left `NULL` and still function properly if
#'   originally specified in a call to [data_fmt()] or [daymet_download()].
#' @param date_year Character. Optional argument to provide the name of the
#'   column containing year data if not contained within the BMDE column
#'   `survey_year`. Can be left `NULL` and still function properly if originally
#'   specified in a call to [data_fmt()] or [daymet_download()].
#' @param date_month Character. Optional argument to provide the name of the
#'   column containing month data if not contained within the BMDE column
#'   `survey_month`. Can be left `NULL` and still function properly if originally
#'   specified in a call to [data_fmt()] or [daymet_download()].
#' @param date_day Character. Optional argument to provide the name of the
#'   column containing day-of-month (i.e., a number from 1 to 31) data if not
#'   contained within the BMDE column `survey_day`. Can be left `NULL` and still
#'   function properly if originally specified in a call to [data_fmt()]  or
#'   [daymet_download()].
#' @param dl_path Character. Optional argument to provide path to downloaded data.
#'   By default, data is downloaded to a subfolder `daymet/` in the working directory.
#' @param verbose Logical. Should messages be displayed?
#' @param retain Logical. Should Daymet data files be kept after extraction? If
#'   `FALSE`, files will be deleted.
#' @param ... Other arguments passed to [terra::extract()] for
#'   `sf` 'POINT' or `terra` 'points' input data or
#'   [exactextractr::exact_extract()] `sf` 'POLYGON' or `terra` 'polygons' input
#'   data. Primarily useful for specifying alternate summary statistics to
#'   extract for `sf` 'POLYGON' or `terra` 'polygons' input data.
#'
#' @returns For sf 'POINT' or terra 'points' input data, original data with
#'  column(s) appended containing the Daymet data value(s) at each point.
#'
#'  For sf 'POLYGON' or terra 'polygons' input data, original data with column(s)
#'   appended containing the requested Daymet data value(s) within each polygon.
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
#' requests <- daymet_download(data = bcch,
#'                             covariates = "daymet_prcp",
#'                             ed_username = ed_username)
#'
#' # Once email is received confirming that request has been processed, execute
#' # download!
#' downloaded <- daymet_download(daymet_reqs = requests,
#'                               covariates = "daymet_prcp",
#'                               ed_username = ed_username)
#'
#' # Once download is complete, extract!
#' extracted <- daymet_extract(data = bcch,
#'                             daymet_reqs = requests,
#'                             covariates = "daymet_prcp")
#'
#'
#' @seealso [daymet_request()] which can be used to submit requests for Daymet
#' data.
#'
#' [daymet_download()] to execute downloads once requests have been
#' submitted and are complete.
#'
#' [nc_covariates_merge()] to merge extracted
#' covariate data into data originally provided to the `data` argument of
#' [data_fmt()].
#'
#' @export

daymet_extract <- function(
  data,
  daymet_reqs, # Named list. Each list element should be named after
  # a year for which data was requested, and should contain the corresponding
  # request ID.
  covariates = "daymet_prcp", # Options listed in nc_covariate_table().
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
  dl_path = NULL, # optional argument to provide path to download data to. By
  # default, data is downloaded to a subfolder 'daymet/' in the working
  # directory.
  verbose = TRUE,
  retain = TRUE,
  ...
) {
  # Check packages
  have_pkg_check(c(
    "sf",
    "readr",
    "terra"
  ))

  # Check data is in the desired format.
  input_fmt <- covariate_fmt_check(data)

  # If not an sf or terra object, return error and point towards data_fmt().
  if (input_fmt$type == "data.frame") {
    stop(
      "[Daymet Extraction] extraction requires an sf or terra object as input",
      " in this workflow. Consider using `data_fmt` to conform data first.",
      call. = FALSE
    )
  }

  # Check that DAYMET request information is supplied.
  if (missing(daymet_reqs)) {
    stop(
      "[Daymet Extraction] no Daymet request details are provided to extract from.",
      " Please provide either a data.frame with a column for the AppEEARS",
      " request name called request_name and a column for the AppEEARS",
      " request ID called request_id, or a filepath to a .rds file",
      " created by daymet_request() containing such data.",
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

  # Store attributes so they don't get lost.

  # List potential attributes.
  attr_names <- c(
    "site_name",
    "coord_lon",
    "coord_lat",
    "date_year",
    "date_month",
    "date_day",
    "date_ordinal",
    "date_lubridate",
    "crs"
  )

  # If any potential attribute names are present in the data attributes,
  # store.
  if (length(attr_names[attr_names %in% names(attributes(data))]) > 0) {
    attrs <- attributes(data)[attr_names[
      attr_names %in% names(attributes(data))
    ]]
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
      "[Daymet Extraction] some specified columns missing from the data: ",
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

  if (!is.null(date_month) & !("survey_month" %in% data_cols)) {
    if (input_fmt$type == "sf") {
      data <- sf::st_sf(data)
    }

    data <- dplyr::rename(data, "survey_month" = !!date_month)
  }

  # Validate month data using month_check()
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

  # Validate day data using dom_check()
  for (i in data$survey_day) {
    dom_check(i)
  }

  data$survey_day <- as.numeric(data$survey_day)

  # Check whether sf object is buffered or not to determine extraction
  # procedure down the line.
  if (input_fmt$type == "sf") {
    buffered <- ifelse(input_fmt$geometry == "POINT", FALSE, TRUE)
  }

  # Check whether terra object is buffered or not to determine extraction
  # procedure down the line.
  if (input_fmt$type == "terra") {
    buffered <- ifelse(input_fmt$geometry == "points", FALSE, TRUE)

    # Convert to sf object for use in workflow.
    data <- sf::st_as_sf(data)
  }

  # If buffered, check for packages necessary in buffered workflow.
  if (buffered == TRUE) {
    have_pkg_check("exactextractr")

    if (hasArg("fun") & !is.function(list(...)[["fun"]])) {
      if ("quantile" %in% list(...)[["fun"]] & !hasArg("quantiles")) {
        stop(
          "[Daymet Extraction] quantile summary requested but",
          " no quantiles supplied to the 'quantiles' argument. Please",
          " supply numeric value(s) of desired quantiles.",
          call. = FALSE
        )
      }

      if (
        TRUE %in%
          (c(
            "weighted_mean",
            "weighted_sum",
            "weighted_stdev",
            "weighted_variance",
            "weighted_frac"
          ) %in%
            list(...)[["fun"]]) &
          !hasArg("weights")
      ) {
        stop(
          "[Daymet Extraction] weighted summary requested but no",
          " weights supplied via the 'weights' argument. Please supply",
          " either a weighting raster or 'area' to use the cell areas of",
          " the Daymet raster as weights.",
          call. = FALSE
        )
      }
    }
  }

  # Create index using requested covariates.
  daymet_vars <- gsub(
    pattern = "daymet_",
    replacement = "",
    grep("daymet_", covariates, value = TRUE)
  )

  appeears <- daymet_reqs

  # Open list to store information file that comes with downloaded Daymet data.
  daymet_stats <- list()

  # Open vector to store date data.
  all_dates <- c()

  # Loop through each year and check dates data is available for. This
  # information is sourced from the DAYMET-004-Statistics.csv file that comes
  # with downloads. If this file can't be found, return error.
  for (i in appeears$request_name) {
    if (
      file.exists(ifelse(
        is.null(dl_path),
        paste0(
          "./daymet/",
          i,
          "/DAYMET-004-Statistics.csv"
        ),
        paste0(
          dl_path,
          "/daymet/",
          i,
          "/DAYMET-004-Statistics.csv"
        )
      ))
    ) {
      daymet_stats[[i]] <- readr::read_csv(
        ifelse(
          is.null(dl_path),
          paste0(
            "./daymet/",
            i,
            "/DAYMET-004-Statistics.csv"
          ),
          paste0(
            dl_path,
            "/daymet/",
            i,
            "/DAYMET-004-Statistics.csv"
          )
        ),
        show_col_types = FALSE
      )

      all_dates <- c(all_dates, unique(daymet_stats[[i]]$Date))
    } else {
      stop(
        "[Daymet Extraction] cannot find ",
        ifelse(
          is.null(dl_path),
          paste0(
            "./daymet/",
            i,
            "/DAYMET-004-Statistics.csv"
          ),
          paste0(
            dl_path,
            "/daymet/",
            i,
            "/DAYMET-004-Statistics.csv"
          )
        ),
        ". Please provide this file along with all downloaded rasters in",
        " folders for each request_name in daymet_reqs under a folder named",
        "'daymet' in your working",
        " directory (default) or under the path specified using the dl_path",
        " argument.",
        call. = FALSE
      )
    }
  }

  # Convert to date objects
  all_dates <- as.Date(all_dates)

  # Create comparable date objects in original data.
  data$date <- as.Date(paste0(
    data$survey_year,
    "-",
    data$survey_month,
    "-",
    data$survey_day
  ))

  # Note any dates that do not have available Daymet data. Warn.
  missing_dates <- sort(data$date[!(data$date %in% all_dates)])

  if (length(missing_dates) > 0) {
    warning(
      "[Daymet Extraction] data has not been provided for some dates. These",
      " are: ",
      stringr::str_flatten_comma(as.character(missing_dates)),
      ". No value will be returned for these dates. Keep in mind that Daymet",
      " data for the current year may not be available yet.",
      call. = FALSE
    )
  }

  # Fetch all dates with available data.
  dates <- as.character(sort(unique(data$date[data$date %in% all_dates])))

  # Open vector to store site names that are outside of spatial extent of
  # provided Daymet files.
  bad_sites <- c()

  # Loop through each requested Daymet variable and extract.
  for (i in daymet_vars) {
    # Loop through each date with data.
    for (j in dates) {
      j_date <- as.Date(j)

      # Grab all observations needing data from date j.
      pts_to_fill <- dplyr::filter(data, .data$date == j_date)

      request_name <- grep(
        pattern = j,
        appeears$request_name,
        value = TRUE
      )

      # Access corresponding file name from data in information file.
      filename <- gsub(
        pattern = "DAYMET_",
        replacement = "DAYMET.",
        daymet_stats[[request_name]]$`File Name`[
          daymet_stats[[request_name]]$Date == j_date &
            daymet_stats[[request_name]]$Dataset == i
        ]
      )

      # Read in data for date j.
      daymet <- terra::rast(ifelse(
        is.null(dl_path),
        paste0(
          "./daymet/",
          request_name,
          "/",
          filename,
          ".tif"
        ),
        paste0(
          dl_path,
          "/daymet/",
          request_name,
          "/",
          filename,
          ".tif"
        )
      ))

      # Loop through each site and extract.
      for (k in unique(pts_to_fill$SurveyAreaIdentifier)) {
        tmp <- pts_to_fill %>%
          dplyr::filter(.data$SurveyAreaIdentifier == k) %>%
          dplyr::select("SurveyAreaIdentifier", "geometry") %>%
          dplyr::distinct() %>%
          sf::st_transform(sf::st_crs(daymet))

        # Check if the site falls outside of or is only partially covered by
        # the spatial extent of the provided Daymet rasters. If so, warn and
        # store site name to avoid extracting data for it later.
        if (
          !terra::is.related(
            daymet,
            terra::vect(tmp),
            relation = "intersects"
          )
        ) {
          warning(
            "[Daymet (",
            i,
            ") Extraction]  site ",
            k,
            " falls outside of the spatial extent of the DAYMET rasters",
            " provided. No value will be returned.",
            call. = FALSE
          )

          bad_sites <- c(bad_sites, k)
          range <- "out"
        } else if (
          terra::is.related(
            daymet,
            terra::vect(tmp),
            relation = "intersects"
          ) &
            !terra::is.related(
              daymet,
              terra::vect(tmp),
              relation = "contains"
            )
        ) {
          warning(
            "[Daymet (",
            i,
            ") Extraction] site ",
            k,
            "'s buffered area is only partially contained by the spatial",
            " extent of the DAYMET rasters provided. Returned ",
            i,
            " value will be derived from the available values.",
            call. = FALSE
          )

          range <- "overlaps"
        } else {
          range <- "in"
        }

        if (range %in% c("overlaps", "in")) {
          # If no issues with coverage, proceed to extraction. If buffered,
          # extract using exactextractr::exact_extract(). If not, extract
          # using terra::extract().
          if (buffered == TRUE) {
            # Check if function information is stored in ...
            if (!hasArg("fun")) {
              funs <- "mean"
            } else {
              funs <- list(...)[["fun"]]
            }

            # Check whether fun = NULL. In exactextractr::exact_extract() this is
            # used to extract cell values and coverage fractions. fun = 'frac' is
            # a valid alternative that works here.
            if (is.null(funs)) {
              stop(
                "[Daymet Extraction] support is not provided for fun",
                " = NULL. If wanting to extract cell values and coverage",
                " fractions consider fun = 'frac'. Keep in mind that this can",
                " produce a lot of columns. Direct use of",
                " exactextractr::exact_extract() may be more useful here.",
                call. = FALSE
              )
            } else if (is.function(funs)) {
              # If fun is a user-specified function, attempt to run.
              val <- exactextractr::exact_extract(daymet, tmp, ...)

              # If function returns more than one value or a data.frame, stop.
              if (
                length(val) > 1 |
                  is.data.frame(val)
              ) {
                stop(
                  "[Daymet Extraction] support for custom summary",
                  " functions is currently limited to functions returning a",
                  " single value (not stored in a data.frame) to allow accurate",
                  " joining to input data.",
                  call. = FALSE
                )
              }

              # If user-defined function returns acceptable value, join to data.
              data[
                data$SurveyAreaIdentifier == k & data$date == j_date,
                paste0(i, "_user_defined_function")
              ] <- val
            } else {
              # If fun is one or more pre-defined summary functions (see
              # ?exactextractr::exact_extract()), loop through options and extract.
              for (l in funs) {
                # Check if any summary functions requested required tailored
                # joining.
                if (
                  l == "quantile" &
                    length(list(...)[["quantiles"]]) > 1
                ) {
                  # Multiple quantiles cause exactextractr::exact_extract() to
                  # return a data.frame with a column for each requested quantile,
                  # and so must be joined in a tailored way.

                  # Build arguments so that calls with multiple functions
                  # requested in fun don't try and extract all requested functions
                  # on each loop iteration.
                  args <- list(...)
                  args$x <- daymet
                  args$y <- tmp
                  args$fun <- l

                  # Overwrite redundant args.
                  args$append_cols <- NULL
                  args$force_df <- FALSE

                  # Extract.
                  q_table <- do.call(exactextractr::exact_extract, args)

                  # Join each requested quantile to original data.
                  for (m in names(q_table)) {
                    data[
                      data$SurveyAreaIdentifier == k & data$date == j_date,
                      paste0(
                        i,
                        "_",
                        l,
                        "_",
                        sub(pattern = "q", replacement = "", x = m)
                      )
                    ] <- q_table[, m]
                  }
                } else if (l %in% c("frac", "weighted_frac")) {
                  # Extracting fraction or weighted fraction causes
                  # exactextractr::exact_extract() to return a data.frame with a
                  # column for each unique cell value, and so must be joined in a
                  # tailored way.

                  # Build arguments so that calls with multiple functions
                  # requested in fun don't try and extract all requested functions
                  # on each loop iteration.
                  args <- list(...)
                  args$x <- daymet
                  args$y <- tmp
                  args$fun <- l

                  # Overwrite redundant args.
                  args$append_cols <- NULL
                  args$force_df <- FALSE

                  # Extract.
                  frac_table <- do.call(exactextractr::exact_extract, args)

                  if (frac_table == 1) {
                    value <- unique(terra::values(terra::crop(
                      daymet,
                      tmp
                    )))

                    data[
                      data$SurveyAreaIdentifier == k & data$date == j_date,
                      paste0(
                        i,
                        "_",
                        l,
                        "_",
                        value
                      )
                    ] <- 1
                  } else {
                    # Join each fractional value to original data.
                    for (m in names(frac_table)) {
                      data[
                        data$SurveyAreaIdentifier == k & data$date == j_date,
                        paste0(
                          i,
                          "_",
                          l,
                          "_",
                          as.numeric(sub(
                            pattern = "frac_",
                            replacement = "",
                            x = m
                          ))
                        )
                      ] <- frac_table[, m]
                    }
                  }
                } else {
                  # If no tailored joining needed, just build arguments so that
                  # calls with multiple functions requested in fun don't try and
                  # extract all requested functions on each loop iteration.
                  args <- list(...)
                  args$x <- daymet
                  args$y <- tmp
                  args$fun <- l

                  # Overwrite redundant args.
                  args$append_cols <- NULL
                  args$force_df <- FALSE

                  # Extract and join requested value to input data.
                  data[
                    data$SurveyAreaIdentifier == k & data$date == j_date,
                    paste0(
                      i,
                      "_",
                      l
                    )
                  ] <- do.call(exactextractr::exact_extract, args)
                }
              }
            }
          } else {
            data[
              data$SurveyAreaIdentifier == k & data$date == j_date,
              i
            ] <- terra::extract(
              x = daymet,
              y = tmp,
              ...
            )[, `if`(hasArg("layer"), "value", terra::names(daymet))]
          }
        }
      }

      # Progress bar.
      if (verbose) {
        message(paste0(
          "[Daymet ",
          i,
          " Extraction] Date ",
          which(dates == j),
          " of ",
          length(dates),
          " complete."
        ))
      }
    }

    # Code to grab nearest raster value for sites outside of raster coverage.
    # Not sure whether to keep this since we are warning users about these sites
    # and saying nothing will be returned. Maybe keep as an option
    # (nearest = TRUE)?
    #
    # if (
    #   TRUE %in%
    #     is.na(data[
    #       data$date %in% dates & !(data$SurveyAreaIdentifier %in% bad_sites),
    #       i
    #     ])
    # ) {
    #   warning(paste0(
    #     "[Daymet (",
    #     i,
    #     ") Extraction] some points are close to shore, and so fall outside of",
    #     " raster coverage. For these cells, the nearest cell value will be",
    #     " used. Repairing now."
    #   ))
    #
    #   for (j in dates) {
    #     sites_to_fill <- unique(data$SurveyAreaIdentifier[
    #       is.na(data[, i]) & data$date == j
    #     ])
    #
    #     if (nrow(sites_to_fill) > 0) {
    #       j_date <- as.Date(j)
    #
    #       filename <- gsub(
    #         pattern = "DAYMET_",
    #         replacement = "DAYMET.",
    #         daymet_stats[[as.character(lubridate::year(j_date))]]$`File Name`[
    #           daymet_stats[[as.character(lubridate::year(j_date))]]$Date ==
    #             j_date &
    #             daymet_stats[[as.character(lubridate::year(
    #               j_date
    #             ))]]$Dataset ==
    #               i
    #         ]
    #       )
    #
    #       daymet <- terra::rast(ifelse(
    #         is.null(dl_path),
    #         paste0(
    #           "./daymet/",
    #           appeears[[as.character(lubridate::year(j_date))]],
    #           "/",
    #           filename,
    #           ".tif"
    #         ),
    #         paste0(
    #           dl_path,
    #           "/daymet/",
    #           appeears[[as.character(lubridate::year(j_date))]],
    #           "/",
    #           filename,
    #           ".tif"
    #         )
    #       ))
    #
    #       for (k in sites_to_fill) {
    #         tmp <- data %>%
    #           dplyr::filter(SurveyAreaIdentifier == k) %>%
    #           dplyr::select(SurveyAreaIdentifier, geometry) %>%
    #           dplyr::distinct() %>%
    #           sf::st_buffer(2500) %>%
    #           sf::st_transform(terra::crs(daymet))
    #
    #         daymet_crop <- terra::crop(daymet, terra::vect(tmp)) %>%
    #           terra::as.points()
    #
    #         near.pt <- terra::nearest(terra::vect(tmp), daymet_crop)$to_id
    #
    #         data[
    #           data$SurveyAreaIdentifier == k & data$date == j,
    #           i
    #         ] <- mean(terra::values(daymet_crop[near.pt])[, filename])
    #       }
    #     }
    #   }
    # }
  }

  # Remove temporary date column from original data.
  data <- dplyr::select(data, -"date")

  # Ensure geometry column is in the last column position.
  data <- dplyr::relocate(
    data,
    "geometry",
    .after = dplyr::last(names(data)[!(names(data) == "geometry")])
  )

  # Check if attributes were found and stored from input data. If they were
  # found reattach.
  if (exists("attrs")) {
    # Reattach attributes

    attributes(data)[names(attrs)] <- attrs
  }

  # Reinstate user's specified column names.
  if (!is.null(site_name)) {
    names(data)[names(data) == "SurveyAreaIdentifier"] <- site_name
  }

  if (!is.null(date_year)) {
    names(data)[names(data) == "survey_year"] <- date_year
  }

  if (!is.null(date_month)) {
    names(data)[names(data) == "survey_month"] <- date_month
  }

  if (!is.null(date_day)) {
    names(data)[names(data) == "survey_day"] <- date_day
  }

  # Remove Daymet files if requested.
  if (retain == FALSE) {
    if (verbose) {
      message("[Daymet Extraction] task complete. Removing files.")
    }

    file.remove(list.files(
      ifelse(is.null(dl_path), "./daymet", paste0(dl_path, "/daymet")),
      full.names = TRUE
    ))
  }

  # Return input data with appended Daymet columns.
  return(data)
}
