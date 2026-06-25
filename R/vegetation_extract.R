#' Extract MODIS NDVI/EVI Data
#'
#' Extracts [16-day NDVI/EVI data](https://doi.org/10.5067/MODIS/MOD13A1.061)
#' derived from imagery from the MODIS Terra and Aqua satellites at
#' approximately 500 m spatial resolution. This data can be downloaded using
#' [vegetation_download()]. The user guide for these data can be found
#' [here](https://lpdaac.usgs.gov/documents/621/MOD13_User_Guide_V61.pdf).
#'
#' Both NDVI and EVI are available through this function and
#' can be accessed by supplying the following arguments to the `covariates`
#' argument:
#' - `modis_ndvi` - NDVI
#' - `modis_evi` - EVI
#'
#' Details on the calculation of these indices can be found in the
#' [MOD13 user guide](https://lpdaac.usgs.gov/documents/621/MOD13_User_Guide_V61.pdf).
#'
#' NDVI/EVI calculations are sensitive to the presence of snow/ice and cloudiness.
#' So users can assess the quality of data extracted at each site, we have included
#' the option to extract pixel reliability assessments included in these NDVI/EVI
#' products by setting argument `reliability = TRUE`. The reliability scale is
#' as in the [user manual](https://lpdaac.usgs.gov/documents/621/MOD13_User_Guide_V61.pdf),
#' and is supplied to the user in the `vegetation_reliability` column if requested.
#'
#' @inheritParams vegetation_download
#'
#' @param covariates Character, vector if both NDVI and EVI desired. By
#'   default, extracts NDVI (`modis_ndvi`).
#' @param vegetation_files Character, vector if multiple files. File-path(s) to
#'   downloaded MODIS vegetation data file(s). We recommend using
#'   [vegetation_download()] to download MODIS files to ensure all files
#'   necessary for your data are captured. Direct output of
#'   [vegetation_download()] can be supplied here.
#' @param reliability Logical. Should pixel reliability information be extracted
#'   at each site?
#' @param site_name Character. Optional argument to provide name of the column
#'   containing site names if not contained within the BMDE column
#'   `SurveyAreaIdentifier`. Can be left `NULL` and still function properly if
#'   originally specified in a call to [data_fmt()] or [vegetation_download()].
#' @param date_year Character. Optional argument to provide the name of the
#'   column containing year data if not contained within the BMDE column
#'   `survey_year`. Can be left `NULL` and still function properly if originally
#'   specified in a call to [data_fmt()] or [vegetation_download()].
#' @param date_month Character. Optional argument to provide the name of the
#'   column containing month data if not contained within the BMDE column
#'   `survey_month`. Can be left `NULL` and still function properly if originally
#'   specified in a call to [data_fmt()] or [vegetation_download()].
#' @param date_day Character. Optional argument to provide the name of the
#'   column containing day-of-month (i.e., a number from 1 to 31) data if not
#'   contained within the BMDE column `survey_day`. Can be left `NULL` and still
#'   function properly if originally specified in a call to [data_fmt()] or
#'   [vegetation_download()].
#' @param retain Logical. Should MODIS data files be kept after extraction. If
#'   `FALSE`, files will be deleted.
#'
#' @returns For `sf` 'POINT' or `terra` 'points' input data, original data with
#'   numeric column(s) `ndvi` and/or `evi` appended containing the NDVI/EVI value
#'   at that point. If reliability information requested, an additional
#'   `vegetation_reliability` column is appended containing the reliability
#'   assessment as defined in table 4 of the
#'   [product's user manual](https://lpdaac.usgs.gov/documents/621/MOD13_User_Guide_V61.pdf).
#'
#'   For `sf` 'POLYGON' or `terra` 'polygons' input data, original data with
#'   numeric column(s) `ndvi` and/or `evi` appended containing the mean NDVI/EVI
#'   value within each polygon. If reliability information requested, an additional
#'   `vegetation_reliability` column is appended containing the percentage of
#'   pixels overlapped by each polygon in each reliability assessment as defined in table 4 of the
#'   [product's user manual](https://lpdaac.usgs.gov/documents/621/MOD13_User_Guide_V61.pdf).
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
#' # Enter EarthData email
#' ed_email <- readline(prompt = "Enter EarthData email: ")
#'
#' # Download MODIS data
#' modis_files <- vegetation_download(
#'   bcch,
#'   ed_email = ed_email
#' )
#'
#' # Extract vegetation data
#' output <- vegetation_extract(
#'   data = bcch,
#'   covariates = "modis_ndvi",
#'   vegetation_files = modis_files,
#'   retain = FALSE
#' )
#'
#' @seealso [terra::extract()] which is used to extract values from MODIS data
#'   for `sf` 'POINT' and `terra` 'points' input data.
#'
#'   [exactextractr::exact_extract()] which is used to extract values from MODIS
#'   data for `sf` 'POLYGON' or `terra` 'polygons' input data.
#'
#' @export

# Function to extract vegetation data from provided MODIS MOD13A1 data files.
vegetation_extract <- function(
  data,
  covariates = "modis_ndvi", # Other options listed in nc_covariate_table().
  vegetation_files, # Character vector of filepaths to downloaded files.
  reliability = FALSE, # Should pixel reliability information be extracted at
  # each site?
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
  retain = TRUE # Should data files be kept after extraction?
) {
  # Check packages
  have_pkg_check(c(
    "sf",
    "luna",
    "terra"
  ))

  # Catch misspecified covariates. Return error if any exist.
  if (FALSE %in% (covariates %in% nc_covariate_table()$covariate_name)) {
    stop(
      "[MODIS NDVI/EVI Extraction] covariates either not listed or one or more are invalid. Please provide covariate names as listed under `covariate_name` in nc_covariate_table().",
      call. = FALSE
    )
  }

  # If no vegetation files are provided, return error.
  if (missing(vegetation_files) | length(vegetation_files) == 0) {
    stop(
      "[MODIS NDVI/EVI Extraction] no vegetation files provided to extract from. Please provide a vector containing filepaths of all necessary MODIS files for your data. Data can be downloaded using vegetation_download().",
      call. = FALSE
    )
  }

  # Check data is in the desired format.
  input_fmt <- covariate_fmt_check(data)

  # If not an sf or terra object, return error and point towards data_fmt().
  if (input_fmt$type == "data.frame") {
    stop(
      "[MODIS NDVI/EVI Extraction] extraction requires an sf or terra object as input in this workflow. Consider using `data_fmt` to conform data first.",
      call. = FALSE
    )
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
      "[MODIS NDVI/EVI Extraction] some specified columns missing from the data: ",
      stringr::str_flatten_comma(specified_cols[
        !(specified_cols %in% data_cols)
      ]),
      ". Use arguments to specify alternate column names if using data that diverges from naturecounts default column names.",
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

  month_corr <- c()

  for (i in 1:length(data$survey_month)) {
    month_corr[i] <- month_check(data$survey_month[i])
  }

  # Use month_check() to validate month data.
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
    data <- sf::st_as_sf(data) # Maybe down the line write full process out in
    # terra for terra data.
  }

  # If buffered, check for packages necessary in buffered workflow.
  if (buffered == TRUE) {
    have_pkg_check("exactextractr")
  }

  # Remove any observations missing year, month, or day data.
  if (
    TRUE %in%
      is.na(data$survey_year) |
      TRUE %in% is.na(data$survey_month) |
      TRUE %in% is.na(data$survey_day)
  ) {
    warning(
      "[MODIS NDVI/EVI Extraction] Missing date data detected. Complete year,",
      " month, and day data is needed for extraction. Observations missing",
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

  # Parse dates stored in filenames of MODIS data files and append column to
  # filenames.
  modis_files <- luna::modisDate(vegetation_files)

  # As each files contains data covering a 16 day period, create an end date of
  # each files coverage.
  modis_files$enddate <- modis_files$date + 16

  modis_files$year <- as.numeric(modis_files$year)
  modis_files$month <- as.numeric(modis_files$month)
  modis_files$day <- as.numeric(modis_files$day)

  modis_files$endyear <- lubridate::year(modis_files$enddate)
  modis_files$endmonth <- lubridate::month(modis_files$enddate)
  modis_files$endday <- lubridate::day(modis_files$enddate)

  modis_files$yday <- lubridate::yday(modis_files$date)
  modis_files$endyday <- lubridate::yday(modis_files$enddate)

  # Function for quick conversion of ordinal dates.
  yearyearday <- function(yr, yd) {
    base <- as.Date(paste0(yr, "-01-01")) # take Jan 1 of year
    day <- base + yd - 1
  }

  # Some date windows have multiple files produced at different times. Extract
  # and store production dates so we can select between these files later.
  modis_files$productiondate <- yearyearday(
    as.numeric(substr(
      gsub(pattern = ".*modis", replacement = "./modis", modis_files$filename),
      61 - 16,
      61 - 13
    )),
    as.numeric(substr(
      gsub(pattern = ".*modis", replacement = "./modis", modis_files$filename),
      61 - 12,
      61 - 10
    ))
  ) +
    lubridate::hms(paste0(
      substr(
        gsub(
          pattern = ".*modis",
          replacement = "./modis",
          modis_files$filename
        ),
        61 - 9,
        61 - 8
      ),
      ":",
      substr(
        gsub(
          pattern = ".*modis",
          replacement = "./modis",
          modis_files$filename
        ),
        61 - 7,
        61 - 6
      ),
      ":",
      substr(
        gsub(
          pattern = ".*modis",
          replacement = "./modis",
          modis_files$filename
        ),
        61 - 5,
        61 - 4
      )
    )) # So long as date format in files stays consistent, this should work
  # fine.

  # Extract and bind spatial extent of each data file.
  modis_files <- cbind(
    modis_files,
    as.data.frame(luna::modisExtent(modis_files$filename))
  )

  # Build object to use in matching sites to their respective MODIS data file.
  modis_match <- data %>%
    dplyr::mutate(
      date = as.Date(paste0(
        .data$survey_year,
        "-",
        .data$survey_month,
        "-",
        .data$survey_day
      ))
    ) %>%
    dplyr::mutate(yday = lubridate::yday(.data$date)) %>%
    dplyr::select(
      "SurveyAreaIdentifier",
      "survey_year",
      "date",
      "yday",
      "geometry"
    ) %>%
    sf::st_transform(terra::crs(terra::rast(modis_files$filename[1])))

  # If buffered, extract coordinates from centroids. Append coordinates.
  if (buffered == TRUE) {
    suppressWarnings(
      modis_match <- cbind(
        modis_match,
        sf::st_coordinates(sf::st_centroid(modis_match))
      )
    )
  } else {
    modis_match <- cbind(modis_match, sf::st_coordinates(modis_match))
  }

  # Open vectors to store site/date information for sites/dates that are unable
  # to be matched to a data file.
  warning_sites <- c()
  warning_years <- c()
  warning_dates <- c()

  # Loop through each site-date combination and match to a data file.
  for (i in unique(modis_match$SurveyAreaIdentifier)) {
    for (j in unique(modis_match$survey_year[
      modis_match$SurveyAreaIdentifier == i
    ])) {
      for (k in unique(modis_match$date[
        modis_match$SurveyAreaIdentifier == i & modis_match$survey_year == j
      ])) {
        # Create temporary object containing only data for site i on day k
        # of year j.
        tmp <- dplyr::filter(
          modis_match,
          .data$SurveyAreaIdentifier == i,
          .data$survey_year == j,
          .data$date == k
        )

        # Check to see whether the site-date combination can be matched to a
        # data file.
        if (
          nrow(modis_files[
            modis_files$year == tmp$survey_year &
              modis_files$xmin < tmp$X &
              modis_files$xmax > tmp$X &
              modis_files$ymin < tmp$Y &
              modis_files$ymax > tmp$Y &
              modis_files$date <= tmp$date &
              modis_files$enddate > tmp$date,
          ]) ==
            0
        ) {
          # Do the coordinates fall within the area covered by any of
          # the data files?
          spatial_check <- ifelse(
            nrow(modis_files[
              modis_files$xmin < tmp$X &
                modis_files$xmax > tmp$X &
                modis_files$ymin < tmp$Y &
                modis_files$ymax > tmp$Y,
            ]) >
              0,
            TRUE,
            FALSE
          )

          # Does data exist for the data's year?
          year_check <- ifelse(
            nrow(modis_files[modis_files$year == tmp$survey_year, ]) > 0,
            TRUE,
            FALSE
          )

          # Does the date fall within the date windows covered by any of the
          # data files?
          yday_check <- ifelse(
            nrow(modis_files[
              modis_files$date <= tmp$date & modis_files$enddate > tmp$date,
            ]) >
              0,
            TRUE,
            FALSE
          )

          # If any checks not passed, store for later warning message.
          if (!(spatial_check)) {
            warning_sites <- c(warning_sites, i)
          }

          if (!(year_check)) {
            warning_years <- c(warning_years, j)
          }

          # Only warn about date if the data is within the spatial extent of
          # the provided MODIS data and is in a year covered by the data.
          if (spatial_check & year_check & !(yday_check)) {
            warning_dates <- c(warning_dates, k)
          }
        } else {
          # If no issues with coverage, match site-date combinations to
          # respective files.

          # List all files that match the location and date.
          suppressWarnings(
            {
              poss_files <- modis_files[
                modis_files$year == tmp$survey_year &
                  modis_files$xmin < tmp$X &
                  modis_files$xmax > tmp$X &
                  modis_files$ymin < tmp$Y &
                  modis_files$ymax > tmp$Y &
                  modis_files$date <= tmp$date &
                  modis_files$enddate > tmp$date,
              ]

              # Pick the most recently produced file.
              modis_match[
                modis_match$SurveyAreaIdentifier == i &
                  modis_match$survey_year == j &
                  modis_match$date == k,
                "filename"
              ] <- poss_files$filename[
                poss_files$productiondate == max(poss_files$productiondate)
              ]
            }
          )
        }
      }
    }
  }

  # Order sites and dates for warning message.
  warning_sites <- sort(unique(warning_sites))
  warning_years <- sort(unique(warning_years))
  warning_dates <- sort(unique(warning_dates))

  # Warn about sites that fall outside of the spatial extent of the provided
  # MODIS data.
  if (length(warning_sites) > 0) {
    if (length(warning_sites) == 1) {
      warning(
        "[MODIS NDVI/EVI Extraction] site ",
        stringr::str_flatten_comma(unique(warning_sites)),
        " falls outside of the spatial extent of the files provided. No value",
        " will be returned.",
        call. = FALSE
      )
    } else {
      warning(
        "[MODIS NDVI/EVI Extraction] sites ",
        stringr::str_flatten_comma(unique(warning_sites)),
        " fall outside of the spatial extent of the files provided. No value",
        " will be returned.",
        call. = FALSE
      )
    }
  }

  # Warn about observations in years that fall outside of the temporal coverage
  # of the provided MODIS data.
  if (length(warning_years) > 0) {
    if (length(warning_years) == 1) {
      warning(
        "[MODIS NDVI/EVI Extraction] observations from year ",
        stringr::str_flatten_comma(unique(warning_years)),
        " fall outside of the temporal extent of the files provided. Is it in",
        " a year where data is unavailable from this dataset? No value will",
        " be returned.",
        call. = FALSE
      )
    } else {
      warning(
        "[MODIS NDVI/EVI Extraction] observations from years ",
        stringr::str_flatten_comma(unique(warning_years)),
        " fall outside of the temporal extent of the files provided. Is it in",
        " a year where data is unavailable from this dataset? No value will be",
        " returned.",
        call. = FALSE
      )
    }
  }

  # Warn about observations on dates that fall outside of the temporal coverage
  # of the provided MODIS data.
  if (length(warning_dates) > 0) {
    warning(
      "[MODIS NDVI/EVI Extraction] observations on ",
      stringr::str_flatten_comma(unique(as.Date(warning_dates))),
      " fall outside of the temporal extent of the files provided. You have",
      " provided data for this year but not this 16-day window. No value will",
      " be returned.",
      call. = FALSE
    )
  }

  # Remove observations without matches.
  modis_match <- dplyr::filter(modis_match, !is.na(.data$filename))

  # Edge case: there is a column in the data called yday that we don't want to
  # overwrite.

  if ("yday" %in% names(data)) {
    yday_storage <- data$yday
    yday_before <- names(data)[which(names(data) == "yday") - 1]
  }

  # Create an ordinal date column in original data for later joining.
  data$yday <- paste0(
    data$survey_year,
    "-",
    data$survey_month,
    "-",
    data$survey_day
  ) %>%
    as.Date() %>%
    lubridate::yday()

  # Loop through each requested vegetation metric, extract, and join to original
  # data.
  for (i in `if`(
    "modis_ndvi" %in% covariates,
    `if`(
      "modis_evi" %in% covariates,
      c("modis_ndvi", "modis_evi"),
      "modis_ndvi"
    ),
    "modis_evi"
  )) {
    message(paste0(
      "[MODIS NDVI/EVI Extraction] calculating MODIS ",
      ifelse(i == "modis_ndvi", "NDVI", "EVI"),
      "."
    ))

    # Create index to access appropriate data layer from MODIS rasters.
    index <- ifelse(
      i == "modis_ndvi",
      "\"500m 16 days NDVI\"",
      "\"500m 16 days EVI\""
    )

    # Loop through each matched MODIS data file.
    for (j in unique(modis_match$filename)) {
      # Create object with all site-date combinations that matched to file j.
      pts_to_fill <- data[
        data$SurveyAreaIdentifier %in%
          modis_match$SurveyAreaIdentifier[modis_match$filename == j] &
          data$survey_year %in%
            modis_match$survey_year[modis_match$filename == j] &
          data$yday %in% modis_match$yday[modis_match$filename == j],
      ]

      # Open the requested layer in file j. `raw` argument is to avoid erroneous
      # scaling factor specified in file. See https://github.com/rspatial/terra/issues/1620.
      modis <- terra::rast(j, raw = TRUE)[index]

      if (reliability == TRUE) {
        modis_reliability <- terra::rast(j)[
          "\"500m 16 days pixel reliability\""
        ]
      }

      # Replace fill values with NAs.
      modis <- terra::subst(modis, -3000, NA)

      # Loop through each site matched to file j and extract.
      for (k in unique(pts_to_fill$SurveyAreaIdentifier)) {
        # If buffered, extract using exactextractr::exact_extract(). If not,
        # extract using terra::extract().
        if (buffered == TRUE) {
          # Create temporary object containing only the buffer for site k.
          tmp <- data %>%
            dplyr::filter(
              .data$SurveyAreaIdentifier == k,
              .data$survey_year %in%
                modis_match$survey_year[modis_match$filename == j]
            ) %>%
            dplyr::select("SurveyAreaIdentifier", "geometry") %>%
            dplyr::distinct() %>%
            sf::st_transform(terra::crs(modis))

          # Crop MODIS data file to site k's buffer.
          modis_clip <- terra::crop(modis, tmp)

          if (reliability == TRUE) {
            modis_reliability_clip <- terra::crop(modis_reliability, tmp)
          }

          # Extract using exactextractr::exact_extract().
          data[
            data$SurveyAreaIdentifier == k &
              data$survey_year ==
                modis_match$survey_year[
                  modis_match$filename == j &
                    modis_match$SurveyAreaIdentifier == k
                ] &
              data$yday %in%
                modis_match$yday[
                  modis_match$filename == j &
                    modis_match$SurveyAreaIdentifier == k
                ],
            ifelse(i == "modis_ndvi", "ndvi", "evi")
          ] <- exactextractr::exact_extract(modis_clip, tmp, fun = "mean")

          # Extract pixel reliability information if requested.
          if (reliability == TRUE) {
            pixel_vals <- table(
              exactextractr::exact_extract(modis_reliability_clip, tmp)[[
                1
              ]]$value
            )

            str_components <- c()
            for (l in 1:length(pixel_vals)) {
              labels <- data.frame(
                value = c(-1:3),
                label = c(
                  "Fill/No Data",
                  "Good Data",
                  "Marginal Data",
                  "Snow/Ice",
                  "Cloudy"
                )
              )
              str_components[l] <- paste0(
                labels$label[labels$value == names(pixel_vals)[l]],
                " (",
                round((unname(pixel_vals[l]) / sum(pixel_vals)) * 100, 2),
                "%)"
              )
            }

            data[
              data$SurveyAreaIdentifier == k &
                data$survey_year ==
                  modis_match$survey_year[
                    modis_match$filename == j &
                      modis_match$SurveyAreaIdentifier == k
                  ] &
                data$yday %in%
                  modis_match$yday[
                    modis_match$filename == j &
                      modis_match$SurveyAreaIdentifier == k
                  ],
              "vegetation_reliability"
            ] <- stringr::str_flatten_comma(str_components)
          }
        } else {
          # Create temporary object containing only the point for site k.
          tmp <- data %>%
            dplyr::filter(
              .data$SurveyAreaIdentifier == k,
              .data$survey_year %in%
                modis_match$survey_year[modis_match$filename == j]
            ) %>%
            dplyr::select("SurveyAreaIdentifier", "geometry") %>%
            dplyr::distinct() %>%
            sf::st_transform(terra::crs(modis)) %>%
            terra::vect()

          # Extract using terra::extract().
          data[
            data$SurveyAreaIdentifier == k &
              data$survey_year ==
                modis_match$survey_year[
                  modis_match$filename == j &
                    modis_match$SurveyAreaIdentifier == k
                ] &
              data$yday %in%
                modis_match$yday[
                  modis_match$filename == j &
                    modis_match$SurveyAreaIdentifier == k
                ],
            ifelse(i == "modis_ndvi", "ndvi", "evi")
          ] <- terra::extract(modis, tmp)[, index]

          # Extract pixel reliability information if requested.
          if (reliability == TRUE) {
            labels <- data.frame(
              value = c(-1:3),
              label = c(
                "Fill/No Data",
                "Good Data",
                "Marginal Data",
                "Snow/Ice",
                "Cloudy"
              )
            )

            data[
              data$SurveyAreaIdentifier == k &
                data$survey_year ==
                  modis_match$survey_year[
                    modis_match$filename == j &
                      modis_match$SurveyAreaIdentifier == k
                  ] &
                data$yday %in%
                  modis_match$yday[
                    modis_match$filename == j &
                      modis_match$SurveyAreaIdentifier == k
                  ],
              "vegetation_reliability"
            ] <- labels$label[
              labels$value ==
                unique(terra::extract(modis_reliability, tmp)[, 2])
            ]
          }
        }
      }
    }
  }

  # Apply scaling factor.
  if ("modis_ndvi" %in% covariates) {
    data$ndvi <- data$ndvi * 0.0001
  }

  if ("modis_evi" %in% covariates) {
    data$evi <- data$evi * 0.0001
  }

  # Remove ordinal date column from original data.
  data <- dplyr::select(data, -"yday")

  # If a yday column was stored, return it here
  if (exists("yday_storage")) {
    data$yday <- yday_storage
    data <- dplyr::relocate(.data = data, "yday", .after = yday_before)

    rm(yday_storage)
    rm(yday_before)
  }

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

  # If requested, remove MODIS data files.
  if (retain == FALSE) {
    message(paste0(
      "[MODIS NDVI/EVI Extraction] task complete. Removing files."
    ))

    file.remove(modis_files$filename)
  }

  # Return input data with appended vegetation columns.
  return(data)
}
