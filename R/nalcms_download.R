#' Download Landcover Data from the North American Land Change Monitoring System
#'
#' Downloads landcover data from the North American Land Change Monitoring
#' System (NALCMS) via the [North American Environmental Atlas](https://www.cec.org/north-american-environmental-atlas/).
#' Landcover data are available in snapshots every 5 years between 2010 and 2020 at
#' a 30 m resolution covering Canada, the United States, and Mexico. Users
#' should be aware that these are large files (up to 2 Gb per snapshot).
#'
#' Downloads are facilitated by a call to [utils::download.file()].
#'
#' @param data A `data.frame`, `sf` 'POINT' or 'POLYGON' object, or `terra` 'points'
#'   or 'polygons' object containing a column with observation years either named
#'   the BMDE default `survey_year` or another name specified in argument `date_year`.
#'   Not required if `use_date = FALSE`, but must be specified if `use_date = TRUE`
#'   or `countries = NULL`.
#' @param use_date Logical. Should the function use year data provided in `data`
#'   to choose which snapshot(s) to download? If `FALSE`, `snapshot_year` can be used
#'   to specify which snapshot(s) should be downloaded and used.
#' @param interpolate Logical. Should only the snapshots for snapshot years that
#'   exist within `data` be downloaded (`FALSE`), or should all snapshots within 5 years
#'   of any year in `data` be downloaded (`TRUE`)? Only applicable when `use_date = TRUE`.
#' @param interpolate_method Character. One of 1) "closest" to match interceding
#'   years to the nearest snapshot, 2) "next" to match interceding years to the
#'   next snapshot, or 3) "previous" to match interceding years to the previous
#'   snapshot. Ignored if `use_date = FALSE` or `interpolate = FALSE`.
#' @param snapshot_year Numeric, vector if multiple snapshots desired. Snapshot
#'   years to download. Options include: 2010, 2015, and 2020. If specified,
#'   takes precedent over dates from `data` when `use_date = TRUE`.
#' @param countries Character, `"Canada"`, `"United States"`,
#'   `"Mexico"` or multiple of these. If left `NULL`, function will attempt to
#'   identify countries needed based on locations in `data`.
#' @param site_name Character. Optional argument to provide the name of the
#'   column containing site names if not contained within the BMDE column
#'   `SurveyAreaIdentifier`. Can be left `NULL` and still function properly if
#'   originally specified in a call to [data_fmt()].
#' @param date_year Character. Optional argument to provide the name of the
#'   column containing year data if not contained within the BMDE column
#'   `survey_year`. Can be left `NULL` and still function properly if originally
#'   specified in a call to [data_fmt()].
#' @param timeout Numeric. Number of seconds before downloads timeout. This should
#'   be in the 10s of thousands of seconds, depending on internet download speed.
#'   Default value assumes largest snapshot is being requested, with download
#'   speeds of 0.2 Mb/s.
#' @param dl_path Character. Optional argument to provide path to download data
#'   to. By default, data is downloaded to a subfolder `nalcms/` in the working
#'   directory.
#' @param progress Logical. Should progress bars be displayed?
#'
#' @returns A character vector containing file-paths to downloaded NALCMS landcover files.
#'
#' @examplesIf interactive()
#' # Convert included test data on black-capped chickadees to sf POINT object
#' bcch <- sf::st_as_sf(
#'   bcch,
#'   coords = c("longitude", "latitude"),
#'   crs = 4326
#' )
#'
#' # Download NALCMS Landcover data - uses the dates in the data to determine which
#' # snapshot years and countries to download.
#' output <- nalcms_download(data = bcch,
#'                           progress = FALSE)
#'
#' # We can also manually specify the snapshot years and countries to download
#' # with no input data required like this:
#' output <- nalcms_download(use_date = FALSE,
#'                        snapshot_year = c(2015, 2020),
#'                        countries = "Canada",
#'                        progress = FALSE)
#'
#'
#' @seealso [nalcms_extract()] which can be used to extract data from loaded
#' NALCMS data files.
#'
#' @export
# Function to download data from the Spatialized Canadian National Forest
# Inventory using download.file().
nalcms_download <- function(
  data = NULL, # Only necessary if use_date = TRUE - needed to fetch year data.
  use_date = TRUE, # Should the most recent snapshot be downloaded (FALSE), or
  # should all relevant snapshots be downloaded for extraction (TRUE). Can
  # result in multiple large downloads.
  interpolate = FALSE,
  interpolate_method = "closest",
  snapshot_year = NULL, # If use_date = FALSE, the desired snapshot year to be
  # used. If not specified, the most recent (2025) is used.
  countries = NULL, # Character vector of country names or ISO3 codes. If left
  # NULL, country will be auto-detected.
  site_name = NULL,
  date_year = NULL, # optional argument to provide column name containing year
  # data. Default is assumed to be the BMDE column 'survey_year'. Can
  # be left NULL and still function properly if originally specified in a call
  # to data_fmt().
  timeout = 10000,
  dl_path = NULL, # optional argument to provide path
  # to download data to. By default, data is
  # downloaded to a subfolder 'scanfi/' in the
  # working directory.
  progress = TRUE
) {
  # Check packages
  have_pkg_check("terra")

  if (!missing(data)) {
    # Check data is in the desired format.
    input_fmt <- covariate_fmt_check(data)
  }

  # Use user specified snapshot years over automatically selected years
  if (!is.null(snapshot_year) & use_date == TRUE) {
    use_date <- FALSE

    warning(
      "[NALCMS Download] Specific snapshot years requested but use_date",
      " set as TRUE, suggesting function should determine necessary",
      " snapshots to download from years in data argument. Overriding",
      " and proceeding to download snapshots requested in snapshot_year.",
      call. = FALSE
    )
  }

  # Warn if interpolate = TRUE and use_date = FALSE.
  if (interpolate == TRUE & use_date == FALSE) {
    warning(
      "[NALCMS Download] when use_date = FALSE, interpolate is ignored.",
      "Snapshots specified in snapshot_year will be downloaded.",
      call. = FALSE
    )
  }

  if (use_date == TRUE) {
    # Check data is in the desired format.
    input_fmt <- covariate_fmt_check(data)

    # If not an sf or terra object, return error and point towards data_fmt().
    if (input_fmt$type == "data.frame") {
      stop(
        "[NALCMS Extraction] extraction requires an sf or terra object as input",
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
        (!("survey_year" %in% data_cols) |
          !("SurveyAreaIdentifier" %in% data_cols))
    ) {
      stop(
        "[NALCMS Landcover Download] some specified columns missing from the",
        " data: ",
        stringr::str_flatten_comma(specified_cols[
          !(specified_cols %in% data_cols)
        ]),
        ". Use arguments to specify alternate column names if using data that",
        " diverges from naturecounts default column names.",
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

    # Get necessary years for download from data.

    available_years <- seq(from = 2010, to = 2020, by = 5)

    if (interpolate == TRUE) {
      closest_year <- data.frame(data_year = sort(unique(data$survey_year)))

      outside_years <- c(
        closest_year$data_year[closest_year$data_year < 2005],
        closest_year$data_year[closest_year$data_year > 2025]
      )

      if (length(outside_years) > 0) {
        warning(
          "[NALCMS Landcover Download] Data contains years more than 5 years away",
          " from nearest NALCMS snapshot (",
          stringr::str_flatten_comma(outside_years),
          "). No value will be returned for observations in these years.",
          call. = FALSE
        )
      }

      closest_year <- dplyr::filter(
        closest_year,
        !(.data$data_year %in% outside_years)
      )

      if (interpolate_method == "closest") {
        for (i in closest_year$data_year) {
          closest_year$nalcms_year[
            closest_year$data_year == i
          ] <- available_years[which(
            abs(i - available_years) == min(abs(i - available_years))
          )]
        }
      } else if (interpolate_method == "previous") {
        for (i in closest_year$data_year) {
          if (!(i < min(available_years))) {
            closest_year$nalcms_year[
              closest_year$data_year == i
            ] <- max(available_years[available_years <= i])
          }
        }
      } else if (interpolate_method == "next") {
        for (i in closest_year$data_year) {
          if (!(i > max(available_years))) {
            closest_year$nalcms_year[
              closest_year$data_year == i
            ] <- min(available_years[available_years >= i])
          }
        }
      } else {
        stop(
          "[NALCMS Landcover Download] invalid option provided to",
          " interpolate_method. Please supply one of 'closest', 'near', or",
          "'previous'. See documentations for more details on each.",
          call. = FALSE
        )
      }

      necessary_years <- stats::na.omit(unique(closest_year$nalcms_year))
    } else {
      if (!any(available_years %in% data$survey_year)) {
        stop(
          "[NALCMS Landcover Download] Data provided to data argument does not contain",
          " observations within the NALCMS snapshot years (2010, 2015, 2020).",
          " If wanting to match interceding years to snapshots, use interpolate",
          " = TRUE.",
          call. = FALSE
        )
      }

      necessary_years <- sort(unique(available_years[
        available_years %in% data$survey_year
      ]))
    }
  } else {
    necessary_years <- `if`(is.null(snapshot_year), 2020, snapshot_year)

    if (!all(necessary_years %in% seq(from = 2010, to = 2020, by = 5))) {
      stop(
        "[NALCMS Landcover Download] Invalid snapshot year(s) provided to",
        " snapshot_year argument: ",
        necessary_years[
          (necessary_years %in% seq(from = 2010, to = 2020, by = 5)) == FALSE
        ],
        ". Valid snapshot years are ",
        stringr::str_flatten_comma(seq(from = 2010, to = 2020, by = 5)),
        "."
      )
    }
  }

  # Create download path if it doesn't already exist.
  if (is.null(dl_path) & !dir.exists("./nalcms")) {
    dir.create("./nalcms", recursive = TRUE)
  }

  if (!is.null(dl_path) & !dir.exists(paste0(dl_path, "/nalcms"))) {
    dir.create(paste0(dl_path, "/nalcms"), recursive = TRUE)
  }

  # Check that either data or countries argument is specified.
  if (missing(data) & is.null(countries)) {
    stop(
      "[NALCMS Landcover Download] no information provided to select countries",
      " to download data for. Please provide either a sf object with",
      " `POINT` or `POLYGON` geometry, or a terra SpatVector object with",
      " `points` or `polygons` geometry to the data argument, or a character",
      " string containing the name(s) of the countries to",
      " download data for.",
      call. = FALSE
    )
  }

  # Unless user specified, attempt to automatically detect the countries for
  # which data must be downloaded.
  if (is.null(countries)) {
    # Check for additional package necessary in this workflow.
    have_pkg_check("spData")

    # If not an sf or terra object, return error and point towards data_fmt().
    if (input_fmt$type == "data.frame") {
      stop(
        "[NALCMS Landcover Download] downloading requires an sf or terra object as",
        " input in this workflow. Consider using `data_fmt` to conform",
        " data first.",
        call. = FALSE
      )
    }

    countries <- c()

    # Loop across all sites and identify the country that each falls within.
    for (i in unique(data$SurveyAreaIdentifier)) {
      world <- sf::st_read(
        system.file("shapes/world.gpkg", package = "spData"),
        quiet = TRUE
      ) %>%
        terra::vect()

      if (input_fmt$type == "sf") {
        tmp <- terra::vect(data[data$SurveyAreaIdentifier == i, ]) %>%
          terra::project(terra::crs(world))
      } else {
        tmp <- data[data$SurveyAreaIdentifier == i, ] %>%
          terra::project(terra::crs(world))
      }

      world <- suppressWarnings(terra::intersect(world, terra::buffer(tmp, 1)))

      country <- unique(world$name_long)

      data$country[
        data$SurveyAreaIdentifier == i
      ] <- stringr::str_flatten_comma(country)

      countries <- unique(c(countries, country))[
        !(unique(c(countries, country)) == "")
      ]
    }
  }

  # Check that Canada, the US, or Mexico are in the country names. If others
  # are, warn and only download Canada, US, or Mexico. If none are, return
  # error.
  if (
    any(
      c(
        "Canada",
        "CAN",
        "United States",
        "United States of America",
        "USA",
        "US",
        "Mexico",
        "MEX"
      ) %in%
        countries
    )
  ) {
    if (
      !all(
        countries %in%
          c(
            "Canada",
            "CAN",
            "United States",
            "United States of America",
            "USA",
            "US",
            "Mexico",
            "MEX"
          )
      )
    ) {
      warning(
        "[NALCMS Landcover Download] data from countries other than",
        " Canada, the United States, and Mexico found in the data provided to",
        " data argument. NALCMS Landcover data is only available for",
        " Canada, the United States, or Mexico. Download will proceed,",
        " but NALCMS Landcover data will be unavailable for some objects",
        " present in data.",
        call. = FALSE
      )
    }
  } else {
    stop(
      "[NALCMS Landcover Download] NALCMS Landcover data is only available for",
      " Canada, the United States, or Mexico. Data provided to data",
      " argument not found to be in any of those countries.",
      call. = FALSE
    )
  }

  if (use_date == TRUE) {
    # Create vector of download links for each requested NALCMS landcover year.
    filename <- list()

    for (i in countries) {
      filename[[i]] <- if (interpolate == TRUE) {
        data.frame(
          year = stats::na.omit(unique(closest_year$nalcms_year[
            closest_year$data_year %in%
              data$survey_year[stringr::str_detect(data$country, i)]
          ]))
        )
      } else {
        data.frame(
          year = unique(necessary_years[
            necessary_years %in%
              data$survey_year[stringr::str_detect(data$country, i)]
          ])
        )
      }
      if (nrow(filename[[i]]) > 0) {
        filename[[i]]$country <- i
      }
    }

    filename <- purrr::list_rbind(filename)
  } else {
    filename <- data.frame(
      country = rep(countries, each = length(necessary_years)),
      year = rep(necessary_years, times = length(countries))
    )
  }

  filename <- filename %>%
    dplyr::mutate(
      filename = dplyr::case_when(
        year == 2010 & country %in% c("Canada", "CAN") ~
          "https://www.cec.org/files/atlas_layers/1_terrestrial_ecosystems/1_01_2_land_cover_2010_30m/can_land_cover_2010v3_30m_tif.zip",
        year == 2010 &
          country %in%
            c("United States", "United States of America", "USA", "US") ~
          "https://www.cec.org/files/atlas_layers/1_terrestrial_ecosystems/1_01_2_land_cover_2010_30m/usa_land_cover_2010v3_30m_tif.zip",
        year == 2010 & country %in% c("Mexico", "MEX") ~
          "https://www.cec.org/files/atlas_layers/1_terrestrial_ecosystems/1_01_2_land_cover_2010_30m/mex_land_cover_2010v3_30m_tif.zip",
        year == 2015 & country %in% c("Canada", "CAN") ~
          "https://www.cec.org/files/atlas_layers/1_terrestrial_ecosystems/1_01_1_land_cover_2015_30m/can_land_cover_2015v4_30m_tif.zip",
        year == 2015 &
          country %in%
            c("United States", "United States of America", "USA", "US") ~
          "https://www.cec.org/files/atlas_layers/1_terrestrial_ecosystems/1_01_1_land_cover_2015_30m/usa_land_cover_2015v4_30m_tif.zip",
        year == 2015 & country %in% c("Mexico", "MEX") ~
          "https://www.cec.org/files/atlas_layers/1_terrestrial_ecosystems/1_01_1_land_cover_2015_30m/mex_land_cover_2015v4_30m_tif.zip",
        year == 2020 & country %in% c("Canada", "CAN") ~
          "https://www.cec.org/files/atlas_layers/1_terrestrial_ecosystems/1_01_0_land_cover_2020_30m/can_land_cover_2020v2_30m_tif.zip",
        year == 2020 &
          country %in%
            c("United States", "United States of America", "USA", "US") ~
          "https://www.cec.org/files/atlas_layers/1_terrestrial_ecosystems/1_01_0_land_cover_2020_30m/usa_land_cover_2020v2_30m_tif.zip",
        year == 2020 & country %in% c("Mexico", "MEX") ~
          "https://www.cec.org/files/atlas_layers/1_terrestrial_ecosystems/1_01_0_land_cover_2020_30m/mex_land_cover_2020v2_30m_tif.zip"
      )
    )

  # Store user default timeout option
  user_timeout <- getOption("timeout")

  # Set timeout as specified in arguments.
  options("timeout" = timeout)

  # Open vector to store filenames of downloaded and extracted data.
  nalcms_files <- c()

  # Loop through each requested NALCMS Landcover file and download.
  for (i in filename$filename) {
    # If file doesn't already exist, download requested variable.
    if (
      !file.exists(ifelse(
        is.null(dl_path),
        paste0(
          "./nalcms/",
          dplyr::last(unlist(stringr::str_split(
            i,
            "/"
          )))
        ),
        paste0(
          dl_path,
          "/nalcms/",
          dplyr::last(unlist(stringr::str_split(
            i,
            "/"
          )))
        )
      ))
    ) {
      message(
        "[NALCMS Download] downloading NALCMS Landcover for ",
        filename$country[filename$filename == i],
        " snapshot year ",
        filename$year[filename$filename == i],
        ". Files are large and may require a fair bit of download and processing time."
      )

      # tryCatch needed to handle curl issues and redirect users to downloading
      # manually and reading using scanfi_read().
      tryCatch(
        utils::download.file(
          url = i,
          destfile = ifelse(
            is.null(dl_path),
            paste0(
              "./nalcms/",
              dplyr::last(unlist(stringr::str_split(
                i,
                "/"
              )))
            ),
            paste0(
              dl_path,
              "/nalcms/",
              dplyr::last(unlist(stringr::str_split(
                i,
                "/"
              )))
            )
          ),
          mode = "wb",
          quiet = !progress
        ),
        error = function(e) {
          if (conditionMessage(e) == "'curl' call had nonzero exit status") {
            stop(
              "[NALCMS Download] 'curl' call had nonzero exist status. Please download files directly from https://www.cec.org/north-american-environmental-atlas/.",
              call. = FALSE
            )
          } else {
            stop(conditionMessage(e), call. = FALSE)
          }
        }
      )

      message("[NALCMS Download] Unzipping.")

      # Unzip downloaded file.
      utils::unzip(
        zipfile = ifelse(
          is.null(dl_path),
          paste0(
            "./nalcms/",
            dplyr::last(unlist(stringr::str_split(
              i,
              "/"
            )))
          ),
          paste0(
            dl_path,
            "/nalcms/",
            dplyr::last(unlist(stringr::str_split(
              i,
              "/"
            )))
          )
        ),
        exdir = ifelse(
          is.null(dl_path),
          "./nalcms/",
          paste0(
            dl_path,
            "/nalcms/"
          )
        )
      )
    }

    nalcms_files <- c(
      nalcms_files,
      ifelse(
        is.null(dl_path),
        paste0(
          sub(
            pattern = ".zip",
            replacement = "",
            x = paste0(
              "./nalcms/",
              dplyr::last(unlist(stringr::str_split(
                i,
                "/"
              )))
            )
          ),
          "/",
          ifelse(
            filename$country[filename$filename == i] %in% c("Canada", "CAN"),
            "CAN",
            ifelse(
              filename$country[filename$filename == i] %in%
                c("United States", "United States of America", "USA", "US"),
              "USA",
              "MEX"
            )
          ),
          "_NALCMS_landcover_",
          ifelse(
            filename$year[filename$filename == i] == 2010,
            "2010v3",
            ifelse(
              filename$year[filename$filename == i] == 2015,
              "2015v4",
              "2020v2"
            )
          ),
          "_30m/data/",
          ifelse(
            filename$country[filename$filename == i] %in% c("Canada", "CAN"),
            "CAN",
            ifelse(
              filename$country[filename$filename == i] %in%
                c("United States", "United States of America", "USA", "US"),
              "USA",
              "MEX"
            )
          ),
          "_NALCMS_landcover_",
          ifelse(
            filename$year[filename$filename == i] == 2010,
            "2010v3",
            ifelse(
              filename$year[filename$filename == i] == 2015,
              "2015v4",
              "2020v2"
            )
          ),
          "_30m.tif"
        ),
        paste0(
          sub(
            pattern = ".zip",
            replacement = "",
            x = paste0(
              dl_path,
              "/nalcms/",
              dplyr::last(unlist(stringr::str_split(
                i,
                "/"
              )))
            )
          ),
          "/",
          ifelse(
            filename$country[filename$filename == i] %in% c("Canada", "CAN"),
            "CAN",
            ifelse(
              filename$country[filename$filename == i] %in%
                c("United States", "United States of America", "USA", "US"),
              "USA",
              "MEX"
            )
          ),
          "_NALCMS_landcover_",
          ifelse(
            filename$year[filename$filename == i] == 2010,
            "2010v3",
            ifelse(
              filename$year[filename$filename == i] == 2015,
              "2015v4",
              "2020v2"
            )
          ),
          "_30m/data/",
          ifelse(
            filename$country[filename$filename == i] %in% c("Canada", "CAN"),
            "CAN",
            ifelse(
              filename$country[filename$filename == i] %in%
                c("United States", "United States of America", "USA", "US"),
              "USA",
              "MEX"
            )
          ),
          "_NALCMS_landcover_",
          ifelse(
            filename$year[filename$filename == i] == 2010,
            "2010v3",
            ifelse(
              filename$year[filename$filename == i] == 2015,
              "2015v4",
              "2020v2"
            )
          ),
          "_30m.tif"
        )
      )
    )

    # Add Alaska filepath if US data downloaded.
    if (
      filename$country[filename$filename == i] %in%
        c("United States", "United States of America", "USA", "US")
    ) {
      nalcms_files <- c(
        nalcms_files,
        ifelse(
          is.null(dl_path),
          paste0(
            sub(
              pattern = ".zip",
              replacement = "",
              x = paste0(
                "./nalcms/",
                dplyr::last(unlist(stringr::str_split(
                  i,
                  "/"
                )))
              )
            ),
            "/ASK_NALCMS_landcover_",
            ifelse(
              filename$year[filename$filename == i] == 2010,
              "2010v3",
              ifelse(
                filename$year[filename$filename == i] == 2015,
                "2015v4",
                "2020v2"
              )
            ),
            "_30m/data/ASK_NALCMS_landcover_",
            ifelse(
              filename$year[filename$filename == i] == 2010,
              "2010v3",
              ifelse(
                filename$year[filename$filename == i] == 2015,
                "2015v4",
                "2020v2"
              )
            ),
            "_30m.tif"
          ),
          paste0(
            sub(
              pattern = ".zip",
              replacement = "",
              x = paste0(
                dl_path,
                "/nalcms/",
                dplyr::last(unlist(stringr::str_split(
                  i,
                  "/"
                )))
              )
            ),
            "/ASK_NALCMS_landcover_",
            ifelse(
              filename$year[filename$filename == i] == 2010,
              "2010v3",
              ifelse(
                filename$year[filename$filename == i] == 2015,
                "2015v4",
                "2020v2"
              )
            ),
            "_30m/data/ASK_NALCMS_landcover_",
            ifelse(
              filename$year[filename$filename == i] == 2010,
              "2010v3",
              ifelse(
                filename$year[filename$filename == i] == 2015,
                "2015v4",
                "2020v2"
              )
            ),
            "_30m.tif"
          )
        )
      )
    }
  }

  # Reset user default timeout option.
  options("timeout" = user_timeout)

  # Return vector of filepaths to NALCMS rasters.
  return(nalcms_files)
}
