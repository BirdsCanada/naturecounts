#' Extract MODIS Landcover Data
#'
#' Extracts [annual landcover data](https://doi.org/10.5067/MODIS/MCD12Q1.061)
#' derived from imagery from the MODIS Terra and Aqua satellites at
#' approximately 500 m spatial resolution. This data can be downloaded using
#' [landcover_download()].
#'
#' Five landcover classification schemes are available through this function and
#' can be accessed by supplying the following arguments to the `covariates`
#' argument:
#' - `modis_lctype1` - IGBP global vegetation classification scheme
#' - `modis_lctype2` - University of Maryland (UMD) scheme
#' - `modis_lctype3` - MODIS-derived LAI/fPAR scheme
#' - `modis_lctype4` - MODIS-derived Net Primary Production (NPP) scheme
#' - `modis_lctype5` - Plant Functional Type (PFT) scheme
#'
#' Details on these classification schemes can be found in Chapter 5 of the
#' [MODIS User
#' Guide](https://lpdaac.usgs.gov/documents/101/MCD12_User_Guide_V6.pdf). By
#' default, the function extracts the University of Maryland scheme
#' (`modis_lctype1`), but we strongly recommend users consider the strengths and
#' weaknesses of each classification scheme in the context of their analysis and
#' choose their desired classification scheme appropriately.
#'
#' @inheritParams landcover_download
#'
#' @param covariates Character, vector if multiple landcover types desired. By
#'   default, extracts the IGBP global vegetation classification scheme
#'   (`modis_lctype1`).
#' @param landcover_files Character, vector if multiple files. File-path(s) to
#'   downloaded MODIS landcover data file(s). We recommend using
#'   [landcover_download()] to download MODIS files to ensure all files
#'   necessary for your data are captured. Direct output of
#'   [landcover_download()] can be supplied here.
#' @param site_name Character. Optional argument to provide name of the column
#'   containing site names if not contained within the BMDE column
#'   `SurveyAreaIdentifier`. Can be left `NULL` and still function properly if
#'   originally specified in a call to [data_fmt()] or [landcover_download()].
#' @param date_year Character. Optional argument to provide the name of the
#'   column containing year data if not contained within the BMDE column
#'   `survey_year`. Can be left NULL and still function properly if originally
#'   specified in a call to [data_fmt()] or [landcover_download()].
#' @param retain Logical. Should MODIS data files be kept after extraction. If
#'   `FALSE`, files will be deleted.
#' @param ... Other arguments passed to [landscapemetrics::calculate_lsm()] for
#'   `sf` 'POLYGON' or `terra` 'polygons' input data. Primarily useful for
#'   specifying metrics other than the proportional cover of each landcover
#'   class. See [landscapemetrics::list_lsm()] for other options.
#'
#' @returns For `sf` 'POINT' or `terra` 'points' input data, original data with
#'   a character column `lctype1` appended containing the name of the landcover
#'   class that point falls within.
#'
#'   For `sf` 'POLYGON' or `terra` 'polygons' input data, original data with
#'   numeric columns containing the requested landscape metrics (see
#'   [landscapemetrics::list_lsm()] for options). By default, returns columns
#'   containing the proportion of each polygon that is covered
#'   by each landcover type.
#'
#' @examplesIf interactive()
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
#' # Enter EarthData email
#' ed_email <- readline(prompt = "Enter EarthData email: ")
#'
#' # Download MODIS data
#' modis_files <- landcover_download(
#'   bcch,
#'   ed_email = ed_email
#' )
#'
#' # Extract landcover data
#' output <- landcover_extract(
#'   data = bcch,
#'   covariates = "modis_lctype1",
#'   landcover_files = modis_files,
#'   retain = FALSE
#' )
#'
#' @seealso [landcover_download()] which can be used to download data from
#'   the MODIS Landcover dataset.
#'
#'   [nc_covariates_merge()] to merge extracted
#'   covariate data into data originally provided to the `data` argument of
#'   [data_fmt()].
#'
#'   [landscapemetrics::list_lsm()] to view options for landscape metrics that
#'   can be calculated for buffered input data.
#'
#' @export

# Function to extract land cover data from provided MODIS MCD12Q1 data files.
landcover_extract <- function(
  data,
  covariates = "modis_lctype1", # Other options listed in nc_covariate_table().
  landcover_files, # Character vector of filepaths to downloaded files.
  site_name = NULL, # optional argument to provide column name containing site
  # names. Default is assumed to be the BMDE column 'SurveyAreaIdentifier'. Can
  # be left NULL and still function properly if originally specified in a call
  # to data_fmt().
  date_year = NULL, # optional argument to provide column name containing year
  # data. Default is assumed to be the BMDE column 'survey_year'. Can
  # be left NULL and still function properly if originally specified in a call
  # to data_fmt().
  retain = TRUE, # Should data files be kept after extraction?
  ...
) {
  # Check packages
  have_pkg_check(c(
    "sf",
    "luna",
    "terra",
    "stats"
  ))

  # Catch misspecified covariates. Return error if any exist.
  if (FALSE %in% (covariates %in% nc_covariate_table()$covariate_name)) {
    stop(
      "[MODIS Landcover Extraction] covariates either not listed or one or",
      " more are invalid. Please provide covariate names as listed under",
      " `covariate_name` in nc_covariate_table().",
      call. = FALSE
    )
  }

  # If no landcover files are provided, return error.
  if (missing(landcover_files) | length(landcover_files) == 0) {
    stop(
      "[MODIS Landcover Extraction] no landcover files provided to extract from. Please provide a vector containing filepaths of all necessary MODIS files for your data. Data can be downloaded using landcover_download().",
      call. = FALSE
    )
  }

  # Check data is in the desired format.
  input_fmt <- covariate_fmt_check(data)

  # If not an sf or terra object, return error and point towards data_fmt().
  if (input_fmt$type == "data.frame") {
    stop(
      "[MODIS Landcover Extraction] extraction requires an sf or terra object as input in this workflow. Consider using `data_fmt` to conform data first.",
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
      "[MODIS Landcover Extraction] some specified columns missing from the data: ",
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

  # Check whether object is buffered or not to determine extraction
  # procedure down the line.
  if (input_fmt$type == "sf") {
    buffered <- ifelse(input_fmt$geometry == "POINT", FALSE, TRUE)
  }

  if (input_fmt$type == "terra") {
    buffered <- ifelse(input_fmt$geometry == "points", FALSE, TRUE)

    # Convert to sf object for use in workflow.
    data <- sf::st_as_sf(data) # Maybe down the line write full process out in terra for terra data.
  }

  # If buffered, check for packages necessary in buffered workflow.
  if (buffered == TRUE) {
    have_pkg_check("landscapemetrics")
  }

  # Parse dates stored in filenames of MODIS data files and append column to
  # filenames.
  modis_files <- luna::modisDate(landcover_files)
  modis_files <- cbind(
    modis_files,
    as.data.frame(luna::modisExtent(modis_files$filename))
  )

  modis_files$year <- as.numeric(modis_files$year)

  # Build object to use in matching sites to their respective MODIS data file.
  modis_match <- data %>%
    dplyr::select("SurveyAreaIdentifier", "survey_year", "geometry") %>%
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

  warning_years <- c()
  nearest_years <- c()
  # Loop through years to check that all are represented in the MODIS data.
  # When requests are placed for data containing years not covered by MODIS,
  # nothing in the downloading process alerts the user to this. Warn here, and
  # use nearest year.
  for (i in sort(unique(modis_match$survey_year))) {
    if (!(i %in% modis_files$year)) {
      warning_years <- c(warning_years, i)
      nearest_years <- unique(c(
        nearest_years,
        unique(modis_files$year)[which(
          abs(i - unique(modis_files$year)) ==
            min(abs(i - unique(modis_files$year)))
        )]
      ))
    }
  }

  if (length(warning_years) > 0) {
    warning(
      paste0(
        "[MODIS Landcover Extraction]: MODIS data not available for ",
        stringr::str_flatten_comma(warning_years),
        " - using data from nearest year(s) (",
        stringr::str_flatten_comma(nearest_years),
        ")."
      ),
      call. = FALSE
    )
  }

  # Open vector to store names of out of range sites. NOTE: this might not be
  # that informative for datasets without dedicated site names.
  out_of_range <- c()

  # Loop through each site-year combination and match to appropriate file.
  for (i in unique(modis_match$SurveyAreaIdentifier)) {
    for (j in unique(modis_match$survey_year[
      modis_match$SurveyAreaIdentifier == i
    ])) {
      # Create temporary spatial object containing only the buffer for site i.
      tmp <- dplyr::filter(
        modis_match,
        .data$SurveyAreaIdentifier == i,
        .data$survey_year == j
      ) %>%
        dplyr::distinct()

      # Check if the coordinates of that site fall within the coverage of the
      # provided MODIS files. If not, warn and note site name. If not, proceed
      # with file-matching.
      if (
        all(tmp$X > modis_files$xmax) |
          all(tmp$X < modis_files$xmin) |
          all(tmp$Y > modis_files$ymax) |
          all(tmp$Y < modis_files$ymin)
      ) {
        warning(
          "[MODIS Landcover Extraction] site ",
          i,
          " falls outside of the spatial extent of the MODIS files provided.",
          " No value will be assigned.",
          call. = FALSE
        )

        out_of_range <- c(out_of_range, i)
      } else {
        # Match to appropriate file, using either the nearest year covered by
        # MODIS if the data's year is outside MODIS coverage, or the data's
        # year, and the site's coordinates.
        suppressWarnings(
          if (!(j %in% modis_files$year)) {
            modis_match[
              modis_match$SurveyAreaIdentifier == i &
                modis_match$survey_year == j,
              "filename"
            ] <- modis_files$filename[
              modis_files$year ==
                unique(modis_files$year)[which(
                  abs(j - unique(modis_files$year)) ==
                    abs(min(j - unique(modis_files$year)))
                )] &
                modis_files$xmin < tmp$X &
                modis_files$xmax > tmp$X &
                modis_files$ymin < tmp$Y &
                modis_files$ymax > tmp$Y
            ]
          } else {
            modis_match[
              modis_match$SurveyAreaIdentifier == i &
                modis_match$survey_year == j,
              "filename"
            ] <- modis_files$filename[
              modis_files$year == tmp$survey_year &
                modis_files$xmin < tmp$X &
                modis_files$xmax > tmp$X &
                modis_files$ymin < tmp$Y &
                modis_files$ymax > tmp$Y
            ]
          }
        )
      }
    }

    rm(tmp)
  }

  # Create object with parseable names for MODIS classes. Transcribed from
  # documentation at
  # https://lpdaac.usgs.gov/documents/101/MCD12_User_Guide_V6.pdf where
  # class definitions are also available. NOTE: might be worth transcribing
  # these into an object within NatureCounts.
  modis_classes <- list(
    modis_lctype1 = data.frame(
      class = c(1:17, 255),
      name = c(
        "evergreen_needleleaf_forests",
        "evergreen_broadleaf_forests",
        "decidious_needleleaf_forests",
        "deciduous_broadleaf_forests",
        "mixed_forests",
        "closed_shrublands",
        "open_shrublands",
        "woody_savannas",
        "savannas",
        "grasslands",
        "permanent_wetlands",
        "croplands",
        "urban_builtup_lands",
        "cropland_natural_vegetation_mosaic",
        "permanent_snow_ice",
        "barren",
        "water_bodies",
        "unclassified"
      )
    ),
    modis_lctype2 = data.frame(
      class = c(0:15, 255),
      name = c(
        "water_bodies",
        "evergreen_needleleaf_forests",
        "evergreen_broadleaf_forests",
        "deciduous_needleleaf_forests",
        "deciduous_broadleaf_forests",
        "mixed_forests",
        "closed_shrublands",
        "open_shrublands",
        "woody_savannas",
        "savannas",
        "grasslands",
        "permanent_wetlands",
        "croplands",
        "urban_builtup_lands",
        "cropland_natural_vegetation_mosaic",
        "nonvegetated_lands",
        "unclassified"
      )
    ),
    modis_lctype3 = data.frame(
      class = c(0:10, 255),
      name = c(
        "water_bodies",
        "grasslands",
        "shrublands",
        "broadleaf_croplands",
        "savannas",
        "evergreen_broadleaf_forests",
        "deciduous_broadleaf_forests",
        "evergreen_needleleaf_forests",
        "deciduous_needleleaf_forests",
        "nonvegetated_lands",
        "urban_builtup_lands",
        "unclassified"
      )
    ),
    modis_lctype4 = data.frame(
      class = c(0:8, 255),
      name = c(
        "water_bodies",
        "evergreen_needleleaf_vegetation",
        "evergreen_broadleaf_vegetation",
        "deciduous_needleleaf_vegetation",
        "deciduous_broadleaf_vegetation",
        "annual_broadleaf_vegetation",
        "annual_grass_vegetation",
        "nonvegetated_lands",
        "urban_builtup_lands",
        "unclassified"
      )
    ),
    modis_lctype5 = data.frame(
      class = c(0:11, 255),
      name = c(
        "water_bodies",
        "evergreen_needleleaf_trees",
        "evergreen_broadleaf_trees",
        "deciduous_needleleaf_trees",
        "deciduous_broadleaf_trees",
        "shrub",
        "grass",
        "cereal_croplands",
        "broadleaf_croplands",
        "urban_builtup_lands",
        "permanent_snow_ice",
        "barren",
        "unclassified"
      )
    )
  )

  # Open loop going through each requested land cover classification and
  # extracting.
  for (i in grep("modis_lc", covariates, value = TRUE)) {
    # Parse covariate name for layer name used by MODIS data files.
    index <- gsub("modis_lct", "LC_T", i)

    message(paste0(
      "[MODIS Landcover Extraction] calculating MODIS ",
      gsub("_", " ", index),
      "."
    ))

    # Loop through each matched MODIS data file.
    for (j in stats::na.omit(unique(modis_match$filename))) {
      # Create object with all sites that matched to file j.
      pts_to_fill <- data[
        data$SurveyAreaIdentifier %in%
          modis_match$SurveyAreaIdentifier[modis_match$filename == j],
      ]

      # Open the requested layer in file j.
      if (packageVersion("terra") == "1.9.34") {
        md <- FALSE
      } else {
        md <- TRUE
      }
      modis <- terra::rast(j, md = md)[index]

      # Loop through each site matched to file j and extract.
      for (k in unique(pts_to_fill$SurveyAreaIdentifier)) {
        # If buffered, extract using landscapemetrics::calculate_lsm(). If not,
        # extract using terra::extract().
        if (buffered == TRUE) {
          # Create temporary object containing only the buffer for site k.
          tmp <- data %>%
            dplyr::filter(.data$SurveyAreaIdentifier == k) %>%
            dplyr::select("SurveyAreaIdentifier", "geometry") %>%
            dplyr::distinct() %>%
            sf::st_transform(terra::crs(modis)) %>%
            terra::vect()

          # Crop MODIS data file to site k's buffer.
          modis_clip <- terra::crop(modis, tmp)

          # Check if landscapemetrics::calculate_lsm() arguments are stored in
          # ..., if not then set defaults.
          if (
            !hasArg("level") &
              !hasArg("metric") &
              !hasArg("name") &
              !hasArg("type")
          ) {
            # Use landscapemetrics::calculate_lsm() to calculate the proportion
            # of each land cover type present in the cropped raster ("pland").
            modis_lsm <- landscapemetrics::calculate_lsm(
              modis_clip,
              metric = "pland",
              ...
            )
          } else {
            # Use landscapemetrics::calculate_lsm() to calculate requested
            # landscape metrics stored in ...
            modis_lsm <- landscapemetrics::calculate_lsm(
              modis_clip,
              ...
            )
          }

          # Throw error if metrics requested at the patch scale.
          if ("patch" %in% unique(modis_lsm$level)) {
            stop(
              "[MODIS Landcover Extraction] landscape metrics requested at",
              " the patch scale, which is currently incompatible with",
              " landcover_extract(). Consult",
              " landscapemetrics::list_lsm(level = 'patch') to determine",
              " which metrics are patch scale.",
              call. = FALSE
            )
          }

          # Check if metrics at the landscape scale were requested. If so,
          # append metric at site k in the appropriate year to input data.
          if ("landscape" %in% unique(modis_lsm$level)) {
            for (l in unique(modis_lsm$metric[
              modis_lsm$level == "landscape"
            ])) {
              {
                data[
                  data$SurveyAreaIdentifier == k &
                    data$survey_year %in%
                      modis_match$survey_year[modis_match$filename == j],
                  paste0(index, "_", l, "_landscape")
                ] <- modis_lsm$value[
                  modis_lsm$level == "landscape" & modis_lsm$metric == l
                ]
              }
            }
          }

          # Check if metrics at the class scale were requested. If so, loop
          # through each land cover type present in the cropped raster
          # and append proportion at site k in the appropriate year to input
          # data. Create parseable column names using names for each
          # class listed above
          if ("class" %in% unique(modis_lsm$level)) {
            for (l in modis_lsm$metric[modis_lsm$level == "class"]) {
              for (m in modis_lsm$class[
                modis_lsm$level == "class" & modis_lsm$metric == l
              ]) {
                data[
                  data$SurveyAreaIdentifier == k &
                    data$survey_year %in%
                      modis_match$survey_year[modis_match$filename == j],
                  paste0(
                    index,
                    "_",
                    l,
                    "_",
                    modis_classes[[i]]$name[modis_classes[[i]]$class == m]
                  )
                ] <- modis_lsm$value[
                  modis_lsm$level == "class" &
                    modis_lsm$metric == l &
                    modis_lsm$class == m
                ]
              }

              # Check whether any land cover classes were never in the cropped
              # raster. For certain metrics these are true zeros, but would be
              # left out otherwise. Add these columns in with 0 values. If not
              # necessarily a true 0, fill with NA.
              missing_cols <- paste0(
                index,
                "_",
                l,
                "_",
                modis_classes[[i]]$name
              )[
                !(paste0(index, "_", l, "_", modis_classes[[i]]$name) %in%
                  names(data))
              ]
              if (
                l %in%
                  c(
                    "area_cv",
                    "area_mn",
                    "area_sd",
                    "ca",
                    "core_cv",
                    "core_mn",
                    "core_sd",
                    "cpland",
                    "dcad",
                    "dcore_cv",
                    "dcore_mn",
                    "dcore_sd",
                    "ed",
                    "ndca",
                    "np",
                    "pd",
                    "pland",
                    "tca",
                    "te"
                  )
              ) {
                for (m in missing_cols) {
                  data[, m] <- 0
                }

                # Replace NAs present in columns for land cover classes that were
                # found at some sites but not others with the true zeros they
                # represent.
                for (m in paste0(index, "_", l, "_", modis_classes[[i]]$name)) {
                  data[
                    is.na(data[, m] %>% sf::st_drop_geometry()) &
                      !(data$SurveyAreaIdentifier %in% out_of_range),
                    m
                  ] <- 0
                }
              } else {
                for (m in missing_cols) {
                  data[, m] <- NA
                }
              }

              # Reorder columns to match class order provided in MODIS
              # documentation.
              data <- data[, c(
                grep(
                  paste0("_", l, "_"),
                  names(data),
                  value = TRUE,
                  invert = TRUE
                ),
                `if`(
                  "landscape" %in%
                    modis_lsm$level &
                    l %in% modis_lsm$metric[modis_lsm$level == "landscape"],
                  c(
                    paste0(index, "_", l, "_landscape"),
                    paste0(index, "_", l, "_", modis_classes[[i]]$name)
                  ),
                  paste0(index, "_", l, "_", modis_classes[[i]]$name)
                )
              )]
            }
          }

          # Remove false zeroes for any sites that fall outside of data coverage.
          for (l in grep(pattern = index, x = names(data))) {
            data[data$SurveyAreaIdentifier %in% out_of_range, l] <- NA
          }
        } else {
          # Create temporary object containing only the point for site k.
          tmp <- data %>%
            dplyr::filter(.data$SurveyAreaIdentifier == k) %>%
            dplyr::select("SurveyAreaIdentifier", "geometry") %>%
            dplyr::distinct() %>%
            sf::st_transform(terra::crs(modis))

          # Extract point value from MODIS raster. It appears to be possible
          # that a point falls such that it extracts from two raster tiles,
          # so handle that possibility below.
          extr_table <- terra::extract(modis, tmp, fun = unique)[,
            2
          ]

          # Whether only a single value was extracted (class == "integer") or
          # multiple values (else) prepare to pass to input data.
          if (length(extr_table) == 1) {
            extr_table <- extr_table %>%
              as.data.frame()

            names(extr_table) <- "class"

            extr_table <- dplyr::left_join(
              extr_table,
              modis_classes[[i]],
              by = "class"
            )
          } else {
            extr_table <- extr_table %>%
              as.data.frame()

            names(extr_table) <- "class"

            extr_table <- dplyr::left_join(
              extr_table,
              modis_classes[[i]],
              by = "class"
            )
          }

          # Join extracted value to input data. If multiple values were
          # extracted, join the first value in extr_table and warn the user
          # about potential values so they can adjust manually.
          tryCatch(
            data[
              data$SurveyAreaIdentifier == k &
                data$survey_year %in%
                  modis_match$survey_year[modis_match$filename == j],
              paste0(index, "_Class")
            ] <- modis_classes[[i]]$name[
              modis_classes[[i]]$class ==
                terra::extract(modis, tmp, fun = unique)[, 2]
            ],
            warning = function(w) {
              if (
                conditionMessage(w) ==
                  paste0(
                    "longer object length is not a multiple of shorter",
                    "object length"
                  )
              ) {
                warning(
                  paste0(
                    "[MODIS Landcover Extraction] MODIS ",
                    index,
                    ": Site ",
                    k,
                    " in year(s) ",
                    stringr::str_flatten_comma(sort(unique(modis_match$survey_year[
                      modis_match$filename == j
                    ]))),
                    " touches multiple cells. Extraction returned `",
                    suppressWarnings(modis_classes[[i]]$name[
                      modis_classes[[i]]$class ==
                        terra::extract(modis, tmp, fun = unique)[, 2]
                    ]),
                    "` but possible values were `",
                    stringr::str_flatten(extr_table$name, collapse = "`, `"),
                    "`. Please examine to choose desired output and replace if",
                    " necessary."
                  ),
                  call. = FALSE
                )
              } else {
                warning(conditionMessage(w))
              }
            }
          )
        }
      }
    }
  }

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

  # If requested, remove MODIS data files.
  if (retain == FALSE) {
    message(paste0(
      "[MODIS Landcover Extraction] extraction complete. Removing files."
    ))

    file.remove(modis_files$filename)
  }

  # Return input data with appended land cover columns.
  return(data)
}
