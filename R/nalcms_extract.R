#' Extract Landcover Data from the North American Land Change Monitoring System
#'
#' Extracts landcover data from the North American Land Change Monitoring
#' System (NALCMS) via the [North American Environmental Atlas](https://www.cec.org/north-american-environmental-atlas/).
#' Landcover data are available in snapshots every 5 years between 2010 and 2020 at
#' a 30 m resolution covering Canada, the United States, and Mexico. Necessary
#' files can be downloaded and loaded with [nalcms_download()].
#'
#' With `sf` 'POLYGON' or `terra` polygons' input data, the proportion of
#' polygon area covered by each MODIS Landcover class (`pland`) is returned by default.
#' Other summary metrics can be requested by specifying
#' the `level`, `class`, `metric`, or `name` arguments, which are passed to
#' [landscapemetrics::calculate_lsm()]. See [landscapemetrics::list_lsm()] for
#' metric options. At this time, only metrics at the `landscape` or `class`
#' level are accepted.
#'
#' @param data A `sf` 'POINT' or 'POLYGON' object, or `terra` 'points'
#'   or 'polygons' object containing a column with observation years either named
#'   the BMDE default `survey_year` or another name specified in argument `date_year`.
#' @param nalcms_files Character, vector if multiple files. File-path(s) to
#'   downloaded NALCMS landcover data file(s). We recommend using
#'   [nalcms_download()] to download MODIS files to ensure all files
#'   necessary for your data are captured. Direct output of
#'   [nalcms_download()] can be supplied here.
#' @param interpolate Logical. Should years in between snapshots be assigned the
#'   nearest snapshot's value?
#' @param site_name Character. Optional argument to provide the name of the
#'   column containing site names if not contained within the BMDE column
#'   `SurveyAreaIdentifier`. Can be left `NULL` and still function properly if
#'   originally specified in a call to [data_fmt()].
#' @param date_year Character. Optional argument to provide the name of the
#'   column containing year data if not contained within the BMDE column
#'   `survey_year`. Can be left `NULL` and still function properly if originally
#'   specified in a call to [data_fmt()].
#' @param retain Logical. Should NALCMS data files be kept after extraction? If
#'   `FALSE`, files will be deleted.
#' @param ... Other arguments passed to [landscapemetrics::calculate_lsm()] for
#'   `sf` 'POLYGON' or `terra` 'polygons' input data. Primarily useful for
#'   specifying metrics other than the proportional cover of each landcover
#'   class. See [landscapemetrics::list_lsm()] for other options.
#'
#' @returns For `sf` 'POINT' or `terra` 'points' input data, original data with
#'   a character column `nalcms_class` appended containing the name of the
#'   landcover class that point falls within.
#'
#'   For `sf` 'POLYGON' or `terra` 'polygons' input data, original data with
#'   numeric columns containing the requested landscape metrics (see
#'   [landscapemetrics::list_lsm()] for options). By default, returns columns
#'   containing the proportion of each polygon that is covered
#'   by each landcover type.
#'
#' @examples
#' # Convert included test data on black-capped chickadees to sf POINT object
#' bcch <- sf::st_as_sf(
#'   bcch,
#'   coords = c("longitude", "latitude"),
#'   crs = 4326
#' )
#'
#' # Download NALCMS Landcover data - uses the dates in the data to determine which
#' # snapshot years and countries to download.
#' nalcms_files <- nalcms_download(data = bcch,
#'                           progress = FALSE)
#'
#' # Extract first only for the snapshot years.
#' output <- nalcms_extract(data = bcch,
#'                          nalcms_files = nalcms_files)
#'
#' # Extract with interpolation for interceding years.
#' output <- nalcms_extract(data = bcch,
#'                          nalcms_files = nalcms_files,
#'                          interpolate = TRUE)
#'
#' @seealso [nalcms_download()] which can be used to download NALCMS data.
#'
#'   [nc_covariates_merge()] to merge extracted
#'   covariate data into data originally provided to the `data` argument of
#'   [data_fmt()].
#'
#' @export
nalcms_extract <- function(
  data,
  nalcms_files, # Filepaths of NALCMS rasters.
  interpolate = FALSE, # should years between snapshots be filled with values
  # from nearest snapshot.
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
    "terra",
    "spData"
  ))

  # If no NALCMS rasters are provided, return error.
  if (missing(nalcms_files)) {
    stop(
      "[NALCMS Extraction] no filepaths to NALCMS rasters provided to extract",
      " from. Please provide a vector of filepaths to NALCMS rasters.",
      " Data can be downloaded using nalcms_download().",
      call. = FALSE
    )
  }

  if (!inherits(nalcms_files, "character")) {
    stop(
      "[NALCMS Extraction] no filepaths to NALCMS rasters provided to extract",
      " from. Please provide a vector of filepaths to NALCMS rasters.",
      " Data can be downloaded using nalcms_download().",
      call. = FALSE
    )
  }

  # Check that all provided files exist.
  if (!all(file.exists(nalcms_files))) {
    stop(
      "[NALCMS Extraction] some provided filepaths point to files that do",
      " not exist.",
      call. = FALSE
    )
  }

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
      "[NALCMS Extraction] some specified columns missing from the data: ",
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

  if (input_fmt$type == "sf") {
    # Check whether sf object is buffered or not to determine extraction
    # procedure down the line.
    buffered <- ifelse(input_fmt$geometry == "POINT", FALSE, TRUE)
  }

  if (input_fmt$type == "terra") {
    # Check whether terra object is buffered or not to determine extraction
    # procedure down the line.
    buffered <- ifelse(input_fmt$geometry == "points", FALSE, TRUE)

    # Convert to sf object for use in workflow.
    data <- sf::st_as_sf(data)
  }

  # Get years and countries available in NALCMS data
  nalcms_files <- data.frame(filename = nalcms_files) %>%
    dplyr::mutate(
      year = dplyr::case_when(
        stringr::str_detect(filename, "2010v3_30m.tif") ~ 2010,
        stringr::str_detect(nalcms_files, "2015v4_30m.tif") ~ 2015,
        stringr::str_detect(nalcms_files, "2020v2_30m.tif") ~ 2020
      ),
      country = substr(
        filename,
        start = stringr::str_locate(filename, "data/")[, "end"] + 1,
        stop = stringr::str_locate(filename, "data/")[, "end"] + 3
      )
    )

  # Match observations in the data to the NALCMS snapshot year data should be
  # extracted from.
  if (interpolate == FALSE) {
    # If interpolate = FALSE, check that some observations have been made in
    # the snapshot years.
    if (!any(c(2010, 2015, 2020) %in% data$survey_year)) {
      stop(
        "[NALCMS Landcover Extraction] Data provided to data argument does not contain",
        " observations within the NALCMS snapshot years (2010, 2015, 2020).",
        " If wanting to match interceding years to snapshots, use interpolate",
        " = TRUE.",
        call. = FALSE
      )
    }
    # For non-interpolated data, build object only containing NALCMS snapshot \
    # years which observations are made in.
    closest_year <- data.frame(
      data_year = sort(unique(data$survey_year[
        data$survey_year %in% unique(nalcms_files$year)
      ]))
    )

    closest_year$nalcms_year <- closest_year$data_year
  } else {
    # For interpolated data, match years in data to closest NALCMS snapshot,
    # within 5 years.
    closest_year <- data.frame(
      data_year = sort(unique(data$survey_year)),
      nalcms_year = NA
    )

    for (i in closest_year$data_year) {
      closest_year$nalcms_year[
        closest_year$data_year == i
      ] <- unique(nalcms_files$year)[which(
        abs(i - unique(nalcms_files$year)) ==
          min(abs(i - unique(nalcms_files$year)))
      )]
    }

    outside_years <- closest_year$data_year[
      abs(closest_year$nalcms_year - closest_year$data_year) > 5
    ]

    if (length(outside_years) > 0) {
      # If interpolate = TRUE, check that some observations have been made within
      # 5 years of a snapshot year
      if (all(unique(closest_year$data_year) %in% outside_years)) {
        stop(
          "[NALCMS Landcover Extraction] Data provided to data argument does not contain",
          " observations within 5 years of the NALCMS snapshot years (2010, 2015, 2020).",
          call. = FALSE
        )
      }

      if (any(outside_years %in% 2005:2025)) {
        warning(
          "[NALCMS Landcover Extraction] Data contains years more than 5 years away",
          " from nearest NALCMS snapshot (",
          stringr::str_flatten_comma(outside_years),
          "). No value will be returned for observations in these years.",
          " Nearby (< 5 years away) snapshots are available for data",
          " years (",
          stringr::str_flatten_comma(outside_years[
            outside_years %in% 2005:2025
          ]),
          "), but were not provided via the nalcms_files argument. These can be",
          " downloaded with nalcms_download().",
          call. = FALSE
        )
      } else {
        warning(
          "[NALCMS Landcover Extraction] Data contains years more than 5 years away",
          " from nearest NALCMS snapshot (",
          stringr::str_flatten_comma(outside_years),
          "). No value will be returned for observations in these years.",
          call. = FALSE
        )
      }
    }

    closest_year <- dplyr::filter(
      closest_year,
      !(.data$data_year %in% outside_years)
    )
  }

  # Assign country each data geometry stems from.

  # In case a column named country exists in the data, store it as not to
  # overwrite it.
  if ("country" %in% data_cols) {
    country_storage <- data$country
  }

  if ("relationship" %in% data_cols) {
    relationship_storage <- data$relationship
  }

  # Loop through each geometry and identify country of origin.
  # Read in raster files to compare sites to.
  nalcms_compare <- list()

  for (i in unique(nalcms_files$country)) {
    # Read in the first file available for each country.
    nalcms_compare[[i]] <- terra::rast(nalcms_files$filename[
      nalcms_files$country == i &
        nalcms_files$year ==
          unique(nalcms_files$year[nalcms_files$country == i])[1]
    ])
  }

  for (i in unique(data$SurveyAreaIdentifier)) {
    tmp <- terra::vect(data[data$SurveyAreaIdentifier == i, ]) %>%
      terra::project(terra::crs(nalcms_compare[[1]]))

    countries <- c()

    for (j in names(nalcms_compare)) {
      extracted <- unique(terra::extract(
        nalcms_compare[[j]],
        tmp,
        fun = max,
        raw = TRUE
      )[, terra::names(nalcms_compare[[j]])])

      if (
        !(all(extracted == 0)) &
          !(all(is.nan(extracted)))
      ) {
        countries <- c(countries, j)
      }
    }

    if (rlang::is_empty(countries)) {
      data$country[data$SurveyAreaIdentifier == i] <- "NONE"
    } else {
      data$country[
        data$SurveyAreaIdentifier == i
      ] <- stringr::str_flatten_comma(countries)
    }

    if (buffered == TRUE) {
      world <- sf::st_read(
        system.file("shapes/world.gpkg", package = "spData"),
        quiet = TRUE
      ) %>%
        terra::vect()

      tmp <- terra::project(tmp, terra::crs(world))

      world <- suppressWarnings(terra::intersect(world, tmp))

      countries <- unique(world$name_long)

      if (rlang::is_empty(countries)) {
        data$relationship[data$SurveyAreaIdentifier == i] <- "out"
      } else if (
        !terra::is.related(
          terra::buffer(terra::aggregate(terra::union(world)), 1),
          tmp,
          "contains"
        ) &
          terra::is.related(
            terra::buffer(terra::aggregate(terra::union(world)), 1),
            tmp,
            "overlaps"
          )
      ) {
        data$relationship[data$SurveyAreaIdentifier == i] <- "overlap"
      } else {
        data$relationship[data$SurveyAreaIdentifier == i] <- "in"
      }
    }
  }

  rm(nalcms_compare)

  # If any observations lack an associated country, this indicates they are
  # not within the land polygons of Canada, the US, or Mexico. Warn that these
  # sites will receive no data.

  if ("NONE" %in% unique(data$country)) {
    # Make sure that warning is only given if it will impact sites that would
    # be given data anyway (i.e., don't warn for sites not in snapshot years
    # if interpolate = FALSE).
    if (interpolate == FALSE) {
      if (
        any(data$survey_year[data$country == "NONE"] %in% c(2010, 2015, 2020))
      ) {
        warning(
          "[NALCMS Landcover Extraction] site(s) ",
          stringr::str_flatten_comma(unique(data$SurveyAreaIdentifier[
            data$country == "NONE" & data$survey_year %in% c(2010, 2015, 2020)
          ])),
          " fall outside of the spatial extent of the NALCMS rasters provided.",
          " No value will be returned.",
          call. = FALSE
        )
      }
    } else {
      warning(
        "[NALCMS Landcover Extraction] site(s) ",
        stringr::str_flatten_comma(unique(data$SurveyAreaIdentifier[
          data$country == "NONE"
        ])),
        " fall outside of the spatial extent of the NALCMS rasters provided.",
        " No value will be returned.",
        call. = FALSE
      )
    }
  }

  # If any observations are only overlapping a country, not totally contained
  # by it, warn.

  if ("overlap" %in% data$relationship) {
    # Make sure that warning is only given if it will impact sites that would
    # be given data anyway (i.e., don't warn for sites not in snapshot years
    # if interpolate = FALSE).
    if (interpolate == FALSE) {
      if (
        any(
          data$survey_year[data$relationship == "overlap"] %in%
            c(2010, 2015, 2020)
        )
      ) {
        warning(
          "[NALCMS Landcover Extraction] site(s) ",
          stringr::str_flatten_comma(unique(data$SurveyAreaIdentifier[
            data$relationship == "overlap" &
              data$survey_year %in% c(2010, 2015, 2020)
          ])),
          " fall outside of the spatial extent of the NALCMS rasters provided.",
          " No value will be returned.",
          call. = FALSE
        )
      }
    } else {
      warning(
        "[NALCMS Landcover Extraction] site(s) ",
        stringr::str_flatten_comma(unique(data$SurveyAreaIdentifier[
          data$relationship == "overlap"
        ])),
        " buffered area(s) are only partially contained by the spatial extent",
        " of the NALCMS rasters provided. Returned proportional coverage",
        " values will be derived from the available values.",
        call. = FALSE
      )
    }
  }

  # If buffered, check for packages necessary in buffered workflow.
  if (buffered == TRUE) {
    have_pkg_check("landscapemetrics")
  }

  # Loop through all (combinations of) countries that observations fall into.
  for (i in unique(data$country)[!(unique(data$country) == "NONE")]) {
    # As polygon input data may overlap multiple countries, detect all countries
    # within the data$country string for this loop iteration. For polygon input
    # countries may be a character vector of > 1 length, for point input data
    # countries will be a single character value equal to i, as point input
    # cannot overlap multiple countries.
    countries <- `if`(
      stringr::str_detect(i, ","),
      stringr::str_split_1(i, pattern = ", "),
      i
    )

    # Grab input data that falls within appropriate country.
    country_data <- dplyr::filter(
      data,
      country == i
    )

    # Grab needed years for each country.
    country_years <- sort(unique(closest_year$nalcms_year[
      closest_year$data_year %in% unique(country_data$survey_year)
    ]))

    # Loop along needed years to load in appropriate data and extract.
    for (j in country_years) {
      # Open list to store NALCMS rasters.
      nalcms <- list()

      # Loop through all needed countries and read in raster data.
      for (k in countries) {
        # Check that file at filepath actually exists.
        if (
          FALSE %in%
            file.exists(nalcms_files$filename[
              nalcms_files$country == k & nalcms_files$year == j
            ])
        ) {
          stop(
            "[NALCMS Landcover Extraction] file at ",
            nalcms_files$filename[
              nalcms_files$country == k & nalcms_files$year == j
            ][
              !file.exists(nalcms_files$filename[
                nalcms_files$country == k & nalcms_files$year == j
              ])
            ],
            " could not be found. Does it exist? Is the filepath correct?",
            call. = FALSE
          )
        }

        # Read in appropriate data file.
        nalcms[[k]] <- terra::rast(
          nalcms_files$filename[
            nalcms_files$year == j & nalcms_files$country == k
          ]
        )
      }

      # Loop through each site and extract.
      for (k in unique(country_data$SurveyAreaIdentifier)) {
        if (
          (interpolate == FALSE &
            j %in%
              data$survey_year[data$SurveyAreaIdentifier == k]) |
            interpolate == TRUE
        ) {
          # Create temporary object with only point/buffer for site k.
          tmp <- data %>%
            dplyr::filter(.data$SurveyAreaIdentifier == k) %>%
            dplyr::select("SurveyAreaIdentifier", "country", "geometry") %>%
            dplyr::distinct() %>%
            sf::st_transform(terra::crs(nalcms[[1]]))

          # Build table containing parseable names for NALCMS classes.
          nalcms_classes <- data.frame(
            class = c(1:19),
            name = c(
              "temperate_subpolar_needleleaf_forest",
              "subpolar_taiga_needleleaf_forest",
              "tropical_subtropical_broadleaf_evergreen_forest",
              "tropical_subtropical_broadleaf_deciduous_forest",
              "temperate_subpolar_broadleaf_deciduous_forest",
              "mixed_forest",
              "tropical_subtropical_shrubland",
              "temperate_subpolar_shrubland",
              "tropical_subtropical_grassland",
              "temperate_subpolar_grassland",
              "subpolar_polar_shrubland_lichen_moss",
              "subpolar_polar_grassland_lichen_moss",
              "subpolar_polar_barren_lichen_moss",
              "wetland",
              "cropland",
              "barren_lands",
              "urban_built_up",
              "water",
              "snow_ice"
            )
          )

          # For buffered input data, crop NALCMS snapshots to the area of tmp
          # and extract using landscapemetrics::calculate_lsm().
          if (buffered == TRUE) {
            # Open list to store cropped NALCMS rasters for each country needed.
            nalcms_clip <- list()

            for (l in countries) {
              # For all needed data, crop to site area.
              nalcms_clip[[l]] <- terra::crop(nalcms[[l]], tmp) %>%
                terra::trim(value = 0) %>%
                terra::subst(0, NA, raw = TRUE)
            }

            # Turn all list elements into a SpatRasterCollection and merge.
            nalcms_clip <- terra::sprc(nalcms_clip)
            nalcms_clip <- terra::merge(nalcms_clip, na.rm = TRUE)

            # Check if landscapemetrics::calculate_lsm() arguments are stored in
            # ..., if not then set defaults.
            if (
              !hasArg("level") &
                !hasArg("metric") &
                !hasArg("name") &
                !hasArg("type")
            ) {
              # Ensure that we only grab arguments from ... that are
              # arguments for landscapemetrics::calculate_lsm().
              args <- list(...)[
                names(list(...)) %in%
                  names(formals(landscapemetrics::calculate_lsm))
              ]
              args$landscape <- nalcms_clip
              args$metric <- "pland"

              # Use landscapemetrics::calculate_lsm() to calculate the proportion
              # of each land cover type present in the cropped raster ("pland").
              nalcms_lsm <- do.call(landscapemetrics::calculate_lsm, args)
            } else {
              # Ensure that we only grab arguments from ... that are
              # arguments for landscapemetrics::calculate_lsm().
              args <- list(...)[
                names(list(...)) %in%
                  names(formals(landscapemetrics::calculate_lsm))
              ]
              args$landscape <- nalcms_clip

              # Use landscapemetrics::calculate_lsm() to calculate requested
              # landscape metrics stored in ...
              nalcms_lsm <- do.call(landscapemetrics::calculate_lsm, args)
            }

            # Throw error if metrics requested at the patch scale.
            if ("patch" %in% unique(nalcms_lsm$level)) {
              stop(
                "[NALCMS Landcover Extraction] landscape metrics requested at",
                " the patch scale, which is currently incompatible with",
                " nalcms_extract(). Consult",
                " landscapemetrics::list_lsm(level = 'patch') to determine",
                " which metrics are patch scale.",
                call. = FALSE
              )
            }

            # Check if metrics at the landscape scale were requested. If so,
            # append metric at site k in the appropriate year to input data.
            if ("landscape" %in% unique(nalcms_lsm$level)) {
              for (l in unique(nalcms_lsm$metric[
                nalcms_lsm$level == "landscape"
              ])) {
                {
                  data[
                    data$SurveyAreaIdentifier == k &
                      data$survey_year %in%
                        closest_year$data_year[
                          closest_year$nalcms_year == j
                        ],
                    paste0("nalcms_", l, "_landscape")
                  ] <- nalcms_lsm$value[
                    nalcms_lsm$level == "landscape" & nalcms_lsm$metric == l
                  ]
                }
              }
            }

            # Check if metrics at the class scale were requested. If so, loop
            # through each land cover type present in the cropped raster
            # and append proportion at site k in the appropriate year to input
            # data. Create parseable column names using names for each
            # class listed above
            if ("class" %in% unique(nalcms_lsm$level)) {
              for (l in unique(nalcms_lsm$metric[
                nalcms_lsm$level == "class"
              ])) {
                for (m in nalcms_lsm$class[
                  nalcms_lsm$level == "class" & nalcms_lsm$metric == l
                ]) {
                  data[
                    data$SurveyAreaIdentifier == k &
                      data$survey_year %in%
                        closest_year$data_year[
                          closest_year$nalcms_year == j
                        ],
                    paste0(
                      "nalcms_",
                      l,
                      "_",
                      nalcms_classes$name[nalcms_classes$class == m]
                    )
                  ] <- nalcms_lsm$value[
                    nalcms_lsm$level == "class" &
                      nalcms_lsm$metric == l &
                      nalcms_lsm$class == m
                  ]
                }

                # Check whether any land cover classes were never in the cropped
                # raster. For certain metrics these are true zeros, but would be
                # left out otherwise. Add these columns in with 0 values. If not
                # necessarily a true 0, fill with NA.
                missing_cols <- paste0(
                  "nalcms_",
                  l,
                  "_",
                  nalcms_classes$name
                )[
                  !(paste0("nalcms_", l, "_", nalcms_classes$name) %in%
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
                    data[
                      !(data$country == "NONE") &
                        data$survey_year %in%
                          closest_year$data_year[
                            closest_year$nalcms_year == j
                          ],
                      m
                    ] <- 0
                  }

                  # Replace NAs present in columns for land cover classes that were
                  # found at some sites but not others with the true zeros they
                  # represent.
                  for (m in paste0(
                    "nalcms_",
                    l,
                    "_",
                    nalcms_classes$name
                  )) {
                    data[
                      is.na(data[, m] %>% sf::st_drop_geometry()) &
                        !(data$country == "NONE") &
                        data$survey_year %in%
                          closest_year$data_year[
                            closest_year$nalcms_year == j
                          ],
                      m
                    ] <- 0
                  }
                } else {
                  for (m in missing_cols) {
                    data[, m] <- NA
                  }
                }

                # Reorder columns to match class order provided in NALCMS
                # documentation.
                data <- data[, c(
                  grep(
                    paste0(l, "_"),
                    names(data),
                    value = TRUE,
                    invert = TRUE
                  ),
                  `if`(
                    "landscape" %in%
                      nalcms_lsm$level &
                      l %in%
                        nalcms_lsm$metric[nalcms_lsm$level == "landscape"],
                    c(
                      paste0("nalcms_", l, "_landscape"),
                      paste0("nalcms_", l, "_", nalcms_classes$name)
                    ),
                    paste0("nalcms_", l, "_", nalcms_classes$name)
                  )
                )]
              }
            }
          } else {
            # Extract point value from NALCMS raster. It appears to be possible
            # that a point falls such that it extracts from two raster tiles,
            # so handle that possibility below.
            extr_table <- terra::extract(
              nalcms[[countries]],
              tmp,
              fun = unique,
              raw = TRUE
            )[, terra::names(nalcms[[countries]])]

            # Catch sites near the border that fall outside of data coverage.
            if (extr_table == 0 & j == country_years[1]) {
              warning(
                "[NALCMS Landcover Extraction] landcover data is",
                " unavailable at site ",
                k,
                ". This can occur if points are particularly close to",
                " borders between countries. Buffering your input data by ",
                "some small distance using data_buff() and extracting the",
                " NALCMS values in that area may provide a solution.",
                call. = FALSE
              )
            } else {
              # Whether only a single value was extracted (class == "integer") or
              # multiple values (else) prepare to pass to input data.
              if (inherits(extr_table, "numeric")) {
                extr_table <- extr_table %>%
                  as.data.frame()

                names(extr_table) <- "class"

                extr_table <- dplyr::left_join(
                  extr_table,
                  nalcms_classes,
                  by = "class"
                )
              } else {
                extr_table <- extr_table %>%
                  as.data.frame() %>%
                  dplyr::select(terra::names(nalcms[[countries]]))

                names(extr_table) <- "class"

                extr_table <- dplyr::left_join(
                  extr_table,
                  nalcms_classes,
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
                      closest_year$data_year[
                        closest_year$nalcms_year == j
                      ],
                  "nalcms_class"
                ] <- nalcms_classes$name[
                  nalcms_classes$class ==
                    terra::extract(
                      nalcms[[countries]],
                      tmp,
                      fun = unique,
                      raw = TRUE
                    )[,
                      terra::names(nalcms[[countries]])
                    ]
                ],
                # },
                warning = function(w) {
                  if (
                    conditionMessage(w) ==
                      paste0(
                        "longer object length is not a multiple of shorter",
                        " object length"
                      )
                  ) {
                    warning(paste0(
                      "[NALCMS Landcover Extraction] site ",
                      k,
                      " touches multiple cells. Extraction returned `",
                      suppressWarnings(
                        nalcms_classes$name[
                          nalcms_classes$class ==
                            terra::extract(
                              nalcms[[countries]],
                              tmp,
                              fun = unique,
                              raw = TRUE
                            )[,
                              terra::names(nalcms[[countries]])
                            ]
                        ]
                      ),
                      "` but possible values were `",
                      stringr::str_flatten(
                        extr_table$name,
                        collapse = "`, `"
                      ),
                      "`. Please examine to choose desired output and replace if",
                      " necessary.",
                      call. = FALSE
                    ))
                  } else {
                    warning(conditionMessage(w), call. = FALSE)
                  }
                }
              )
            }
          }
        }
      }
    }
  }

  # Reinstate original SurveyAreaIdentifiers if dummies needed to be created
  if (exists("SAI_storage")) {
    data$SurveyAreaIdentifier <- SAI_storage
  }

  # If there was no country column initially, remove it.
  if (!("country" %in% data_cols)) {
    data$country <- NULL
  }

  # Reinstate original country column if needed
  if (exists("country_storage")) {
    data$country <- country_storage
  }

  # If there was no relationship column initially, remove it.
  if (!("relationship" %in% data_cols)) {
    data$relationship <- NULL
  }

  # Reinstate original relationship column if needed.
  if (exists("relationship_storage")) {
    data$relationship <- relationship_storage
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

  # If requested, remove NALCMS data files.
  if (retain == FALSE) {
    message(paste0(
      "[NALCMS Landcover Extraction] extraction complete. Removing files."
    ))

    file.remove(nalcms_files$filename)
  }

  # Return input data with appended SCANFI columns.
  return(data)
}
