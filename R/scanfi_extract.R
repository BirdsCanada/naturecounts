#' Extract Data from the Spatialized Canadian National Forest Inventory (SCANFI)
#'
#' Extracts all available variables from the
#' [SCANFI v2 dataset](https://open.canada.ca/data/en/dataset/07653869-f303-46c2-a04e-9ab479b73cbf).
#' All variables are available in snapshots every 5 years between 1985 and 2025 at
#' a 30 m resolution. Necessary files can be downloaded and loaded with [scanfi_download()].
#'
#' One (or multiple) SCANFI variable(s) can be extracted by specifying the following
#' values to the `covariates` argument:
#' - Forest age (years): `scanfi_age`
#' - Forest biomass (tons/ha): `scanfi_biomass`
#' - Crown closure (% of pixel covered by tree canopy): `scanfi_closure`
#' - Forest height (m): `scanfi_height`
#' - National Forest Inventory land cover (NFILC) class: `scanfi_nfilc`
#' - Balsam Fir cover (% of pixel): `scanfi_balsamfir`
#' - Black Spruce cover (% of pixel): `scanfi_blackspruce`
#' - Douglas Fir cover (% of pixel): `scanfi_douglasfir`
#' - Jack Pine cover (% of pixel): `scanfi_jackpine`
#' - Lodgepole Pine cover (% of pixel): `scanfi_lodgepolepine`
#' - Ponderosa Pine cover (% of pixel): `scanfi_ponderosapine`
#' - Tamarack cover (% of pixel): `scanfi_tamarack`
#' - White and Red Pine cover (% of pixel): `scanfi_whiteredpine`
#' - Broadleaf tree species cover (% of pixel): `scanfi_broadleaf`
#' - Other conifer species cover (% of pixel): `scanfi_otherconifer`
#'
#' @param data A sf` 'POINT' or 'POLYGON' object, or `terra` 'points'
#'   or 'polygons' object containing a column with observation years either named
#'   the BMDE default `survey_year` or another name specified in argument `date_year`.
#' @param scanfi_data Named `list` of `terra SpatRaster`s. First index names should
#'   be the snapshot years contained data is from, and second index names should be
#'   variable names as in [nc_covariate_table()], with the "scanfi_" removed.
#'   We recommend using [scanfi_download()] to ensure that all data necessary to
#'   match your input data are captured and that list formatting is correct.
#'   Direct output of [scanfi_download()] can be supplied here.
#' @param covariates Character, vector if multiple SCANFI data types desired. By
#'   default, downloads SCANFI forest height data.
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
#' @param dl_path Character. Optional argument to provide path to download data
#'   to. By default, data is downloaded to a subfolder `scanfi/` in the working
#'   directory.
#' @param retain Logical. Should SCANFI data files be kept after extraction? If
#'   `FALSE`, files will be deleted.
#'
#' @returns For sf 'POINT' or terra 'points' input data, original data with
#'  column(s) appended containing the SCANFI data value(s) at each point.
#'
#'  For sf 'POLYGON' or terra 'polygons' input data, original data with column(s)
#'   appended containing the mean SCANFI data value(s) within each polygon or,
#'   if NFI Landcover requested, the proportion the polygon area covered by of
#'   each land cover class.
#'
#' @examples
#' # Convert included test data on black-capped chickadees to sf POINT object
#' bcch <- sf::st_as_sf(
#'   bcch,
#'   coords = c("longitude", "latitude"),
#'   crs = 4326
#' )
#'
#' # Download SCANFI data - uses the dates in the data to determine which
#' # snapshot years to download.
#' scanfi <- scanfi_download(data = bcch,
#'                           covariates = "scanfi_ponderosapine",
#'                           progress = FALSE)
#'
#' # Create sf object to use in extraction.
#' bcch <- data_fmt(bcch)
#'
#' # Extract first only for the snapshot years.
#' output <- scanfi_extract(data = bcch,
#'                          scanfi_data = scanfi)
#'
#' # Extract with interpolation for interceding years.
#' output <- scanfi_extract(data = bcch,
#'                          scanfi_data = scanfi,
#'                          interpolate = TRUE)
#'
#' @seealso [scanfi_download()] which can be used to download data from SCANFI
#'   data files and load them into the environment.
#'
#'   [nc_covariates_merge()] to merge extracted
#'   covariate data into data originally provided to the `data` argument of
#'   [data_fmt()].
#'
#' @references Guindon L., Correia D.L.P, Manka F. and Smiley B. 2026. SCANFI v2: Spatialized CAnadian National Forest Inventory data product v2. Natural Resources Canada, Canadian Forest Service, Laurentian Forestry Centre, Quebec, Canada. <https://doi.org/10.23687/07653869-f303-46c2-a04e-9ab479b73cbf>.
#'
#' @export

scanfi_extract <- function(
  data,
  scanfi_data, # named list containing SpatRaster containing
  # SCANFI data, downloadable via scanfi_download(). Names derived from
  # SCANFI variables ("height", "biomass", etc.)
  covariates = "scanfi_height", # Other options listed in nc_covariate_table().
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
  dl_path = NULL, # Path to downloaded files. Only needed if retain = TRUE and
  # custom dl_path is used.
  retain = TRUE # Should data files be kept after extraction?
) {
  # Check packages
  have_pkg_check(c(
    "sf",
    "terra"
  ))

  # If no SCANFI rasters are provided, return error.
  if (missing(scanfi_data)) {
    stop(
      "[SCANFI Extraction] no SCANFI rasters provided to extract from. Please",
      " provide a list containing one entry for every snapshot year, each",
      " containing one raster for each listed SCANFI covariate.",
      " Data can be downloaded using scanfi_download().",
      call. = FALSE
    )
  }

  if (
    !((inherits(scanfi_data, "list")) &
      (inherits(scanfi_data[[1]], "list")) &
      (inherits(scanfi_data[[1]][[1]], "SpatRaster")))
  ) {
    stop(
      "[SCANFI Extraction] no SCANFI rasters provided to extract from. Please",
      " provide a list containing one entry for every snapshot year, each",
      " containing one raster for each listed SCANFI covariate.",
      " Data can be downloaded using scanfi_download().",
      call. = FALSE
    )
  }

  # Grab covariate names from scanfi_data object if not explicitly specified.
  if (missing(covariates)) {
    yrs <- names(scanfi_data)
    layers <- c()

    for (i in yrs) {
      layers <- unique(c(layers, names(scanfi_data[[i]])))
    }

    covariates <- paste0("scanfi_", layers)

    warning(
      "[SCANFI Extraction] no covariates specified in the covariates",
      " argument. Proceeding to extract the covariates found in",
      " scanfi_data layers: ",
      stringr::str_flatten_comma(covariates),
      ".",
      call. = FALSE
    )
  }

  # Catch misspecified covariates. Return error if any exist.
  if (FALSE %in% (covariates %in% nc_covariate_table()$covariate_name)) {
    stop(
      "[SCANFI Extraction] covariates either not listed or one or more are",
      " invalid. Please provide covariate names as listed under",
      " `covariate_name` in nc_covariate_table().",
      call. = FALSE
    )
  }

  # If no SCANFI rasters are provided, return error.
  if (missing(scanfi_data)) {
    stop(
      "[SCANFI Extraction] no SCANFI rasters provided to extract from. Please",
      " provide a list containing one entry for every snapshot year, each",
      " containing one raster for each listed SCANFI covariate.",
      " Data can be downloaded using scanfi_download().",
      call. = FALSE
    )
  }

  if (
    !((inherits(scanfi_data, "list")) &
      (inherits(scanfi_data[[1]], "list")) &
      (inherits(scanfi_data[[1]][[1]], "SpatRaster")))
  ) {
    stop(
      "[SCANFI Extraction] no SCANFI rasters provided to extract from. Please",
      " provide a list containing one entry for every snapshot year, each",
      " containing one raster for each listed SCANFI covariate.",
      " Data can be downloaded using scanfi_download().",
      call. = FALSE
    )
  }

  # Check data is in the desired format.
  input_fmt <- covariate_fmt_check(data)

  # If not an sf or terra object, return error and point towards data_fmt().
  if (input_fmt$type == "data.frame") {
    stop(
      "[SCANFI Extraction] extraction requires an sf or terra object as input in this workflow. Consider using `data_fmt` to conform data first.",
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
      "[SCANFI Extraction] some specified columns missing from the data: ",
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

  # For sf objects, create area of interest to crop SCANFI rasters to to
  # reduce memory load.
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

  # For terra objects, create area of interest to crop SCANFI rasters to to
  # reduce memory load.
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

  if (interpolate == FALSE) {
    match_years <- as.character(sort(unique(data$survey_year[
      data$survey_year %in% as.numeric(names(scanfi_data))
    ])))

    if (length(match_years) == 0) {
      stop(
        "[SCANFI Extraction] Data does not contain observations within the",
        " SCANFI snapshot years (",
        stringr::str_flatten_comma(sort(as.numeric(names(scanfi_data)))),
        ") in scanfi_data.",
        " If wanting to match interceding years to snapshots, use interpolate",
        " = TRUE.",
        call. = FALSE
      )
    }

    closest_year <- data.frame(
      data_year = sort(unique(data$survey_year[
        data$survey_year %in% as.numeric(names(scanfi_data))
      ])),
      scanfi_year = sort(names(scanfi_data))
    )

    closest_year$scanfi_year <- as.character(closest_year$scanfi_year)
  } else {
    closest_year <- data.frame(
      data_year = sort(unique(data$survey_year)),
      scanfi_year = NA
    )

    available_years <- sort(as.numeric(names(scanfi_data)))

    for (i in closest_year$data_year) {
      closest_year$scanfi_year[
        closest_year$data_year == i
      ] <- available_years[which(
        abs(i - available_years) == min(abs(i - available_years))
      )]
    }

    outside_years <- closest_year$data_year[
      abs(closest_year$scanfi_year - closest_year$data_year) > 5
    ]

    if (length(outside_years) > 0) {
      if (TRUE %in% (outside_years %in% 1980:2030)) {
        warning(
          "[SCANFI Download] Data contains years more than 5 years away",
          " from nearest SCANFI snapshot (",
          stringr::str_flatten_comma(outside_years),
          "). No value will be returned for observations in these years.",
          " Nearby (< 5 years away) snapshots are available for some data",
          " years (",
          stringr::str_flatten_comma(outside_years[
            outside_years %in% 1980:2030
          ]),
          "), but were not provided via the scanfi_data argument. These can be",
          " downloaded with scanfi_download().",
          call. = FALSE
        )
      } else {
        warning(
          "[SCANFI Download] Data contains years more than 5 years away",
          " from nearest SCANFI snapshot (",
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

    closest_year$scanfi_year <- as.character(closest_year$scanfi_year)

    match_years <- unique(closest_year$scanfi_year)
  }

  # Fetch index from covariates argument.
  scanfi_vars <- gsub(
    pattern = "scanfi_",
    replacement = "",
    grep("scanfi_", covariates, value = TRUE)
  )

  # Loop through each requested SCANFI variable.
  for (i in scanfi_vars) {
    # Loop through each snapshot year
    for (j in match_years) {
      message("[SCANFI Extraction] extracting SCANFI ", i, ".")

      # If buffered, check for packages necessary in buffered workflow.
      if (buffered == TRUE & i == "nfilc") {
        have_pkg_check("landscapemetrics")
      }

      if (buffered == TRUE & !(i == "nfilc")) {
        have_pkg_check("exactextractr")
      }

      # Check that required raster is available.
      if (is.null(scanfi_data[[j]][[i]])) {
        stop(
          "[SCANFI Extraction] requested covariate raster unavailable for ",
          "SCANFI ",
          i,
          " in snapshot year ",
          j,
          ". This can be downloaded using scanfi_download().",
          call. = FALSE
        )
      }

      terra::terraOptions(progress = 0)

      # Crop SCANFI data to study area.
      scanfi_data[[j]][[i]] <- terra::crop(
        scanfi_data[[j]][[i]],
        terra::project(study_area, terra::crs(scanfi_data[[j]][[i]]))
      )

      # Create filled extent polygon of cropped scanfi layer.

      scanfi_filled <- terra::as.polygons(terra::subst(
        scanfi_data[[j]][[i]],
        from = unname(c(stats::na.omit(unique(terra::values(scanfi_data[[j]][[
          i
        ]]))))),
        to = 1,
        raw = TRUE
      ))

      # Loop through each site and extract.
      for (k in unique(data$SurveyAreaIdentifier)) {
        if (
          (interpolate == FALSE &
            as.numeric(j) %in%
              data$survey_year[data$SurveyAreaIdentifier == k]) |
            interpolate == TRUE
        ) {
          # Create temporary object with only point/buffer for site k.
          tmp <- data %>%
            dplyr::filter(.data$SurveyAreaIdentifier == k) %>%
            dplyr::select("SurveyAreaIdentifier", "geometry") %>%
            dplyr::distinct() %>%
            sf::st_transform(terra::crs(scanfi_data[[j]][[i]]))

          # Check if the site out of or only partially covered by the spatial
          # extent of the provided SCANFI data. Warn if so.
          if (
            !terra::is.related(scanfi_filled, terra::vect(tmp), "contains") &
              !terra::is.related(scanfi_filled, terra::vect(tmp), "overlaps")
          ) {
            warning(
              "[SCANFI (",
              i,
              ") Extraction] site ",
              k,
              " falls outside of the spatial extent of the SCANFI rasters provided.",
              " No value will be returned.",
              call. = FALSE
            )
          } else if (
            !terra::is.related(scanfi_filled, terra::vect(tmp), "contains") &
              terra::is.related(scanfi_filled, terra::vect(tmp), "overlaps")
          ) {
            warning(
              "[SCANFI (",
              i,
              ") Extraction] site ",
              k,
              "'s buffered area is only partially contained by the spatial extent",
              " of the SCANFI rasters provided. Returned ",
              i,
              " value will be derived from the available values.",
              call. = FALSE
            )
          } else {
            # If no issues with coverage, proceed to extract. For NFI Land Cover,
            # extract with landscapemetrics::calculate_lsm() if buffered and with
            # terra::extract() if not. Otherwise, extract with
            # exactextractr::exact_extract() if buffered, and terra::extract()
            # if not.
            if (i == "nfilc") {
              # Create object containing parseable names for NFI Land Cover classes.
              nfilc_classes <- data.frame(
                class = c(1:8),
                name = c(
                  "bryoid",
                  "herbs",
                  "rock",
                  "shrub",
                  "treed_broadleaf",
                  "treed_conifer",
                  "treed_mixed",
                  "water"
                )
              )

              # If buffered, extract with landscapemetrics::calculate_lsm().
              if (buffered == TRUE) {
                # Convert temporary object to SpatVector to use with terra:crop()
                tmp <- tmp %>%
                  terra::vect()

                # Crop SCANFI data to site buffer.
                scanfi_clip <- terra::crop(scanfi_data[[j]][[i]], tmp)

                # Use landscapemetrics::calculate_lsm() to calculate the proportion
                # of each land cover type present in the cropped raster ("pland").
                scanfi_pland <- landscapemetrics::calculate_lsm(
                  scanfi_clip,
                  metric = "pland"
                )

                # Loop through each land cover type present in the cropped raster
                # and append proportion at site k to input data. Create parseable
                # column names using names for each class listed above.
                for (l in scanfi_pland$class) {
                  data[
                    data$SurveyAreaIdentifier == k &
                      data$survey_year %in%
                        closest_year$data_year[closest_year$scanfi_year == j],
                    paste0(
                      "nfilc_",
                      nfilc_classes$name[nfilc_classes$class == l]
                    )
                  ] <- scanfi_pland$value[scanfi_pland$class == l]
                }

                # Check whether any land cover classes were never in the cropped
                # raster. These are true zeros, but would be left out otherwise.
                # Add these columns in with 0 values.
                missing_cols <- paste0("nfilc_", nfilc_classes$name)[
                  !(paste0("nfilc_", nfilc_classes$name) %in% names(data))
                ]

                for (l in missing_cols) {
                  data[
                    data$survey_year %in%
                      closest_year$data_year[closest_year$scanfi_year == j],
                    l
                  ] <- 0
                }

                # Replace NAs present in columns for land cover classes that were
                # found at some sites but not others with the true zeros they
                # represent.
                for (l in paste0(
                  "nfilc_",
                  nfilc_classes$name[
                    paste0("nfilc_", nfilc_classes$name) %in% names(data)
                  ]
                )) {
                  data[
                    is.na(data[, l] %>% sf::st_drop_geometry()) &
                      data$survey_year %in%
                        closest_year$data_year[closest_year$scanfi_year == j],
                    l
                  ] <- 0
                }

                # Reorder columns to match class order provided in NFILC
                # documentation.
                data <- data[, c(
                  grep("nfilc_", names(data), value = TRUE, invert = TRUE),
                  paste0("nfilc_", nfilc_classes$name)
                )]
              } else {
                # Extract point value from SCANFI raster. It appears to be possible
                # that a point falls such that it extracts from two raster tiles,
                # so handle that possibility below.
                extr_table <- terra::extract(
                  scanfi_data[[j]][[i]],
                  tmp,
                  fun = unique
                )[, 2]

                # Whether only a single value was extracted (class == "integer") or
                # multiple values (else) prepare to pass to input data.
                if (inherits(extr_table, "integer")) {
                  extr_table <- extr_table %>%
                    as.data.frame()

                  names(extr_table) <- "class"

                  extr_table <- dplyr::left_join(
                    extr_table,
                    nfilc_classes,
                    by = "class"
                  )
                } else {
                  extr_table <- extr_table %>%
                    as.data.frame() %>%
                    dplyr::select(terra::names(scanfi_data[[j]][[i]]))

                  names(extr_table) <- "class"

                  extr_table <- dplyr::left_join(
                    extr_table,
                    nfilc_classes,
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
                        closest_year$data_year[closest_year$scanfi_year == j],
                    "nfilc_class"
                  ] <- nfilc_classes$name[
                    nfilc_classes$class ==
                      terra::extract(scanfi_data[[j]][[i]], tmp, fun = unique)[,
                        terra::names(scanfi_data[[j]][[i]])
                      ]
                  ],
                  warning = function(w) {
                    if (
                      conditionMessage(w) ==
                        paste0(
                          "longer object length is not a multiple of shorter",
                          " object length"
                        )
                    ) {
                      warning(paste0(
                        "[SCANFI (",
                        i,
                        ") Extraction] site ",
                        k,
                        " touches multiple cells. Extraction returned `",
                        suppressWarnings(nfilc_classes$name[
                          nfilc_classes$class ==
                            terra::extract(
                              scanfi_data[[j]][[i]],
                              tmp,
                              fun = unique
                            )[,
                              terra::names(scanfi_data[[j]][[i]])
                            ]
                        ]),
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
            } else {
              # For other SCANFI variables, if buffered, extract using
              # exactextractr::exact_extract(). If not, extract using
              # terra::extract().
              if (buffered == TRUE) {
                data[
                  data$SurveyAreaIdentifier == k &
                    data$survey_year %in%
                      closest_year$data_year[closest_year$scanfi_year == j],
                  paste0("scanfi_", i)
                ] <- exactextractr::exact_extract(
                  x = scanfi_data[[j]][[i]],
                  y = tmp,
                  fun = "mean"
                )
              } else {
                data[
                  data$SurveyAreaIdentifier == k &
                    data$survey_year %in%
                      closest_year$data_year[closest_year$scanfi_year == j],
                  paste0("scanfi_", i)
                ] <- terra::extract(
                  x = scanfi_data[[j]][[i]],
                  y = tmp,
                  fun = "mean",
                  na.rm = TRUE
                )[, 2]
              }
            }
          }
        }
      }
    }
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

  # Remove SCANFI files if requested.
  if (retain == FALSE) {
    # Check that if default directory doesn't exist an alterate has been
    # specified.
    if (is.null(dl_path) & !dir.exists("./scanfi")) {
      warning(
        "[SCANFI Extraction] unable to find default SCANFI",
        " directory and no alternate specified using dl_path argument",
        ". No files will be removed.",
        call. = FALSE
      )
    } else {
      message(paste0("[SCANFI Extraction] task complete. Removing files."))

      file.remove(list.files(
        ifelse(is.null(dl_path), "./scanfi", paste0(dl_path, "/scanfi")),
        full.names = TRUE
      ))
    }
  }

  # Return input data with appended SCANFI columns.
  return(data)
}
