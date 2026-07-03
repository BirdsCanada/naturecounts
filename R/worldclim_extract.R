#' Extract WorldClim Climate Data
#'
#' Extracts monthly WorldClim Monthly Climate Norms, averaged over 1970-2000,
#' from downloaded [WorldClim version 2.1](https://www.worldclim.org/data/worldclim21.html)
#' (Fick & Hijmans 2017). Several climate variables can be extracted with this
#' functions: minimum, maximum, and average temperature (°C),
#' precipitation (mm), solar radiation (kJ/m^2/day), and wind speed (m/s).
#' Data can be downloaded with [worldclim_download()]
#'
#' One (or multiple) climate variable(s) can be extracted by specifying the following
#' values to the `covariates` argument
#' - Minimum temperature: `worldclim_tmin`
#' - Maximum temperature: `worldclim_tmax`
#' - Average temperature: `worldclim_tavg`
#' - Precipitation: `worldclim_prec`
#' - Solar radiation: `wordclim_srad`
#' - Wind speed: `worldclim_wind`
#'
#' @inheritParams worldclim_download
#'
#' @param worldclim_data `terra SpatRaster` or `list` of `terra SpatRaster`s if
#'  extracting multiple climate variables. We recommend using
#'   [worldclim_download()] to ensure that all data necessary to match your
#'   input data are captured. Direct output of [worldclim_download()] can be
#'   supplied here.
#' @param covariates Character, vector if multiple climate data types desired. By
#'   default, extracts WorldClim average temperature data.
#' @param site_name Character. Optional argument to provide the name of the
#'   column containing site names if not contained within the BMDE column
#'   `SurveyAreaIdentifier`. Can be left `NULL` and still function properly if
#'   originally specified in a call to [data_fmt()] or [worldclim_download()].
#' @param date_month Character. Optional argument to provide the name of the
#'   column containing month data if not contained within the BMDE column
#'   `survey_month`. Can be left `NULL` and still function properly if originally
#'   specified in a call to [data_fmt()] or [worldclim_download()].
#' @param dl_path Character. Path to downloaded files. Only needed if `retain = TRUE`
#'   and custom download filepath used.
#' @param retain Logical. Should WorldClim data files be kept after extraction? If
#'   `FALSE`, files will be deleted.
#'
#' @returns For sf 'POINT' or terra 'points' input data, original data with
#' numeric column(s) appended containing the climate data value(s) at each point.
#'
#' For sf 'POLYGON' or terra 'polygons' input data, original data with numeric
#' column(s) appended containing the mean climate data value(s) within each polygon.
#'
#' @examples
#' # Convert included test data on black-capped chickadees to sf POINT object
#' bcch <- sf::st_as_sf(
#'   bcch,
#'   coords = c("longitude", "latitude"),
#'   crs = 4326
#' )
#'
#' # Load WorldClim data
#' tavg <- worldclim_download(data = bcch,
#'                            covariates = "worldclim_wind",
#'                            progress = FALSE)
#'
#' # Extract average temperature
#' output <- worldclim_extract(data = bcch,
#'                             worldclim_data = tavg,
#'                             covariates = "worldclim_wind",
#'                             retain = FALSE)
#'
#' @seealso [worldclim_download()] which can be used to download WorldClim data files.
#'
#'   [nc_covariates_merge()] to merge extracted
#'   covariate data into data originally provided to the `data` argument of
#'   [data_fmt()].
#'
#' @references Fick, S.E. and R.J. Hijmans, 2017. WorldClim 2: new 1km spatial resolution climate surfaces for global land areas. International Journal of Climatology 37 (12): 4302-4315.
#'
#' @export

# Function to extract WorldClim data from provided WorldClim SpatRaster(s).
worldclim_extract <- function(
  data,
  worldclim_data, # named list containing SpatRaster containing
  # WorldClim data, downloadable via WorldClim_download(). Names derived from
  # WorldClim variable names ("tmin", "tmax", "tavg", "prec", "wind", "vapr",
  # "bio").
  covariates = "worldclim_tavg", # Other options listed in nc_covariate_table().
  site_name = NULL, # optional argument to provide column name containing site
  # names. Default is assumed to be the BMDE column 'SurveyAreaIdentifier'. Can
  # be left NULL and still function properly if originally specified in a call
  # to data_fmt().
  date_month = NULL, # optional argument to provide column name containing month
  # data. Default is assumed to be the BMDE column 'survey_month'. Can
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

  # Catch misspecified covariates. Return error if any exist.
  if (FALSE %in% (covariates %in% nc_covariate_table()$covariate_name)) {
    stop(
      "[WorldClim Extraction] covariates either not listed or one or more are",
      " invalid. Please provide covariate names as listed under",
      " `covariate_name` in nc_covariate_table().",
      call. = FALSE
    )
  }

  # If no WorldClim rasters are provided, return error.
  if (missing(worldclim_data)) {
    stop(
      "[WorldClim Extraction] no WorldClim rasters provided to extract from.",
      " Please provide a list of the necessary rasters. Data can be downloaded",
      " using worldclim_download().",
      call. = FALSE
    )
  }

  if (
    !(((inherits(worldclim_data, "list")) &
      (inherits(worldclim_data[[1]], "SpatRaster"))) |
      inherits(worldclim_data, "SpatRaster"))
  ) {
    stop(
      "[WorldClim Extraction] no WorldClim rasters provided to extract from.",
      " Please provide a list of the necessary rasters. Data can be downloaded",
      " using worldclim_download().",
      call. = FALSE
    )
  }

  # Check data is in the desired format.
  input_fmt <- covariate_fmt_check(data)

  # If not an sf or terra object, return error and point towards data_fmt().
  if (input_fmt$type == "data.frame") {
    stop(
      "[WorldClim Extraction] downloading requires an sf or terra object as",
      " input in this workflow. Consider using `data_fmt` to conform data",
      " first.",
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

  if (is.null(date_month) & !is.null(attr(data, "date_month"))) {
    date_month <- attr(data, "date_month")
  }

  # Check that all specified column names are present in the data.
  specified_cols <- c(site_name, date_month)

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
        !("survey_month" %in% data_cols))
  ) {
    stop(
      "[WorldClim Extraction] some specified columns missing from the data: ",
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

  # For sf objects, create area of interest to crop WorldClim rasters to to
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

  # For terra objects, create area of interest to crop WorldClim rasters to to
  # reduce memory load. Convert to sf.
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

  # If buffered, check for packages necessary in buffered workflow.
  if (buffered == TRUE) {
    have_pkg_check("exactextractr")
  }

  clim <- worldclim_data

  if (inherits(worldclim_data, "list")) {
    loop <- names(worldclim_data)
  } else if (inherits(worldclim_data, "SpatRaster")) {
    loop <- gsub(
      pattern = "worldclim_",
      replacement = "",
      grep("worldclim_", covariates, value = TRUE)
    )
  }

  terra::terraOptions(progress = 0)

  # Loop through each requested WorldClim variable.
  for (i in loop) {
    message("[WorldClim Extraction] extracting WorldClim ", i, ".")
    if (length(loop) > 1) {
      source <- clim[[i]]
    } else {
      source <- clim
    }
    # Loop through each site and extract.
    for (j in unique(data$SurveyAreaIdentifier)) {
      # Create temporary object with only point/buffer for site i.
      tmp <- data %>%
        dplyr::filter(.data$SurveyAreaIdentifier == j) %>%
        dplyr::select("SurveyAreaIdentifier", "survey_month", "geometry") %>%
        dplyr::distinct() %>%
        sf::st_transform(terra::crs(source))

      # Loop through each month site i was visited, extract.
      for (k in unique(data$survey_month[data$SurveyAreaIdentifier == j])) {
        # Use variable name and month to pull correct layer from WorldClim
        # raster.

        layername <- paste0(
          substr(
            names(source)[1],
            start = 1,
            stop = nchar(names(source)[1]) - 1
          ),
          k
        )

        # In the first iteration of the loop, check that the site falls within
        # or is only partially covered by the spatial extent of the provided
        # WorldClim rasters. If not, warn.
        if (
          which(
            unique(data$survey_month[data$SurveyAreaIdentifier == j]) == k
          ) ==
            1
        ) {
          if (
            all(is.na(terra::extract(source[[layername]], tmp)[, layername]))
          ) {
            warning(
              "[WorldClim (",
              i,
              ") Extraction] site ",
              j,
              " falls outside of the spatial extent of the WorldClim rasters",
              " provided. No value will be returned.",
              call. = FALSE
            )
          } else if (
            TRUE %in%
              is.na(terra::extract(source[[layername]], tmp)[, layername])
          ) {
            warning(
              "[WorldClim (",
              i,
              ") Extraction] site ",
              j,
              "'s buffered area is only partially contained by the spatial",
              " extent of the WorldClim rasters provided. Returned mean ",
              i,
              " value will be derived from the available values.",
              call. = FALSE
            )

            data[
              data$SurveyAreaIdentifier == j & data$survey_month == k,
              i
            ] <- exactextractr::exact_extract(
              x = source[[layername]],
              y = tmp %>% dplyr::filter(.data$survey_month == k),
              fun = "mean"
            )
          } else {
            # If no issues with coverage, proceed to extract. If buffered,
            # extract using exactextractr::exact_extract(). If not, extract
            # using terra::extract().
            if (buffered == TRUE) {
              data[
                data$SurveyAreaIdentifier == j & data$survey_month == k,
                i
              ] <- exactextractr::exact_extract(
                x = source[[layername]],
                y = tmp %>% dplyr::filter(.data$survey_month == k),
                fun = "mean"
              )
            } else {
              data[
                data$SurveyAreaIdentifier == j & data$survey_month == k,
                i
              ] <- terra::extract(
                x = source[[layername]],
                y = tmp %>% dplyr::filter(.data$survey_month == k),
                fun = "mean",
                na.rm = TRUE
              )[, layername]
            }
          }
        } else {
          # For all iterations after the first, extract if covered by the
          # WorldClim rasters. Issue no further warnings if not.
          if (
            all(is.na(terra::extract(source[[layername]], tmp)[, layername]))
          ) {
            data[
              data$SurveyAreaIdentifier == j & data$survey_month == k,
              i
            ] <- NA
          } else {
            if (buffered == TRUE) {
              data[
                data$SurveyAreaIdentifier == j & data$survey_month == k,
                i
              ] <- exactextractr::exact_extract(
                x = source[[layername]],
                y = tmp %>% dplyr::filter(.data$survey_month == k),
                fun = "mean"
              )
            } else {
              data[
                data$SurveyAreaIdentifier == j & data$survey_month == k,
                i
              ] <- terra::extract(
                x = source[[layername]],
                y = tmp %>% dplyr::filter(.data$survey_month == k),
                fun = "mean",
                na.rm = TRUE
              )[, layername]
            }
          }
        }
      }
    }

    terra::terraOptions(progress = 1)

    # Code to grab nearest raster value for sites outside of raster coverage.
    # Not sure whether to keep this since we are warning users about these sites
    # and saying nothing will be returned. Maybe keep as an option
    # (nearest = TRUE)?
    #   if (TRUE %in% is.na(data[, i])) {
    #     for (j in unique(data$SurveyAreaIdentifier[is.na(data[, i])])) {
    #       for (k in unique(data$survey_month[data$SurveyAreaIdentifier == j])) {
    #         layername <- paste0(
    #           substr(
    #             names(clim[[i]])[1],
    #             start = 1,
    #             stop = nchar(names(clim[[i]])[1]) - 1
    #           ),
    #           k
    #         )
    #
    #         tmp <- data %>%
    #           dplyr::filter(SurveyAreaIdentifier == j) %>%
    #           dplyr::select(SurveyAreaIdentifier, survey_month, geometry) %>%
    #           dplyr::distinct() %>%
    #           sf::st_transform(terra::crs(clim[[i]]))
    #
    #         if (
    #           terra::is.related(
    #             clim[[i]],
    #             terra::vect(tmp),
    #             relation = "intersects"
    #           )
    #         ) {
    #           if (
    #             which(
    #               unique(data$SurveyAreaIdentifier[is.na(
    #                 data$SurveyAreaIdentifier
    #               )]) ==
    #                 j
    #             ) ==
    #               1
    #           ) {
    #             warning(
    #               paste0(
    #                 "[WorldClim (",
    #                 i,
    #                 ") Extraction] some points are close to shore, and so fall outside of raster coverage. For these cells, the nearest cell value has been used."
    #               ),
    #               call. = FALSE
    #             )
    #           }
    #
    #           tmp <- data %>%
    #             dplyr::filter(SurveyAreaIdentifier == j, survey_month == k) %>%
    #             dplyr::select(SurveyAreaIdentifier, survey_month, geometry) %>%
    #             dplyr::distinct() %>%
    #             sf::st_buffer(2500) %>%
    #             sf::st_transform(terra::crs(clim[[i]]))
    #
    #           clim_crop <- terra::crop(
    #             clim[[i]][[layername]],
    #             terra::vect(tmp)
    #           ) %>%
    #             terra::as.points()
    #
    #           data[
    #             data$SurveyAreaIdentifier == j & data$survey_month == k,
    #             i
    #           ] <- terra::values(clim_crop[
    #             terra::nearest(terra::vect(tmp), clim_crop)$to_id
    #           ])
    #         }
    #       }
    #     }
    #   }
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

  if (!is.null(date_month)) {
    names(data)[names(data) == "survey_month"] <- date_month
  }

  # Remove WorldClim files if requested.
  if (retain == FALSE) {
    # Check that if default directory doesn't exist an alterate has been
    # specified.
    if (is.null(dl_path) & !dir.exists("./worldclim")) {
      warning(
        "[WorldClim Extraction] unable to find default WorldClim",
        " directory and no alternate specified using dl_path argument",
        ". No files will be removed.",
        call. = FALSE
      )
    } else {
      message(paste0("[WorldClim Extraction] task complete. Removing files."))

      unlink(
        ifelse(
          is.null(dl_path),
          "./worldclim/climate",
          paste0(dl_path, "/worldclim/climate")
        ),
        recursive = TRUE
      )
    }
  }

  # Return input data with appended WorldClim columns.
  return(data)
}
