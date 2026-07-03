#' Extract Terrain Tiles Elevation Data
#'
#' Extracts [Mapzen Terrain Tiles elevation data](https://github.com/tilezen/joerd/tree/master/docs)
#' from a `terra SpatRaster`, as delivered by [elevation_download()].
#'
#' Users should be conscious of the final spatial resolution of their elevation data,
#' as this varies by latitude and zoom level specified in [elevation_download()].
#' This can be accessed using [terra::res()].
#'
#' @param data An `sf` 'POINT' or 'POLYGON' object, or `terra` 'points' or
#'   'polygons' object.
#' @param elevation_data `terra SpatRaster`. Terrain Tiles elevation data. We recommend using
#'   [elevation_download()] to ensure that all data necessary to match your
#'   input data are captured. Direct output of [elevation_download()] can be
#'   supplied here.
#' @param site_name Character. Optional argument to provide the name of the
#'   column containing site names if not contained within the BMDE column
#'   `SurveyAreaIdentifier`. Can be left `NULL` and still function properly if
#'   originally specified in a call to [data_fmt()] or [elevation_download()].
#'
#' @returns For sf 'POINT' or terra 'points' input data, original data with
#' numeric column `elevation` appended containing the elevation value (metres
#' above sea level) at each point.
#'
#' For sf 'POLYGON' or terra 'polygons' input data, original data with numeric
#' column `elevation` appended containing the mean elevation value (metres above
#' sea level) within each polygon.
#'
#' @examples
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
#' # Load Terrain Tiles data
#' elev <- elevation_download(data = bcch)
#'
#' # Extract Terrain Tiles data
#' output <- elevation_extract(data = bcch, elevation_data = elev)
#'
#' @seealso [elevation_download()] which can be used to download data from
#'   the MapZen Terrain Tiles database.
#'
#'   [nc_covariates_merge()] to merge extracted
#'   covariate data into data originally provided to the `data` argument of
#'   [data_fmt()].
#'
#' @export

# Function to extract elevation data from provided elevation SpatRaster.
elevation_extract <- function(
  data,
  elevation_data, # SpatRaster derived from elevatr::get_elev_raster(),
  # downloadable via elevation_download().
  site_name = NULL # optional argument to provide column name containing site
  # names. Default is assumed to be the BMDE column 'SurveyAreaIdentifier'. Can
  # be left NULL and still function properly if originally specified in a call
  # to data_fmt().
) {
  # Check packages
  have_pkg_check(c(
    "sf",
    "terra"
  ))

  # If no elevation raster is provided, return error.
  if (missing(elevation_data)) {
    stop(
      "[Elevation Extraction] no elevation data provided to extract from. Please provide a terra SpatRaster containing the necessary elevation data. Elevation data can be downloaded using elevation_download().",
      call. = FALSE
    )
  }

  # If elevation_data is provided, but is not a SpatRaster return error.
  if (!(inherits(elevation_data, "SpatRaster"))) {
    stop(
      "[Elevation Extraction] data provided to elevation_data argument is not a SpatRaster. Please provide a terra SpatRaster containing the necessary elevation data. Elevation data can be downloaded using elevation_download().",
      call. = FALSE
    )
  }

  # Check data is in the desired format.
  input_fmt <- covariate_fmt_check(data)

  # If not an sf or terra object, return error and point towards data_fmt().
  if (input_fmt$type == "data.frame") {
    stop(
      "[Elevation Extraction] extraction requires an sf or terra object as input in this workflow. Consider using `data_fmt` to conform data first.",
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
  attrs <- attributes(data)[attr_names[attr_names %in% names(attributes(data))]]

  # Check whether information on alternate column names has been stored
  # in the attributes by data_fmt(). However, prioritize alternate column names
  # specified in the current call.
  if (is.null(site_name) & !is.null(attr(data, "site_name"))) {
    site_name <- attr(data, "site_name")
  }

  # Check that all specified column names are present in the data.
  specified_cols <- c(site_name)

  # Remove any that haven't been specified.
  specified_cols <- specified_cols[!is.null(specified_cols)]

  data_cols <- names(data)

  # Compare to columns present in data. Return error if any specified columns
  # are not present. 'if' wrapper needed for when alternate column names exist
  # in the attributes of the data, but conversion of those columns to
  # standardized names has already taken place in data_fmt().
  if (
    !(all(specified_cols %in% data_cols)) &
      !("SurveyAreaIdentifier" %in% data_cols)
  ) {
    stop(
      "[Elevation Extraction] some specified columns missing from the data: ",
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
  }

  elev <- elevation_data

  message("[Elevation Extraction] extracting elevation data.")

  # Loop through each site and extract.
  for (i in unique(data$SurveyAreaIdentifier)) {
    # Create temporary object with only point/buffer for site i.
    tmp <- data %>%
      dplyr::filter(.data$SurveyAreaIdentifier == i) %>%
      dplyr::select("SurveyAreaIdentifier", "geometry") %>%
      dplyr::distinct()

    # Check if site i falls within the spatial extent of the provided elevation
    # raster. If not, warn. If only partially, warn.

    #### BECAUSE OF THE EXTRA NANs WILL NEED TO REWORK THIS
    if (!terra::is.related(elev, terra::vect(tmp), relation = "intersects")) {
      warning(
        "[Elevation Extraction] site ",
        i,
        " falls outside of the spatial extent of the elevation rasters",
        " provided. No value will be returned.",
        call. = FALSE
      )
    } else if (buffered == TRUE) {
      if (all(is.nan(terra::values(terra::crop(elev, tmp))))) {
        warning(
          "[Elevation Extraction] site ",
          i,
          " falls outside of the spatial extent of the elevation rasters",
          " provided. No value will be returned.",
          call. = FALSE
        )
      } else if (TRUE %in% is.nan(terra::values(terra::crop(elev, tmp)))) {
        warning(
          "[Elevation Extraction] site ",
          i,
          "'s buffered area is only partially contained by the spatial extent of",
          " the elevation rasters provided. Returned mean elevation value will",
          " be derived from the available values.",
          call. = FALSE
        )

        data[
          data$SurveyAreaIdentifier == i,
          "elevation"
        ] <- exactextractr::exact_extract(
          x = elev,
          y = tmp,
          fun = "mean",
          progress = FALSE
        )
      } else {
        data[
          data$SurveyAreaIdentifier == i,
          "elevation"
        ] <- exactextractr::exact_extract(
          x = elev,
          y = tmp,
          fun = "mean",
          progress = FALSE
        )
      }
    } else {
      if (is.na(terra::extract(elev, tmp)[, 2])) {
        warning(
          "[Elevation Extraction] site ",
          i,
          " falls outside of the spatial extent of the elevation rasters",
          " provided. No value will be returned.",
          call. = FALSE
        )
      } else {
        # If no issues with coverage, proceed to extract. If buffered, extract
        # using exactextractr::exact_extract(). If not, extract using
        # terra::extract().
        data[data$SurveyAreaIdentifier == i, "elevation"] <- terra::extract(
          x = elev,
          y = tmp,
          fun = "mean"
        )[, names(elev)]
      }
    }
  }

  # Code to grab nearest raster value for sites outside of raster coverage.
  # Not sure whether to keep this since we are warning users about these sites
  # and saying nothing will be returned. Maybe keep as an option
  # (nearest = TRUE)?
  # if (TRUE %in% is.na(data$elevation)) {
  #   warning(
  #     "[Elevation Extraction] some points are close to shore, and so fall into cells with negative elevation (below sea level). For these cells, the nearest positive elevation has been used.",
  #     call. = FALSE
  #   )
  #
  #   for (i in unique(data$SurveyAreaIdentifier[is.na(data$elevation)])) {
  #     tmp <- data %>%
  #       dplyr::filter(SurveyAreaIdentifier == i) %>%
  #       dplyr::select(SurveyAreaIdentifier, geometry) %>%
  #       dplyr::distinct() %>%
  #       sf::st_buffer(2500)
  #
  #     if (terra::is.related(elev, terra::vect(tmp), relation = "intersects")) {
  #       elev_crop <- terra::crop(elev, vect(tmp)) %>%
  #         terra::as.points()
  #
  #       data$elevation[
  #         data$SurveyAreaIdentifier == i
  #       ] <- terra::values(elev_crop[
  #         terra::nearest(terra::vect(tmp), elev_crop)$to_id
  #       ])
  #     }
  #   }
  # }

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

  # Return input data with appended elevation columns.
  return(data)
}
