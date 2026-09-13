#' Merge Extracted Covariate Data into Original Input Data
#'
#' Data formatted for covariate extraction using [data_fmt()] is transformed
#' to an `sf` object containing a row for each unique site-date combination,
#' with columns being appended to this by the various covariate extraction
#' functions within [naturecounts]. Users may wish, instead, to have their
#' covariate data appended to original data in a different format (e.g., a
#' row for each observation) and can use this function to merge the two data
#' types accurately.
#'
#' @param original_data `data.frame`, `sf` 'POINT' or 'POLYGON' object, or `terra` 'points'
#'   or 'polygons'. Object containing data to match covariate data to. For
#'   example, the original input data to a call to [data_fmt()].
#' @param covariate_data `sf` 'POINT' or 'POLYGON' object. Object containing data
#'   output by one of the covariate extraction functions within [naturecounts]:
#'   [landcover_extract()], [vegetation_extract()], [elevation_extract()],
#'   [worldclim_extract()], [scanfi_extract()], or [daymet_extract].
#' @param site_name Character. Optional argument to provide the name of the
#'   column containing site names if not contained within the BMDE column
#'   `SurveyAreaIdentifier`.
#' @param coord_lon Character. Optional argument to provide the name of the
#'   column containing year data if not contained within the BMDE column
#'   `longitude`.
#' @param coord_lat Character. Optional argument to provide the name of the
#'   column containing year data if not contained within the BMDE column
#'   `latitude`.
#' @param date_year Character. Optional argument to provide the name of the
#'   column containing year data if not contained within the BMDE column
#'   `survey_year`.
#' @param date_month Character. Optional argument to provide the name of the
#'   column containing month data if not contained within the BMDE column
#'   `survey_month`.
#' @param date_day Character. Optional argument to provide the name of the
#'   column containing day of month data if not contained within the BMDE column
#'   `survey_day`.
#' @param date_lubridate Character. Optional argument to provide the name of a
#'   column containing date data in `lubridate` formats.
#' @param date_ordinal Character. Optional argument to provide the name of a
#'   column containing date data in ordinal format.
#'
#' @returns Data provided in `original_data` with covariate data columns from
#' `covariate_data` appended.
#'
#' @examplesIf interactive()
#'
#' # Using the included, test data on black-capped chickadees
#' bcch # look at the data
#'
#' # Format
#' formatted <- data_fmt(bcch)
#'
#' # Download and extract some covariate data.
#' elev <- elevation_download(data = formatted,
#'                            progress = FALSE)
#'
#' extracted <- elevation_extract(data = formatted,
#'                                elevation_data = elev)
#'
#' # Merge with original data
#' merged <- nc_covariates_merge(original_data = bcch,
#'                               covariate_data = extracted)
#'
#' merged
#'
#' @export

# Function to merge outputs of extraction functions to original data.
nc_covariates_merge <- function(
  original_data, # Data input to data_fmt() or an extraction function.
  covariate_data, # Output of an extraction function.
  coord_lon = NULL, # as in cosewic_ranges
  coord_lat = NULL, # as in cosewic_ranges
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
  date_lubridate = NULL, # optional argument to provide column name containing
  # 'lubridate' date objects.
  date_ordinal = NULL # optional argument to provide column name containing
  # ordinal dates.
) {
  # Check packages.
  have_pkg_check(c(
    "sf",
    "terra",
    "tidyterra"
  ))

  # Fetch format of original data.
  input_fmt <- covariate_fmt_check(original_data)

  # Store relevant information depending on original data format.
  if (input_fmt$type == "data.frame") {
    original_fmt <- "data.frame"
  }

  if (input_fmt$type == "sf") {
    original_fmt <- "sf"

    original_cols <- names(original_data)

    original_geom <- input_fmt$geometry

    original_crs <- sf::st_crs(original_data)
  }

  if (input_fmt$type == "terra") {
    original_fmt <- "terra"

    original_cols <- names(original_data)

    original_geom <- input_fmt$geometry

    original_crs <- terra::crs(original_data)
  }

  # Fetch format of covariate data.
  output_fmt <- covariate_fmt_check(covariate_data)

  # Covariate data should be the output of an extraction function, which is
  # expected to be an sf object. If not, return error.
  if (!(output_fmt$type == "sf")) {
    stop(
      "[Data Merging] Provided covariate data not in expected format.",
      " 'sf' object expected as output by NatureCounts covariate extraction",
      " functions.",
      call. = FALSE
    )
  }

  # Check whether sf object is buffered or not to determine joining
  # procedure down the line.
  if (output_fmt$type == "sf") {
    buffer <- ifelse(output_fmt$geometry == "POLYGON", TRUE, FALSE)
  }

  # Check whether information on alternate column names has been stored
  # in the attributes by data_fmt(). However, prioritize alternate column names
  # specified in the current call.
  if (is.null(coord_lon) & !is.null(attr(covariate_data, "coord_lon"))) {
    coord_lon <- attr(covariate_data, "coord_lon")
  }

  if (is.null(coord_lat) & !is.null(attr(covariate_data, "coord_lat"))) {
    coord_lat <- attr(covariate_data, "coord_lat")
  }

  if (is.null(site_name) & !is.null(attr(covariate_data, "site_name"))) {
    site_name <- attr(covariate_data, "site_name")
  }

  if (is.null(date_year) & !is.null(attr(covariate_data, "date_year"))) {
    date_year <- attr(covariate_data, "date_year")
  }

  if (is.null(date_month) & !is.null(attr(covariate_data, "date_month"))) {
    date_month <- attr(covariate_data, "date_month")
  }

  if (is.null(date_day) & !is.null(attr(covariate_data, "date_day"))) {
    date_day <- attr(covariate_data, "date_day")
  }

  if (is.null(date_ordinal) & !is.null(attr(covariate_data, "date_ordinal"))) {
    date_ordinal <- attr(covariate_data, "date_ordinal")
  }

  if (
    is.null(date_lubridate) & !is.null(attr(covariate_data, "date_lubridate"))
  ) {
    date_lubridate <- attr(covariate_data, "date_lubridate")
  }

  # Joining procedure for original data in data frame objects.
  if (original_fmt == "data.frame") {
    # Remove SurveyAreaIdentifier column as this is a less reliable joiner
    # than coordinate data.
    covariate_data[, ifelse(
      is.null(site_name),
      "SurveyAreaIdentifier",
      site_name
    )] <- NULL

    # Convert covariate data to data frame.
    covariate_data <- sf::st_drop_geometry(covariate_data)

    # If lubridate or ordinal date data is present, use to join. If not, use
    # individual date columns. If both lubridate and ordinal date data is
    # present, use lubridate data to join.
    if (is.null(date_ordinal) & is.null(date_lubridate)) {
      matched_data <- dplyr::left_join(
        original_data,
        covariate_data,
        by = c(
          ifelse(is.null(coord_lon), "longitude", coord_lon),
          ifelse(is.null(coord_lat), "latitude", coord_lat),
          ifelse(is.null(date_year), "survey_year", date_year),
          ifelse(is.null(date_month), "survey_month", date_month),
          ifelse(is.null(date_day), "survey_day", date_day)
        )
      )
    } else {
      if (!is.null(date_ordinal)) {
        if (!is.null(date_lubridate)) {
          covariate_data <- dplyr::select(
            covariate_data,
            -"survey_year",
            -"survey_month",
            -"survey_day"
          )

          matched_data <- dplyr::left_join(
            original_data,
            covariate_data,
            by = c(
              ifelse(is.null(coord_lon), "longitude", coord_lon),
              ifelse(is.null(coord_lat), "latitude", coord_lat),
              date_lubridate
            )
          )
        } else {
          covariate_data <- dplyr::select(
            covariate_data,
            -"survey_month",
            -"survey_day"
          )

          matched_data <- dplyr::left_join(
            original_data,
            covariate_data,
            by = c(
              ifelse(is.null(coord_lon), "longitude", coord_lon),
              ifelse(is.null(coord_lat), "latitude", coord_lat),
              ifelse(is.null(date_year), "survey_year", date_year),
              date_ordinal
            )
          )
        }
      }

      if (!is.null(date_lubridate)) {
        covariate_data <- dplyr::select(
          covariate_data,
          -"survey_year",
          -"survey_month",
          -"survey_day"
        )

        matched_data <- dplyr::left_join(
          original_data,
          covariate_data,
          by = c(
            ifelse(is.null(coord_lon), "longitude", coord_lon),
            ifelse(is.null(coord_lat), "latitude", coord_lat),
            date_lubridate
          )
        )
      }
    }
  }

  # Joining procedure for original data in sf objects.
  if (original_fmt == "sf") {
    # Remove SurveyAreaIdentifier column as this is a less reliable joiner
    # than coordinate data.
    covariate_data[, ifelse(
      is.null(site_name),
      "SurveyAreaIdentifier",
      site_name
    )] <- NULL

    # Convert covariate data to data frame.
    covariate_data <- sf::st_drop_geometry(covariate_data)

    # Edge case: there is a column in the original data called X that needs
    # to be preserved.
    if ("X" %in% names(original_data)) {
      x_storage <- original_data$X

      original_data$X <- NULL
    }

    # Edge case: there is a column in the original data called Y that needs
    # to be preserved.
    if ("Y" %in% names(original_data)) {
      y_storage <- original_data$Y

      original_data$Y <- NULL
    }

    # Edge case: there is a column in the original data called longitude that
    # needs to be preserved.
    if ("longitude" %in% names(original_data)) {
      lon_storage <- original_data$longitude

      original_data$longitude <- NULL
    }

    # Edge case: there is a column in the original data called latitude that
    # needs to be preserved.
    if ("latitude" %in% names(original_data)) {
      lat_storage <- original_data$latitude

      original_data$latitude <- NULL
    }

    # Create coordinate columns to join with. For polygon original data, use
    # centroids.
    if (original_geom == "POLYGON") {
      original_data <- cbind(
        original_data,
        sf::st_coordinates(suppressWarnings(sf::st_centroid(original_data)))
      ) %>%
        dplyr::rename("longitude" = "X", "latitude" = "Y")
    } else {
      original_data <- cbind(
        original_data,
        sf::st_coordinates(original_data)
      ) %>%
        dplyr::rename("longitude" = "X", "latitude" = "Y")
    }

    # Restore X and Y columns if they needed to be preserved.
    if (exists("x_storage")) {
      original_data$X <- x_storage

      rm(x_storage)
    }

    if (exists("y_storage")) {
      original_data$Y <- y_storage

      rm(y_storage)
    }

    if (!is.null(coord_lon)) {
      names(original_data)[names(original_data) == "longitude"] <- coord_lon
    }

    if (!is.null(coord_lat)) {
      names(original_data)[names(original_data) == "latitude"] <- coord_lat
    }

    # If lubridate or ordinal date data is present, use to join. If not, use
    # individual date columns. If both lubridate and ordinal date data is
    # present, use lubridate data to join.
    if (is.null(date_ordinal) & is.null(date_lubridate)) {
      matched_data <- dplyr::left_join(
        original_data,
        covariate_data,
        by = c(
          ifelse(is.null(coord_lon), "longitude", coord_lon),
          ifelse(is.null(coord_lat), "latitude", coord_lat),
          ifelse(is.null(date_year), "survey_year", date_year),
          ifelse(is.null(date_month), "survey_month", date_month),
          ifelse(is.null(date_day), "survey_day", date_day)
        )
      )
    } else {
      if (!is.null(date_ordinal)) {
        if (!is.null(date_lubridate)) {
          covariate_data <- dplyr::select(
            covariate_data,
            -"survey_year",
            -"survey_month",
            -"survey_day"
          )

          matched_data <- dplyr::left_join(
            original_data,
            covariate_data,
            by = c(
              ifelse(is.null(coord_lon), "longitude", coord_lon),
              ifelse(is.null(coord_lat), "latitude", coord_lat),
              date_lubridate
            )
          )
        } else {
          covariate_data <- dplyr::select(
            covariate_data,
            -"survey_month",
            -"survey_day"
          )

          matched_data <- dplyr::left_join(
            original_data,
            covariate_data,
            by = c(
              ifelse(is.null(coord_lon), "longitude", coord_lon),
              ifelse(is.null(coord_lat), "latitude", coord_lat),
              ifelse(is.null(date_year), "survey_year", date_year),
              date_ordinal
            )
          )
        }
      }

      if (!is.null(date_lubridate)) {
        covariate_data <- dplyr::select(
          covariate_data,
          -"survey_year",
          -"survey_month",
          -"survey_day"
        )

        matched_data <- dplyr::left_join(
          original_data,
          covariate_data,
          by = c(
            ifelse(is.null(coord_lon), "longitude", coord_lon),
            ifelse(is.null(coord_lat), "latitude", coord_lat),
            date_lubridate
          )
        )
      }
    }

    # Remove coordinate columns used for joining.
    matched_data[, ifelse(is.null(coord_lon), "longitude", coord_lon)] <- NULL
    matched_data[, ifelse(is.null(coord_lat), "latitude", coord_lat)] <- NULL

    # Restore latitude/longitude columns if they needed to be preserved.
    if (exists("lon_storage")) {
      matched_data$longitude <- lon_storage

      rm(lon_storage)
    }

    if (exists("lat_storage")) {
      matched_data$latitude <- lat_storage

      rm(lat_storage)
    }

    # Reorder columns to match original data.
    matched_data <- matched_data[, c(
      original_cols[!(original_cols == "geometry")],
      names(matched_data)[!(names(matched_data) %in% original_cols)],
      "geometry"
    )]
  }

  # Joining procedure for original data in data frame objects.
  if (original_fmt == "terra") {
    # Remove SurveyAreaIdentifier column as this is a less reliable joiner
    # than coordinate data.
    covariate_data[, ifelse(
      is.null(site_name),
      "SurveyAreaIdentifier",
      site_name
    )] <- NULL

    # Convert covariate data to data frame.
    covariate_data <- sf::st_drop_geometry(covariate_data)

    # Edge case: there is a column in the original data called x that needs
    # to be preserved.
    if ("x" %in% names(original_data)) {
      x_storage <- original_data$x

      original_data$x <- NULL
    }

    # Edge case: there is a column in the original data called y that needs
    # to be preserved.
    if ("y" %in% names(original_data)) {
      y_storage <- original_data$y

      original_data$y <- NULL
    }

    # Edge case: there is a column in the original data called longitude that
    # needs to be preserved.
    if ("longitude" %in% names(original_data)) {
      lon_storage <- original_data$longitude

      original_data$longitude <- NULL
    }

    # Edge case: there is a column in the original data called latitude that
    # needs to be preserved.
    if ("latitude" %in% names(original_data)) {
      lat_storage <- original_data$latitude

      original_data$latitude <- NULL
    }

    # Create coordinate columns to join with. For polygon original data, use
    # centroids.
    if (input_fmt$geometry == "polygons") {
      original_data <- cbind(
        original_data,
        terra::crds(terra::centroids(original_data))
      ) %>%
        tidyterra::rename("longitude" = "x", "latitude" = "y")
    } else {
      original_data <- cbind(original_data, terra::crds(original_data)) %>%
        tidyterra::rename("longitude" = "x", "latitude" = "y")
    }

    # Restore X and Y columns if they needed to be preserved.
    if (exists("x_storage")) {
      original_data$x <- x_storage

      rm(x_storage)
    }

    if (exists("y_storage")) {
      original_data$y <- y_storage

      rm(y_storage)
    }

    if (!is.null(coord_lon)) {
      names(original_data)[names(original_data) == "longitude"] <- coord_lon
    }

    if (!is.null(coord_lat)) {
      names(original_data)[names(original_data) == "latitude"] <- coord_lat
    }

    # If lubridate or ordinal date data is present, use to join. If not, use
    # individual date columns. If both lubridate and ordinal date data is
    # present, use lubridate data to join.
    if (is.null(date_ordinal) & is.null(date_lubridate)) {
      matched_data <- tidyterra::left_join(
        original_data,
        covariate_data,
        by = c(
          ifelse(is.null(coord_lon), "longitude", coord_lon),
          ifelse(is.null(coord_lat), "latitude", coord_lat),
          ifelse(is.null(date_year), "survey_year", date_year),
          ifelse(is.null(date_month), "survey_month", date_month),
          ifelse(is.null(date_day), "survey_day", date_day)
        )
      )
    } else {
      if (!is.null(date_ordinal)) {
        if (!is.null(date_lubridate)) {
          covariate_data <- dplyr::select(
            covariate_data,
            -"survey_year",
            -"survey_month",
            -"survey_day"
          )

          matched_data <- tidyterra::left_join(
            original_data,
            covariate_data,
            by = c(
              ifelse(is.null(coord_lon), "longitude", coord_lon),
              ifelse(is.null(coord_lat), "latitude", coord_lat),
              date_lubridate
            )
          )
        } else {
          covariate_data <- dplyr::select(
            covariate_data,
            -"survey_month",
            -"survey_day"
          )

          matched_data <- tidyterra::left_join(
            original_data,
            covariate_data,
            by = c(
              ifelse(is.null(coord_lon), "longitude", coord_lon),
              ifelse(is.null(coord_lat), "latitude", coord_lat),
              ifelse(is.null(date_year), "survey_year", date_year),
              date_ordinal
            )
          )
        }
      }

      if (!is.null(date_lubridate)) {
        covariate_data <- dplyr::select(
          covariate_data,
          -"survey_year",
          -"survey_month",
          -"survey_day"
        )

        matched_data <- tidyterra::left_join(
          original_data,
          covariate_data,
          by = c(
            ifelse(is.null(coord_lon), "longitude", coord_lon),
            ifelse(is.null(coord_lat), "latitude", coord_lat),
            date_lubridate
          )
        )
      }
    }

    # Remove coordinate columns used for joining.
    matched_data[, ifelse(is.null(coord_lon), "longitude", coord_lon)] <- NULL
    matched_data[, ifelse(is.null(coord_lat), "latitude", coord_lat)] <- NULL

    # Restore latitude/longitude columns if they needed to be preserved.
    if (exists("lon_storage")) {
      matched_data$longitude <- lon_storage

      rm(lon_storage)
    }

    if (exists("lat_storage")) {
      matched_data$latitude <- lat_storage

      rm(lat_storage)
    }

    # Reorder columns to match original data.
    matched_data <- matched_data[, c(
      original_cols,
      names(matched_data)[!(names(matched_data) %in% original_cols)]
    )]
  }

  # Remove lingering attribute.
  if (!is.null(attr(matched_data, "site_name"))) {
    attr(matched_data, "site_name") <- NULL
  }

  # Return original data with appended covariate columns.
  return(matched_data)
}
