#' Extract Terrain Tiles Elevation Data
#'
#' Extracts [Mapzen Terrain Tiles elevation data](https://github.com/tilezen/joerd/tree/master/docs)
#' from a `terra SpatRaster`, as delivered by [elevation_download()].
#'
#' Users should be conscious of the final spatial resolution of their elevation data,
#' as this varies by latitude and zoom level specified in [elevation_download()].
#' This can be accessed using [terra::res()].
#'
#' #' By default, for `sf` 'POLYGON' or `terra` 'polygons' input data the mean
#' elevation value will be returned. Other summary statistics can be extracted
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
#' @param ... Other arguments passed to [terra::extract()] for
#'   `sf` 'POINT' or `terra` 'points' input data or
#'   [exactextractr::exact_extract()] `sf` 'POLYGON' or `terra` 'polygons' input
#'   data. Primarily useful for specifying alternate summary statistics to
#'   extract for `sf` 'POLYGON' or `terra` 'polygons' input data.
#'
#' @returns For sf 'POINT' or terra 'points' input data, original data with
#' numeric column `elevation` appended containing the elevation value (metres
#' above sea level) at each point.
#'
#' For sf 'POLYGON' or terra 'polygons' input data, original data with numeric
#' column(s) appended containing the requested elevation value(s) (metres above
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
  site_name = NULL, # optional argument to provide column name containing site
  # names. Default is assumed to be the BMDE column 'SurveyAreaIdentifier'. Can
  # be left NULL and still function properly if originally specified in a call
  # to data_fmt().
  ...
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

    if (hasArg("fun") & !is.function(list(...)[["fun"]])) {
      if ("quantile" %in% list(...)[["fun"]] & !hasArg("quantiles")) {
        stop(
          "[Elevation Extraction] quantile summary requested but",
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
          "[Elevation Extraction] weighted summary requested but no",
          " weights supplied via the 'weights' argument. Please supply",
          " either a weighting raster or 'area' to use the cell areas of",
          " the elevation raster as weights.",
          call. = FALSE
        )
      }
    }
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

    if (!terra::is.related(elev, terra::vect(tmp), relation = "intersects")) {
      warning(
        "[Elevation Extraction] site ",
        i,
        " falls outside of the spatial extent of the elevation rasters",
        " provided. No value will be returned.",
        call. = FALSE
      )
    } else if (buffered == TRUE) {
      # Check if function information is stored in ...
      if (!hasArg("fun")) {
        funs <- "mean"
      } else {
        funs <- list(...)[["fun"]]
      }

      if (all(is.nan(terra::values(terra::crop(elev, tmp))))) {
        warning(
          "[Elevation Extraction] site ",
          i,
          " falls outside of the spatial extent of the elevation rasters",
          " provided. No value will be returned.",
          call. = FALSE
        )

        range <- "out"
      } else if (TRUE %in% is.nan(terra::values(terra::crop(elev, tmp)))) {
        warning(
          "[Elevation Extraction] site ",
          i,
          "'s buffered area is only partially contained by the spatial extent of",
          " the elevation rasters provided. Returned elevation value will",
          " be derived from the available values.",
          call. = FALSE
        )

        range <- "overlap"
      } else {
        range <- "in"
      }

      if (range %in% c("overlap", "in")) {
        # Check whether fun = NULL. In exactextractr::exact_extract() this is
        # used to extract cell values and coverage fractions. fun = 'frac' is
        # a valid alternative that works here.
        if (is.null(funs)) {
          stop(
            "[Elevation Extraction] support is not provided for fun",
            " = NULL. If wanting to extract cell values and coverage",
            " fractions consider fun = 'frac'. Keep in mind that this can",
            " produce a lot of columns. Direct use of",
            " exactextractr::exact_extract() may be more useful here.",
            call. = FALSE
          )
        } else if (is.function(funs)) {
          # If fun is a user-specified function, attempt to run.
          val <- exactextractr::exact_extract(elev, tmp, ...)

          # If function returns more than one value or a data.frame, stop.
          if (
            length(val) > 1 |
              is.data.frame(val)
          ) {
            stop(
              "[Elevation Extraction] support for custom summary",
              " functions is currently limited to functions returning a",
              " single value (not stored in a data.frame) to allow accurate",
              " joining to input data.",
              call. = FALSE
            )
          }

          # If user-defined function returns acceptable value, join to data.
          data[
            data$SurveyAreaIdentifier == i,
            "elevation_user_defined_function"
          ] <- val
        } else {
          # If fun is one or more pre-defined summary functions (see
          # ?exactextractr::exact_extract()), loop through options and extract.
          for (j in funs) {
            # Check if any summary functions requested required tailored
            # joining.
            if (
              j == "quantile" &
                length(list(...)[["quantiles"]]) > 1
            ) {
              # Multiple quantiles cause exactextractr::exact_extract() to
              # return a data.frame with a column for each requested quantile,
              # and so must be joined in a tailored way.

              # Build arguments so that calls with multiple functions
              # requested in fun don't try and extract all requested functions
              # on each loop iteration.
              args <- list(...)
              args$x <- elev
              args$y <- tmp
              args$fun <- j

              # Overwrite redundant args.
              args$append_cols <- NULL
              args$force_df <- FALSE

              # Extract.
              q_table <- do.call(exactextractr::exact_extract, args)

              # Join each requested quantile to original data.
              for (k in names(q_table)) {
                data[
                  data$SurveyAreaIdentifier == i,
                  paste0(
                    "elevation_",
                    j,
                    "_",
                    sub(pattern = "q", replacement = "", x = k)
                  )
                ] <- q_table[, k]
              }
            } else if (j %in% c("frac", "weighted_frac")) {
              # Extracting fraction or weighted fraction causes
              # exactextractr::exact_extract() to return a data.frame with a
              # column for each unique cell value, and so must be joined in a
              # tailored way.

              # Build arguments so that calls with multiple functions
              # requested in fun don't try and extract all requested functions
              # on each loop iteration.
              args <- list(...)
              args$x <- elev
              args$y <- tmp
              args$fun <- j

              # Overwrite redundant args.
              args$append_cols <- NULL
              args$force_df <- FALSE

              # Extract.
              frac_table <- do.call(exactextractr::exact_extract, args)

              if (frac_table == 1) {
                value <- unique(terra::values(terra::crop(
                  elev,
                  tmp
                )))

                data[
                  data$SurveyAreaIdentifier == i,
                  paste0(
                    "elevation_",
                    j,
                    "_",
                    value
                  )
                ] <- 1
              } else {
                # Join each fractional value to original data.
                for (k in names(frac_table)) {
                  data[
                    data$SurveyAreaIdentifier == i,
                    paste0(
                      "elevation_",
                      j,
                      "_",
                      as.numeric(sub(
                        pattern = "frac_",
                        replacement = "",
                        x = k
                      ))
                    )
                  ] <- frac_table[, k]
                }
              }
            } else {
              # If no tailored joining needed, just build arguments so that
              # calls with multiple functions requested in fun don't try and
              # extract all requested functions on each loop iteration.
              args <- list(...)
              args$x <- elev
              args$y <- tmp
              args$fun <- j

              # Overwrite redundant args.
              args$append_cols <- NULL
              args$force_df <- FALSE

              # Extract and join requested value to input data.
              data[
                data$SurveyAreaIdentifier == i,
                paste0(
                  "elevation_",
                  j
                )
              ] <- do.call(exactextractr::exact_extract, args)
            }
          }
        }
      }
    } else {
      if (
        is.na(terra::extract(elev, tmp, ...)[, `if`(
          hasArg("layer"),
          "value",
          terra::names(elev)
        )])
      ) {
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
          ...
        )[, `if`(
          hasArg("layer"),
          "value",
          terra::names(elev)
        )]
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

  # Return input data with appended elevation columns.
  return(data)
}
