#' Buffer Data for Covariate Download and Extraction
#'
#' Buffers input data by a specifiable distance.
#'
#' If input data is an `sf` 'POINT' object or a `terra` 'points' object, points
#' are buffered to a circle with the radius specified in `buffer_distance` (500
#' m by default). If input is an `sf` 'POLYGON' object or a `terra` 'polygons'
#' object, the polygon is buffered on all sides by the distance specified in
#' `buffer_distance` (500 m by default).
#'
#' @param data An `sf` 'POINT' or 'POLYGON' object, or `terra` 'points' or
#'   'polygons' object.
#' @param buffer Logical. Should the provided data be buffered? `TRUE` by
#'   default.
#' @param buffer_distance Numeric. Distance that the provided points or
#'   polygons should be buffered by. 500 by default.
#' @param buffer_units Character. Units of provided distance. Options are "m"
#'   (metres), "km" (kilometers), "ft" (feet), "yd" (yards), "mi" (miles), or
#'   "naut_mi" (nautical miles). Metres ("m") by default.
#'
#' @returns Either `sf` 'POLYGON' or `terra` 'polygons' (depending on format of
#'   input data) with original coordinate reference system and columns
#'   preserved, and all geometries buffered by requested distance.
#'
#' @examples
#' # Using the included, test data on black-capped chickadees
#' bcch # look at the data
#'
#' # Convert to sf POINT object
#' bcch <- sf::st_as_sf(bcch, coords = c("longitude", "latitude"), crs = 4326)
#'
#' # Buffer by 1 km
#' output <- data_buff(bcch, buffer = TRUE, buffer_distance = 1, buffer_units = "km")
#'
#' @seealso [data_fmt()] for a convenient way to convert `data.frame` data to a
#'   compatible `sf` or `terra` object.
#'   
#'   [sf::st_buffer()] which this function wraps.
#'
#' @export

data_buff <- function(
    data,
    buffer = TRUE, # Should the data be buffered?
    buffer_distance = 500, # Distance to buffer by.
    buffer_units = "m" # Units of provided distance.
) {
  # Unless buffering requested, do nothing.
  if (buffer == TRUE) {
    # Check packages
    have_pkg_check(c("terra", "sf", "measurements"))
    
    # Check data is in the desired format
    input_fmt <- covariate_fmt_check(data)
    
    # If not an sf or terra object, return error and point towards data_fmt().
    if (input_fmt$type == "data.frame") {
      stop(
        "[Data Buffering] buffering requires an sf or terra object as input in",
        " this workflow. Consider using `data_fmt` to conform data first.",
        call. = FALSE
      )
    }
    
    # Ensure radius is coercable to a numeric value.
    buffer_distance <- as.numeric(buffer_distance)
    
    # If unit provided is not compatible with measurements::conv_unit(), return
    # error.
    if (!(buffer_units %in% c("m", "km", "ft", "yd", "mi", "naut_mi"))) {
      stop(
        "[Data Buffering] buffer units not recognized: please set buffer_units to one of 'm' [metres], 'km' [kilometers], 'ft' [feet], 'yd' [yards], 'mi' [miles], or 'naut_mi' [nautical miles].",
        call. = FALSE
      )
    }
    
    message(
      "[Data Buffering] buffering sites by ",
      buffer_distance,
      buffer_units,
      " radius",
      ifelse(buffer_distance == 500 & buffer_units == "m", " (default)", ""),
      "."
    )
    
    # Buffer sf objects by requested amount.
    if (input_fmt$type == "sf") {
      # Store original CRS so data can be returned as provided.
      orig_crs <- terra::crs(data)
      
      # If not already in CRS used herein, transform.
      if (!(orig_crs == terra::crs("ESRI:102001"))) {
        data <- sf::st_transform(data, "ESRI:102001")
      }
      
      # If sf object contains polygon, warn that polygons will be buffered on
      # all sides. This might help users catch mistakes when pre-buffered data
      # is provided and they don't want it additionally buffered.
      if (input_fmt$geometry == "POLYGON") {
        warning(
          "[Data Buffering] sf POLYGON geometry provided. Existing polygons",
          " will be buffered by an additional ",
          buffer_distance,
          buffer_units,
          ".",
          call. = FALSE
        )
      }
      
      # Buffer. Use measurements::conv_unit() to handle units other than metres.
      data <- sf::st_buffer(
        data,
        measurements::conv_unit(
          x = buffer_distance,
          from = buffer_units,
          to = "m"
        )
      )
      
      # Back-transform to original CRS if it wasn't the CRS used herein.
      if (!(orig_crs == terra::crs("ESRI:102001"))) {
        data <- sf::st_transform(data, orig_crs)
      }
    }
    
    # Buffer terra objects by requested amount.
    if (input_fmt$type == "terra") {
      # Store original CRS so data can be returned as provided.
      orig_crs <- terra::crs(data)
      
      # If not already in CRS used herein, transform.
      if (!(orig_crs == terra::crs("ESRI:102001"))) {
        data <- terra::project(data, "ESRI:102001")
      }
      
      # If terra object contains polygon, warn that polygons will be buffered on
      # all sides. This might help users catch mistakes when pre-buffered data
      # is provided and they don't want it additionally buffered.
      if (input_fmt$geometry == "polygons") {
        warning(
          "[Data Buffering] terra polygons provided. Existing polygons will",
          " be buffered by an additional ",
          buffer_distance,
          buffer_units,
          ".",
          call. = FALSE
        )
      }
      
      # Buffer. Use measurements::conv_unit() to handle units other than metres.
      data <- terra::buffer(
        data,
        measurements::conv_unit(
          x = buffer_distance,
          from = buffer_units,
          to = "m"
        )
      )
      
      # Back-transform to original CRS if it wasn't the CRS used herein.
      if (!(orig_crs == terra::crs("ESRI:102001"))) {
        data <- terra::project(data, orig_crs)
      }
    }
  }
  
  # Return provided data if no buffering requested, or buffered data if
  # buffering requested.
  return(data)
}