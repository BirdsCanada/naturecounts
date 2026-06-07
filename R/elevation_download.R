#' Load Terrain Tiles Elevation Data
#'
#' Loads [Mapzen Terrain Tiles elevation data](https://github.com/tilezen/joerd/tree/master/docs),
#' delivered at varying spatial resolutions. This data is open access via [Amazon Web Services](https://registry.opendata.aws/terrain-tiles/),
#' and is a global composite of a variety of [data sources](https://github.com/tilezen/joerd/blob/master/docs/data-sources.md).
#' Data is loaded into the R environment, and is not permanently downloaded onto
#' the user's operating system. 
#' 
#' Users should be conscious of the final spatial resolution of their elevation data,
#' as this varies by latitude and zoom level. This can be accessed using
#' [terra::res()].
#'
#' Downloads are facilitated by a call to [elevatr::get_elev_raster()].
#'
#' @param data An `sf` 'POINT' or 'POLYGON' object, or `terra` 'points' or
#'   'polygons' object.
#' @param site_name Character. Optional argument to provide the name of the
#'   column containing site names if not contained within the BMDE column
#'   `SurveyAreaIdentifier`. Can be left `NULL` and still function properly if
#'   originally specified in a call to [data_fmt()].
#' @param z Numeric. Zoom level to fetch, determining the resulting spatial 
#'   resolution of downloaded elevation data. More information can be found 
#'   [here](https://github.com/tilezen/joerd/blob/master/docs/data-sources.md#what-is-the-ground-resolution).
#'
#' @returns A `terra SpatRaster` in the projection of the data supplied to the `data`
#' argument, covering the bounding box of the supplied data.
#'
#' @examples
#' # Convert included, test data on black-capped chickadees to sf POINT object
#' bcch <- sf::st_as_sf(
#'   bcch,
#'   coords = c("longitude", "latitude"),
#'   crs = 4326
#' )
#'
#' # Load Terrain Tiles data
#' output <- elevation_download(data = bcch)
#'
#' @seealso [elevatr::get_elev_raster()] which this function wraps. [elevation_extract()]
#' which can be used to extract data from loaded elevation data files.
#'
#' @export

# Function to download elevation data from Terrain Tiles. Wrapper for
# elevatr::get_elev_raster().
elevation_download <- function(
    data,
    site_name = NULL,
    z = 7
) {
  # Check packages
  have_pkg_check(c(
    "sf",
    "elevatr",
    "terra"
  ))
  
  # Check data is in the desired format.
  input_fmt <- covariate_fmt_check(data)
  
  # If not an sf or terra object, return error and point towards data_fmt().
  if (input_fmt$type == "data.frame") {
    stop(
      "[Elevation Download] downloading requires an sf or terra object as input in this workflow. Consider using `data_fmt` to conform data first.",
      call. = FALSE
    )
  }
  
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
      "[Elevation Download] some specified columns missing from the data: ",
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

  message("[Elevation Download] downloading data.")
  
  # Call to API using elevatr::get_elev_raster() and store in SpatRaster.
  elev <- elevatr::get_elev_raster(
    locations = sf::st_transform(data, "ESRI:102001"),
    z = z,
    prj = sf::st_crs("ESRI:102001"),
    src = "aws",
    neg_to_na = TRUE, # Turn ocean tiles with negative elevation to NAs.
    expand = 10000, # Arbitrarily high number selected (10km).
    # Maybe unnecessary, could reduce download size.
    verbose = FALSE
  ) %>%
    terra::rast()
  
  # Return SpatRaster of downloaded elevation data.
  return(elev)
}