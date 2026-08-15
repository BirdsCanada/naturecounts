#' Load WorldClim Climate Data
#'
#' Downloads monthly WorldClim Monthly Climate Norms, averaged over 1970-2000,
#' from [WorldClim version 2.1](https://www.worldclim.org/data/worldclim21.html)
#' at a ~ 1 km^2 spatial resolution (Fick & Hijmans 2017). Several climate variables
#' are available in this dataset: minimum, maximum, and average temperature (°C),
#' precipitation (mm), solar radiation (kJ/m^2/day), and wind speed (m/s).
#' Users should note that these files are downloaded at the country-scale so
#' they can be quite large.
#'
#' One (or multiple) climate variable(s) can be downloaded by specifying the following
#' values to the `covariates` argument
#' - Minimum temperature: `worldclim_tmin`
#' - Maximum temperature: `worldclim_tmax`
#' - Average temperature: `worldclim_tavg`
#' - Precipitation: `worldclim_prec`
#' - Solar radiation: `wordclim_srad`
#' - Wind speed: `worldclim_wind`
#'
#' Downloads are facilitated by a call to [geodata::worldclim_country()].
#'
#' @param data An `sf` 'POINT' or 'POLYGON' object, or `terra` 'points' or
#'   'polygons' object. Not required if `countries` is specified, but must be
#'   specified if `countries` is left unspecified.
#' @param covariates Character, vector if multiple climate data types desired. By
#'   default, downloads WorldClim average temperature data.
#' @param countries Character, vector if multiple countries. Country names or
#'   [ISO3 country codes](https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3) for which
#'   data should be downloaded. If left `NULL`, function will attempt to identify
#'   countries needed based on locations in `data`.
#' @param dl_path Character. Optional argument to provide path to download data
#'   to. By default, data is downloaded to a subfolder `WorldClim/` in the working
#'   directory.
#' @param progress Logical. Should progress bars and download messages be displayed?
#'
#' @returns A merged `terra SpatRaster` containing all requested data. A `list` of
#' multiple `terra SpatRaster` objects if multiple climate data types requested.
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
#' output <- worldclim_download(data = bcch,
#'                              covariates = "worldclim_wind",
#'                              progress = FALSE)
#'
#' @seealso [geodata::worldclim_country()] which this function wraps.
#'
#'   [worldclim_extract()]
#'   which can be used to extract data from loaded WorldClim data files.
#'
#' @references Fick, S.E. and R.J. Hijmans, 2017. WorldClim 2: new 1km spatial resolution climate surfaces for global land areas. International Journal of Climatology 37 (12): 4302-4315.
#'
#' @export

# Function for downloading WorldClim data. Wrapper for
# geodata::worldclim_country().
worldclim_download <- function(
  data = NULL,
  covariates = "worldclim_tavg", # Other options listed in nc_covariate_table().
  countries = NULL, # Character vector of country names or ISO3 codes. If left
  # NULL, country will be auto-detected.
  dl_path = NULL, # optional argument to provide path to download data to. By
  # default, data is downloaded to a subfolder 'worldclim/' in the working
  # directory.
  progress = TRUE
) {
  # Check packages
  have_pkg_check(c(
    "sf",
    "geodata",
    "terra"
  ))

  # Catch misspecified covariates. Return error if any exist.
  if (FALSE %in% (covariates %in% nc_covariate_table()$covariate_name)) {
    stop(
      "[WorldClim Download] covariates either not listed or one or more are",
      " invalid. Please provide covariate names as listed under",
      " `covariate_name` in nc_covariate_table().",
      call. = FALSE
    )
  }

  # Create download path if it doesn't already exist.
  if (is.null(dl_path) & !dir.exists("./worldclim")) {
    dir.create("./worldclim", recursive = TRUE)
  }

  if (!is.null(dl_path) & !dir.exists(paste0(dl_path, "/worldclim"))) {
    dir.create(paste0(dl_path, "/worldclim"), recursive = TRUE)
  }

  # Create index for climate variables from covariate request.
  clim_vars <- gsub(
    pattern = "worldclim_",
    replacement = "",
    grep("worldclim_", covariates, value = TRUE)
  )

  # Unless user specified, attempt to automatically detect the countries for
  # which data must be downloaded.
  if (is.null(countries)) {
    # Check for additional package necessary in this workflow.
    have_pkg_check("spData")

    # Check data is in the desired format.
    input_fmt <- covariate_fmt_check(data)

    # If not an sf or terra object, return error and point towards data_fmt().
    if (input_fmt$type == "data.frame") {
      stop(
        "[WorldClim Download] downloading requires an sf or terra object as",
        " input in this workflow. Consider using `data_fmt` to conform",
        " data first.",
        call. = FALSE
      )
    }

    # For sf input, compare to country data from spData package.
    if (input_fmt$type == "sf") {
      world <- sf::st_read(
        system.file("shapes/world.gpkg", package = "spData"),
        quiet = TRUE
      )

      data <- sf::st_transform(data, sf::st_crs(world))

      world <- suppressWarnings(sf::st_intersection(world, data))

      countries <- unique(world$name_long)
    }

    # For terra input, convert to sf and  compare to country data from spData
    # package.
    if (input_fmt$type == "terra") {
      data <- sf::st_as_sf(data)

      world <- sf::st_read(
        system.file("shapes/world.gpkg", package = "spData"),
        quiet = TRUE
      )

      data <- sf::st_transform(data, sf::st_crs(world))

      world <- suppressWarnings(sf::st_intersection(world, data))

      countries <- unique(world$name_long)
    }
  }

  # Open list to store downloaded rasters.
  clim <- list()

  # Loop through each requested WorldClim variable, download.
  for (i in clim_vars) {
    # Loop through each country requested or detected.
    for (j in countries) {
      # Pull country codes table to handle ISO3 codes.
      country_code <- geodata::country_codes()

      # Check if provided country is an ISO3 code, if so, convert.
      if (!(j %in% country_code$ISO3)) {
        country_code <- country_code$ISO3[country_code$NAME == j]
      } else {
        country_code <- j
      }

      # If file doesn't already exist, call geodata::worldclim_country() to
      # download data.
      if (
        !file.exists(ifelse(
          is.null(dl_path),
          paste0(
            "./worldclim/climate/wc2.1_country/",
            country_code,
            "_wc2.1_30s_",
            i,
            ".tif"
          ),
          paste0(
            dl_path,
            "/worldclim/climate/wc2.1_country/",
            country_code,
            "_wc2.1_30s_",
            i,
            ".tif"
          )
        ))
      ) {
        message(
          "[Worldclim Download] downloading WorldClim '",
          i,
          "' data for ",
          j,
          "."
        )

        tryCatch(
          clim[[i]][[j]] <- geodata::worldclim_country(
            var = i,
            country = j,
            path = ifelse(
              is.null(dl_path),
              "./worldclim",
              paste0(dl_path, "/worldclim")
            ),
            quiet = !progress
          ),
          message = function(m) {
            if (
              stringr::str_detect(
                string = conditionMessage(m),
                pattern = "geodata server seems to be temporary out of service."
              )
            ) {
              stop(
                "[WorldClim Download] Download failed for ",
                j,
                " [",
                i,
                "]. The geodata server appears to be temporarily down. Try again later.",
                call. = FALSE
              )
            } else {
              message(conditionMessage(m), call. = FALSE)
            }
          }
        )
      } else {
        clim[[i]][[j]] <- terra::rast(ifelse(
          is.null(dl_path),
          paste0(
            "./worldclim/climate/wc2.1_country/",
            country_code,
            "_wc2.1_30s_",
            i,
            ".tif"
          ),
          paste0(
            dl_path,
            "/worldclim/climate/wc2.1_country/",
            country_code,
            "_wc2.1_30s_",
            i,
            ".tif"
          )
        ))
      }
    }

    # Convert each variable's different country rasters to a SpatRasterCollection
    # then merge into a single layer for each variable.

    if (!is.null(clim[[i]][[j]])) {
      clim[[i]] <- terra::sprc(clim[[i]])

      terra::terraOptions(progress = 0)

      if (length(clim_vars) == 1) {
        clim <- terra::merge(clim[[i]])
      } else {
        clim[[i]] <- terra::merge(clim[[i]])
      }

      terra::terraOptions(progress = 1)
    }
  }

  # Return WorldClim SpatRasters
  return(clim)
}
