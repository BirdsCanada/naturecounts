#' Download and Load Data from the Spatialized Canadian National Forest Inventory (SCANFI)
#'
#' Downloads and loads into the environment all available variables from the
#' [SCANFI v2 dataset](https://open.canada.ca/data/en/dataset/07653869-f303-46c2-a04e-9ab479b73cbf).
#' All variables are available in snapshots every 5 years between 1985 and 2025 at
#' a 30 m resolution. Users should be aware that these are very large files (usually
#' 1-5 Gb per snapshot per variable).
#'
#' One (or multiple) SCANFI variable(s) can be downloaded by specifying the following
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
#' Downloads are facilitated by a call to [utils::download.file()].
#'
#' @param data A `data.frame`, `sf` 'POINT' or 'POLYGON' object, or `terra` 'points'
#'   or 'polygons' object containing a column with observation years either named
#'   the BMDE default `survey_year` or another name specified in argument `date_year`.
#'   Not required if `use_date = FALSE`, but must be specified if `use_date = TRUE`.
#' @param covariates Character, vector if multiple SCANFI data types desired. By
#'   default, downloads SCANFI forest height data.
#' @param use_date Logical. Should the function use year data provided in `data`
#'   to choose which snapshot to download? If `FALSE`, `snapshot_year` can be used
#'   to specify which snapshot(s) should be downloaded and used.
#' @param snapshot_year Numeric, vector if multiple snapshots desired. Snapshot
#'   years to download. Options include: 1985, 1990, 1995, 2000, 2005, 2010, 2015,
#'   2020, and 2025. If specified, takes precedent over dates from `data` when
#'   `use_date = TRUE`.
#' @param date_year Character. Optional argument to provide the name of the
#'   column containing year data if not contained within the BMDE column
#'   `survey_year`. Can be left `NULL` and still function properly if originally
#'   specified in a call to [data_fmt()].
#' @param timeout Numeric. Number of seconds before downloads timeout. This should
#'   be in the 10s of thousands of seconds, depending on internet download speed.
#'   Default value assumes largest SCANFI file is being requested, with download
#'   speeds of 0.2 Mb/s.
#' @param dl_path Character. Optional argument to provide path to download data
#'   to. By default, data is downloaded to a subfolder `scanfi/` in the working
#'   directory.
#' @param progress Logical. Should progress bars be displayed?
#'
#' @returns A named list containing `terra SpatRaster`s of all requested data.
#'   Each list element represents the SCANFI snapshot year of the data,
#'   with a named `terra SpatRaster` for each requested variable.
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
#' output <- scanfi_download(data = bcch,
#'                           covariates = "scanfi_ponderosapine",
#'                           progress = FALSE)
#'
#' # We can also manually specify the snapshot years to download with no input
#' # data required like this:
#' output <- scanfi_download(covariates = "scanfi_ponderosapine",
#'                           use_date = FALSE,
#'                           snapshot_year = c(2015, 2020),
#'                           progress = FALSE)
#'
#'
#' @seealso [scanfi_extract()] which can be used to extract data from loaded
#' SCANFI data files.
#'
#' @references Fick, S.E. and R.J. Hijmans, 2017. WorldClim 2: new 1km spatial resolution climate surfaces for global land areas. International Journal of Climatology 37 (12): 4302-4315.
#'
#' @export
# Function to download data from the Spatialized Canadian National Forest
# Inventory using download.file().

scanfi_download <- function(
  data = NULL, # Only necessary if use_date = TRUE - needed to fetch year data.
  covariates = "scanfi_height", # Other options
  # listed in nc_covariate_table().
  use_date = TRUE, # Should the most recent snapshot be downloaded (FALSE), or
  # should all relevant snapshots be downloaded for extraction (TRUE). Can
  # result in multiple large downloads.
  snapshot_year = NULL, # If use_date = FALSE, the desired snapshot year to be
  # used. If not specified, the most recent (2025) is used.
  date_year = NULL, # optional argument to provide column name containing year
  # data. Default is assumed to be the BMDE column 'survey_year'. Can
  # be left NULL and still function properly if originally specified in a call
  # to data_fmt().
  timeout = 32000,
  dl_path = NULL, # optional argument to provide path
  # to download data to. By default, data is
  # downloaded to a subfolder 'scanfi/' in the
  # working directory.
  progress = TRUE
) {
  # Check packages
  have_pkg_check("terra")

  # Catch misspecified covariates. Return error if any exist.
  if (FALSE %in% (covariates %in% nc_covariate_table()$covariate_name)) {
    stop(
      "[SCANFI Download] covariates either not listed or one or more are",
      " invalid. Please provide covariate names as listed under",
      " `covariate_name` in nc_covariate_table().",
      call. = FALSE
    )
  }

  # Check data is in the desired format.
  input_fmt <- covariate_fmt_check(data)

  # Use user specified snapshot years over automatically selected years
  if (!is.null(snapshot_year) & use_date == TRUE) {
    use_date <- FALSE

    warning(
      "[SCANFI Download] Specific snapshot years requested but use_date",
      " set as TRUE, suggesting function should determine necessary",
      " snapshots to download from years in data argument. Overriding",
      " and proceeding to download snapshots requested in snapshot_year.",
      call. = FALSE
    )
  }

  if (use_date == TRUE) {
    # Check whether information on alternate column names has been stored
    # in the attributes by data_fmt(). However, prioritize alternate column names
    # specified in the current call.
    if (is.null(date_year) & !is.null(attr(data, "date_year"))) {
      date_year <- attr(data, "date_year")
    }

    # Check that all specified column names are present in the data.

    # Gather all potentially specified columns.
    specified_cols <- c(date_year)

    # Remove any that haven't been specified.
    specified_cols <- specified_cols[!is.null(specified_cols)]

    data_cols <- names(data)

    # Compare to columns present in data. Return error if any specified columns
    # are not present. 'if' wrapper needed for when alternate column names exist
    # in the attributes of the data, but conversion of those columns to
    # standardized names has already taken place in data_fmt().
    if (
      !(all(specified_cols %in% data_cols)) &
        !("survey_year" %in% data_cols)
    ) {
      stop(
        "[SCANFI Download] some specified columns missing from the",
        " data: ",
        stringr::str_flatten_comma(specified_cols[
          !(specified_cols %in% data_cols)
        ]),
        ". Use arguments to specify alternate column names if using data that",
        " diverges from naturecounts default column names.",
        call. = FALSE
      )
    }

    # Conform specified columns to naturecounts default column names. Calls to
    # st_sf() needed to avoid sf specific issue with attributes.
    if (!is.null(date_year) & !("survey_year" %in% data_cols)) {
      if (input_fmt$type == "sf") {
        data <- sf::st_sf(data)
      }

      data <- dplyr::rename(data, "survey_year" = !!date_year)
    }

    data$survey_year <- as.numeric(data$survey_year)

    # Get necessary years for download from data.

    available_years <- seq(from = 1985, to = 2025, by = 5)

    closest_year <- data.frame(data_year = sort(unique(data$survey_year)))

    outside_years <- c(
      closest_year$data_year[closest_year$data_year < 1980],
      closest_year$data_year[closest_year$data_year > 2030]
    )

    if (length(outside_years) > 0) {
      warning(
        "[SCANFI Download] Data contains years more than 5 years away",
        " from nearest SCANFI snapshot (",
        stringr::str_flatten_comma(outside_years),
        "). No value will be returned for observations in these years.",
        call. = FALSE
      )
    }

    closest_year <- dplyr::filter(
      closest_year,
      !(.data$data_year %in% outside_years)
    )

    for (i in closest_year$data_year) {
      closest_year$scanfi_year[
        closest_year$data_year == i
      ] <- available_years[which(
        abs(i - available_years) == min(abs(i - available_years))
      )]
    }

    necessary_years <- unique(closest_year$scanfi_year)
  } else {
    necessary_years <- `if`(is.null(snapshot_year), 2025, snapshot_year)

    if (!all(necessary_years %in% seq(from = 1985, to = 2025, by = 5))) {
      stop(
        "[SCANFI Download] Invalid snapshot year(s) provided to",
        " snapshot_year argument: ",
        necessary_years[
          (necessary_years %in% seq(from = 1985, to = 2025, by = 5)) == FALSE
        ],
        ". Valid snapshot years are ",
        stringr::str_flatten_comma(seq(from = 1985, to = 2025, by = 5))
      )
    }
  }

  # Create download path if it doesn't already exist.
  if (is.null(dl_path) & !dir.exists("./scanfi")) {
    dir.create("./scanfi", recursive = TRUE)
  }

  if (!is.null(dl_path) & !dir.exists(paste0(dl_path, "/scanfi"))) {
    dir.create(paste0(dl_path, "/scanfi"), recursive = TRUE)
  }

  # Create index for SCANFI variables from requested covariates.
  scanfi_vars <- gsub(
    pattern = "scanfi_",
    replacement = "",
    grep("scanfi_", covariates, value = TRUE)
  )

  # Create table of download links for each SCANFI variable.
  filename <- list()

  for (i in as.character(necessary_years)) {
    filename[[i]] <- data.frame(variable = scanfi_vars) %>%
      dplyr::mutate(
        filename = dplyr::case_when(
          variable == "age" ~ paste0(
            "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v2/SCANFI_age_median_",
            i,
            "_v2_20260119.tif"
          ),
          variable == "biomass" ~ paste0(
            "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v2/SCANFI_att_biomass_",
            i,
            "_v2_20260119.tif"
          ),
          variable == "closure" ~ paste0(
            "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v2/SCANFI_att_closure_",
            i,
            "_v2_20260119.tif"
          ),
          variable == "height" ~ paste0(
            "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v2/SCANFI_att_height_",
            i,
            "_v2_20260119.tif"
          ),
          variable == "nfilc" ~ paste0(
            "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v2/SCANFI_att_nfiLandcover_",
            i,
            "_v2_20260119.tif"
          ),
          variable == "balsamfir" ~ paste0(
            "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v2/SCANFI_spsCC_balsamFir_",
            i,
            "_v2_20260119.tif"
          ),
          variable == "blackspruce" ~ paste0(
            "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v2/SCANFI_spsCC_blackSpruce_",
            i,
            "_v2_20260119.tif"
          ),
          variable == "douglasfir" ~ paste0(
            "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v2/SCANFI_spsCC_douglasFir_",
            i,
            "_v2_20260119.tif"
          ),
          variable == "jackpine" ~ paste0(
            "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v2/SCANFI_spsCC_jackPine_",
            i,
            "_v2_20260119.tif"
          ),
          variable == "lodgepolepine" ~ paste0(
            "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v2/SCANFI_spsCC_lodgepolePine_",
            i,
            "_v2_20260119.tif"
          ),
          variable == "ponderosapine" ~ paste0(
            "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v2/SCANFI_spsCC_ponderosaPine_",
            i,
            "_v2_20260119.tif"
          ),
          variable == "tamarack" ~ paste0(
            "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v2/SCANFI_spsCC_tamarack_",
            i,
            "_v2_20260119.tif"
          ),
          variable == "whiteredpine" ~ paste0(
            "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v2/SCANFI_spsCC_whiteRedPine_",
            i,
            "_v2_20260119.tif"
          ),
          variable == "broadleaf" ~ paste0(
            "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v2/SCANFI_spsCC_broadleaf_",
            i,
            "_v2_20260119.tif"
          ),
          variable == "otherconifer" ~ paste0(
            "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v2/SCANFI_spsCC_otherConiferous_",
            i,
            "_v2_20260119.tif"
          )
        )
      )
  }

  # Open list to store SCANFI rasters.
  scanfi <- list()

  # Store user default timeout option
  user_timeout <- getOption("timeout")

  # Set timeout as specified in arguments.
  options("timeout" = timeout)

  # Loop through each requested SCANFI variable and download.
  for (i in scanfi_vars) {
    ### WILL NEED TO CHECK IF DATA IS IN ARCTIC RANGE AND WARN ABOUT NFI LAND
    ### COVER MODELING PROCESS.

    for (j in as.character(necessary_years)) {
      # If file doesn't already exist, download requested variable.
      if (
        !file.exists(ifelse(
          is.null(dl_path),
          paste0(
            "./scanfi/",
            dplyr::last(unlist(stringr::str_split(
              filename[[j]]$filename[filename[[j]]$variable == i],
              "/"
            )))
          ),
          paste0(
            dl_path,
            "/scanfi/",
            dplyr::last(unlist(stringr::str_split(
              filename[[j]]$filename[filename[[j]]$variable == i],
              "/"
            )))
          )
        ))
      ) {
        message(
          "[SCANFI Download] downloading SCANFI ",
          i,
          ". Files are large and may require a fair bit of download and processing time."
        )

        ### USING METHODS OTHER THAN CURL SEEMS TO CAUSE ISSUES WITH DOWNLOADED FILE - NEED TO CONSIDER CURL COMPATIBILITY WITH OTHER OS'S.

        # tryCatch needed to handle curl issues and redirect users to downloading
        # manually and reading using scanfi_read().
        tryCatch(
          utils::download.file(
            url = filename[[j]]$filename[filename[[j]]$variable == i],
            destfile = ifelse(
              is.null(dl_path),
              paste0(
                "./scanfi/",
                dplyr::last(unlist(stringr::str_split(
                  filename[[j]]$filename[filename[[j]]$variable == i],
                  "/"
                )))
              ),
              paste0(
                dl_path,
                "/scanfi/",
                dplyr::last(unlist(stringr::str_split(
                  filename[[j]]$filename[filename[[j]]$variable == i],
                  "/"
                )))
              )
            ),
            mode = "wb",
            quiet = !progress
          ),
          error = function(e) {
            if (conditionMessage(e) == "'curl' call had nonzero exit status") {
              stop(
                "[SCANFI Download] 'curl' call had nonzero exist status. Please download files directly from https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/SCANFI/v1/ and read in using scanfi_read().",
                call. = FALSE
              )
            } else {
              stop(conditionMessage(e), call. = FALSE)
            }
          }
        )

        # Read in downloaded variable and store in list.
        scanfi[[j]][[i]] <- terra::rast(ifelse(
          is.null(dl_path),
          paste0(
            "./scanfi/",
            dplyr::last(unlist(stringr::str_split(
              filename[[j]]$filename[filename[[j]]$variable == i],
              "/"
            )))
          ),
          paste0(
            dl_path,
            "/scanfi/",
            dplyr::last(unlist(stringr::str_split(
              filename[[j]]$filename[filename[[j]]$variable == i],
              "/"
            )))
          )
        ))
      } else {
        scanfi[[j]][[i]] <- terra::rast(ifelse(
          is.null(dl_path),
          paste0(
            "./scanfi/",
            dplyr::last(unlist(stringr::str_split(
              filename[[j]]$filename[filename[[j]]$variable == i],
              "/"
            )))
          ),
          paste0(
            dl_path,
            "/scanfi/",
            dplyr::last(unlist(stringr::str_split(
              filename[[j]]$filename[filename[[j]]$variable == i],
              "/"
            )))
          )
        ))
      }
    }
  }

  # Reset user default timeout option.
  options("timeout" = user_timeout)

  # Return list of scanfi rasters.
  return(scanfi)
}
