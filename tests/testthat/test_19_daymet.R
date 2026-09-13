if (!dir.exists("./testdir")) {
  dir.create("./testdir")
}

bcch_restricted <- bcch[
  bcch$survey_year == 2011 & bcch$survey_month %in% c(1:6),
]

skip("local only")

test_that("daymet_request() submits requests successfully with all expected inputs.", {
  expect_silent(
    requests_sf_pt <<- suppressMessages(daymet_request(
      data_fmt(
        bcch_restricted,
        coord_lon = "longitude",
        coord_lat = "latitude",
        crs = 4326
      ),
      ed_username = "rdjmacklin_bc",
      covariates = "daymet_prcp",
      request_name = "sf_pt",
      dl_path = "./testdir",
      save = FALSE,
      verbose = FALSE
    ))
  )

  expect_silent(
    requests_sf_poly <<- suppressMessages(daymet_request(
      data_buff(data_fmt(
        bcch_restricted,
        coord_lon = "longitude",
        coord_lat = "latitude",
        crs = 4326
      )),
      ed_username = "rdjmacklin_bc",
      covariates = "daymet_prcp",
      request_name = "sf_poly",
      dl_path = "./testdir",
      save = FALSE,
      verbose = FALSE
    ))
  )

  expect_silent(
    requests_terra_pt <<- suppressMessages(daymet_request(
      data_fmt(terra::vect(
        bcch_restricted,
        crs = "epsg:4326",
        geom = c("longitude", "latitude")
      )),
      ed_username = "rdjmacklin_bc",
      covariates = "daymet_prcp",
      request_name = "terra_pt",
      dl_path = "./testdir",
      save = FALSE,
      verbose = FALSE
    ))
  )

  expect_silent(
    requests_terra_poly <<- suppressMessages(daymet_request(
      data_buff(data_fmt(terra::vect(
        bcch_restricted,
        crs = "epsg:4326",
        geom = c("longitude", "latitude")
      ))),
      ed_username = "rdjmacklin_bc",
      covariates = "daymet_prcp",
      request_name = "terra_poly",
      dl_path = "./testdir",
      save = FALSE,
      verbose = FALSE
    ))
  )

  expect_true(all(
    inherits(requests_sf_pt, "data.frame"),
    inherits(requests_sf_poly, "data.frame"),
    inherits(requests_terra_pt, "data.frame"),
    inherits(requests_terra_poly, "data.frame")
  ))

  expect_named(requests_sf_pt, c("request_name", "request_id", "date"))
  expect_named(requests_sf_poly, c("request_name", "request_id", "date"))
  expect_named(requests_terra_pt, c("request_name", "request_id", "date"))
  expect_named(requests_terra_poly, c("request_name", "request_id", "date"))

  dates <- sort(unique(paste0(
    bcch_restricted$survey_year,
    "-",
    ifelse(
      nchar(bcch_restricted$survey_month) == 1,
      paste0(0, bcch_restricted$survey_month),
      bcch_restricted$survey_month
    ),
    "-",
    ifelse(
      nchar(bcch_restricted$survey_day) == 1,
      paste0(0, bcch_restricted$survey_day),
      bcch_restricted$survey_day
    )
  )))

  expect_true(nrow(requests_sf_pt) == length(dates))
  expect_true(nrow(requests_sf_poly) == length(dates))
  expect_true(nrow(requests_terra_pt) == length(dates))
  expect_true(nrow(requests_terra_poly) == length(dates))

  expect_true(all(
    substr(requests_sf_pt$request_name, start = 1, stop = 5) == "sf_pt"
  ))
  expect_true(all(
    substr(
      requests_sf_pt$request_name,
      start = nchar(requests_sf_pt$request_name) - 9,
      stop = nchar(requests_sf_pt$request_name)
    ) ==
      dates
  ))
  expect_true(all(
    substr(requests_sf_poly$request_name, start = 1, stop = 7) == "sf_poly"
  ))
  expect_true(all(
    substr(
      requests_sf_poly$request_name,
      start = nchar(requests_sf_poly$request_name) - 9,
      stop = nchar(requests_sf_poly$request_name)
    ) ==
      dates
  ))
  expect_true(all(
    substr(requests_terra_pt$request_name, start = 1, stop = 8) == "terra_pt"
  ))
  expect_true(all(
    substr(
      requests_terra_pt$request_name,
      start = nchar(requests_terra_pt$request_name) - 9,
      stop = nchar(requests_terra_pt$request_name)
    ) ==
      dates
  ))
  expect_true(all(
    substr(requests_terra_poly$request_name, start = 1, stop = 10) ==
      "terra_poly"
  ))
  expect_true(all(
    substr(
      requests_terra_poly$request_name,
      start = nchar(requests_terra_poly$request_name) - 9,
      stop = nchar(requests_terra_poly$request_name)
    ) ==
      dates
  ))

  expect_true(all(dates %in% requests_sf_pt$date))
  expect_true(all(dates %in% requests_sf_poly$date))
  expect_true(all(dates %in% requests_terra_pt$date))
  expect_true(all(dates %in% requests_terra_poly$date))
})

skip("local only")

test_that("daymet_request() succeeds with alternate column names, either passed through attributes or specified explicitly.", {
  expect_silent(
    requests_attr <- suppressMessages(daymet_request(
      data_fmt(
        dplyr::rename(
          bcch_restricted,
          "sites" = "SurveyAreaIdentifier",
          "yr" = "survey_year",
          "mth" = "survey_month",
          "dy" = "survey_day"
        ),
        coord_lon = "longitude",
        coord_lat = "latitude",
        site_name = "sites",
        date_year = "yr",
        date_month = "mth",
        date_day = "dy",
        crs = 4326
      ),
      ed_username = "rdjmacklin_bc",
      covariates = "daymet_prcp",
      request_name = "colnames_attrs",
      dl_path = "./testdir",
      save = FALSE,
      verbose = FALSE
    ))
  )

  expect_silent(
    requests_explicit <<- suppressMessages(daymet_request(
      dplyr::rename(
        data_fmt(
          bcch_restricted,
          coord_lon = "longitude",
          coord_lat = "latitude",
          crs = 4326
        ),
        "sites" = "SurveyAreaIdentifier",
        "yr" = "survey_year",
        "mth" = "survey_month",
        "dy" = "survey_day"
      ),
      ed_username = "rdjmacklin_bc",
      covariates = "daymet_prcp",
      request_name = "colnames_explicit",
      site_name = "sites",
      date_year = "yr",
      date_month = "mth",
      date_day = "dy",
      dl_path = "./testdir",
      save = TRUE,
      verbose = FALSE
    ))
  )
})

skip("local only")

test_that("daymet_check() basic functionality", {
  expect_silent(
    status <- daymet_check(
      daymet_reqs = requests_explicit,
      ed_username = "rdjmacklin_bc",
      verbose = FALSE
    )
  )

  expect_silent(
    status_path <- daymet_check(
      daymet_reqs = "./testdir/daymet/colnames_explicit.RDS",
      ed_username = "rdjmacklin_bc",
      verbose = FALSE
    )
  )

  expect_true(inherits(status, "data.frame"))
  expect_named(
    status,
    c("request_name", "request_id", "date", "status", "expires_on")
  )
  expect_true(identical(status, status_path))
  expect_true(all(requests_explicit$request_name %in% status$request_name))
})

skip("local only")

test_that("daymet_check() expected error messages", {
  expect_error(
    daymet_check(
      daymet_reqs = "./fake/file/path.rds",
      ed_username = "rdjmacklin_bc",
      verbose = FALSE
    ),
    "\\[Daymet Request Checking\\] daymet_reqs in an unexpected format. Please provide either a data.frame with a column for the AppEEARS request name called request_name and a column for the AppEEARS request ID called request_id, or a filepath to a .rds file created by daymet_request\\(\\) containing such data."
  )

  expect_error(
    daymet_check(
      daymet_reqs = 27,
      ed_username = "rdjmacklin_bc",
      verbose = FALSE
    ),
    "\\[Daymet Request Checking\\] daymet_reqs in an unexpected format. Please provide either a data.frame with a column for the AppEEARS request name called request_name and a column for the AppEEARS request ID called request_id, or a filepath to a .rds file created by daymet_request\\(\\) containing such data."
  )

  bad_reqs <- requests_explicit
  names(bad_reqs)[1] <- "rq_nm"

  expect_error(
    daymet_check(
      daymet_reqs = bad_reqs,
      ed_username = "rdjmacklin_bc",
      verbose = FALSE
    ),
    "\\[Daymet Request Checking\\] daymet_reqs in an unexpected format. Please provide either a data.frame with a column for the AppEEARS request name called request_name and a column for the AppEEARS request ID called request_id, or a filepath to a .rds file created by daymet_request\\(\\) containing such data."
  )

  bad_reqs <- requests_explicit
  bad_reqs$request_id <- 1:nrow(requests_explicit)

  invisible(capture.output(
    errmsg <- cat(
      "\\[Daymet Request Checking\\] request(s) ",
      stringr::str_flatten_comma(bad_reqs$request_name),
      " provided in daymet_reqs are not registered under EarthData user rdjmacklin_bc. Are they more than a month old \\(i.e., expired\\), or entered incorrectly\\?"
    )
  ))

  expect_error(
    daymet_check(
      daymet_reqs = bad_reqs,
      ed_username = "rdjmacklin_bc",
      verbose = FALSE
    ),
    errmsg
  )
})

skip("local only")

test_that("daymet_download() successfully downloads files from completed requests.", {
  expect_silent(
    downloads_sf_pt <<- daymet_download(
      daymet_reqs = requests_sf_pt,
      ed_username = "rdjmacklin_bc",
      dl_path = "./testdir",
      verbose = FALSE
    )
  )

  expected_dirs <- paste0("./testdir/daymet/", requests_sf_pt$request_name)
  expect_true(all(dir.exists(expected_dirs)))
  expect_true(all(file.exists(paste0(
    expected_dirs,
    "/DAYMET-004-Statistics.csv"
  ))))

  for (i in requests_sf_pt$request_name) {
    daymet_stats <- readr::read_csv(
      paste0("./testdir/daymet/", i, "/DAYMET-004-Statistics.csv"),
      show_col_types = FALSE
    )

    filename <- gsub(
      pattern = "DAYMET_",
      replacement = "DAYMET.",
      daymet_stats$`File Name`[
        daymet_stats$Date ==
          requests_sf_pt$date[requests_sf_pt$request_name == i] &
          daymet_stats$Dataset == "prcp"
      ]
    )

    expect_true(file.exists(paste0(
      "./testdir/daymet/",
      i,
      "/",
      filename,
      ".tif"
    )))
    expect_true(
      file.size(paste0("./testdir/daymet/", i, "/", filename, ".tif")) > 2000
    )
  }
})

skip("local only")

test_that("daymet_download() fails when a request is incomplete.", {
  expect_silent(
    requests_fresh <- suppressMessages(daymet_request(
      data_fmt(
        bcch_restricted,
        coord_lon = "longitude",
        coord_lat = "latitude",
        crs = 4326
      ),
      ed_username = "rdjmacklin_bc",
      covariates = "daymet_prcp",
      request_name = "fresh",
      dl_path = "./testdir",
      save = FALSE,
      verbose = FALSE
    ))
  )

  invisible(capture.output(
    errmsg <- cat(
      "\\[Daymet Download\\] some supplied Daymet requests are incomplete. Please wait for confirmation at the email address associated with your EarthData account 'rdjmacklin_bc' or use daymet_check\\(\\) to confirm that requests with the following request IDs are complete: ",
      stringr::str_flatten_comma(requests_fresh$request_name),
      "."
    )
  ))

  expect_error(
    downloads_sf_pt <- daymet_download(
      daymet_reqs = requests_fresh,
      ed_username = "rdjmacklin_bc",
      dl_path = "./testdir",
      verbose = FALSE
    ),
    errmsg
  )
})

skip("local only")

test_that("daymet_extract() basic functionality with all expected inputs.", {
  expect_output(
    downloads_sf_poly <<- daymet_download(
      daymet_reqs = requests_sf_poly,
      ed_username = "rdjmacklin_bc",
      dl_path = "./testdir",
      verbose = FALSE
    ),
    "\n"
  )

  expect_output(
    downloads_terra_pt <<- daymet_download(
      daymet_reqs = requests_terra_pt,
      ed_username = "rdjmacklin_bc",
      dl_path = "./testdir",
      verbose = FALSE
    ),
    "\n"
  )

  expect_output(
    downloads_terra_poly <<- daymet_download(
      daymet_reqs = requests_terra_poly,
      ed_username = "rdjmacklin_bc",
      dl_path = "./testdir",
      verbose = FALSE
    ),
    "\n"
  )

  expect_output(
    extracted_sf_pt <- daymet_extract(
      data = suppressMessages(data_fmt(
        bcch_restricted,
        coord_lon = "longitude",
        coord_lat = "latitude",
        crs = 4326
      )),
      daymet_reqs = downloads_sf_pt,
      covariates = "daymet_prcp",
      dl_path = "./testdir",
      verbose = FALSE
    ),
    "\n"
  )

  expect_output(
    extracted_sf_poly <- daymet_extract(
      data = suppressMessages(data_buff(data_fmt(
        bcch_restricted,
        coord_lon = "longitude",
        coord_lat = "latitude",
        crs = 4326
      ))),
      daymet_reqs = downloads_sf_poly,
      covariates = "daymet_prcp",
      dl_path = "./testdir",
      verbose = FALSE
    ),
    "\n"
  )

  expect_output(
    extracted_terra_pt <- daymet_extract(
      data = suppressMessages(data_fmt(terra::vect(
        bcch_restricted,
        crs = "epsg:4326",
        geom = c("longitude", "latitude")
      ))),
      daymet_reqs = downloads_terra_pt,
      covariates = "daymet_prcp",
      dl_path = "./testdir",
      verbose = FALSE
    ),
    "\n"
  )

  expect_output(
    extracted_terra_poly <- daymet_extract(
      data = suppressMessages(data_buff(data_fmt(terra::vect(
        bcch_restricted,
        crs = "epsg:4326",
        geom = c("longitude", "latitude")
      )))),
      daymet_reqs = downloads_terra_poly,
      covariates = "daymet_prcp",
      dl_path = "./testdir",
      verbose = FALSE
    ),
    "\n"
  )

  expect_s3_class(extracted_sf_pt, "sf")
  expect_equal(
    as.character(sf::st_geometry_type(extracted_sf_pt, by_geometry = FALSE)),
    "POINT"
  )
  expect_named(
    extracted_sf_pt,
    c(
      "SurveyAreaIdentifier",
      "latitude",
      "longitude",
      "survey_year",
      "survey_month",
      "survey_day",
      "prcp",
      "geometry"
    )
  )
  expect_true(inherits(extracted_sf_pt$prcp, "numeric"))
  expect_equal(
    format(sf::st_crs(extracted_sf_pt)),
    "Canada_Albers_Equal_Area_Conic"
  )
  expect_true(all(!is.na(extracted_sf_pt$prcp)))

  expect_s3_class(extracted_sf_poly, "sf")
  expect_equal(
    as.character(sf::st_geometry_type(extracted_sf_poly, by_geometry = FALSE)),
    "POLYGON"
  )
  expect_named(
    extracted_sf_poly,
    c(
      "SurveyAreaIdentifier",
      "latitude",
      "longitude",
      "survey_year",
      "survey_month",
      "survey_day",
      "prcp_mean",
      "geometry"
    )
  )
  expect_true(inherits(extracted_sf_poly$prcp, "numeric"))
  expect_equal(
    format(sf::st_crs(extracted_sf_poly)),
    "Canada_Albers_Equal_Area_Conic"
  )
  expect_true(all(!is.na(extracted_sf_poly$prcp)))

  expect_s3_class(extracted_terra_pt, "sf")
  expect_equal(
    as.character(sf::st_geometry_type(extracted_terra_pt, by_geometry = FALSE)),
    "POINT"
  )
  expect_named(
    extracted_terra_pt,
    c(
      "SurveyAreaIdentifier",
      "latitude",
      "longitude",
      "survey_year",
      "survey_month",
      "survey_day",
      "prcp",
      "geometry"
    )
  )
  expect_true(inherits(extracted_terra_pt$prcp, "numeric"))
  expect_equal(
    format(sf::st_crs(extracted_terra_pt)),
    "Canada_Albers_Equal_Area_Conic"
  )
  expect_true(all(!is.na(extracted_terra_pt$prcp)))

  expect_s3_class(extracted_terra_poly, "sf")
  expect_equal(
    as.character(sf::st_geometry_type(
      extracted_terra_poly,
      by_geometry = FALSE
    )),
    "POLYGON"
  )
  expect_named(
    extracted_terra_poly,
    c(
      "SurveyAreaIdentifier",
      "latitude",
      "longitude",
      "survey_year",
      "survey_month",
      "survey_day",
      "prcp_mean",
      "geometry"
    )
  )
  expect_true(inherits(extracted_terra_poly$prcp, "numeric"))
  expect_equal(
    format(sf::st_crs(extracted_terra_poly)),
    "Canada_Albers_Equal_Area_Conic"
  )
  expect_true(all(!is.na(extracted_terra_poly$prcp)))
})

skip("local only")

test_that("daymet_extract() returns warnings and errors for out of coverage dates and sites.", {
  bcch_modified <- bcch_restricted
  bcch_modified$survey_year[1] <- 2010

  expect_warning(
    extracted_sf_pt <- daymet_extract(
      data = suppressMessages(data_fmt(
        bcch_modified,
        coord_lon = "longitude",
        coord_lat = "latitude",
        crs = 4326
      )),
      daymet_reqs = downloads_sf_pt,
      covariates = "daymet_prcp",
      dl_path = "./testdir",
      verbose = FALSE
    ),
    "\\[Daymet Extraction\\] data has not been provided for some dates. These are: 2010-02-12. No value will be returned for these dates. Keep in mind that Daymet data for the current year may not be available yet."
  )

  bcch_modified <- bcch_restricted
  bcch_modified$latitude[1] <- 80

  expect_warning(
    extracted_sf_pt <- daymet_extract(
      data = suppressMessages(data_fmt(
        bcch_modified,
        coord_lon = "longitude",
        coord_lat = "latitude",
        crs = 4326
      )),
      daymet_reqs = downloads_sf_pt,
      covariates = "daymet_prcp",
      dl_path = "./testdir",
      verbose = FALSE
    ),
    "\\[Daymet \\(prcp\\) Extraction\\]  site FilledSurveyArea1 falls outside of the spatial extent of the DAYMET rasters provided. No value will be returned."
  )

  bcch_modified <- bcch_restricted
  bcch_modified$latitude[1] <- 46.13

  expect_warning(
    extracted_sf_poly <- daymet_extract(
      data = suppressMessages(data_buff(
        data_fmt(
          bcch_modified,
          coord_lon = "longitude",
          coord_lat = "latitude",
          crs = 4326
        ),
        buffer_distance = 5,
        buffer_units = "km"
      )),
      daymet_reqs = downloads_sf_pt,
      covariates = "daymet_prcp",
      dl_path = "./testdir",
      verbose = FALSE
    ),
    "\\[Daymet \\(prcp\\) Extraction\\] site FilledSurveyArea1's buffered area is only partially contained by the spatial extent of the DAYMET rasters provided. Returned prcp value will be derived from the available values."
  )
})

skip("local only")

test_that("daymet_extract() succeeds with alternate summary statistics, and throws error when needed.", {
  sf_pt <- suppressWarnings(suppressMessages(data_fmt(
    bcch_restricted[1, ],
    coord_lon = "longitude",
    coord_lat = "latitude",
    crs = 4326
  )))
  sf_poly <- suppressWarnings(suppressMessages(data_buff(data_fmt(
    bcch_restricted[1, ],
    coord_lon = "longitude",
    coord_lat = "latitude",
    crs = 4326
  ))))

  expect_output(
    extracted <- suppressMessages(daymet_extract(
      sf_pt,
      daymet_reqs = downloads_sf_pt,
      dl_path = "./testdir",
      method = "bilinear", # Add some arguments of terra::extract() to check
      # for errors.
      layer = 1
    )),
    "\n"
  )

  expect_named(
    extracted,
    c(
      "SurveyAreaIdentifier",
      "latitude",
      "longitude",
      "survey_year",
      "survey_month",
      "survey_day",
      "prcp",
      "geometry"
    )
  )
  expect_true(inherits(extracted$prcp, "numeric"))

  # Test a few standard functions.
  expect_output(
    extracted <- suppressMessages(daymet_extract(
      sf_poly,
      daymet_reqs = downloads_sf_poly,
      dl_path = "./testdir",
      fun = c("median", "max", "stdev")
    )),
    "\n"
  )

  expect_named(
    extracted,
    c(
      "SurveyAreaIdentifier",
      "latitude",
      "longitude",
      "survey_year",
      "survey_month",
      "survey_day",
      "prcp_median",
      "prcp_max",
      "prcp_stdev",
      "geometry"
    )
  )
  expect_true(inherits(extracted$prcp_median, "numeric"))
  expect_true(inherits(extracted$prcp_max, "numeric"))
  expect_true(inherits(extracted$prcp_stdev, "numeric"))

  # Test functions with specific requirements

  # Check that quantile requires quantiles argument.
  expect_error(
    extracted <- suppressMessages(daymet_extract(
      sf_poly,
      daymet_reqs = downloads_sf_poly,
      dl_path = "./testdir",
      fun = "quantile"
    )),
    "\\[Daymet Extraction\\] quantile summary requested but no quantiles supplied to the 'quantiles' argument. Please supply numeric value\\(s\\) of desired quantiles."
  )

  # Check that one or more quantile joins correctly.
  expect_output(
    extracted <- suppressMessages(daymet_extract(
      sf_poly,
      daymet_reqs = downloads_sf_poly,
      dl_path = "./testdir",
      fun = "quantile",
      quantiles = c(0.25)
    )),
    "\n"
  )

  expect_named(
    extracted,
    c(
      "SurveyAreaIdentifier",
      "latitude",
      "longitude",
      "survey_year",
      "survey_month",
      "survey_day",
      "prcp_quantile",
      "geometry"
    )
  )
  expect_true(inherits(extracted$prcp_quantile, "numeric"))

  expect_output(
    extracted <- suppressMessages(daymet_extract(
      sf_poly,
      daymet_reqs = downloads_sf_poly,
      dl_path = "./testdir",
      fun = "quantile",
      quantiles = c(0.25, 0.75)
    )),
    "\n"
  )
  expect_named(
    extracted,
    c(
      "SurveyAreaIdentifier",
      "latitude",
      "longitude",
      "survey_year",
      "survey_month",
      "survey_day",
      "prcp_quantile_25",
      "prcp_quantile_75",
      "geometry"
    )
  )
  expect_true(inherits(extracted$prcp_quantile_25, "numeric"))
  expect_true(inherits(extracted$prcp_quantile_75, "numeric"))

  # Check that weighted functions require weights argument.
  expect_error(
    extracted <- suppressMessages(daymet_extract(
      sf_poly,
      daymet_reqs = downloads_sf_poly,
      dl_path = "./testdir",
      fun = "weighted_mean"
    )),
    "\\[Daymet Extraction\\] weighted summary requested but no weights supplied via the 'weights' argument. Please supply either a weighting raster or 'area' to use the cell areas of the Daymet raster as weights."
  )

  # Check that fractions join correctly.
  expect_output(
    extracted <- suppressMessages(daymet_extract(
      sf_poly,
      daymet_reqs = downloads_sf_poly,
      dl_path = "./testdir",
      fun = "frac"
    )),
    "\n"
  )

  expect_named(
    extracted,
    c(
      "SurveyAreaIdentifier",
      "latitude",
      "longitude",
      "survey_year",
      "survey_month",
      "survey_day",
      "prcp_frac_0.790000021457672",
      "prcp_frac_0.810000002384186",
      "prcp_frac_0.819999992847443",
      "geometry"
    )
  )
  expect_true(inherits(extracted$prcp_frac_0.790000021457672, "numeric"))

  # Test that user specified functions work.
  my_function <- function(value, cov_frac) {
    mean(value * cov_frac)
  }

  expect_output(
    extracted <- suppressMessages(daymet_extract(
      sf_poly,
      daymet_reqs = downloads_sf_poly,
      dl_path = "./testdir",
      fun = my_function
    )),
    "\n"
  )

  expect_named(
    extracted,
    c(
      "SurveyAreaIdentifier",
      "latitude",
      "longitude",
      "survey_year",
      "survey_month",
      "survey_day",
      "prcp_user_defined_function",
      "geometry"
    )
  )

  # Test that functions that return more than one value throw error.
  my_function <- function(value, cov_frac) {
    value * cov_frac
  }

  expect_error(
    extracted <- suppressMessages(daymet_extract(
      sf_poly,
      daymet_reqs = downloads_sf_poly,
      dl_path = "./testdir",
      fun = my_function
    )),
    "\\[Daymet Extraction\\] support for custom summary functions is currently limited to functions returning a single value \\(not stored in a data.frame\\) to allow accurate joining to input data."
  )
})

unlink("./testdir", recursive = TRUE)
