if (!dir.exists("./testdir")) {
  dir.create("./testdir")
}

test_that("nc_covariates_merge() succeeds with data.frame, sf, and terra point inputs.", {
  formatted <- suppressWarnings(suppressMessages(data_fmt(bcch)))

  elev <- suppressMessages(elevation_download(
    data = formatted,
    dl_path = "./testdir",
    progress = FALSE
  ))

  extracted <- suppressMessages(elevation_extract(
    data = formatted,
    elevation_data = elev
  ))

  vegetation <- suppressWarnings(suppressMessages(vegetation_download(
    data = extracted,
    ed_email = "rmacklin@birdscanada.org",
    dl_path = "./testdir",
    progress = FALSE
  )))

  extracted <- suppressWarnings(suppressMessages(vegetation_extract(
    data = extracted,
    covariates = "modis_ndvi",
    vegetation_files = vegetation
  )))

  expect_silent(
    merged <- nc_covariates_merge(
      original_data = bcch,
      covariate_data = extracted
    )
  )

  expect_equal(c(names(bcch), "elevation", "ndvi"), names(merged))

  original_match <- merged %>%
    dplyr::select(
      "latitude",
      "longitude",
      "survey_year",
      "survey_month",
      "survey_day",
      "elevation",
      "ndvi"
    ) %>%
    dplyr::distinct() %>%
    dplyr::arrange(
      .data$latitude,
      .data$longitude,
      .data$survey_year,
      .data$survey_month,
      .data$survey_day
    )

  covariate_match <- extracted %>%
    dplyr::select(
      "latitude",
      "longitude",
      "survey_year",
      "survey_month",
      "survey_day",
      "elevation",
      "ndvi"
    ) %>%
    dplyr::arrange(
      .data$latitude,
      .data$longitude,
      .data$survey_year,
      .data$survey_month,
      .data$survey_day
    ) %>%
    sf::st_drop_geometry()

  expect_true(identical(original_match, covariate_match))
})

test_that("nc_covariates_merge() succeeds with data.frame, sf, and terra polygon inputs.", {
  formatted <- suppressWarnings(suppressMessages(data_buff(data_fmt(bcch))))

  elev <- suppressMessages(elevation_download(
    data = formatted,
    dl_path = "./testdir",
    progress = FALSE
  ))

  extracted <- suppressMessages(elevation_extract(
    data = formatted,
    elevation_data = elev
  ))

  vegetation <- suppressWarnings(suppressMessages(vegetation_download(
    data = extracted,
    ed_email = "rmacklin@birdscanada.org",
    dl_path = "./testdir",
    progress = FALSE
  )))

  extracted <- suppressWarnings(suppressMessages(vegetation_extract(
    data = extracted,
    covariates = "modis_ndvi",
    vegetation_files = vegetation
  )))

  expect_silent(
    merged <- nc_covariates_merge(
      original_data = bcch,
      covariate_data = extracted
    )
  )

  expect_equal(c(names(bcch), "elevation", "ndvi"), names(merged))

  original_match <- merged %>%
    dplyr::select(
      "latitude",
      "longitude",
      "survey_year",
      "survey_month",
      "survey_day",
      "elevation",
      "ndvi"
    ) %>%
    distinct() %>%
    dplyr::arrange(
      .data$latitude,
      .data$longitude,
      .data$survey_year,
      .data$survey_month,
      .data$survey_day
    )

  covariate_match <- extracted %>%
    dplyr::select(
      "latitude",
      "longitude",
      "survey_year",
      "survey_month",
      "survey_day",
      "elevation",
      "ndvi"
    ) %>%
    dplyr::arrange(
      .data$latitude,
      .data$longitude,
      .data$survey_year,
      .data$survey_month,
      .data$survey_day
    ) %>%
    sf::st_drop_geometry()

  expect_true(identical(original_match, covariate_match))
})

test_that("nc_covariates_merge() succeeds with alternate column names, either specified explicitly or passed through attributes.", {
  formatted <- suppressWarnings(suppressMessages(data_fmt(
    dplyr::rename(
      bcch,
      "sites" = "SurveyAreaIdentifier",
      "yr" = "survey_year",
      "mth" = "survey_month",
      "dy" = "survey_day"
    ),
    site_name = "sites",
    date_year = "yr",
    date_month = "mth",
    date_day = "dy"
  )))

  elev <- suppressMessages(elevation_download(
    data = formatted,
    dl_path = "./testdir",
    progress = FALSE
  ))

  extracted <- suppressMessages(elevation_extract(
    data = formatted,
    elevation_data = elev
  ))

  vegetation <- suppressWarnings(suppressMessages(vegetation_download(
    data = extracted,
    ed_email = "rmacklin@birdscanada.org",
    dl_path = "./testdir",
    progress = FALSE
  )))

  extracted <- suppressWarnings(suppressMessages(vegetation_extract(
    data = extracted,
    covariates = "modis_ndvi",
    vegetation_files = vegetation
  )))

  expect_silent(
    merged <- nc_covariates_merge(
      original_data = dplyr::rename(
        bcch,
        "sites" = "SurveyAreaIdentifier",
        "yr" = "survey_year",
        "mth" = "survey_month",
        "dy" = "survey_day"
      ),
      covariate_data = extracted
    )
  )

  expect_equal(
    c(
      names(dplyr::rename(
        bcch,
        "sites" = "SurveyAreaIdentifier",
        "yr" = "survey_year",
        "mth" = "survey_month",
        "dy" = "survey_day"
      )),
      "elevation",
      "ndvi"
    ),
    names(merged)
  )

  original_match <- merged %>%
    dplyr::select(
      "latitude",
      "longitude",
      "yr",
      "mth",
      "dy",
      "elevation",
      "ndvi"
    ) %>%
    dplyr::distinct() %>%
    dplyr::arrange(
      .data$latitude,
      .data$longitude,
      .data$yr,
      .data$mth,
      .data$dy
    )

  covariate_match <- extracted %>%
    dplyr::select(
      "latitude",
      "longitude",
      "yr",
      "mth",
      "dy",
      "elevation",
      "ndvi"
    ) %>%
    dplyr::arrange(
      .data$latitude,
      .data$longitude,
      .data$yr,
      .data$mth,
      .data$dy
    ) %>%
    sf::st_drop_geometry()

  expect_true(identical(original_match, covariate_match))

  formatted <- suppressWarnings(suppressMessages(data_fmt(bcch)))

  elev <- suppressMessages(elevation_download(
    data = formatted,
    dl_path = "./testdir",
    progress = FALSE
  ))

  extracted <- suppressMessages(elevation_extract(
    data = formatted,
    elevation_data = elev
  ))

  vegetation <- suppressWarnings(suppressMessages(vegetation_download(
    data = extracted,
    ed_email = "rmacklin@birdscanada.org",
    dl_path = "./testdir",
    progress = FALSE
  )))

  extracted <- suppressWarnings(suppressMessages(vegetation_extract(
    data = extracted,
    covariates = "modis_ndvi",
    vegetation_files = vegetation
  )))

  extracted <- dplyr::rename(
    extracted,
    "sites" = "SurveyAreaIdentifier",
    "yr" = "survey_year",
    "mth" = "survey_month",
    "dy" = "survey_day"
  )
  bcch <- dplyr::rename(
    bcch,
    "sites" = "SurveyAreaIdentifier",
    "yr" = "survey_year",
    "mth" = "survey_month",
    "dy" = "survey_day"
  )

  expect_silent(
    merged <- nc_covariates_merge(
      original_data = bcch,
      covariate_data = extracted,
      site_name = "sites",
      date_year = "yr",
      date_month = "mth",
      date_day = "dy"
    )
  )

  expect_equal(c(names(bcch), "elevation", "ndvi"), names(merged))

  original_match <- merged %>%
    dplyr::select(
      "latitude",
      "longitude",
      "yr",
      "mth",
      "dy",
      "elevation",
      "ndvi"
    ) %>%
    dplyr::distinct() %>%
    dplyr::arrange(
      .data$latitude,
      .data$longitude,
      .data$yr,
      .data$mth,
      .data$dy
    )

  covariate_match <- extracted %>%
    dplyr::select(
      "latitude",
      "longitude",
      "yr",
      "mth",
      "dy",
      "elevation",
      "ndvi"
    ) %>%
    dplyr::arrange(
      .data$latitude,
      .data$longitude,
      .data$yr,
      .data$mth,
      .data$dy
    ) %>%
    sf::st_drop_geometry()

  expect_true(identical(original_match, covariate_match))
})

test_that("nc_covariates_merge() succeeds with lubridate or ordinal dates.", {})
