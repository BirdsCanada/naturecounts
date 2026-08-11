if (!dir.exists("./testdir")) {
  dir.create("./testdir")
}

test_that("nc_covariates_merge() succeeds with data.frame, sf, and terra point inputs.", {
  # Test with original_data = data.frame
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

  # Test with original_data = sf POINT
  formatted <- suppressWarnings(suppressMessages(data_fmt(sf::st_as_sf(
    bcch,
    coords = c("longitude", "latitude"),
    crs = 4326
  ))))

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
      original_data = sf::st_as_sf(
        bcch,
        coords = c("longitude", "latitude"),
        crs = 4326
      ),
      covariate_data = extracted
    )
  )

  expect_equal(
    c(
      names(sf::st_drop_geometry(sf::st_as_sf(
        bcch,
        coords = c("longitude", "latitude"),
        crs = 4326
      ))),
      "elevation",
      "ndvi",
      "geometry"
    ),
    names(merged)
  )

  original_match <- merged %>%
    cbind(., sf::st_coordinates(.)) %>%
    dplyr::rename("longitude" = "X", "latitude" = "Y") %>%
    sf::st_drop_geometry() %>%
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

  # Test with original_data = terra point
  formatted <- suppressWarnings(suppressMessages(data_fmt(terra::vect(
    bcch,
    geom = c("longitude", "latitude"),
    crs = "epsg:4326"
  ))))

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
      original_data = terra::vect(
        bcch,
        geom = c("longitude", "latitude"),
        crs = "epsg:4326"
      ),
      covariate_data = extracted
    )
  )

  expect_equal(
    c(
      names(terra::vect(
        bcch,
        geom = c("longitude", "latitude"),
        crs = "epsg:4326"
      )),
      "elevation",
      "ndvi"
    ),
    names(merged)
  )

  original_match <- merged %>%
    cbind(
      .,
      terra::crds(terra::vect(
        bcch,
        geom = c("longitude", "latitude"),
        crs = "epsg:4326"
      ))
    ) %>%
    dplyr::rename("longitude" = "x", "latitude" = "y") %>%
    as.data.frame() %>%
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

  # Test with original_data = sf POLYGON
  formatted <- suppressWarnings(suppressMessages(data_fmt(data_buff(sf::st_as_sf(
    bcch,
    coords = c("longitude", "latitude"),
    crs = 4326
  )))))

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
      original_data = suppressMessages(data_buff(sf::st_as_sf(
        bcch,
        coords = c("longitude", "latitude"),
        crs = 4326
      ))),
      covariate_data = extracted
    )
  )

  expect_equal(
    c(
      names(sf::st_drop_geometry(suppressMessages(data_buff(sf::st_as_sf(
        bcch,
        coords = c("longitude", "latitude"),
        crs = 4326
      ))))),
      "elevation",
      "ndvi",
      "geometry"
    ),
    names(merged)
  )

  original_match <- merged %>%
    cbind(., sf::st_coordinates(suppressWarnings(sf::st_centroid(.)))) %>%
    dplyr::rename("longitude" = "X", "latitude" = "Y") %>%
    sf::st_drop_geometry() %>%
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

  # Test with original_data = terra polygons
  formatted <- suppressWarnings(suppressMessages(data_fmt(data_buff(terra::vect(
    bcch,
    geom = c("longitude", "latitude"),
    crs = "epsg:4326"
  )))))

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
      original_data = suppressMessages(data_buff(terra::vect(
        bcch,
        geom = c("longitude", "latitude"),
        crs = "epsg:4326"
      ))),
      covariate_data = extracted
    )
  )

  expect_equal(
    c(
      names(terra::vect(
        bcch,
        geom = c("longitude", "latitude"),
        crs = "epsg:4326"
      )),
      "elevation",
      "ndvi"
    ),
    names(merged)
  )

  original_match <- merged %>%
    cbind(
      .,
      terra::crds(terra::centroids(suppressMessages(data_buff(terra::vect(
        bcch,
        geom = c("longitude", "latitude"),
        crs = "epsg:4326"
      )))))
    ) %>%
    dplyr::rename("longitude" = "x", "latitude" = "y") %>%
    as.data.frame() %>%
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

test_that("nc_covariates_merge() succeeds with lubridate or ordinal dates.", {
  bcch_lubridate <- bcch %>%
    dplyr::mutate(
      date = as.Date(paste0(
        .data$survey_year,
        "-",
        .data$survey_month,
        "-",
        .data$survey_day
      )),
      .keep = "unused"
    )

  formatted <- suppressWarnings(suppressMessages(data_fmt(
    bcch_lubridate,
    date_lubridate = "date"
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
      original_data = bcch_lubridate,
      covariate_data = extracted,
      date_lubridate = "date"
    )
  )

  expect_equal(c(names(bcch_lubridate), "elevation", "ndvi"), names(merged))

  original_match <- merged %>%
    dplyr::select(
      "latitude",
      "longitude",
      "date",
      "elevation",
      "ndvi"
    ) %>%
    dplyr::distinct() %>%
    dplyr::arrange(
      .data$latitude,
      .data$longitude,
      .data$date
    )

  covariate_match <- extracted %>%
    dplyr::select(
      "latitude",
      "longitude",
      "date",
      "elevation",
      "ndvi"
    ) %>%
    dplyr::arrange(
      .data$latitude,
      .data$longitude,
      .data$date
    ) %>%
    sf::st_drop_geometry()

  expect_true(identical(original_match, covariate_match))

  bcch_ordinal <- bcch_lubridate %>%
    dplyr::mutate(
      survey_year = lubridate::year(.data$date),
      doy = lubridate::yday(.data$date),
      .keep = "unused"
    )

  formatted <- suppressWarnings(suppressMessages(data_fmt(
    bcch_ordinal,
    date_ordinal = "doy"
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
      original_data = bcch_ordinal,
      covariate_data = extracted,
      date_ordinal = "doy"
    )
  )

  expect_equal(c(names(bcch_ordinal), "elevation", "ndvi"), names(merged))

  original_match <- merged %>%
    dplyr::select(
      "latitude",
      "longitude",
      "survey_year",
      "doy",
      "elevation",
      "ndvi"
    ) %>%
    dplyr::distinct() %>%
    dplyr::arrange(
      .data$latitude,
      .data$longitude,
      .data$survey_year,
      .data$doy
    )

  covariate_match <- extracted %>%
    dplyr::select(
      "latitude",
      "longitude",
      "survey_year",
      "doy",
      "elevation",
      "ndvi"
    ) %>%
    dplyr::arrange(
      .data$latitude,
      .data$longitude,
      .data$survey_year,
      .data$doy
    ) %>%
    sf::st_drop_geometry()

  expect_true(identical(original_match, covariate_match))
})
