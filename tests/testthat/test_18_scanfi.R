if (!dir.exists("./testdir")) {
  dir.create("./testdir")
}

bcch_restricted <- bcch[bcch$survey_year %in% c(2000:2007), ]

test_that("scanfi_download() downloads correct files with all expected inputs.", {
  expect_silent(
    ponderosa_sf_pt <<- suppressMessages(scanfi_download(
      data_fmt(
        bcch_restricted,
        coord_lon = "longitude",
        coord_lat = "latitude",
        crs = 4326
      ),
      covariates = "scanfi_ponderosapine",
      dl_path = "./testdir",
      progress = FALSE
    ))
  )

  file.remove(
    "./testdir/scanfi/SCANFI_spsCC_ponderosaPine_2000_v2_20260119.tif",
    "./testdir/scanfi/SCANFI_spsCC_ponderosaPine_2005_v2_20260119.tif"
  )

  expect_silent(
    ponderosa_sf_poly <<- suppressMessages(scanfi_download(
      data_buff(data_fmt(
        bcch_restricted,
        coord_lon = "longitude",
        coord_lat = "latitude",
        crs = 4326
      )),
      covariates = "scanfi_ponderosapine",
      dl_path = "./testdir",
      progress = FALSE
    ))
  )

  file.remove(
    "./testdir/scanfi/SCANFI_spsCC_ponderosaPine_2000_v2_20260119.tif",
    "./testdir/scanfi/SCANFI_spsCC_ponderosaPine_2005_v2_20260119.tif"
  )

  expect_silent(
    ponderosa_terra_pt <<- suppressMessages(scanfi_download(
      data_fmt(terra::vect(
        bcch_restricted,
        crs = "epsg:4326",
        geom = c("longitude", "latitude")
      )),
      covariates = "scanfi_ponderosapine",
      dl_path = "./testdir",
      progress = FALSE
    ))
  )

  file.remove(
    "./testdir/scanfi/SCANFI_spsCC_ponderosaPine_2000_v2_20260119.tif",
    "./testdir/scanfi/SCANFI_spsCC_ponderosaPine_2005_v2_20260119.tif"
  )

  expect_silent(
    ponderosa_terra_poly <<- suppressMessages(scanfi_download(
      data_buff(data_fmt(terra::vect(
        bcch_restricted,
        crs = "epsg:4326",
        geom = c("longitude", "latitude")
      ))),
      covariates = "scanfi_ponderosapine",
      dl_path = "./testdir",
      progress = FALSE
    ))
  )

  file.remove(
    "./testdir/scanfi/SCANFI_spsCC_ponderosaPine_2000_v2_20260119.tif",
    "./testdir/scanfi/SCANFI_spsCC_ponderosaPine_2005_v2_20260119.tif"
  )

  expect_silent(
    ponderosa_manualyear <<- suppressMessages(scanfi_download(
      use_date = FALSE,
      snapshot_year = c(2000, 2005),
      covariates = "scanfi_ponderosapine",
      dl_path = "./testdir",
      progress = FALSE
    ))
  )
})

test_that("Results downloaded from scanfi_download() have expected features.", {
  expect_true(dir.exists("./testdir/scanfi")) # Bonus test of custom file path specification

  expect_true(all(file.exists(
    "./testdir/scanfi/SCANFI_spsCC_ponderosaPine_2000_v2_20260119.tif",
    "./testdir/scanfi/SCANFI_spsCC_ponderosaPine_2005_v2_20260119.tif"
  )))
  expect_true(all(
    file.size(
      "./testdir/scanfi/SCANFI_spsCC_ponderosaPine_2000_v2_20260119.tif",
      "./testdir/scanfi/SCANFI_spsCC_ponderosaPine_2005_v2_20260119.tif"
    ) >
      50000000
  )) # Check files contain a reasonable amount of data

  expect_true(inherits(ponderosa_sf_pt, "list"))
  expect_named(ponderosa_sf_pt, c("2000", "2005"))
  expect_true(inherits(ponderosa_sf_pt$`2000`, "list"))
  expect_named(ponderosa_sf_pt$`2000`, "ponderosapine")
  expect_s4_class(ponderosa_sf_pt$`2000`$ponderosapine, "SpatRaster")
  expect_true(inherits(ponderosa_sf_pt$`2005`, "list"))
  expect_named(ponderosa_sf_pt$`2005`, "ponderosapine")
  expect_s4_class(ponderosa_sf_pt$`2005`$ponderosapine, "SpatRaster")

  expect_true(inherits(ponderosa_sf_poly, "list"))
  expect_named(ponderosa_sf_poly, c("2000", "2005"))
  expect_true(inherits(ponderosa_sf_poly$`2000`, "list"))
  expect_named(ponderosa_sf_poly$`2000`, "ponderosapine")
  expect_s4_class(ponderosa_sf_poly$`2000`$ponderosapine, "SpatRaster")
  expect_true(inherits(ponderosa_sf_poly$`2005`, "list"))
  expect_named(ponderosa_sf_poly$`2005`, "ponderosapine")
  expect_s4_class(ponderosa_sf_poly$`2005`$ponderosapine, "SpatRaster")

  expect_true(inherits(ponderosa_terra_pt, "list"))
  expect_named(ponderosa_terra_pt, c("2000", "2005"))
  expect_true(inherits(ponderosa_terra_pt$`2000`, "list"))
  expect_named(ponderosa_terra_pt$`2000`, "ponderosapine")
  expect_s4_class(ponderosa_terra_pt$`2000`$ponderosapine, "SpatRaster")
  expect_true(inherits(ponderosa_terra_pt$`2005`, "list"))
  expect_named(ponderosa_terra_pt$`2005`, "ponderosapine")
  expect_s4_class(ponderosa_terra_pt$`2005`$ponderosapine, "SpatRaster")

  expect_true(inherits(ponderosa_terra_poly, "list"))
  expect_named(ponderosa_terra_poly, c("2000", "2005"))
  expect_true(inherits(ponderosa_terra_poly$`2000`, "list"))
  expect_named(ponderosa_terra_poly$`2000`, "ponderosapine")
  expect_s4_class(ponderosa_terra_poly$`2000`$ponderosapine, "SpatRaster")
  expect_true(inherits(ponderosa_terra_poly$`2005`, "list"))
  expect_named(ponderosa_terra_poly$`2005`, "ponderosapine")
  expect_s4_class(ponderosa_terra_poly$`2005`$ponderosapine, "SpatRaster")

  expect_true(inherits(ponderosa_manualyear, "list"))
  expect_named(ponderosa_manualyear, c("2000", "2005"))
  expect_true(inherits(ponderosa_manualyear$`2000`, "list"))
  expect_named(ponderosa_manualyear$`2000`, "ponderosapine")
  expect_s4_class(ponderosa_manualyear$`2000`$ponderosapine, "SpatRaster")
  expect_true(inherits(ponderosa_manualyear$`2005`, "list"))
  expect_named(ponderosa_manualyear$`2005`, "ponderosapine")
  expect_s4_class(ponderosa_manualyear$`2005`$ponderosapine, "SpatRaster")

  expect_true(terra::is.related(
    ponderosa_manualyear$`2000`$ponderosapine,
    terra::project(
      terra::vect(suppressMessages(data_fmt(
        bcch_restricted,
        coord_lon = "longitude",
        coord_lat = "latitude",
        crs = 4326
      ))),
      terra::crs(ponderosa_manualyear$`2000`$ponderosapine)
    ),
    "contains"
  ))

  expect_true(terra::is.related(
    ponderosa_manualyear$`2005`$ponderosapine,
    terra::project(
      terra::vect(suppressMessages(data_fmt(
        bcch_restricted,
        coord_lon = "longitude",
        coord_lat = "latitude",
        crs = 4326
      ))),
      terra::crs(ponderosa_manualyear$`2005`$ponderosapine)
    ),
    "contains"
  ))
})

test_that("scanfi_download() succeeds with alternate column names, either passed through attributes or specified explicitly.", {
  file.remove(
    "./testdir/scanfi/SCANFI_spsCC_ponderosaPine_2000_v2_20260119.tif",
    "./testdir/scanfi/SCANFI_spsCC_ponderosaPine_2005_v2_20260119.tif"
  )

  expect_silent(
    ponderosa_attr <- suppressMessages(scanfi_download(
      data_fmt(
        dplyr::rename(bcch_restricted, "yr" = "survey_year"),
        coord_lon = "longitude",
        coord_lat = "latitude",
        date_year = "yr",
        crs = 4326
      ),
      covariates = "scanfi_ponderosapine",
      dl_path = "./testdir",
      progress = FALSE
    ))
  )

  expect_true(all(file.exists(
    "./testdir/scanfi/SCANFI_spsCC_ponderosaPine_2000_v2_20260119.tif",
    "./testdir/scanfi/SCANFI_spsCC_ponderosaPine_2005_v2_20260119.tif"
  )))
  expect_true(all(
    file.size(
      "./testdir/scanfi/SCANFI_spsCC_ponderosaPine_2000_v2_20260119.tif",
      "./testdir/scanfi/SCANFI_spsCC_ponderosaPine_2005_v2_20260119.tif"
    ) >
      50000000
  ))

  expect_true(inherits(ponderosa_attr, "list"))
  expect_named(ponderosa_attr, c("2000", "2005"))
  expect_true(inherits(ponderosa_attr$`2000`, "list"))
  expect_named(ponderosa_attr$`2000`, "ponderosapine")
  expect_s4_class(ponderosa_attr$`2000`$ponderosapine, "SpatRaster")
  expect_true(inherits(ponderosa_attr$`2005`, "list"))
  expect_named(ponderosa_attr$`2005`, "ponderosapine")
  expect_s4_class(ponderosa_attr$`2005`$ponderosapine, "SpatRaster")

  file.remove(
    "./testdir/scanfi/SCANFI_spsCC_ponderosaPine_2000_v2_20260119.tif",
    "./testdir/scanfi/SCANFI_spsCC_ponderosaPine_2005_v2_20260119.tif"
  )

  expect_silent(
    ponderosa_attr <- suppressMessages(scanfi_download(
      dplyr::rename(
        data_fmt(
          bcch_restricted,
          coord_lon = "longitude",
          coord_lat = "latitude",
          crs = 4326
        ),
        "yr" = "survey_year"
      ),
      covariates = "scanfi_ponderosapine",
      date_year = "yr",
      dl_path = "./testdir",
      progress = FALSE
    ))
  )

  expect_true(all(file.exists(
    "./testdir/scanfi/SCANFI_spsCC_ponderosaPine_2000_v2_20260119.tif",
    "./testdir/scanfi/SCANFI_spsCC_ponderosaPine_2005_v2_20260119.tif"
  )))
  expect_true(all(
    file.size(
      "./testdir/scanfi/SCANFI_spsCC_ponderosaPine_2000_v2_20260119.tif",
      "./testdir/scanfi/SCANFI_spsCC_ponderosaPine_2005_v2_20260119.tif"
    ) >
      50000000
  ))

  expect_true(inherits(ponderosa_attr, "list"))
  expect_named(ponderosa_attr, c("2000", "2005"))
  expect_true(inherits(ponderosa_attr$`2000`, "list"))
  expect_named(ponderosa_attr$`2000`, "ponderosapine")
  expect_s4_class(ponderosa_attr$`2000`$ponderosapine, "SpatRaster")
  expect_true(inherits(ponderosa_attr$`2005`, "list"))
  expect_named(ponderosa_attr$`2005`, "ponderosapine")
  expect_s4_class(ponderosa_attr$`2005`$ponderosapine, "SpatRaster")
})

test_that("scanfi_download() returns appropriate warnings and errors for misspecifed arguments", {
  expect_error(
    suppressMessages(scanfi_download(
      dplyr::select(
        data_fmt(
          bcch_restricted,
          coord_lon = "longitude",
          coord_lat = "latitude",
          crs = 4326
        ),
        -"survey_year"
      ),
      covariates = "scanfi_ponderosapine",
      date_year = "yr",
      dl_path = "./testdir",
      progress = FALSE
    )),
    "\\[SCANFI Download\\] some specified columns missing from the data: yr. Use arguments to specify alternate column names if using data that diverges from naturecounts default column names."
  )

  expect_error(
    suppressMessages(scanfi_download(
      use_date = FALSE,
      snapshot_year = 2007,
      covariates = "scanfi_ponderosapine",
      dl_path = "./testdir",
      progress = FALSE
    )),
    "\\[SCANFI Download\\] Invalid snapshot year\\(s\\) provided to snapshot_year argument: 2007. Valid snapshot years are 1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020, 2025."
  )

  expect_warning(
    suppressMessages(scanfi_download(
      data = data_fmt(
        bcch_restricted,
        coord_lon = "longitude",
        coord_lat = "latitude",
        crs = 4326
      ),
      use_date = TRUE,
      snapshot_year = 2005,
      covariates = "scanfi_ponderosapine",
      dl_path = "./testdir",
      progress = FALSE
    )),
    "\\[SCANFI Download\\] Specific snapshot years requested but use_date set as TRUE, suggesting function should determine necessary snapshots to download from years in data argument. Overriding and proceeding to download snapshots requested in snapshot_year."
  )

  bcch_modified <- bcch_restricted
  bcch_modified$survey_year[1] <- 1975

  expect_warning(
    suppressMessages(scanfi_download(
      data = data_fmt(
        bcch_modified,
        coord_lon = "longitude",
        coord_lat = "latitude",
        crs = 4326
      ),
      use_date = TRUE,
      covariates = "scanfi_ponderosapine",
      dl_path = "./testdir",
      progress = FALSE
    )),
    "\\[SCANFI Download\\] Data contains years more than 5 years away from nearest SCANFI snapshot \\(1975\\). No value will be returned for observations in these years."
  )
})

test_that("scanfi_extract() throws appropriate error when inappropriate object provided to scanfi_data or argument is missing.", {
  expect_error(
    scanfi_extract(suppressWarnings(suppressMessages(data_fmt(bcch)))),
    "\\[SCANFI Extraction\\] no SCANFI rasters provided to extract from. Please provide a list containing one entry for every snapshot year, each containing one raster for each listed SCANFI covariate. Data can be downloaded using scanfi_download\\(\\)."
  )

  expect_error(
    scanfi_extract(
      suppressWarnings(suppressMessages(data_fmt(bcch))),
      scanfi_data = suppressWarnings(suppressMessages(data_fmt(bcch)))
    ),
    "\\[SCANFI Extraction\\] no SCANFI rasters provided to extract from. Please provide a list containing one entry for every snapshot year, each containing one raster for each listed SCANFI covariate. Data can be downloaded using scanfi_download\\(\\)."
  )
})

test_that("scanfi_extract() basic functionality with all expected data inputs.", {
  sf_pt <- suppressWarnings(suppressMessages(data_fmt(bcch_restricted)))
  sf_poly <- suppressWarnings(suppressMessages(data_buff(data_fmt(
    bcch_restricted
  ))))
  terra_pt <- terra::vect(sf_pt)
  terra_poly <- terra::vect(sf_poly)

  # Basic test of sf_pt input
  expect_silent(
    extracted <- suppressWarnings(suppressMessages(scanfi_extract(
      sf_pt,
      covariates = "scanfi_ponderosapine",
      scanfi_data = ponderosa_sf_pt
    )))
  )

  expect_s3_class(extracted, "sf")
  expect_equal(
    as.character(sf::st_geometry_type(extracted, by_geometry = FALSE)),
    "POINT"
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
      "geometry",
      "scanfi_ponderosapine"
    )
  )
  expect_true(inherits(extracted$scanfi_ponderosapine, "integer"))
  expect_equal(
    dplyr::select(extracted, -"scanfi_ponderosapine"),
    sf_pt,
    ignore_attr = TRUE
  ) # Ignores attributes to confirm that data has not been otherwise modified.
  expect_equal(format(sf::st_crs(extracted)), "Canada_Albers_Equal_Area_Conic")
  expect_true(all(is.na(extracted$scanfi_ponderosapine[
    !(extracted$survey_year %in% c(2000, 2005))
  ])))
  expect_true(all(
    !is.na(extracted$scanfi_ponderosapine[
      extracted$survey_year %in% c(2000, 2005)
    ])
  ))

  # Test interpolation
  expect_silent(
    extracted <- suppressWarnings(suppressMessages(scanfi_extract(
      sf_pt,
      covariates = "scanfi_ponderosapine",
      scanfi_data = ponderosa_sf_pt,
      interpolate = TRUE
    )))
  )

  expect_true(all(!is.na(extracted$scanfi_ponderosapine)))
  expect_true(
    unique(extracted$scanfi_ponderosapine[extracted$survey_year == 2002]) ==
      unique(extracted$scanfi_ponderosapine[extracted$survey_year == 2000])
  )

  # Basic test of sf_poly
  expect_silent(
    extracted <- suppressWarnings(suppressMessages(scanfi_extract(
      sf_poly,
      covariates = "scanfi_ponderosapine",
      scanfi_data = ponderosa_sf_poly
    )))
  )

  expect_s3_class(extracted, "sf")
  expect_equal(
    as.character(sf::st_geometry_type(extracted, by_geometry = FALSE)),
    "POLYGON"
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
      "geometry",
      "scanfi_ponderosapine"
    )
  )
  expect_true(inherits(extracted$scanfi_ponderosapine, "numeric"))
  expect_equal(
    dplyr::select(extracted, -"scanfi_ponderosapine"),
    sf_poly,
    ignore_attr = TRUE
  ) # Ignores attributes to confirm that data has not been otherwise modified.
  expect_equal(format(sf::st_crs(extracted)), "Canada_Albers_Equal_Area_Conic")
  expect_true(all(is.na(extracted$scanfi_ponderosapine[
    !(extracted$survey_year %in% c(2000, 2005))
  ])))
  expect_true(all(
    !is.na(extracted$scanfi_ponderosapine[
      extracted$survey_year %in% c(2000, 2005)
    ])
  ))

  # Test interpolation
  expect_silent(
    extracted <- suppressWarnings(suppressMessages(scanfi_extract(
      sf_poly,
      covariates = "scanfi_ponderosapine",
      scanfi_data = ponderosa_sf_poly,
      interpolate = TRUE
    )))
  )

  expect_true(all(!is.na(extracted$scanfi_ponderosapine)))
  expect_true(
    unique(extracted$scanfi_ponderosapine[extracted$survey_year == 2002]) ==
      unique(extracted$scanfi_ponderosapine[extracted$survey_year == 2000])
  )

  # Basic test of terra_pt
  expect_silent(
    extracted <- suppressWarnings(suppressMessages(scanfi_extract(
      terra_pt,
      covariates = "scanfi_ponderosapine",
      scanfi_data = ponderosa_terra_pt
    )))
  )

  expect_s3_class(extracted, "sf")
  expect_equal(
    as.character(sf::st_geometry_type(extracted, by_geometry = FALSE)),
    "POINT"
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
      "geometry",
      "scanfi_ponderosapine"
    )
  )
  expect_true(inherits(extracted$scanfi_ponderosapine, "integer"))
  expect_equal(
    dplyr::select(extracted, -"scanfi_ponderosapine"),
    sf_pt,
    ignore_attr = TRUE
  ) # Ignores attributes to confirm that data has not been otherwise modified.
  expect_equal(format(sf::st_crs(extracted)), "Canada_Albers_Equal_Area_Conic")
  expect_true(all(is.na(extracted$scanfi_ponderosapine[
    !(extracted$survey_year %in% c(2000, 2005))
  ])))
  expect_true(all(
    !is.na(extracted$scanfi_ponderosapine[
      extracted$survey_year %in% c(2000, 2005)
    ])
  ))

  # Test interpolation
  expect_silent(
    extracted <- suppressWarnings(suppressMessages(scanfi_extract(
      terra_pt,
      covariates = "scanfi_ponderosapine",
      scanfi_data = ponderosa_terra_pt,
      interpolate = TRUE
    )))
  )

  expect_true(all(!is.na(extracted$scanfi_ponderosapine)))
  expect_true(
    unique(extracted$scanfi_ponderosapine[extracted$survey_year == 2002]) ==
      unique(extracted$scanfi_ponderosapine[extracted$survey_year == 2000])
  )

  # Basic test of terra_poly
  expect_silent(
    extracted <- suppressWarnings(suppressMessages(scanfi_extract(
      terra_poly,
      covariates = "scanfi_ponderosapine",
      scanfi_data = ponderosa_terra_poly
    )))
  )

  expect_s3_class(extracted, "sf")
  expect_equal(
    as.character(sf::st_geometry_type(extracted, by_geometry = FALSE)),
    "POLYGON"
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
      "geometry",
      "scanfi_ponderosapine"
    )
  )
  expect_true(inherits(extracted$scanfi_ponderosapine, "numeric"))
  expect_equal(
    dplyr::select(extracted, -"scanfi_ponderosapine"),
    sf_poly,
    ignore_attr = TRUE
  ) # Ignores attributes to confirm that data has not been otherwise modified.
  expect_equal(format(sf::st_crs(extracted)), "Canada_Albers_Equal_Area_Conic")
  expect_true(all(is.na(extracted$scanfi_ponderosapine[
    !(extracted$survey_year %in% c(2000, 2005))
  ])))
  expect_true(all(
    !is.na(extracted$scanfi_ponderosapine[
      extracted$survey_year %in% c(2000, 2005)
    ])
  ))

  # Test interpolation
  expect_silent(
    extracted <- suppressWarnings(suppressMessages(scanfi_extract(
      terra_poly,
      covariates = "scanfi_ponderosapine",
      scanfi_data = ponderosa_terra_poly,
      interpolate = TRUE
    )))
  )

  expect_true(all(!is.na(extracted$scanfi_ponderosapine)))
  expect_true(
    unique(extracted$scanfi_ponderosapine[extracted$survey_year == 2002]) ==
      unique(extracted$scanfi_ponderosapine[extracted$survey_year == 2000])
  )
})


test_that("scanfi_extract() succeeds with alternate column names, either passed through attributes or specified explicitly.", {
  sf_pt <- suppressMessages(data_fmt(
    dplyr::rename(
      bcch_restricted,
      "sites" = "SurveyAreaIdentifier",
      "yr" = "survey_year"
    ),
    coord_lon = "longitude",
    coord_lat = "latitude",
    site_name = "sites",
    date_year = "yr",
    crs = 4326
  ))

  expect_warning(
    extracted <- suppressMessages(scanfi_extract(
      sf_pt,
      scanfi_data = ponderosa_sf_pt
    )),
    "\\[SCANFI Extraction\\] no covariates specified in the covariates argument. Proceeding to extract the covariates found in scanfi_data layers: scanfi_ponderosapine."
  ) # Bonus test of whether leaving covariates unspecified works

  expect_s3_class(extracted, "sf")
  expect_equal(
    as.character(sf::st_geometry_type(extracted, by_geometry = FALSE)),
    "POINT"
  )
  expect_named(
    extracted,
    c(
      "sites",
      "latitude",
      "longitude",
      "yr",
      "survey_month",
      "survey_day",
      "geometry",
      "scanfi_ponderosapine"
    )
  )
  expect_true(inherits(extracted$scanfi_ponderosapine, "integer"))
  expect_equal(
    dplyr::select(extracted, -"scanfi_ponderosapine"),
    sf_pt,
    ignore_attr = TRUE
  ) # Ignores attributes to confirm that data has not been otherwise modified.
  expect_equal(format(sf::st_crs(extracted)), "Canada_Albers_Equal_Area_Conic")
  expect_true(all(is.na(extracted$scanfi_ponderosapine[
    !(extracted$survey_year %in% c(2000, 2005))
  ])))
  expect_true(all(
    !is.na(extracted$scanfi_ponderosapine[
      extracted$survey_year %in% c(2000, 2005)
    ])
  ))

  sf_pt <- dplyr::rename(
    suppressMessages(data_fmt(
      bcch_restricted,
      coord_lon = "longitude",
      coord_lat = "latitude",
      crs = 4326
    )),
    "sites" = "SurveyAreaIdentifier",
    "yr" = "survey_year"
  )

  expect_silent(
    extracted <- suppressWarnings(suppressMessages(scanfi_extract(
      sf_pt,
      scanfi_data = ponderosa_sf_pt,
      site_name = "sites",
      date_year = "yr"
    )))
  )

  expect_s3_class(extracted, "sf")
  expect_equal(
    as.character(sf::st_geometry_type(extracted, by_geometry = FALSE)),
    "POINT"
  )
  expect_named(
    extracted,
    c(
      "sites",
      "latitude",
      "longitude",
      "yr",
      "survey_month",
      "survey_day",
      "geometry",
      "scanfi_ponderosapine"
    )
  )
  expect_true(inherits(extracted$scanfi_ponderosapine, "integer"))
  expect_equal(
    dplyr::select(extracted, -"scanfi_ponderosapine"),
    sf_pt,
    ignore_attr = TRUE
  ) # Ignores attributes to confirm that data has not been otherwise modified.
  expect_equal(format(sf::st_crs(extracted)), "Canada_Albers_Equal_Area_Conic")
  expect_true(all(is.na(extracted$scanfi_ponderosapine[
    !(extracted$survey_year %in% c(2000, 2005))
  ])))
  expect_true(all(
    !is.na(extracted$scanfi_ponderosapine[
      extracted$survey_year %in% c(2000, 2005)
    ])
  ))
})

test_that("scanfi_extract() returns appropriate warnings for out of coverage points and dates.", {
  bcch_modified <- bcch_restricted[
    bcch_restricted$survey_year %in% c(2000, 2005),
  ]
  bcch_modified$latitude[1] <- 35

  sf_pt <- suppressMessages(data_fmt(
    bcch_modified,
    coord_lon = "longitude",
    coord_lat = "latitude",
    crs = 4326
  ))

  expect_warning(
    extracted <- suppressMessages(scanfi_extract(
      sf_pt,
      covariates = "scanfi_ponderosapine",
      scanfi_data = ponderosa_sf_pt
    )),
    "\\[SCANFI \\(ponderosapine\\) Extraction\\] site FilledSurveyArea1 falls outside of the spatial extent of the SCANFI rasters provided. No value will be returned."
  )

  expect_true(is.na(extracted$scanfi_ponderosapine[1]))

  bcch_modified <- bcch_restricted[
    bcch_restricted$survey_year %in% c(2000, 2005),
  ]

  bcch_modified$latitude[1] <- 44.98126
  bcch_modified$longitude[1] <- -73.63115

  sf_poly <- suppressMessages(data_buff(
    data_fmt(
      bcch_modified,
      coord_lon = "longitude",
      coord_lat = "latitude",
      crs = 4326
    ),
    buffer_distance = 10,
    buffer_units = "km"
  ))

  expect_warning(
    extracted <- suppressMessages(scanfi_extract(
      sf_poly,
      covariates = "scanfi_ponderosapine",
      scanfi_data = ponderosa_sf_poly
    )),
    "\\[SCANFI \\(ponderosapine\\) Extraction\\] site FilledSurveyArea1's buffered area is only partially contained by the spatial extent of the SCANFI rasters provided. Returned ponderosapine value will be derived from the available values."
  )

  bcch_modified <- bcch_restricted[
    bcch_restricted$survey_year %in% c(2000, 2005),
  ]
  bcch_modified$survey_year <- 1990

  sf_pt <- suppressMessages(data_fmt(
    bcch_modified,
    coord_lon = "longitude",
    coord_lat = "latitude",
    crs = 4326
  ))

  expect_error(
    extracted <- suppressMessages(scanfi_extract(
      sf_pt,
      covariates = "scanfi_ponderosapine",
      scanfi_data = ponderosa_sf_pt
    )),
    "\\[SCANFI Extraction\\] Data does not contain observations within the SCANFI snapshot years \\(2000, 2005\\) in scanfi_data. If wanting to match interceding years to snapshots, use interpolate \\= TRUE."
  )

  bcch_modified <- bcch_restricted[
    bcch_restricted$survey_year %in% c(2000, 2005),
  ]
  bcch_modified$survey_year[1] <- 1990

  sf_pt <- suppressMessages(data_fmt(
    bcch_modified,
    coord_lon = "longitude",
    coord_lat = "latitude",
    crs = 4326
  ))

  expect_warning(
    extracted <- suppressMessages(scanfi_extract(
      sf_pt,
      covariates = "scanfi_ponderosapine",
      scanfi_data = ponderosa_sf_pt,
      interpolate = TRUE
    )),
    "\\[SCANFI Download\\] Data contains years more than 5 years away from nearest SCANFI snapshot \\(1990\\). No value will be returned for observations in these years. Nearby \\(< 5 years away\\) snapshots are available for some data years \\(1990\\), but were not provided via the scanfi_data argument. These can be downloaded with scanfi_download\\(\\)."
  )

  bcch_modified <- bcch_restricted[
    bcch_restricted$survey_year %in% c(2000, 2005),
  ]
  bcch_modified$survey_year[1] <- 1970

  sf_pt <- suppressMessages(data_fmt(
    bcch_modified,
    coord_lon = "longitude",
    coord_lat = "latitude",
    crs = 4326
  ))

  expect_warning(
    extracted <- suppressMessages(scanfi_extract(
      sf_pt,
      covariates = "scanfi_ponderosapine",
      scanfi_data = ponderosa_sf_pt,
      interpolate = TRUE
    )),
    "\\[SCANFI Download\\] Data contains years more than 5 years away from nearest SCANFI snapshot \\(1970\\). No value will be returned for observations in these years."
  )
})

# Tests for NFI Landcover - only run locally due to large filesize.

skip("local only")

test_that("scanfi_extract() functionality with NFI landcover data.", {
  scanfi_lc <- scanfi_download(
    bcch_restricted,
    covariates = "scanfi_nfilc",
    dl_path = "./testdir"
  )

  sf_pt <- suppressMessages(data_fmt(
    bcch_restricted,
    coord_lon = "longitude",
    coord_lat = "latitude",
    crs = 4326
  ))

  expect_silent(
    extracted <- suppressMessages(scanfi_extract(
      sf_pt,
      scanfi_data = scanfi_lc,
      covariates = "scanfi_nfilc",
      interpolate = FALSE
    ))
  )

  expect_s3_class(extracted, "sf")
  expect_equal(
    as.character(sf::st_geometry_type(extracted, by_geometry = FALSE)),
    "POINT"
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
      "geometry",
      "nfilc_class"
    )
  )
  expect_true(inherits(extracted$nfilc_class, "character"))
  expect_equal(
    dplyr::select(extracted, -"nfilc_class"),
    sf_pt,
    ignore_attr = TRUE
  ) # Ignores attributes to confirm that data has not been otherwise modified.
  expect_equal(format(sf::st_crs(extracted)), "Canada_Albers_Equal_Area_Conic")
  expect_true(all(is.na(extracted$nfilc_class[
    !(extracted$survey_year %in% c(2000, 2005))
  ])))
  expect_true(all(
    !is.na(extracted$nfilc_class[
      extracted$survey_year %in% c(2000, 2005)
    ])
  ))

  # Test interpolation
  expect_silent(
    extracted <- suppressWarnings(suppressMessages(scanfi_extract(
      sf_pt,
      covariates = "scanfi_nfilc",
      scanfi_data = scanfi_lc,
      interpolate = TRUE
    )))
  )

  expect_true(all(!is.na(extracted$nfilc_class)))
  expect_true(
    extracted$nfilc_class[
      extracted$survey_year == 2003 &
        extracted$SurveyAreaIdentifier == "FilledSurveyArea8"
    ] ==
      extracted$nfilc_class[
        extracted$survey_year == 2005 &
          extracted$SurveyAreaIdentifier == "FilledSurveyArea8"
      ]
  )

  sf_poly <- suppressMessages(data_buff(sf_pt))

  expect_silent(
    extracted <- suppressMessages(scanfi_extract(
      sf_poly,
      scanfi_data = scanfi_lc,
      covariates = "scanfi_nfilc",
      interpolate = FALSE
    ))
  )

  expect_s3_class(extracted, "sf")
  expect_equal(
    as.character(sf::st_geometry_type(extracted, by_geometry = FALSE)),
    "POLYGON"
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
      "geometry",
      "nfilc_bryoid",
      "nfilc_herbs",
      "nfilc_rock",
      "nfilc_shrub",
      "nfilc_treed_broadleaf",
      "nfilc_treed_conifer",
      "nfilc_treed_mixed",
      "nfilc_water"
    )
  )
  expect_true(inherits(extracted$nfilc_bryoid, "numeric"))
  expect_equal(
    dplyr::select(extracted, -tidyselect::starts_with("nfilc_")),
    sf_poly,
    ignore_attr = TRUE
  ) # Ignores attributes to confirm that data has not been otherwise modified.
  expect_equal(format(sf::st_crs(extracted)), "Canada_Albers_Equal_Area_Conic")
  expect_true(all(is.na(extracted$nfilc_bryoid[
    !(extracted$survey_year %in% c(2000, 2005))
  ])))
  expect_true(all(
    !is.na(extracted$nfilc_bryoid[
      extracted$survey_year %in% c(2000, 2005)
    ])
  ))

  # Test interpolation
  expect_silent(
    extracted <- suppressWarnings(suppressMessages(scanfi_extract(
      sf_poly,
      covariates = "scanfi_nfilc",
      scanfi_data = scanfi_lc,
      interpolate = TRUE
    )))
  )

  expect_true(all(!is.na(extracted$nfilc_bryoid)))
  expect_true(
    extracted$nfilc_bryoid[
      extracted$survey_year == 2003 &
        extracted$SurveyAreaIdentifier == "FilledSurveyArea8"
    ] ==
      extracted$nfilc_bryoid[
        extracted$survey_year == 2005 &
          extracted$SurveyAreaIdentifier == "FilledSurveyArea8"
      ]
  )
})

unlink("./testdir", recursive = TRUE)
