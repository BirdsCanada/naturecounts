if (!dir.exists("./testdir")) {
  dir.create("./testdir")
}

# Create bcch object that covers a reasonable number of edge cases.
bcch <- dplyr::filter(bcch, survey_year == 2010)

# Add US site.
bcch$latitude[1] <- 40.8

# Add Mexico site.
bcch$latitude[2] <- 26.26
bcch$longitude[2] <- -102.02

# Add Alaska site.
bcch$latitude[3] <- 64.85
bcch$longitude[3] <- -148.97

# Add site at US-Canada border.
bcch$latitude[4] <- 48.99
bcch$longitude[4] <- -108.63

# Add site at shoreline edge.
bcch$latitude[5] <- 56.74
bcch$longitude[5] <- -88.96

# Add site offshore.
bcch$latitude[6] <- 59.6
bcch$longitude[6] <- -86.74

# Add a duplicate of a site to test interpolation
bcch <- bcch[c(1:7, 7), ]

bcch <- sf::st_as_sf(bcch, coords = c("longitude", "latitude"), crs = 4326)

# Add years that are in the NALCMS snapshot years and outside of them.
bcch$survey_year <- c(2010, 2012, 2014, 2015, 2019, 2020, 2023, 2020)

skip("local only")

test_that("nalcms_download() downloads correct files with all expected inputs.", {
  expect_silent(
    nalcms_sf_pt <<- suppressMessages(nalcms_download(
      data_fmt(bcch),
      interpolate = TRUE,
      dl_path = "./testdir",
      progress = FALSE
    ))
  )

  expect_silent(
    nalcms_sf_poly <<- suppressMessages(nalcms_download(
      data_buff(
        data_fmt(bcch),
        buffer_distance = 5000
      ),
      interpolate = TRUE,
      dl_path = "./testdir",
      progress = FALSE
    ))
  )

  expect_silent(
    nalcms_terra_pt <<- suppressMessages(nalcms_download(
      data_fmt(terra::vect(bcch)),
      interpolate = TRUE,
      dl_path = "./testdir",
      progress = FALSE
    ))
  )

  expect_silent(
    nalcms_terra_poly <<- suppressMessages(nalcms_download(
      data_buff(
        data_fmt(terra::vect(bcch)),
        buffer_distance = 5000
      ),
      interpolate = TRUE,
      dl_path = "./testdir",
      progress = FALSE
    ))
  )

  expect_silent(
    nalcms_manualyear <<- suppressMessages(nalcms_download(
      use_date = FALSE,
      snapshot_year = c(2010, 2015, 2020),
      countries = c("USA", "CAN", "MEX"),
      dl_path = "./testdir",
      progress = FALSE
    ))
  )

  expect_silent(
    nalcms_non_interpolated <<- suppressMessages(nalcms_download(
      data_fmt(terra::vect(bcch)),
      interpolate = FALSE,
      dl_path = "./testdir",
      progress = FALSE
    ))
  )
})

skip("local only")

test_that("Results downloaded from nalcms_download() have expected features.", {
  expect_true(dir.exists("./testdir/nalcms")) # Bonus test of custom file path specification

  expect_true(all(file.exists(
    nalcms_manualyear
  )))

  expect_true(all(
    file.size(
      nalcms_manualyear
    ) >
      50000000
  )) # Check files contain a reasonable amount of data

  expect_true(identical(
    nalcms_sf_pt,
    c(
      "./testdir/nalcms/usa_land_cover_2010v3_30m_tif/USA_NALCMS_landcover_2010v3_30m/data/USA_NALCMS_landcover_2010v3_30m.tif",
      "./testdir/nalcms/usa_land_cover_2010v3_30m_tif/ASK_NALCMS_landcover_2010v3_30m/data/ASK_NALCMS_landcover_2010v3_30m.tif",
      "./testdir/nalcms/usa_land_cover_2015v4_30m_tif/USA_NALCMS_landcover_2015v4_30m/data/USA_NALCMS_landcover_2015v4_30m.tif",
      "./testdir/nalcms/usa_land_cover_2015v4_30m_tif/ASK_NALCMS_landcover_2015v4_30m/data/ASK_NALCMS_landcover_2015v4_30m.tif",
      "./testdir/nalcms/mex_land_cover_2010v3_30m_tif/MEX_NALCMS_landcover_2010v3_30m/data/MEX_NALCMS_landcover_2010v3_30m.tif",
      "./testdir/nalcms/can_land_cover_2020v2_30m_tif/CAN_NALCMS_landcover_2020v2_30m/data/CAN_NALCMS_landcover_2020v2_30m.tif"
    )
  ))

  expect_true(identical(
    nalcms_sf_poly,
    c(
      "./testdir/nalcms/usa_land_cover_2010v3_30m_tif/USA_NALCMS_landcover_2010v3_30m/data/USA_NALCMS_landcover_2010v3_30m.tif",
      "./testdir/nalcms/usa_land_cover_2010v3_30m_tif/ASK_NALCMS_landcover_2010v3_30m/data/ASK_NALCMS_landcover_2010v3_30m.tif",
      "./testdir/nalcms/usa_land_cover_2015v4_30m_tif/USA_NALCMS_landcover_2015v4_30m/data/USA_NALCMS_landcover_2015v4_30m.tif",
      "./testdir/nalcms/usa_land_cover_2015v4_30m_tif/ASK_NALCMS_landcover_2015v4_30m/data/ASK_NALCMS_landcover_2015v4_30m.tif",
      "./testdir/nalcms/mex_land_cover_2010v3_30m_tif/MEX_NALCMS_landcover_2010v3_30m/data/MEX_NALCMS_landcover_2010v3_30m.tif",
      "./testdir/nalcms/can_land_cover_2015v4_30m_tif/CAN_NALCMS_landcover_2015v4_30m/data/CAN_NALCMS_landcover_2015v4_30m.tif",
      "./testdir/nalcms/can_land_cover_2020v2_30m_tif/CAN_NALCMS_landcover_2020v2_30m/data/CAN_NALCMS_landcover_2020v2_30m.tif"
    )
  ))

  expect_true(identical(
    nalcms_terra_pt,
    c(
      "./testdir/nalcms/usa_land_cover_2010v3_30m_tif/USA_NALCMS_landcover_2010v3_30m/data/USA_NALCMS_landcover_2010v3_30m.tif",
      "./testdir/nalcms/usa_land_cover_2010v3_30m_tif/ASK_NALCMS_landcover_2010v3_30m/data/ASK_NALCMS_landcover_2010v3_30m.tif",
      "./testdir/nalcms/usa_land_cover_2015v4_30m_tif/USA_NALCMS_landcover_2015v4_30m/data/USA_NALCMS_landcover_2015v4_30m.tif",
      "./testdir/nalcms/usa_land_cover_2015v4_30m_tif/ASK_NALCMS_landcover_2015v4_30m/data/ASK_NALCMS_landcover_2015v4_30m.tif",
      "./testdir/nalcms/mex_land_cover_2010v3_30m_tif/MEX_NALCMS_landcover_2010v3_30m/data/MEX_NALCMS_landcover_2010v3_30m.tif",
      "./testdir/nalcms/can_land_cover_2020v2_30m_tif/CAN_NALCMS_landcover_2020v2_30m/data/CAN_NALCMS_landcover_2020v2_30m.tif"
    )
  ))

  expect_true(identical(
    nalcms_terra_poly,
    c(
      "./testdir/nalcms/usa_land_cover_2010v3_30m_tif/USA_NALCMS_landcover_2010v3_30m/data/USA_NALCMS_landcover_2010v3_30m.tif",
      "./testdir/nalcms/usa_land_cover_2010v3_30m_tif/ASK_NALCMS_landcover_2010v3_30m/data/ASK_NALCMS_landcover_2010v3_30m.tif",
      "./testdir/nalcms/usa_land_cover_2015v4_30m_tif/USA_NALCMS_landcover_2015v4_30m/data/USA_NALCMS_landcover_2015v4_30m.tif",
      "./testdir/nalcms/usa_land_cover_2015v4_30m_tif/ASK_NALCMS_landcover_2015v4_30m/data/ASK_NALCMS_landcover_2015v4_30m.tif",
      "./testdir/nalcms/mex_land_cover_2010v3_30m_tif/MEX_NALCMS_landcover_2010v3_30m/data/MEX_NALCMS_landcover_2010v3_30m.tif",
      "./testdir/nalcms/can_land_cover_2015v4_30m_tif/CAN_NALCMS_landcover_2015v4_30m/data/CAN_NALCMS_landcover_2015v4_30m.tif",
      "./testdir/nalcms/can_land_cover_2020v2_30m_tif/CAN_NALCMS_landcover_2020v2_30m/data/CAN_NALCMS_landcover_2020v2_30m.tif"
    )
  ))

  expect_true(identical(
    nalcms_manualyear,
    c(
      "./testdir/nalcms/usa_land_cover_2010v3_30m_tif/USA_NALCMS_landcover_2010v3_30m/data/USA_NALCMS_landcover_2010v3_30m.tif",
      "./testdir/nalcms/usa_land_cover_2010v3_30m_tif/ASK_NALCMS_landcover_2010v3_30m/data/ASK_NALCMS_landcover_2010v3_30m.tif",
      "./testdir/nalcms/usa_land_cover_2015v4_30m_tif/USA_NALCMS_landcover_2015v4_30m/data/USA_NALCMS_landcover_2015v4_30m.tif",
      "./testdir/nalcms/usa_land_cover_2015v4_30m_tif/ASK_NALCMS_landcover_2015v4_30m/data/ASK_NALCMS_landcover_2015v4_30m.tif",
      "./testdir/nalcms/usa_land_cover_2020v2_30m_tif/USA_NALCMS_landcover_2020v2_30m/data/USA_NALCMS_landcover_2020v2_30m.tif",
      "./testdir/nalcms/usa_land_cover_2020v2_30m_tif/ASK_NALCMS_landcover_2020v2_30m/data/ASK_NALCMS_landcover_2020v2_30m.tif",
      "./testdir/nalcms/can_land_cover_2010v3_30m_tif/CAN_NALCMS_landcover_2010v3_30m/data/CAN_NALCMS_landcover_2010v3_30m.tif",
      "./testdir/nalcms/can_land_cover_2015v4_30m_tif/CAN_NALCMS_landcover_2015v4_30m/data/CAN_NALCMS_landcover_2015v4_30m.tif",
      "./testdir/nalcms/can_land_cover_2020v2_30m_tif/CAN_NALCMS_landcover_2020v2_30m/data/CAN_NALCMS_landcover_2020v2_30m.tif",
      "./testdir/nalcms/mex_land_cover_2010v3_30m_tif/MEX_NALCMS_landcover_2010v3_30m/data/MEX_NALCMS_landcover_2010v3_30m.tif",
      "./testdir/nalcms/mex_land_cover_2015v4_30m_tif/MEX_NALCMS_landcover_2015v4_30m/data/MEX_NALCMS_landcover_2015v4_30m.tif",
      "./testdir/nalcms/mex_land_cover_2020v2_30m_tif/MEX_NALCMS_landcover_2020v2_30m/data/MEX_NALCMS_landcover_2020v2_30m.tif"
    )
  ))

  expect_true(identical(
    nalcms_non_interpolated,
    c(
      "./testdir/nalcms/usa_land_cover_2010v3_30m_tif/USA_NALCMS_landcover_2010v3_30m/data/USA_NALCMS_landcover_2010v3_30m.tif",
      "./testdir/nalcms/usa_land_cover_2010v3_30m_tif/ASK_NALCMS_landcover_2010v3_30m/data/ASK_NALCMS_landcover_2010v3_30m.tif",
      "./testdir/nalcms/usa_land_cover_2015v4_30m_tif/USA_NALCMS_landcover_2015v4_30m/data/USA_NALCMS_landcover_2015v4_30m.tif",
      "./testdir/nalcms/usa_land_cover_2015v4_30m_tif/ASK_NALCMS_landcover_2015v4_30m/data/ASK_NALCMS_landcover_2015v4_30m.tif",
      "./testdir/nalcms/can_land_cover_2020v2_30m_tif/CAN_NALCMS_landcover_2020v2_30m/data/CAN_NALCMS_landcover_2020v2_30m.tif"
    )
  ))
})

skip("local only")

test_that("nalcms_download() succeeds with alternate column names, either passed through attributes or specified explicitly.", {
  expect_silent(
    nalcms_attr <- suppressMessages(nalcms_download(
      data_fmt(
        dplyr::rename(
          bcch,
          "site" = "SurveyAreaIdentifier",
          "yr" = "survey_year"
        ),
        site_name = "site",
        date_year = "yr"
      ),
      interpolate = TRUE,
      dl_path = "./testdir",
      progress = FALSE
    ))
  )

  expect_true(identical(nalcms_sf_pt, nalcms_attr))

  expect_silent(
    nalcms_explicit <- suppressMessages(nalcms_download(
      dplyr::rename(
        data_fmt(bcch),
        "site" = "SurveyAreaIdentifier",
        "yr" = "survey_year"
      ),
      interpolate = TRUE,
      site_name = "site",
      date_year = "yr",
      dl_path = "./testdir",
      progress = FALSE
    ))
  )

  expect_true(identical(nalcms_sf_pt, nalcms_attr))
})

skip("local only")

test_that("nalcms_download() returns appropriate warnings and errors for misspecifed arguments", {
  expect_error(
    suppressMessages(nalcms_download(
      dplyr::select(
        data_fmt(
          bcch
        ),
        -"survey_year"
      ),
      date_year = "yr",
      dl_path = "./testdir",
      progress = FALSE
    )),
    "\\[NALCMS Landcover Download\\] some specified columns missing from the data: yr. Use arguments to specify alternate column names if using data that diverges from naturecounts default column names."
  )

  expect_error(
    suppressMessages(nalcms_download(
      use_date = FALSE,
      snapshot_year = 2007,
      dl_path = "./testdir",
      progress = FALSE
    )),
    "\\[NALCMS Landcover Download\\] Invalid snapshot year\\(s\\) provided to snapshot_year argument: 2007. Valid snapshot years are 2010, 2015, 2020."
  )

  expect_warning(
    suppressMessages(nalcms_download(
      data = data_fmt(bcch),
      use_date = TRUE,
      snapshot_year = 2015,
      dl_path = "./testdir",
      progress = FALSE
    )),
    "\\[NALCMS Download\\] Specific snapshot years requested but use_date set as TRUE, suggesting function should determine necessary snapshots to download from years in data argument. Overriding and proceeding to download snapshots requested in snapshot_year."
  )

  bcch_modified <- bcch
  bcch_modified$survey_year[1] <- 1975

  expect_warning(
    suppressMessages(nalcms_download(
      data = data_fmt(bcch_modified),
      use_date = TRUE,
      interpolate = TRUE,
      dl_path = "./testdir",
      progress = FALSE
    )),
    "\\[NALCMS Landcover Download\\] Data contains years more than 5 years away from nearest NALCMS snapshot \\(1975\\). No value will be returned for observations in these years."
  )
})

skip("local only")

test_that("nalcms_extract() throws appropriate error when inappropriate object provided to nalcms_files or argument is missing.", {
  expect_error(
    nalcms_extract(suppressWarnings(suppressMessages(data_fmt(bcch)))),
    "\\[NALCMS Extraction\\] no filepaths to NALCMS rasters provided to extract from. Please provide a vector of filepaths to NALCMS rasters. Data can be downloaded using nalcms_download\\(\\)."
  )

  expect_error(
    nalcms_extract(
      suppressWarnings(suppressMessages(data_fmt(bcch))),
      nalcms_file = suppressWarnings(suppressMessages(data_fmt(bcch)))
    ),
    "\\[NALCMS Extraction\\] no filepaths to NALCMS rasters provided to extract from. Please provide a vector of filepaths to NALCMS rasters. Data can be downloaded using nalcms_download\\(\\)."
  )
})

skip("local only")

test_that("nalcms_extract() basic functionality with all expected data inputs.", {
  sf_pt <- suppressMessages(data_fmt(bcch))
  sf_poly <- suppressMessages(data_buff(
    data_fmt(
      bcch
    ),
    buffer_distance = 5000
  ))
  terra_pt <- terra::vect(sf_pt)
  terra_poly <- terra::vect(sf_poly)

  # For now, drop sites expected to produce errors or warnings.
  sf_pt <- sf_pt[c(1:4, 7, 8), ]
  sf_poly <- sf_poly[c(1:4, 7, 8), ]
  terra_pt <- terra_pt[c(1:4, 7, 8), ]
  terra_poly <- terra_poly[c(1:4, 7, 8), ]

  # Basic test of sf_pt input
  expect_silent(
    extracted <- suppressMessages(nalcms_extract(
      sf_pt,
      nalcms_files = nalcms_sf_pt
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
      "nalcms_class",
      "geometry"
    )
  )
  expect_true(inherits(extracted$nalcms_class, "character"))
  expect_equal(
    dplyr::select(extracted, -"nalcms_class"),
    sf_pt,
    ignore_attr = TRUE
  ) # Ignores attributes to confirm that data has not been otherwise modified.
  expect_equal(format(sf::st_crs(extracted)), "Canada_Albers_Equal_Area_Conic")
  expect_true(all(is.na(extracted$nalcms_class[
    !(extracted$survey_year %in% c(2010, 2015, 2020))
  ])))
  expect_true(all(
    !is.na(extracted$nalcms_class[
      extracted$survey_year %in% c(2010, 2015, 2020)
    ])
  ))

  # Test interpolation
  expect_silent(
    extracted <- suppressWarnings(suppressMessages(nalcms_extract(
      sf_pt,
      nalcms_files = nalcms_sf_pt,
      interpolate = TRUE
    )))
  )

  expect_true(all(!is.na(extracted$nalcms_class)))
  expect_true(
    unique(extracted$nalcms_class[extracted$survey_year == 2023]) ==
      unique(extracted$nalcms_class[extracted$survey_year == 2020])
  )

  # Basic test of sf_poly
  expect_silent(
    extracted <- suppressMessages(nalcms_extract(
      sf_poly,
      nalcms_files = nalcms_sf_poly
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
      "nalcms_pland_temperate_subpolar_needleleaf_forest",
      "nalcms_pland_subpolar_taiga_needleleaf_forest",
      "nalcms_pland_tropical_subtropical_broadleaf_evergreen_forest",
      "nalcms_pland_tropical_subtropical_broadleaf_deciduous_forest",
      "nalcms_pland_temperate_subpolar_broadleaf_deciduous_forest",
      "nalcms_pland_mixed_forest",
      "nalcms_pland_tropical_subtropical_shrubland",
      "nalcms_pland_temperate_subpolar_shrubland",
      "nalcms_pland_tropical_subtropical_grassland",
      "nalcms_pland_temperate_subpolar_grassland",
      "nalcms_pland_subpolar_polar_shrubland_lichen_moss",
      "nalcms_pland_subpolar_polar_grassland_lichen_moss",
      "nalcms_pland_subpolar_polar_barren_lichen_moss",
      "nalcms_pland_wetland",
      "nalcms_pland_cropland",
      "nalcms_pland_barren_lands",
      "nalcms_pland_urban_built_up",
      "nalcms_pland_water",
      "nalcms_pland_snow_ice",
      "geometry"
    )
  )
  expect_true(inherits(extracted$nalcms_pland_cropland, "numeric"))
  expect_equal(
    dplyr::select(extracted, -dplyr::starts_with("nalcms")),
    sf_poly,
    ignore_attr = TRUE
  ) # Ignores attributes to confirm that data has not been otherwise modified.
  expect_equal(format(sf::st_crs(extracted)), "Canada_Albers_Equal_Area_Conic")
  expect_true(all(is.na(extracted$nalcms_pland_cropland[
    !(extracted$survey_year %in% c(2010, 2015, 2020))
  ])))
  expect_true(all(
    !is.na(extracted$nalcms_pland_cropland[
      extracted$survey_year %in% c(2010, 2015, 2020)
    ])
  ))

  # Test interpolation
  expect_silent(
    extracted <- suppressMessages(nalcms_extract(
      sf_poly,
      nalcms_files = nalcms_sf_poly,
      interpolate = TRUE
    ))
  )

  expect_true(all(!is.na(extracted$nalcms_pland_cropland)))
  expect_true(
    unique(extracted$nalcms_pland_cropland[extracted$survey_year == 2023]) ==
      unique(extracted$nalcms_pland_cropland[extracted$survey_year == 2020])
  )

  # Basic test of terra_pt
  expect_silent(
    extracted <- suppressMessages(nalcms_extract(
      terra_pt,
      nalcms_files = nalcms_terra_pt
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
      "nalcms_class",
      "geometry"
    )
  )
  expect_true(inherits(extracted$nalcms_class, "character"))
  expect_equal(
    dplyr::select(extracted, -"nalcms_class"),
    sf_pt,
    ignore_attr = TRUE
  ) # Ignores attributes to confirm that data has not been otherwise modified.
  expect_equal(format(sf::st_crs(extracted)), "Canada_Albers_Equal_Area_Conic")
  expect_true(all(is.na(extracted$nalcms_class[
    !(extracted$survey_year %in% c(2010, 2015, 2020))
  ])))
  expect_true(all(
    !is.na(extracted$nalcms_class[
      extracted$survey_year %in% c(2010, 2015, 2020)
    ])
  ))

  # Test interpolation
  expect_silent(
    extracted <- suppressMessages(nalcms_extract(
      terra_pt,
      nalcms_files = nalcms_terra_pt,
      interpolate = TRUE
    ))
  )

  expect_true(all(!is.na(extracted$nalcms_class)))
  expect_true(
    unique(extracted$nalcms_class[extracted$survey_year == 2023]) ==
      unique(extracted$nalcms_class[extracted$survey_year == 2020])
  )

  # Basic test of terra_poly
  expect_silent(
    extracted <- suppressMessages(nalcms_extract(
      terra_poly,
      nalcms_files = nalcms_terra_poly
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
      "nalcms_pland_temperate_subpolar_needleleaf_forest",
      "nalcms_pland_subpolar_taiga_needleleaf_forest",
      "nalcms_pland_tropical_subtropical_broadleaf_evergreen_forest",
      "nalcms_pland_tropical_subtropical_broadleaf_deciduous_forest",
      "nalcms_pland_temperate_subpolar_broadleaf_deciduous_forest",
      "nalcms_pland_mixed_forest",
      "nalcms_pland_tropical_subtropical_shrubland",
      "nalcms_pland_temperate_subpolar_shrubland",
      "nalcms_pland_tropical_subtropical_grassland",
      "nalcms_pland_temperate_subpolar_grassland",
      "nalcms_pland_subpolar_polar_shrubland_lichen_moss",
      "nalcms_pland_subpolar_polar_grassland_lichen_moss",
      "nalcms_pland_subpolar_polar_barren_lichen_moss",
      "nalcms_pland_wetland",
      "nalcms_pland_cropland",
      "nalcms_pland_barren_lands",
      "nalcms_pland_urban_built_up",
      "nalcms_pland_water",
      "nalcms_pland_snow_ice",
      "geometry"
    )
  )
  expect_true(inherits(extracted$nalcms_pland_cropland, "numeric"))
  expect_equal(
    dplyr::select(extracted, -dplyr::starts_with("nalcms")),
    sf_poly,
    ignore_attr = TRUE
  ) # Ignores attributes to confirm that data has not been otherwise modified.
  expect_equal(format(sf::st_crs(extracted)), "Canada_Albers_Equal_Area_Conic")
  expect_true(all(is.na(extracted$nalcms_pland_cropland[
    !(extracted$survey_year %in% c(2010, 2015, 2020))
  ])))
  expect_true(all(
    !is.na(extracted$nalcms_pland_cropland[
      extracted$survey_year %in% c(2010, 2015, 2020)
    ])
  ))

  # Test interpolation
  expect_silent(
    extracted <- suppressMessages(nalcms_extract(
      terra_poly,
      nalcms_files = nalcms_terra_poly,
      interpolate = TRUE
    ))
  )

  expect_true(all(!is.na(extracted$nalcms_pland_cropland)))
  expect_true(
    unique(extracted$nalcms_pland_cropland[extracted$survey_year == 2023]) ==
      unique(extracted$nalcms_pland_cropland[extracted$survey_year == 2020])
  )
})


skip("local only")

test_that("nalcms_extract() succeeds with alternate column names, either passed through attributes or specified explicitly.", {
  sf_pt <- suppressMessages(data_fmt(
    dplyr::rename(
      bcch,
      "sites" = "SurveyAreaIdentifier",
      "yr" = "survey_year"
    ),
    site_name = "sites",
    date_year = "yr"
  ))

  sf_pt <- sf_pt[c(1:4, 7, 8), ]

  expect_silent(
    extracted <- suppressMessages(nalcms_extract(
      sf_pt,
      nalcms_files = nalcms_sf_pt
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
      "sites",
      "latitude",
      "longitude",
      "yr",
      "survey_month",
      "survey_day",
      "nalcms_class",
      "geometry"
    )
  )
  expect_true(inherits(extracted$nalcms_class, "character"))
  expect_equal(
    dplyr::select(extracted, -"nalcms_class"),
    sf_pt,
    ignore_attr = TRUE
  ) # Ignores attributes to confirm that data has not been otherwise modified.
  expect_equal(format(sf::st_crs(extracted)), "Canada_Albers_Equal_Area_Conic")
  expect_true(all(is.na(extracted$nalcms_class[
    !(extracted$survey_year %in% c(2010, 2015, 2020))
  ])))
  expect_true(all(
    !is.na(extracted$nalcms_class[
      extracted$survey_year %in% c(2010, 2015, 2020)
    ])
  ))

  sf_pt <- dplyr::rename(
    suppressMessages(data_fmt(bcch)),
    "sites" = "SurveyAreaIdentifier",
    "yr" = "survey_year"
  )

  sf_pt <- sf_pt[c(1:4, 7, 8), ]

  expect_silent(
    extracted <- suppressMessages(nalcms_extract(
      sf_pt,
      nalcms_files = nalcms_sf_pt,
      site_name = "sites",
      date_year = "yr"
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
      "sites",
      "latitude",
      "longitude",
      "yr",
      "survey_month",
      "survey_day",
      "nalcms_class",
      "geometry"
    )
  )
  expect_true(inherits(extracted$nalcms_class, "character"))
  expect_equal(
    dplyr::select(extracted, -"nalcms_class"),
    sf_pt,
    ignore_attr = TRUE
  ) # Ignores attributes to confirm that data has not been otherwise modified.
  expect_equal(format(sf::st_crs(extracted)), "Canada_Albers_Equal_Area_Conic")
  expect_true(all(is.na(extracted$nalcms_class[
    !(extracted$survey_year %in% c(2010, 2015, 2020))
  ])))
  expect_true(all(
    !is.na(extracted$nalcms_class[
      extracted$survey_year %in% c(2010, 2015, 2020)
    ])
  ))
})

skip("local only")

test_that("nalcms_extract() returns appropriate warnings for out of coverage points and dates.", {
  sf_pt <- suppressMessages(data_fmt(bcch))

  expect_warning(
    extracted <- suppressMessages(nalcms_extract(
      sf_pt,
      nalcms_files = nalcms_sf_pt
    )),
    "\\[NALCMS Landcover Extraction\\] site\\(s\\) FilledSurveyArea6 fall outside of the spatial extent of the NALCMS rasters provided. No value will be returned."
  )

  expect_true(is.na(extracted$nalcms_class[6]))

  sf_poly <- suppressMessages(data_buff(
    data_fmt(bcch),
    buffer_distance = 20000
  ))

  # Remove out of coverage site
  sf_poly <- sf_poly[c(1:5, 7, 8), ]

  expect_warning(
    extracted <- suppressMessages(nalcms_extract(
      sf_poly,
      nalcms_files = nalcms_sf_poly,
      interpolate = TRUE
    )),
    "\\[NALCMS Landcover Extraction\\] site\\(s\\) FilledSurveyArea5 buffered area\\(s\\) are only partially contained by the spatial extent of the NALCMS rasters provided. Returned proportional coverage values will be derived from the available values."
  )

  bcch_modified <- bcch
  bcch_modified$survey_year <- 1990

  sf_pt <- suppressMessages(data_fmt(
    bcch_modified
  ))

  sf_pt <- sf_pt[c(1:5, 7, 8), ]

  expect_error(
    extracted <- suppressMessages(nalcms_extract(
      sf_pt,
      nalcms_files = nalcms_sf_pt
    )),
    "\\[NALCMS Landcover Extraction\\] Data provided to data argument does not contain observations within the NALCMS snapshot years \\(2010, 2015, 2020\\). If wanting to match interceding years to snapshots, use interpolate \\= TRUE."
  )

  expect_error(
    extracted <- suppressMessages(nalcms_extract(
      sf_pt,
      nalcms_files = nalcms_sf_pt,
      interpolate = TRUE
    )),
    "\\[NALCMS Landcover Extraction\\] Data provided to data argument does not contain observations within 5 years of the NALCMS snapshot years \\(2010, 2015, 2020\\)."
  )

  bcch_modified <- bcch
  bcch_modified$survey_year[1] <- 1990

  sf_pt <- suppressMessages(data_fmt(
    bcch_modified
  ))

  sf_pt <- sf_pt[c(1:5, 7, 8), ]

  expect_warning(
    extracted <- suppressMessages(nalcms_extract(
      sf_pt,
      nalcms_files = nalcms_sf_pt,
      interpolate = TRUE
    )),
    "\\[NALCMS Landcover Extraction\\] Data contains years more than 5 years away from nearest NALCMS snapshot \\(1990\\). No value will be returned for observations in these years."
  )
})

skip("local only")

test_that("nalcms_extract() functionality with alternative metrics.", {
  sf_poly <- suppressMessages(data_buff(
    data_fmt(
      bcch
    ),
    buffer_distance = 5000
  ))

  sf_poly <- sf_poly[c(1:4, 7, 8), ]

  # Test alternate metric
  expect_silent(
    extracted <- suppressMessages(nalcms_extract(
      sf_poly,
      nalcms_files = nalcms_sf_poly,
      metric = c("ed", "pland")
    ))
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
      "nalcms_ed_landscape",
      "nalcms_ed_temperate_subpolar_needleleaf_forest",
      "nalcms_ed_subpolar_taiga_needleleaf_forest",
      "nalcms_ed_tropical_subtropical_broadleaf_evergreen_forest",
      "nalcms_ed_tropical_subtropical_broadleaf_deciduous_forest",
      "nalcms_ed_temperate_subpolar_broadleaf_deciduous_forest",
      "nalcms_ed_mixed_forest",
      "nalcms_ed_tropical_subtropical_shrubland",
      "nalcms_ed_temperate_subpolar_shrubland",
      "nalcms_ed_tropical_subtropical_grassland",
      "nalcms_ed_temperate_subpolar_grassland",
      "nalcms_ed_subpolar_polar_shrubland_lichen_moss",
      "nalcms_ed_subpolar_polar_grassland_lichen_moss",
      "nalcms_ed_subpolar_polar_barren_lichen_moss",
      "nalcms_ed_wetland",
      "nalcms_ed_cropland",
      "nalcms_ed_barren_lands",
      "nalcms_ed_urban_built_up",
      "nalcms_ed_water",
      "nalcms_ed_snow_ice",
      "nalcms_pland_temperate_subpolar_needleleaf_forest",
      "nalcms_pland_subpolar_taiga_needleleaf_forest",
      "nalcms_pland_tropical_subtropical_broadleaf_evergreen_forest",
      "nalcms_pland_tropical_subtropical_broadleaf_deciduous_forest",
      "nalcms_pland_temperate_subpolar_broadleaf_deciduous_forest",
      "nalcms_pland_mixed_forest",
      "nalcms_pland_tropical_subtropical_shrubland",
      "nalcms_pland_temperate_subpolar_shrubland",
      "nalcms_pland_tropical_subtropical_grassland",
      "nalcms_pland_temperate_subpolar_grassland",
      "nalcms_pland_subpolar_polar_shrubland_lichen_moss",
      "nalcms_pland_subpolar_polar_grassland_lichen_moss",
      "nalcms_pland_subpolar_polar_barren_lichen_moss",
      "nalcms_pland_wetland",
      "nalcms_pland_cropland",
      "nalcms_pland_barren_lands",
      "nalcms_pland_urban_built_up",
      "nalcms_pland_water",
      "nalcms_pland_snow_ice",
      "geometry"
    )
  )

  expect_true(inherits(extracted$nalcms_ed_landscape, "numeric"))

  expect_error(
    extracted <- suppressMessages(nalcms_extract(
      sf_poly,
      nalcms_files = nalcms_sf_poly,
      level = "patch"
    )),
    "\\[NALCMS Landcover Extraction\\] landscape metrics requested at the patch scale, which is currently incompatible with nalcms_extract\\(\\). Consult landscapemetrics::list_lsm\\(level = 'patch'\\) to determine which metrics are patch scale."
  )
})

unlink("./testdir", recursive = TRUE)
