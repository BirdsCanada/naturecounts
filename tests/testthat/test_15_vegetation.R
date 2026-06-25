if (!dir.exists("./testdir")) {
  dir.create("./testdir")
}

test_that("vegetation_download() hits API with all expected inputs. May fail if 
          filename structure is changed server-side.", {
  expected_files <- c(
    "MOD13A1.A2007337.h12v04.061.2021081224633.hdf",
    "MOD13A1.A2007049.h12v04.061.2021055160007.hdf",
    "MOD13A1.A2007097.h12v04.061.2021060063721.hdf",
    "MOD13A1.A2007241.h12v04.061.2021073192515.hdf"
  )

  expect_equal(
    suppressMessages(vegetation_download(
      data_fmt(
        bcch[bcch$survey_year == 2007, ],
        coord_lon = "longitude",
        coord_lat = "latitude",
        crs = 4326
      ),
      ed_transfer = FALSE
    )),
    expected_files
  )
  expect_equal(
    suppressMessages(vegetation_download(
      data_buff(data_fmt(
        bcch[bcch$survey_year == 2007, ],
        coord_lon = "longitude",
        coord_lat = "latitude",
        crs = 4326
      )),
      ed_transfer = FALSE
    )),
    expected_files
  )
  expect_equal(
    suppressMessages(vegetation_download(
      data_fmt(terra::vect(
        bcch[bcch$survey_year == 2007, ],
        crs = "epsg:4326",
        geom = c("longitude", "latitude")
      )),
      ed_transfer = FALSE
    )),
    expected_files
  )
  expect_equal(
    suppressMessages(vegetation_download(
      data_buff(data_fmt(terra::vect(
        bcch[bcch$survey_year == 2007, ],
        crs = "epsg:4326",
        geom = c("longitude", "latitude")
      ))),
      ed_transfer = FALSE
    )),
    expected_files
  )
})


test_that("vegetation_download() successfully downloads requested files with a test user,
          and downloaded files contain all data years and cover all data areas.", {
  expected_files <- c(
    "./testdir/modis/MOD13A1/MOD13A1.A2006337.h12v04.061.2020278214814.hdf",
    "./testdir/modis/MOD13A1/MOD13A1.A2007337.h12v04.061.2021081224633.hdf",
    "./testdir/modis/MOD13A1/MOD13A1.A2007049.h12v04.061.2021055160007.hdf",
    "./testdir/modis/MOD13A1/MOD13A1.A2007097.h12v04.061.2021060063721.hdf",
    "./testdir/modis/MOD13A1/MOD13A1.A2007241.h12v04.061.2021073192515.hdf"
  )

  expect_equal(
    suppressMessages(vegetation_download(
      data_fmt(
        bcch[bcch$survey_year %in% c(2006:2007), ],
        coord_lon = "longitude",
        coord_lat = "latitude",
        crs = 4326
      ),
      ed_email = "rmacklin@birdscanada.org",
      ed_transfer = TRUE,
      dl_path = "./testdir",
      progress = FALSE
    )),
    expected_files
  )
  expect_true(dir.exists("./testdir/modis/MOD13A1")) # Bonus test of custom file path specification
  expect_true(all(
    list.files("./testdir/modis/MOD13A1", full.names = TRUE) %in% expected_files
  ))
  files_years <- luna::modisDate(list.files("./testdir/modis/MOD13A1"))
  expect_true(all(
    bcch$survey_year[bcch$survey_year %in% c(2006:2007)] %in% files_years$year
  ))
  files_extent <- luna::modisExtent(list.files("./testdir/modis/MOD13A1"))
  bcch_spatial <- bcch %>%
    dplyr::filter(survey_year %in% c(2006:2007)) %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    sf::st_transform(terra::crs(terra::rast(list.files(
      "./testdir/modis/MOD13A1",
      full.names = TRUE
    )[1]))) %>%
    sf::st_coordinates()
  expect_true(all(bcch_spatial[, "X"] <= unique(files_extent[, "xmax"])))
  expect_true(all(bcch_spatial[, "X"] >= unique(files_extent[, "xmin"])))
  expect_true(all(bcch_spatial[, "Y"] <= unique(files_extent[, "ymax"])))
  expect_true(all(bcch_spatial[, "Y"] >= unique(files_extent[, "ymin"])))
})

test_that("vegetation_download() succeeds with alternate column names, either
          passed through attributes or specified explicitly.", {
  expected_files <- c(
    "MOD13A1.A2007337.h12v04.061.2021081224633.hdf",
    "MOD13A1.A2007049.h12v04.061.2021055160007.hdf",
    "MOD13A1.A2007097.h12v04.061.2021060063721.hdf",
    "MOD13A1.A2007241.h12v04.061.2021073192515.hdf"
  )

  expect_equal(
    suppressMessages(vegetation_download(
      data_fmt(
        dplyr::rename(
          bcch[bcch$survey_year == 2007, ],
          "sites" = "SurveyAreaIdentifier",
          "yr" = "survey_year"
        ),
        coord_lon = "longitude",
        coord_lat = "latitude",
        site_name = "sites",
        date_year = "yr",
        crs = 4326
      ),
      ed_transfer = FALSE
    )),
    expected_files
  )
  expect_equal(
    suppressMessages(vegetation_download(
      dplyr::rename(
        data_fmt(
          bcch[bcch$survey_year == 2007, ],
          coord_lon = "longitude",
          coord_lat = "latitude",
          crs = 4326
        ),
        "sites" = "SurveyAreaIdentifier",
        "yr" = "survey_year"
      ),
      site_name = "sites",
      date_year = "yr",
      ed_transfer = FALSE
    )),
    expected_files
  )
})

test_that("vegetation_download() returns correct warning with out of coverage dates.", {
  expect_warning(
    suppressMessages(vegetation_download(
      data_fmt(
        bcch[bcch$survey_year == 1998, ],
        coord_lon = "longitude",
        coord_lat = "latitude",
        crs = 4326
      ),
      ed_transfer = FALSE
    )),
    "Observation on date\\(s\\) 1998-12-19 could not be matched to a MODIS vegetation data file. Are they outside of the temporal coverage of the data \\(i.e., before 2000 or in the current year\\)\\?"
  )
})

test_that("vegetation_extract() throws appropriate error when empty vector provided to vegetation_files.", {
  expect_error(
    vegetation_extract(
      suppressWarnings(suppressMessages(data_fmt(bcch))),
      vegetation_files = c()
    ),
    "\\[MODIS NDVI\\/EVI Extraction\\] no vegetation files provided to extract from. Please provide a vector containing filepaths of all necessary MODIS files for your data. Data can be downloaded using vegetation_download\\(\\)."
  )
})

test_that("vegetation_extract() basic functionality with all expected data inputs.", {
  sf_pt <- suppressWarnings(suppressMessages(data_fmt(bcch[
    bcch$survey_year == 2007,
  ])))
  sf_poly <- suppressWarnings(suppressMessages(data_buff(data_fmt(bcch[
    bcch$survey_year == 2007,
  ]))))
  terra_pt <- terra::vect(sf_pt)
  terra_poly <- terra::vect(sf_poly)

  expect_silent(
    extracted <- suppressMessages(vegetation_extract(
      sf_pt,
      vegetation_files = list.files(
        "./testdir/modis/MOD13A1",
        full.names = TRUE
      ),
      covariates = c("modis_ndvi", "modis_evi")
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
      "ndvi",
      "evi",
      "geometry"
    )
  )
  expect_equal(
    dplyr::select(extracted, -"ndvi", -"evi"),
    sf_pt,
    ignore_attr = TRUE
  ) # Ignores attributes to confirm that data has not been otherwise modified.
  expect_equal(format(sf::st_crs(extracted)), "Canada_Albers_Equal_Area_Conic")
  expect_equal(
    unname(apply(
      X = apply(FUN = is.na, X = extracted, MARGIN = 1),
      FUN = unique,
      MARGIN = 1
    )),
    rep(FALSE, times = 9)
  )
  expect_true(all(extracted$ndvi < 1))
  expect_true(all(extracted$ndvi > -1))
  expect_true(all(extracted$evi < 1))
  expect_true(all(extracted$evi > -1))

  expect_silent(
    extracted <- suppressMessages(vegetation_extract(
      sf_poly,
      vegetation_files = list.files(
        "./testdir/modis/MOD13A1",
        full.names = TRUE
      ),
      covariates = c("modis_ndvi", "modis_evi")
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
      "ndvi",
      "evi",
      "geometry"
    )
  )
  expect_equal(
    dplyr::select(extracted, -"ndvi", -"evi"),
    sf_poly,
    ignore_attr = TRUE
  ) # Ignores attributes to confirm that data has not been otherwise modified.
  expect_equal(format(sf::st_crs(extracted)), "Canada_Albers_Equal_Area_Conic")
  expect_equal(
    unname(apply(
      X = apply(FUN = is.na, X = extracted, MARGIN = 1),
      FUN = unique,
      MARGIN = 1
    )),
    rep(FALSE, times = 9)
  )
  expect_true(all(extracted$ndvi < 1))
  expect_true(all(extracted$ndvi > -1))
  expect_true(all(extracted$evi < 1))
  expect_true(all(extracted$evi > -1))

  expect_silent(
    extracted <- suppressMessages(vegetation_extract(
      terra_pt,
      vegetation_files = list.files(
        "./testdir/modis/MOD13A1",
        full.names = TRUE
      ),
      covariates = c("modis_ndvi", "modis_evi")
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
      "ndvi",
      "evi",
      "geometry"
    )
  )
  expect_equal(
    dplyr::select(extracted, -"ndvi", -"evi"),
    sf_pt,
    ignore_attr = TRUE
  ) # Ignores attributes to confirm that data has not been otherwise modified.
  expect_equal(format(sf::st_crs(extracted)), "Canada_Albers_Equal_Area_Conic")
  expect_equal(
    unname(apply(
      X = apply(FUN = is.na, X = extracted, MARGIN = 1),
      FUN = unique,
      MARGIN = 1
    )),
    rep(FALSE, times = 9)
  )
  expect_true(all(extracted$ndvi < 1))
  expect_true(all(extracted$ndvi > -1))
  expect_true(all(extracted$evi < 1))
  expect_true(all(extracted$evi > -1))

  expect_silent(
    extracted <- suppressMessages(vegetation_extract(
      terra_poly,
      vegetation_files = list.files(
        "./testdir/modis/MOD13A1",
        full.names = TRUE
      ),
      covariates = c("modis_ndvi", "modis_evi")
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
      "ndvi",
      "evi",
      "geometry"
    )
  )
  expect_equal(
    dplyr::select(extracted, -"ndvi", -"evi"),
    sf_poly,
    ignore_attr = TRUE
  ) # Ignores attributes to confirm that data has not been otherwise modified.
  expect_equal(format(sf::st_crs(extracted)), "Canada_Albers_Equal_Area_Conic")
  expect_equal(
    unname(apply(
      X = apply(FUN = is.na, X = extracted, MARGIN = 1),
      FUN = unique,
      MARGIN = 1
    )),
    rep(FALSE, times = 9)
  )
  expect_true(all(extracted$ndvi < 1))
  expect_true(all(extracted$ndvi > -1))
  expect_true(all(extracted$evi < 1))
  expect_true(all(extracted$evi > -1))
})

test_that("vegetation_extract() successfully returns reliability information.", {
  sf_pt <- suppressWarnings(suppressMessages(data_fmt(bcch[
    bcch$survey_year == 2007,
  ])))
  sf_poly <- suppressWarnings(suppressMessages(data_buff(data_fmt(bcch[
    bcch$survey_year == 2007,
  ]))))

  expect_silent(
    extracted <- suppressMessages(vegetation_extract(
      sf_pt,
      vegetation_files = list.files(
        "./testdir/modis/MOD13A1",
        full.names = TRUE
      ),
      reliability = TRUE
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
      "ndvi",
      "vegetation_reliability",
      "geometry"
    )
  )
  expect_equal(
    dplyr::select(extracted, -"ndvi", -"vegetation_reliability"),
    sf_pt,
    ignore_attr = TRUE
  ) # Ignores attributes to confirm that data has not been otherwise modified.
  expect_equal(format(sf::st_crs(extracted)), "Canada_Albers_Equal_Area_Conic")
  expect_equal(
    unname(apply(
      X = apply(FUN = is.na, X = extracted, MARGIN = 1),
      FUN = unique,
      MARGIN = 1
    )),
    rep(FALSE, times = 9)
  )
  expect_true(all(
    extracted$vegetation_reliability %in%
      c("Fill/No Data", "Good Data", "Marginal Data", "Snow/Ice", "Cloudy")
  ))

  expect_silent(
    extracted <- suppressMessages(vegetation_extract(
      sf_poly,
      vegetation_files = list.files(
        "./testdir/modis/MOD13A1",
        full.names = TRUE
      ),
      reliability = TRUE
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
      "ndvi",
      "vegetation_reliability",
      "geometry"
    )
  )
  expect_equal(
    dplyr::select(extracted, -"ndvi", -"vegetation_reliability"),
    sf_poly,
    ignore_attr = TRUE
  ) # Ignores attributes to confirm that data has not been otherwise modified.
  expect_equal(format(sf::st_crs(extracted)), "Canada_Albers_Equal_Area_Conic")
  expect_equal(
    unname(apply(
      X = apply(FUN = is.na, X = extracted, MARGIN = 1),
      FUN = unique,
      MARGIN = 1
    )),
    rep(FALSE, times = 9)
  )
  expect_equal(
    stringr::str_flatten_comma(extracted$vegetation_reliability),
    "Snow/Ice (100%), Snow/Ice (66.67%), Cloudy (33.33%), Good Data (100%), Good Data (100%)"
  )
})

test_that("vegetation_extract() returns appropriate warnings for out of coverage points and dates.", {
  bcch_mod <- bcch
  bcch_mod <- dplyr::filter(bcch_mod, .data$survey_year == 2007)
  bcch_mod$latitude[1] <- 80

  sf_pt <- suppressWarnings(suppressMessages(data_fmt(bcch_mod)))
  sf_poly <- suppressWarnings(suppressMessages(data_buff(data_fmt(bcch_mod))))

  expect_warning(
    extracted <- suppressMessages(vegetation_extract(
      sf_pt,
      vegetation_files = list.files(
        "./testdir/modis/MOD13A1",
        full.names = TRUE
      )
    )),
    "\\[MODIS NDVI\\/EVI Extraction\\] site FilledSurveyArea1 falls outside of the spatial extent of the files provided. No value will be returned."
  )
  expect_true(is.na(extracted$ndvi[1]))

  expect_warning(
    extracted <- suppressMessages(vegetation_extract(
      sf_poly,
      vegetation_files = list.files(
        "./testdir/modis/MOD13A1",
        full.names = TRUE
      ),
      reliability = TRUE
    )), # bonus test of whether reliability is returned as NA.
    "\\[MODIS NDVI\\/EVI Extraction\\] site FilledSurveyArea1 falls outside of the spatial extent of the files provided. No value will be returned."
  )
  expect_true(is.na(extracted$ndvi[1]))
  expect_true(is.na(extracted$vegetation_reliability[1]))

  bcch_mod <- bcch
  bcch_mod <- dplyr::filter(bcch_mod, .data$survey_year == 2007)
  bcch_mod$survey_year[1] <- 1999

  sf_pt <- suppressWarnings(suppressMessages(data_fmt(bcch_mod)))
  sf_poly <- suppressWarnings(suppressMessages(data_buff(data_fmt(bcch_mod))))

  expect_warning(
    extracted <- suppressMessages(vegetation_extract(
      sf_pt,
      vegetation_files = list.files(
        "./testdir/modis/MOD13A1",
        full.names = TRUE
      )
    )),
    "\\[MODIS NDVI/EVI Extraction\\] observations from year 1999 fall outside of the temporal extent of the files provided. Is it in a year where data is unavailable from this dataset\\? No value will be returned."
  )
  expect_true(is.na(extracted$ndvi[1]))

  expect_warning(
    extracted <- suppressMessages(vegetation_extract(
      sf_poly,
      vegetation_files = list.files(
        "./testdir/modis/MOD13A1",
        full.names = TRUE
      ),
      reliability = TRUE
    )), # bonus test of whether reliability is returned as NA.
    "\\[MODIS NDVI/EVI Extraction\\] observations from year 1999 fall outside of the temporal extent of the files provided. Is it in a year where data is unavailable from this dataset\\? No value will be returned."
  )
  expect_true(is.na(extracted$ndvi[1]))
  expect_true(is.na(extracted$vegetation_reliability[1]))

  bcch_mod <- bcch
  bcch_mod <- dplyr::filter(bcch_mod, .data$survey_year == 2007)
  bcch_mod$survey_month[1] <- 1
  bcch_mod$survey_day[1] <- 10

  sf_pt <- suppressWarnings(suppressMessages(data_fmt(bcch_mod)))
  sf_poly <- suppressWarnings(suppressMessages(data_buff(data_fmt(bcch_mod))))

  expect_warning(
    extracted <- suppressMessages(vegetation_extract(
      sf_pt,
      vegetation_files = list.files(
        "./testdir/modis/MOD13A1",
        full.names = TRUE
      )
    )),
    "\\[MODIS NDVI/EVI Extraction\\] observations on 2007-01-10 fall outside of the temporal extent of the files provided. You have provided data for this year but not this 16-day window. No value will be returned."
  )
  expect_true(is.na(extracted$ndvi[1]))

  expect_warning(
    extracted <- suppressMessages(vegetation_extract(
      sf_poly,
      vegetation_files = list.files(
        "./testdir/modis/MOD13A1",
        full.names = TRUE
      ),
      reliability = TRUE
    )), # bonus test of whether reliability is returned as NA.
    "\\[MODIS NDVI/EVI Extraction\\] observations on 2007-01-10 fall outside of the temporal extent of the files provided. You have provided data for this year but not this 16-day window. No value will be returned."
  )
  expect_true(is.na(extracted$ndvi[1]))
  expect_true(is.na(extracted$vegetation_reliability[1]))
})


test_that("vegetation_extract() succeeds with alternate column names, either passed through attributes or specified explicitly.", {
  sf_pt <- suppressMessages(data_fmt(
    dplyr::rename(
      bcch[bcch$survey_year == 2007, ],
      "sites" = "SurveyAreaIdentifier",
      "yr" = "survey_year"
    ),
    coord_lon = "longitude",
    coord_lat = "latitude",
    site_name = "sites",
    date_year = "yr",
    crs = 4326
  ))

  expect_silent(
    extracted <- suppressMessages(vegetation_extract(
      sf_pt,
      vegetation_files = list.files(
        "./testdir/modis/MOD13A1",
        full.names = TRUE
      )
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
      "ndvi",
      "geometry"
    )
  )
  expect_equal(dplyr::select(extracted, -"ndvi"), sf_pt, ignore_attr = TRUE) # Ignores attributes to confirm that data has not been otherwise modified.
  expect_equal(format(sf::st_crs(extracted)), "Canada_Albers_Equal_Area_Conic")
  expect_equal(
    unname(apply(
      X = apply(FUN = is.na, X = extracted, MARGIN = 1),
      FUN = unique,
      MARGIN = 1
    )),
    rep(FALSE, times = 8)
  )

  sf_pt <- dplyr::rename(
    suppressMessages(data_fmt(
      bcch[bcch$survey_year == 2007, ],
      coord_lon = "longitude",
      coord_lat = "latitude",
      crs = 4326
    )),
    "sites" = "SurveyAreaIdentifier",
    "yr" = "survey_year"
  )

  expect_silent(
    extracted <- suppressMessages(vegetation_extract(
      sf_pt,
      vegetation_files = list.files(
        "./testdir/modis/MOD13A1",
        full.names = TRUE
      ),
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
      "ndvi",
      "geometry"
    )
  )
  expect_equal(dplyr::select(extracted, -"ndvi"), sf_pt, ignore_attr = TRUE) # Ignores attributes to confirm that data has not been otherwise modified.
  expect_equal(format(sf::st_crs(extracted)), "Canada_Albers_Equal_Area_Conic")
  expect_equal(
    unname(apply(
      X = apply(FUN = is.na, X = extracted, MARGIN = 1),
      FUN = unique,
      MARGIN = 1
    )),
    rep(FALSE, times = 8)
  )
})

unlink("./testdir", recursive = TRUE)
