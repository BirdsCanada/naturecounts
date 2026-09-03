if (!dir.exists("./testdir")) {
  dir.create("./testdir")
}

test_that("elevation_download() hits API with all expected inputs.", {
  expect_silent(
    elev_sf_pt <<- suppressMessages(elevation_download(
      data_fmt(
        bcch,
        coord_lon = "longitude",
        coord_lat = "latitude",
        crs = 4326
      ),
      dl_path = "./testdir"
    ))
  )
  expect_silent(
    elev_sf_poly <<- suppressMessages(elevation_download(
      data_buff(data_fmt(
        bcch,
        coord_lon = "longitude",
        coord_lat = "latitude",
        crs = 4326
      )),
      dl_path = "./testdir"
    ))
  )
  expect_silent(
    elev_terra_pt <<- suppressMessages(elevation_download(
      data_fmt(terra::vect(
        bcch,
        crs = "epsg:4326",
        geom = c("longitude", "latitude")
      )),
      dl_path = "./testdir"
    ))
  )
  expect_silent(
    elev_terra_poly <<- suppressMessages(elevation_download(
      data_buff(data_fmt(terra::vect(
        bcch,
        crs = "epsg:4326",
        geom = c("longitude", "latitude")
      ))),
      dl_path = "./testdir"
    ))
  )
})

test_that("Results downloaded from elevation_download() have expected features.", {
  expect_true(dir.exists("./testdir/TerrainTiles")) # Bonus test of custom file path specification
  expect_true(all(
    file.size(list.files("./testdir/TerrainTiles", full.names = TRUE)) > 100000
  )) # Check files contain a reasonable amount of data

  expect_s4_class(elev_sf_pt, "SpatRaster")
  expect_s4_class(elev_sf_poly, "SpatRaster")
  expect_s4_class(elev_terra_pt, "SpatRaster")
  expect_s4_class(elev_terra_poly, "SpatRaster")

  expect_true(terra::is.related(
    elev_sf_pt,
    terra::vect(suppressMessages(data_fmt(
      bcch,
      coord_lon = "longitude",
      coord_lat = "latitude",
      crs = 4326
    ))),
    "contains"
  ))
  expect_true(terra::is.related(
    elev_sf_poly,
    terra::vect(suppressMessages(data_buff(data_fmt(
      bcch,
      coord_lon = "longitude",
      coord_lat = "latitude",
      crs = 4326
    )))),
    "contains"
  ))
  expect_true(terra::is.related(
    elev_terra_pt,
    suppressMessages(data_fmt(terra::vect(
      bcch,
      crs = "epsg:4326",
      geom = c("longitude", "latitude")
    ))),
    "contains"
  ))
  expect_true(terra::is.related(
    elev_terra_poly,
    suppressMessages(data_buff(data_fmt(terra::vect(
      bcch,
      crs = "epsg:4326",
      geom = c("longitude", "latitude")
    )))),
    "contains"
  ))
})

test_that("elevation_download() succeeds with alternate column names, either
          passed through attributes or specified explicitly.", {
  expect_silent(suppressWarnings(suppressMessages(elevation_download(data_fmt(
    dplyr::rename(bcch, "sites" = "SurveyAreaIdentifier", "yr" = "survey_year"),
    coord_lon = "longitude",
    coord_lat = "latitude",
    site_name = "sites",
    date_year = "yr",
    crs = 4326
  )))))
  expect_silent(suppressWarnings(suppressMessages(elevation_download(
    dplyr::rename(
      data_fmt(
        bcch,
        coord_lon = "longitude",
        coord_lat = "latitude",
        crs = 4326
      ),
      "sites" = "SurveyAreaIdentifier",
      "yr" = "survey_year"
    ),
    site_name = "sites"
  ))))
})

test_that("elevation_extract() throws appropriate error when inappropriate file provided to elevation_data or argument is missing.", {
  expect_error(
    elevation_extract(suppressWarnings(suppressMessages(data_fmt(bcch)))),
    "\\[Elevation Extraction\\] no elevation data provided to extract from. Please provide a terra SpatRaster containing the necessary elevation data. Elevation data can be downloaded using elevation_download\\(\\)."
  )

  expect_error(
    elevation_extract(
      suppressWarnings(suppressMessages(data_fmt(bcch))),
      elevation_data = suppressWarnings(suppressMessages(data_fmt(bcch)))
    ),
    "\\[Elevation Extraction\\] data provided to elevation_data argument is not a SpatRaster. Please provide a terra SpatRaster containing the necessary elevation data. Elevation data can be downloaded using elevation_download\\(\\)."
  )
})

test_that("elevation_extract() basic functionality with all expected data inputs.", {
  sf_pt <- suppressWarnings(suppressMessages(data_fmt(bcch)))
  sf_poly <- suppressWarnings(suppressMessages(data_buff(data_fmt(bcch))))
  terra_pt <- terra::vect(sf_pt)
  terra_poly <- terra::vect(sf_poly)

  expect_silent(
    extracted <- suppressWarnings(suppressMessages(elevation_extract(
      sf_pt,
      elevation_data = elev_sf_pt
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
      "elevation",
      "geometry"
    )
  )
  expect_true(inherits(extracted$elevation, "numeric"))
  expect_equal(
    dplyr::select(extracted, -"elevation"),
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
    rep(FALSE, times = 8)
  )

  expect_silent(
    extracted <- suppressWarnings(suppressMessages(elevation_extract(
      sf_poly,
      elevation_data = elev_sf_poly
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
      "elevation_mean",
      "geometry"
    )
  )
  expect_true(inherits(extracted$elevation, "numeric"))
  expect_equal(
    dplyr::select(extracted, -"elevation_mean"),
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
    rep(FALSE, times = 8)
  )

  expect_silent(
    extracted <- suppressWarnings(suppressMessages(elevation_extract(
      terra_pt,
      elevation_data = elev_terra_pt
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
      "elevation",
      "geometry"
    )
  )
  expect_true(inherits(extracted$elevation, "numeric"))
  expect_equal(
    dplyr::select(extracted, -"elevation"),
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
    rep(FALSE, times = 8)
  )

  expect_silent(
    extracted <- suppressWarnings(suppressMessages(elevation_extract(
      terra_poly,
      elevation_data = elev_terra_poly
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
      "elevation_mean",
      "geometry"
    )
  )
  expect_true(inherits(extracted$elevation, "numeric"))
  expect_equal(
    dplyr::select(extracted, -"elevation_mean"),
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
    rep(FALSE, times = 8)
  )
})

test_that("elevation_extract() succeeds with alternate column names, either passed through attributes or specified explicitly.", {
  sf_pt <- suppressMessages(data_fmt(
    dplyr::rename(bcch, "sites" = "SurveyAreaIdentifier", "yr" = "survey_year"),
    coord_lon = "longitude",
    coord_lat = "latitude",
    site_name = "sites",
    date_year = "yr",
    crs = 4326
  ))

  expect_silent(
    extracted <- suppressWarnings(suppressMessages(elevation_extract(
      sf_pt,
      elevation_data = elev_sf_pt
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
      "elevation",
      "geometry"
    )
  )
  expect_true(inherits(extracted$elevation, "numeric"))
  expect_equal(
    dplyr::select(extracted, -"elevation"),
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
    rep(FALSE, times = 8)
  )

  sf_pt <- dplyr::rename(
    suppressMessages(data_fmt(
      bcch,
      coord_lon = "longitude",
      coord_lat = "latitude",
      crs = 4326
    )),
    "sites" = "SurveyAreaIdentifier",
    "yr" = "survey_year"
  )

  expect_silent(
    extracted <- suppressWarnings(suppressMessages(elevation_extract(
      sf_pt,
      elevation_data = elev_sf_pt,
      site_name = "sites"
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
      "elevation",
      "geometry"
    )
  )
  expect_true(inherits(extracted$elevation, "numeric"))
  expect_equal(
    dplyr::select(extracted, -"elevation"),
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
    rep(FALSE, times = 8)
  )
})

test_that("elevation_extract() returns appropriate warnings for out of coverage points.", {
  bcch_mod <- bcch
  bcch_mod <- dplyr::filter(bcch_mod, .data$survey_year %in% 2005:2015)
  bcch_mod$latitude[1] <- 80

  sf_pt <- suppressWarnings(suppressMessages(data_fmt(bcch_mod)))
  sf_poly <- suppressWarnings(suppressMessages(data_buff(data_fmt(bcch_mod))))

  expect_warning(
    extracted <- suppressMessages(elevation_extract(
      sf_pt,
      elevation_data = elev_sf_pt
    )),
    "\\[Elevation Extraction\\] site FilledSurveyArea1 falls outside of the spatial extent of the elevation rasters provided. No value will be returned."
  )
  expect_true(is.na(extracted$elevation[1]))

  expect_warning(
    extracted <- suppressMessages(elevation_extract(
      sf_poly,
      elevation_data = elev_sf_poly
    )),
    "\\[Elevation Extraction\\] site FilledSurveyArea1 falls outside of the spatial extent of the elevation rasters provided. No value will be returned."
  )
  expect_true(is.na(extracted$elevation[1]))

  bcch_mod <- bcch
  bcch_mod <- dplyr::filter(bcch_mod, .data$survey_year %in% 2005:2015)
  bcch_mod$latitude[1] <- 47.25

  sf_pt <- suppressWarnings(suppressMessages(data_fmt(bcch_mod)))
  sf_poly <- suppressWarnings(suppressMessages(data_buff(data_fmt(bcch_mod))))

  expect_warning(
    extracted <- suppressMessages(elevation_extract(
      sf_pt,
      elevation_data = elev_sf_pt
    )),
    "\\[Elevation Extraction\\] site FilledSurveyArea1 falls outside of the spatial extent of the elevation rasters provided. No value will be returned."
  )
  expect_true(is.na(extracted$elevation[1]))

  expect_warning(
    extracted <- suppressMessages(elevation_extract(
      sf_poly,
      elevation_data = elev_sf_poly
    )),
    "\\[Elevation Extraction\\] site FilledSurveyArea1 falls outside of the spatial extent of the elevation rasters provided. No value will be returned."
  )
  expect_true(is.na(extracted$elevation[1]))

  bcch_mod <- bcch
  bcch_mod <- dplyr::filter(bcch_mod, .data$survey_year %in% 2005:2015)
  bcch_mod$latitude[1] <- 47.045

  sf_poly <- suppressWarnings(suppressMessages(data_buff(
    data_fmt(bcch_mod),
    buffer_distance = 5,
    buffer_units = "km"
  )))

  expect_warning(
    extracted <- suppressMessages(elevation_extract(
      sf_poly,
      elevation_data = elev_sf_poly
    )),
    "\\[Elevation Extraction\\] site FilledSurveyArea1\\'s buffered area is only partially contained by the spatial extent of the elevation rasters provided. Returned elevation value will be derived from the available values."
  )
  expect_true(inherits(extracted$elevation[1], "numeric"))
})


test_that("elevation_extract() succeeds with alternate summary statistics, and throws error when needed.", {
  sf_pt <- suppressWarnings(suppressMessages(data_fmt(
    bcch
  )))
  sf_poly <- suppressWarnings(suppressMessages(data_buff(data_fmt(
    bcch
  ))))

  expect_silent(
    extracted <- suppressMessages(elevation_extract(
      sf_pt,
      elevation_data = elev_sf_pt,
      method = "bilinear", # Add some arguments of terra::extract() to check
      # for errors.
      layer = 1
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
      "elevation",
      "geometry"
    )
  )
  expect_true(inherits(extracted$elevation, "numeric"))

  # Test a few standard functions.
  expect_silent(
    extracted <- suppressMessages(elevation_extract(
      sf_poly,
      elevation_data = elev_sf_poly,
      fun = c("median", "max", "stdev")
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
      "elevation_median",
      "elevation_max",
      "elevation_stdev",
      "geometry"
    )
  )
  expect_true(inherits(extracted$elevation_median, "numeric"))
  expect_true(inherits(extracted$elevation_max, "numeric"))
  expect_true(inherits(extracted$elevation_stdev, "numeric"))

  # Test functions with specific requirements

  # Check that quantile requires quantiles argument.
  expect_error(
    extracted <- suppressMessages(elevation_extract(
      sf_poly,
      elevation_data = elev_sf_poly,
      fun = "quantile"
    )),
    "\\[Elevation Extraction\\] quantile summary requested but no quantiles supplied to the 'quantiles' argument. Please supply numeric value\\(s\\) of desired quantiles."
  )

  # Check that one or more quantile joins correctly.
  expect_silent(
    extracted <- suppressMessages(elevation_extract(
      sf_poly,
      elevation_data = elev_sf_poly,
      fun = "quantile",
      quantiles = 0.25
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
      "elevation_quantile",
      "geometry"
    )
  )
  expect_true(inherits(extracted$elevation_quantile, "numeric"))

  expect_silent(
    extracted <- suppressMessages(elevation_extract(
      sf_poly,
      elevation_data = elev_sf_poly,
      fun = "quantile",
      quantiles = c(0.25, 0.75)
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
      "elevation_quantile_25",
      "elevation_quantile_75",
      "geometry"
    )
  )
  expect_true(inherits(extracted$elevation_quantile_25, "numeric"))
  expect_true(inherits(extracted$elevation_quantile_75, "numeric"))

  # Check that weighted functions require weights argument.
  expect_error(
    extracted <- suppressMessages(elevation_extract(
      sf_poly,
      elevation_data = elev_sf_poly,
      fun = "weighted_mean"
    )),
    "\\[Elevation Extraction\\] weighted summary requested but no weights supplied via the 'weights' argument. Please supply either a weighting raster or 'area' to use the cell areas of the elevation raster as weights."
  )

  # Check that fractions join correctly.
  expect_silent(
    extracted <- suppressMessages(elevation_extract(
      sf_poly[1, ],
      elevation_data = elev_sf_poly,
      fun = "frac"
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
      "elevation_frac_320",
      "elevation_frac_338",
      "elevation_frac_339",
      "elevation_frac_341",
      "elevation_frac_344",
      "elevation_frac_345",
      "elevation_frac_347",
      "elevation_frac_351",
      "elevation_frac_358",
      "elevation_frac_361",
      "geometry"
    )
  )
  expect_true(inherits(extracted$elevation_frac_320, "numeric"))

  # Test that user specified functions work.
  my_function <- function(value, cov_frac) {
    mean(value * cov_frac)
  }

  expect_silent(
    extracted <- suppressMessages(elevation_extract(
      sf_poly,
      elevation_data = elev_sf_poly,
      fun = my_function
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
      "elevation_user_defined_function",
      "geometry"
    )
  )

  # Test that functions that return more than one value throw error.
  my_function <- function(value, cov_frac) {
    value * cov_frac
  }

  expect_error(
    extracted <- suppressMessages(elevation_extract(
      sf_poly,
      elevation_data = elev_sf_poly,
      fun = my_function
    )),
    "\\[Elevation Extraction\\] support for custom summary functions is currently limited to functions returning a single value \\(not stored in a data.frame\\) to allow accurate joining to input data."
  )
})

unlink("./testdir", recursive = TRUE)
