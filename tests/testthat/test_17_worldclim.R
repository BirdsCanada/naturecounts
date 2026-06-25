if (!dir.exists("./testdir")) {
  dir.create("./testdir")
}

test_that("worldclim_download() hits API with all expected inputs.", {
  expect_silent(
    tavg_sf_pt <<- suppressMessages(worldclim_download(
      data_fmt(
        bcch,
        coord_lon = "longitude",
        coord_lat = "latitude",
        crs = 4326
      ),
      covariates = "worldclim_tavg",
      dl_path = "./testdir",
      progress = FALSE
    ))
  )
  expect_silent(
    tavg_sf_poly <<- suppressMessages(worldclim_download(
      data_buff(data_fmt(
        bcch,
        coord_lon = "longitude",
        coord_lat = "latitude",
        crs = 4326
      )),
      covariates = "worldclim_tavg",
      dl_path = "./testdir",
      progress = FALSE
    ))
  )
  expect_silent(
    tavg_terra_pt <<- suppressMessages(worldclim_download(
      data_fmt(terra::vect(
        bcch,
        crs = "epsg:4326",
        geom = c("longitude", "latitude")
      )),
      covariates = "worldclim_tavg",
      dl_path = "./testdir",
      progress = FALSE
    ))
  )
  expect_silent(
    tavg_terra_poly <<- suppressMessages(worldclim_download(
      data_buff(data_fmt(terra::vect(
        bcch,
        crs = "epsg:4326",
        geom = c("longitude", "latitude")
      ))),
      covariates = "worldclim_tavg",
      dl_path = "./testdir",
      progress = FALSE
    ))
  )
  expect_silent(
    tavg_countryname <<- suppressMessages(worldclim_download(
      countries = "Canada",
      covariates = "worldclim_tavg",
      dl_path = "./testdir",
      progress = FALSE
    ))
  )
  expect_silent(
    tavg_countrycode <<- suppressMessages(worldclim_download(
      countries = "CAN",
      covariates = "worldclim_tavg",
      dl_path = "./testdir",
      progress = FALSE
    ))
  )
  expect_silent(
    tavg_countries <<- suppressMessages(worldclim_download(
      countries = c("Canada", "MDG"),
      covariates = "worldclim_tavg",
      dl_path = "./testdir",
      progress = FALSE
    ))
  )
  expect_silent(
    other_vars <<- suppressMessages(worldclim_download(
      countries = "MDG",
      covariates = c(
        "worldclim_tmin",
        "worldclim_tmax",
        "worldclim_prec",
        "worldclim_srad",
        "worldclim_wind"
      ),
      dl_path = "./testdir",
      progress = FALSE
    ))
  )
})

test_that("Results downloaded from scanfi_download() have expected features.", {
  expect_true(dir.exists("./testdir/worldclim")) # Bonus test of custom file path specification
  expect_true(all(
    file.size(list.files(
      "./testdir/TerrainTiles/climate/wc2.1_country",
      full.names = TRUE
    )) >
      100000
  )) # Check files contain a reasonable amount of data

  expect_s4_class(tavg_sf_pt, "SpatRaster")
  expect_s4_class(tavg_sf_poly, "SpatRaster")
  expect_s4_class(tavg_terra_pt, "SpatRaster")
  expect_s4_class(tavg_terra_poly, "SpatRaster")
  expect_s4_class(tavg_countryname, "SpatRaster")
  expect_s4_class(tavg_countrycode, "SpatRaster")
  expect_s4_class(tavg_countries, "SpatRaster")
  expect_true(inherits(other_vars, "list"))
  expect_s4_class(other_vars$tmin, "SpatRaster")
  expect_s4_class(other_vars$tmax, "SpatRaster")
  expect_s4_class(other_vars$prec, "SpatRaster")
  expect_s4_class(other_vars$srad, "SpatRaster")
  expect_s4_class(other_vars$wind, "SpatRaster")

  expect_true(terra::is.related(
    tavg_sf_pt,
    terra::project(
      terra::vect(suppressMessages(data_fmt(
        bcch,
        coord_lon = "longitude",
        coord_lat = "latitude",
        crs = 4326
      ))),
      terra::crs(tavg_sf_pt)
    ),
    "contains"
  ))
  expect_true(terra::is.related(
    tavg_sf_poly,
    terra::project(
      terra::vect(suppressMessages(data_buff(data_fmt(
        bcch,
        coord_lon = "longitude",
        coord_lat = "latitude",
        crs = 4326
      )))),
      terra::crs(tavg_sf_poly)
    ),
    "contains"
  ))
  expect_true(terra::is.related(
    tavg_terra_pt,
    terra::project(
      suppressMessages(data_fmt(terra::vect(
        bcch,
        crs = "epsg:4326",
        geom = c("longitude", "latitude")
      ))),
      terra::crs(tavg_terra_pt)
    ),
    "contains"
  ))
  expect_true(terra::is.related(
    tavg_terra_poly,
    terra::project(
      suppressMessages(data_buff(data_fmt(terra::vect(
        bcch,
        crs = "epsg:4326",
        geom = c("longitude", "latitude")
      )))),
      terra::crs(tavg_terra_poly)
    ),
    "contains"
  ))
})

test_that("worldclim_extract() throws appropriate error when inappropriate file provided to worldclim_data or argument is missing.", {
  expect_error(
    worldclim_extract(suppressWarnings(suppressMessages(data_fmt(bcch)))),
    "\\[WorldClim Extraction\\] no WorldClim rasters provided to extract from. Please provide a list of the necessary rasters. Data can be downloaded using worldclim_download\\(\\)."
  )

  expect_error(
    worldclim_extract(
      suppressWarnings(suppressMessages(data_fmt(bcch))),
      worldclim_data = suppressWarnings(suppressMessages(data_fmt(bcch)))
    ),
    "\\[WorldClim Extraction\\] no WorldClim rasters provided to extract from. Please provide a list of the necessary rasters. Data can be downloaded using worldclim_download\\(\\)."
  )
})

test_that("worldclim_extract() basic functionality with all expected data inputs.", {
  sf_pt <- suppressWarnings(suppressMessages(data_fmt(bcch)))
  sf_poly <- suppressWarnings(suppressMessages(data_buff(data_fmt(bcch))))
  terra_pt <- terra::vect(sf_pt)
  terra_poly <- terra::vect(sf_poly)

  expect_silent(
    extracted <- suppressWarnings(suppressMessages(worldclim_extract(
      sf_pt,
      worldclim_data = tavg_sf_pt
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
      "tavg"
    )
  )
  expect_true(inherits(extracted$tavg, "numeric"))
  expect_equal(dplyr::select(extracted, -"tavg"), sf_pt, ignore_attr = TRUE) # Ignores attributes to confirm that data has not been otherwise modified.
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
    extracted <- suppressWarnings(suppressMessages(worldclim_extract(
      sf_poly,
      worldclim_data = tavg_sf_poly
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
      "tavg"
    )
  )
  expect_true(inherits(extracted$tavg, "numeric"))
  expect_equal(dplyr::select(extracted, -"tavg"), sf_poly, ignore_attr = TRUE) # Ignores attributes to confirm that data has not been otherwise modified.
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
    extracted <- suppressWarnings(suppressMessages(worldclim_extract(
      terra_pt,
      worldclim_data = tavg_terra_pt
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
      "tavg"
    )
  )
  expect_true(inherits(extracted$tavg, "numeric"))
  expect_equal(dplyr::select(extracted, -"tavg"), sf_pt, ignore_attr = TRUE) # Ignores attributes to confirm that data has not been otherwise modified.
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
    extracted <- suppressWarnings(suppressMessages(worldclim_extract(
      terra_poly,
      worldclim_data = tavg_terra_poly
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
      "tavg"
    )
  )
  expect_true(inherits(extracted$tavg, "numeric"))
  expect_equal(dplyr::select(extracted, -"tavg"), sf_poly, ignore_attr = TRUE) # Ignores attributes to confirm that data has not been otherwise modified.
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

test_that("worldclim_extract() succeeds with alternate column names, either passed through attributes or specified explicitly.", {
  sf_pt <- suppressMessages(data_fmt(
    dplyr::rename(
      bcch,
      "sites" = "SurveyAreaIdentifier",
      "mth" = "survey_month"
    ),
    coord_lon = "longitude",
    coord_lat = "latitude",
    site_name = "sites",
    date_month = "mth",
    crs = 4326
  ))

  expect_silent(
    extracted <- suppressWarnings(suppressMessages(worldclim_extract(
      sf_pt,
      worldclim_data = tavg_sf_pt
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
      "survey_year",
      "mth",
      "survey_day",
      "geometry",
      "tavg"
    )
  )
  expect_true(inherits(extracted$tavg, "numeric"))
  expect_equal(dplyr::select(extracted, -"tavg"), sf_pt, ignore_attr = TRUE) # Ignores attributes to confirm that data has not been otherwise modified.
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
    "mth" = "survey_month"
  )

  expect_silent(
    extracted <- suppressWarnings(suppressMessages(worldclim_extract(
      sf_pt,
      worldclim_data = tavg_sf_pt,
      site_name = "sites",
      date_month = "mth"
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
      "survey_year",
      "mth",
      "survey_day",
      "geometry",
      "tavg"
    )
  )
  expect_true(inherits(extracted$tavg, "numeric"))
  expect_equal(dplyr::select(extracted, -"tavg"), sf_pt, ignore_attr = TRUE) # Ignores attributes to confirm that data has not been otherwise modified.
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

test_that("worldclim_extract() returns appropriate warnings for out of coverage points.", {
  bcch_mod <- bcch
  bcch_mod <- dplyr::filter(bcch_mod, .data$survey_year %in% 2005:2015)
  bcch_mod$latitude[1] <- 80
  bcch_mod$longitude[1] <- -127

  sf_pt <- suppressWarnings(suppressMessages(data_fmt(bcch_mod)))
  sf_poly <- suppressWarnings(suppressMessages(data_buff(data_fmt(bcch_mod))))

  expect_warning(
    extracted <- suppressMessages(worldclim_extract(
      sf_pt,
      worldclim_data = tavg_sf_pt
    )),
    "\\[WorldClim \\(tavg\\) Extraction\\] site FilledSurveyArea1 falls outside of the spatial extent of the WorldClim rasters provided. No value will be returned."
  )
  expect_true(is.na(extracted$tavg[1]))

  expect_warning(
    extracted <- suppressMessages(worldclim_extract(
      sf_poly,
      worldclim_data = tavg_sf_poly
    )),
    "\\[WorldClim \\(tavg\\) Extraction\\] site FilledSurveyArea1 falls outside of the spatial extent of the WorldClim rasters provided. No value will be returned."
  )
  expect_true(is.na(extracted$tavg[1]))

  bcch_mod <- bcch[1, ]
  bcch_mod$latitude[1] <- -19.04
  bcch_mod$longitude[1] <- 44.24

  sf_poly <- suppressWarnings(suppressMessages(data_buff(
    data_fmt(bcch_mod),
    buffer_distance = 5,
    buffer_units = "km"
  )))

  expect_warning(
    extracted <- suppressMessages(worldclim_extract(
      sf_poly,
      covariates = "worldclim_tmin",
      worldclim_data = other_vars[["tmin"]]
    )),
    "\\[WorldClim \\(tmin\\) Extraction\\] site FilledSurveyArea1\\'s buffered area is only partially contained by the spatial extent of the WorldClim rasters provided. Returned mean tmin value will be derived from the available values."
  )
  expect_true(inherits(extracted$tmin[1], "numeric"))
})

unlink("./testdir", recursive = TRUE)
