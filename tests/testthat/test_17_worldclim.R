if (!dir.exists("./testdir")) {
  dir.create("./testdir")
}

tryCatch(
  suppressMessages(worldclim_download(suppressWarnings(
    data_fmt(bcch),
    progress = FALSE
  ))),
  error = function(e) {
    if (stringr::str_detect(conditionMessage(e), "temporarily down")) {
      serverdown <<- TRUE
    }
  }
)

if (!exists("serverdown")) {
  serverdown <- FALSE
}

if (serverdown) {
  skip("Geodata server down.")
}

if (!serverdown) {
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

    expect_true(inherits(tavg_sf_pt, "SpatRaster"))
    expect_true(inherits(tavg_sf_poly, "SpatRaster"))
    expect_true(inherits(tavg_terra_pt, "SpatRaster"))
    expect_true(inherits(tavg_terra_poly, "SpatRaster"))
    expect_true(inherits(tavg_countryname, "SpatRaster"))
    expect_true(inherits(tavg_countrycode, "SpatRaster"))
    expect_true(inherits(tavg_countries, "SpatRaster"))
    expect_true(inherits(other_vars, "list"))
    expect_true(inherits(other_vars$tmin, "SpatRaster"))
    expect_true(inherits(other_vars$tmax, "SpatRaster"))
    expect_true(inherits(other_vars$prec, "SpatRaster"))
    expect_true(inherits(other_vars$srad, "SpatRaster"))
    expect_true(inherits(other_vars$wind, "SpatRaster"))

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
        "tavg",
        "geometry"
      )
    )
    expect_true(inherits(extracted$tavg, "numeric"))
    expect_equal(dplyr::select(extracted, -"tavg"), sf_pt, ignore_attr = TRUE) # Ignores attributes to confirm that data has not been otherwise modified.
    expect_equal(
      format(sf::st_crs(extracted)),
      "Canada_Albers_Equal_Area_Conic"
    )
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
        "tavg_mean",
        "geometry"
      )
    )
    expect_true(inherits(extracted$tavg, "numeric"))
    expect_equal(
      dplyr::select(extracted, -"tavg_mean"),
      sf_poly,
      ignore_attr = TRUE
    ) # Ignores attributes to confirm that data has not been otherwise modified.
    expect_equal(
      format(sf::st_crs(extracted)),
      "Canada_Albers_Equal_Area_Conic"
    )
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
        "tavg",
        "geometry"
      )
    )
    expect_true(inherits(extracted$tavg, "numeric"))
    expect_equal(dplyr::select(extracted, -"tavg"), sf_pt, ignore_attr = TRUE) # Ignores attributes to confirm that data has not been otherwise modified.
    expect_equal(
      format(sf::st_crs(extracted)),
      "Canada_Albers_Equal_Area_Conic"
    )
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
        "tavg_mean",
        "geometry"
      )
    )
    expect_true(inherits(extracted$tavg, "numeric"))
    expect_equal(
      dplyr::select(extracted, -"tavg_mean"),
      sf_poly,
      ignore_attr = TRUE
    ) # Ignores attributes to confirm that data has not been otherwise modified.
    expect_equal(
      format(sf::st_crs(extracted)),
      "Canada_Albers_Equal_Area_Conic"
    )
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
        "tavg",
        "geometry"
      )
    )
    expect_true(inherits(extracted$tavg, "numeric"))
    expect_equal(dplyr::select(extracted, -"tavg"), sf_pt, ignore_attr = TRUE) # Ignores attributes to confirm that data has not been otherwise modified.
    expect_equal(
      format(sf::st_crs(extracted)),
      "Canada_Albers_Equal_Area_Conic"
    )
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
        "tavg",
        "geometry"
      )
    )
    expect_true(inherits(extracted$tavg, "numeric"))
    expect_equal(dplyr::select(extracted, -"tavg"), sf_pt, ignore_attr = TRUE) # Ignores attributes to confirm that data has not been otherwise modified.
    expect_equal(
      format(sf::st_crs(extracted)),
      "Canada_Albers_Equal_Area_Conic"
    )
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

  test_that("worldclim_extract() succeeds with alternate summary statistics, and throws error when needed.", {
    sf_pt <- suppressWarnings(suppressMessages(data_fmt(bcch)))
    sf_poly <- suppressWarnings(suppressMessages(data_buff(data_fmt(bcch))))

    expect_silent(
      extracted <- suppressMessages(worldclim_extract(
        sf_pt,
        worldclim_data = tavg_sf_poly,
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
        "tavg",
        "geometry"
      )
    )
    expect_true(inherits(extracted$tavg, "numeric"))

    # Test a few standard functions.
    expect_silent(
      extracted <- suppressMessages(worldclim_extract(
        sf_poly,
        worldclim_data = tavg_sf_poly,
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
        "tavg_median",
        "tavg_max",
        "tavg_stdev",
        "geometry"
      )
    )
    expect_true(inherits(extracted$tavg_median, "numeric"))
    expect_true(inherits(extracted$tavg_max, "numeric"))
    expect_true(inherits(extracted$tavg_stdev, "numeric"))

    # Test functions with specific requirements

    # Check that quantile requires quantiles argument.
    expect_error(
      extracted <- suppressMessages(worldclim_extract(
        sf_poly,
        worldclim_data = tavg_sf_poly,
        fun = "quantile"
      )),
      "\\[WorldClim Extraction\\] quantile summary requested but no quantiles supplied to the 'quantiles' argument. Please supply numeric value\\(s\\) of desired quantiles."
    )

    # Check that one or more quantile joins correctly.
    expect_silent(
      extracted <- suppressMessages(worldclim_extract(
        sf_poly,
        worldclim_data = tavg_sf_poly,
        fun = "quantile",
        quantiles = c(0.25)
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
        "tavg_quantile",
        "geometry"
      )
    )
    expect_true(inherits(extracted$tavg_quantile, "numeric"))

    expect_silent(
      extracted <- suppressMessages(worldclim_extract(
        sf_poly,
        worldclim_data = tavg_sf_poly,
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
        "tavg_quantile_25",
        "tavg_quantile_75",
        "geometry"
      )
    )
    expect_true(inherits(extracted$tavg_quantile_25, "numeric"))
    expect_true(inherits(extracted$tavg_quantile_75, "numeric"))

    # Check that weighted functions require weights argument.
    expect_error(
      extracted <- suppressMessages(worldclim_extract(
        sf_poly,
        worldclim_data = tavg_sf_poly,
        fun = "weighted_mean"
      )),
      "\\[WorldClim Extraction\\] weighted summary requested but no weights supplied via the 'weights' argument. Please supply either a weighting raster or 'area' to use the cell areas of the WorldClim raster as weights."
    )

    # Check that fractions join correctly.
    expect_silent(
      extracted <- suppressMessages(worldclim_extract(
        sf_poly[1, ],
        worldclim_data = tavg_sf_poly,
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
        "tavg_frac_-11.1000003814697",
        "tavg_frac_-11",
        "geometry"
      )
    )
    expect_true(inherits(
      sf::st_drop_geometry(extracted)[, "tavg_frac_-11.1000003814697"],
      "numeric"
    ))

    # Test that user specified functions work.
    my_function <- function(value, cov_frac) {
      mean(value * cov_frac)
    }

    expect_silent(
      extracted <- suppressMessages(worldclim_extract(
        sf_poly,
        worldclim_data = tavg_sf_poly,
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
        "tavg_user_defined_function",
        "geometry"
      )
    )

    # Test that functions that return more than one value throw error.
    my_function <- function(value, cov_frac) {
      value * cov_frac
    }

    expect_error(
      extracted <- suppressMessages(worldclim_extract(
        sf_poly,
        worldclim_data = tavg_sf_poly,
        fun = my_function
      )),
      "\\[WorldClim \\(tavg\\) Extraction\\] support for custom summary functions is currently limited to functions returning a single value \\(not stored in a data.frame\\) to allow accurate joining to input data."
    )
  })
}

unlink("./testdir", recursive = TRUE)
