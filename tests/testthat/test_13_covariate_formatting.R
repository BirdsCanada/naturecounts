test_that("data_fmt() basic functionality with complete BMDE data.frame", {
  expect_warning(
    f <- suppressMessages(data_fmt(bcch)),
    "\\[Data Formatting\\] as the 'crs' argument is not specified, data CRS is assumed to be EPSG:4326."
  )
  expect_s3_class(f, "sf")
  expect_equal(
    as.character(sf::st_geometry_type(f, by_geometry = FALSE)),
    "POINT"
  )
  expect_named(
    f,
    c(
      "SurveyAreaIdentifier",
      "latitude",
      "longitude",
      "survey_year",
      "survey_month",
      "survey_day",
      "geometry"
    )
  )
  expect_equal(
    nrow(f),
    nrow(dplyr::distinct(dplyr::select(
      bcch,
      latitude,
      longitude,
      survey_year,
      survey_month,
      survey_day
    )))
  )
  expect_equal(format(sf::st_crs(f)), "Canada_Albers_Equal_Area_Conic")
  expect_equal(
    unname(apply(
      X = apply(FUN = is.na, X = f, MARGIN = 1),
      FUN = unique,
      MARGIN = 1
    )),
    rep(FALSE, times = 7)
  )
})

test_that("data_fmt() basic functionality with complete BMDE sf POINT", {
  expect_silent(
    f <- suppressMessages(data_fmt(sf::st_as_sf(
      bcch,
      coords = c("longitude", "latitude"),
      crs = 4326
    )))
  )
  expect_s3_class(f, "sf")
  expect_equal(
    as.character(sf::st_geometry_type(f, by_geometry = FALSE)),
    "POINT"
  )
  expect_named(
    f,
    c(
      "SurveyAreaIdentifier",
      "latitude",
      "longitude",
      "survey_year",
      "survey_month",
      "survey_day",
      "geometry"
    )
  )
  expect_equal(
    nrow(f),
    nrow(dplyr::distinct(dplyr::select(
      bcch,
      latitude,
      longitude,
      survey_year,
      survey_month,
      survey_day
    )))
  )
  expect_equal(format(sf::st_crs(f)), "Canada_Albers_Equal_Area_Conic")
  expect_equal(
    unname(apply(
      X = apply(FUN = is.na, X = f, MARGIN = 1),
      FUN = unique,
      MARGIN = 1
    )),
    rep(FALSE, times = 7)
  )
})

test_that("data_fmt() basic functionality with complete BMDE terra points", {
  expect_silent(
    f <- suppressMessages(data_fmt(terra::vect(
      bcch,
      crs = "epsg:4326",
      geom = c("longitude", "latitude")
    )))
  )
  expect_s4_class(f, "SpatVector")
  expect_equal(terra::geomtype(f), "points")
  expect_named(
    f,
    c(
      "SurveyAreaIdentifier",
      "latitude",
      "longitude",
      "survey_year",
      "survey_month",
      "survey_day"
    )
  )
  expect_equal(
    nrow(f),
    nrow(dplyr::distinct(dplyr::select(
      bcch,
      latitude,
      longitude,
      survey_year,
      survey_month,
      survey_day
    )))
  )
  expect_equal(terra::crs(f) == terra::crs("ESRI:102001"), TRUE)
  expect_equal(
    unname(apply(
      X = apply(FUN = is.na, X = terra::values(f), MARGIN = 1),
      FUN = unique,
      MARGIN = 1
    )),
    rep(FALSE, times = 6)
  )
})

test_that("data_fmt() basic functionality with complete BMDE sf POLYGON", {
  expect_silent(
    f <- suppressMessages(data_fmt(sf::st_buffer(
      sf::st_as_sf(bcch, coords = c("longitude", "latitude"), crs = 4326),
      500
    )))
  )
  expect_s3_class(f, "sf")
  expect_equal(
    as.character(sf::st_geometry_type(f, by_geometry = FALSE)),
    "POLYGON"
  )
  expect_named(
    f,
    c(
      "SurveyAreaIdentifier",
      "latitude",
      "longitude",
      "survey_year",
      "survey_month",
      "survey_day",
      "geometry"
    )
  )
  expect_equal(
    nrow(f),
    nrow(dplyr::distinct(dplyr::select(
      bcch,
      latitude,
      longitude,
      survey_year,
      survey_month,
      survey_day
    )))
  )
  expect_equal(format(sf::st_crs(f)), "Canada_Albers_Equal_Area_Conic")
  expect_equal(
    unname(apply(
      X = apply(FUN = is.na, X = f, MARGIN = 1),
      FUN = unique,
      MARGIN = 1
    )),
    rep(FALSE, times = 7)
  )
})

test_that("data_fmt() basic functionality with complete BMDE terra points", {
  expect_silent(
    f <- suppressMessages(data_fmt(terra::buffer(
      terra::vect(bcch, crs = "epsg:4326", geom = c("longitude", "latitude")),
      500
    )))
  )
  expect_s4_class(f, "SpatVector")
  expect_equal(terra::geomtype(f), "polygons")
  expect_named(
    f,
    c(
      "SurveyAreaIdentifier",
      "latitude",
      "longitude",
      "survey_year",
      "survey_month",
      "survey_day"
    )
  )
  expect_equal(
    nrow(f),
    nrow(dplyr::distinct(dplyr::select(
      bcch,
      latitude,
      longitude,
      survey_year,
      survey_month,
      survey_day
    )))
  )
  expect_equal(terra::crs(f) == terra::crs("ESRI:102001"), TRUE)
  expect_equal(
    unname(apply(
      X = apply(FUN = is.na, X = terra::values(f), MARGIN = 1),
      FUN = unique,
      MARGIN = 1
    )),
    rep(FALSE, times = 6)
  )
})

test_that("data_fmt() accepts alternate column names in all data input formats", {
  expect_silent(
    f_df <- suppressMessages(data_fmt(
      dplyr::rename(
        bcch,
        sites = SurveyAreaIdentifier,
        lat = latitude,
        lon = longitude,
        yr = survey_year,
        mth = survey_month,
        dy = survey_day
      ),
      site_name = "sites",
      coord_lon = "lon",
      coord_lat = "lat",
      date_year = "yr",
      date_month = "mth",
      date_day = "dy",
      crs = 4326
    ))
  )
  expect_s3_class(f_df, "sf")
  expect_equal(
    as.character(sf::st_geometry_type(f_df, by_geometry = FALSE)),
    "POINT"
  )
  expect_named(f_df, c("sites", "lat", "lon", "yr", "mth", "dy", "geometry"))
  expect_equal(
    nrow(f_df),
    nrow(dplyr::distinct(dplyr::select(
      bcch,
      latitude,
      longitude,
      survey_year,
      survey_month,
      survey_day
    )))
  )
  expect_equal(format(sf::st_crs(f_df)), "Canada_Albers_Equal_Area_Conic")
  expect_equal(
    unname(apply(
      X = apply(FUN = is.na, X = f_df, MARGIN = 1),
      FUN = unique,
      MARGIN = 1
    )),
    rep(FALSE, times = 7)
  )
  expect_equal(
    c(
      attr(f_df, "site_name"),
      attr(f_df, "coord_lon"),
      attr(f_df, "coord_lat"),
      attr(f_df, "date_year"),
      attr(f_df, "date_month"),
      attr(f_df, "date_day")
    ),
    c("sites", "lon", "lat", "yr", "mth", "dy")
  )

  expect_warning(
    f_sf_pt <- suppressMessages(data_fmt(
      sf::st_as_sf(
        dplyr::rename(
          bcch,
          sites = SurveyAreaIdentifier,
          lat = latitude,
          lon = longitude,
          yr = survey_year,
          mth = survey_month,
          dy = survey_day
        ),
        coords = c("lon", "lat"),
        crs = 4326
      ),
      site_name = "sites",
      coord_lon = "lon",
      coord_lat = "lat",
      date_year = "yr",
      date_month = "mth",
      date_day = "dy"
    )),
    "\\[Data Formatting\\] sf or terra object provided as well as a lat/lon column name. lat/lon will be derived from the spatial data within the sf/terra object and specified lat/lon column will be ignored."
  )
  expect_s3_class(f_sf_pt, "sf")
  expect_equal(
    as.character(sf::st_geometry_type(f_sf_pt, by_geometry = FALSE)),
    "POINT"
  )
  expect_named(
    f_sf_pt,
    c("sites", "latitude", "longitude", "yr", "mth", "dy", "geometry")
  )
  expect_equal(
    nrow(f_sf_pt),
    nrow(dplyr::distinct(dplyr::select(
      bcch,
      latitude,
      longitude,
      survey_year,
      survey_month,
      survey_day
    )))
  )
  expect_equal(format(sf::st_crs(f_sf_pt)), "Canada_Albers_Equal_Area_Conic")
  expect_equal(
    unname(apply(
      X = apply(FUN = is.na, X = f_sf_pt, MARGIN = 1),
      FUN = unique,
      MARGIN = 1
    )),
    rep(FALSE, times = 7)
  )
  expect_equal(
    c(
      attr(f_sf_pt, "site_name"),
      attr(f_sf_pt, "coord_lon"),
      attr(f_sf_pt, "coord_lat"),
      attr(f_sf_pt, "date_year"),
      attr(f_sf_pt, "date_month"),
      attr(f_sf_pt, "date_day")
    ),
    c("sites", "yr", "mth", "dy")
  )

  expect_warning(
    f_terra_pt <- suppressMessages(data_fmt(
      terra::vect(
        dplyr::rename(
          bcch,
          sites = SurveyAreaIdentifier,
          lat = latitude,
          lon = longitude,
          yr = survey_year,
          mth = survey_month,
          dy = survey_day
        ),
        crs = "epsg:4326",
        geom = c("lon", "lat")
      ),
      site_name = "sites",
      coord_lon = "lon",
      coord_lat = "lat",
      date_year = "yr",
      date_month = "mth",
      date_day = "dy"
    )),
    "\\[Data Formatting\\] sf or terra object provided as well as a lat/lon column name. lat/lon will be derived from the spatial data within the sf/terra object and specified lat/lon column will be ignored."
  )
  expect_s4_class(f_terra_pt, "SpatVector")
  expect_equal(terra::geomtype(f_terra_pt), "points")
  expect_named(
    f_terra_pt,
    c("sites", "latitude", "longitude", "yr", "mth", "dy")
  )
  expect_equal(
    nrow(f_terra_pt),
    nrow(dplyr::distinct(dplyr::select(
      bcch,
      latitude,
      longitude,
      survey_year,
      survey_month,
      survey_day
    )))
  )
  expect_equal(terra::crs(f_terra_pt) == terra::crs("ESRI:102001"), TRUE)
  expect_equal(
    unname(apply(
      X = apply(FUN = is.na, X = terra::values(f_terra_pt), MARGIN = 1),
      FUN = unique,
      MARGIN = 1
    )),
    rep(FALSE, times = 6)
  )
  expect_equal(
    c(
      attr(f_terra_pt, "site_name"),
      attr(f_terra_pt, "coord_lon"),
      attr(f_terra_pt, "coord_lat"),
      attr(f_terra_pt, "date_year"),
      attr(f_terra_pt, "date_month"),
      attr(f_terra_pt, "date_day")
    ),
    c("sites", "yr", "mth", "dy")
  )

  expect_warning(
    f_sf_poly <- suppressMessages(data_fmt(
      sf::st_buffer(
        sf::st_as_sf(
          dplyr::rename(
            bcch,
            sites = SurveyAreaIdentifier,
            lat = latitude,
            lon = longitude,
            yr = survey_year,
            mth = survey_month,
            dy = survey_day
          ),
          coords = c("lon", "lat"),
          crs = 4326,
          remove = FALSE
        ),
        500
      ),
      site_name = "sites",
      coord_lon = "lon",
      coord_lat = "lat",
      date_year = "yr",
      date_month = "mth",
      date_day = "dy"
    )),
    "\\[Data Formatting\\] sf or terra object provided as well as a lat/lon column name. lat/lon will be derived from the spatial data within the sf/terra object and specified lat/lon column will be ignored."
  )
  expect_s3_class(f_sf_poly, "sf")
  expect_equal(
    as.character(sf::st_geometry_type(f_sf_poly, by_geometry = FALSE)),
    "POLYGON"
  )
  expect_named(
    f_sf_poly,
    c("sites", "latitude", "longitude", "yr", "mth", "dy", "geometry")
  )
  expect_equal(
    nrow(f_sf_poly),
    nrow(dplyr::distinct(dplyr::select(
      bcch,
      latitude,
      longitude,
      survey_year,
      survey_month,
      survey_day
    )))
  )
  expect_equal(format(sf::st_crs(f_sf_poly)), "Canada_Albers_Equal_Area_Conic")
  expect_equal(
    unname(apply(
      X = apply(FUN = is.na, X = f_sf_poly, MARGIN = 1),
      FUN = unique,
      MARGIN = 1
    )),
    rep(FALSE, times = 7)
  )
  expect_equal(
    c(
      attr(f_sf_poly, "site_name"),
      attr(f_sf_poly, "coord_lon"),
      attr(f_sf_poly, "coord_lat"),
      attr(f_sf_poly, "date_year"),
      attr(f_sf_poly, "date_month"),
      attr(f_sf_poly, "date_day")
    ),
    c("sites", "yr", "mth", "dy")
  )

  expect_warning(
    f_terra_poly <- suppressMessages(data_fmt(
      terra::buffer(
        terra::vect(
          dplyr::rename(
            bcch,
            sites = SurveyAreaIdentifier,
            lat = latitude,
            lon = longitude,
            yr = survey_year,
            mth = survey_month,
            dy = survey_day
          ),
          crs = "epsg:4326",
          geom = c("lon", "lat")
        ),
        500
      ),
      site_name = "sites",
      coord_lon = "lon",
      coord_lat = "lat",
      date_year = "yr",
      date_month = "mth",
      date_day = "dy"
    )),
    "\\[Data Formatting\\] sf or terra object provided as well as a lat/lon column name. lat/lon will be derived from the spatial data within the sf/terra object and specified lat/lon column will be ignored."
  )
  expect_s4_class(f_terra_poly, "SpatVector")
  expect_equal(terra::geomtype(f_terra_poly), "polygons")
  expect_named(
    f_terra_poly,
    c("sites", "latitude", "longitude", "yr", "mth", "dy")
  )
  expect_equal(
    nrow(f_terra_poly),
    nrow(dplyr::distinct(dplyr::select(
      bcch,
      latitude,
      longitude,
      survey_year,
      survey_month,
      survey_day
    )))
  )
  expect_equal(terra::crs(f_terra_poly) == terra::crs("ESRI:102001"), TRUE)
  expect_equal(
    unname(apply(
      X = apply(FUN = is.na, X = terra::values(f_terra_poly), MARGIN = 1),
      FUN = unique,
      MARGIN = 1
    )),
    rep(FALSE, times = 6)
  )
  expect_equal(
    c(
      attr(f_terra_poly, "site_name"),
      attr(f_terra_poly, "coord_lon"),
      attr(f_terra_poly, "coord_lat"),
      attr(f_terra_poly, "date_year"),
      attr(f_terra_poly, "date_month"),
      attr(f_terra_poly, "date_day")
    ),
    c("sites", "yr", "mth", "dy")
  )
})

test_that("data_fmt() date conversion from lubridate works in all input formats", {
  expect_warning(
    f_df <- suppressMessages(data_fmt(
      dplyr::mutate(
        bcch,
        date = as.Date(paste0(survey_year, "-", survey_month, "-", survey_day))
      ),
      date_lubridate = "date"
    )),
    "\\[Data Formatting\\] as the 'crs' argument is not specified, data CRS is assumed to be EPSG:4326."
  )
  expect_s3_class(f_df, "sf")
  expect_equal(
    as.character(sf::st_geometry_type(f_df, by_geometry = FALSE)),
    "POINT"
  )
  expect_named(
    f_df,
    c(
      "SurveyAreaIdentifier",
      "latitude",
      "longitude",
      "date",
      "survey_year",
      "survey_month",
      "survey_day",
      "geometry"
    )
  )
  expect_equal(
    nrow(f_df),
    nrow(dplyr::distinct(dplyr::select(
      bcch,
      latitude,
      longitude,
      survey_year,
      survey_month,
      survey_day
    )))
  )
  expect_equal(format(sf::st_crs(f_df)), "Canada_Albers_Equal_Area_Conic")
  expect_equal(
    unname(apply(
      X = apply(FUN = is.na, X = f_df, MARGIN = 1),
      FUN = unique,
      MARGIN = 1
    )),
    rep(FALSE, times = 8)
  )
  expect_equal(
    all(
      f_df$date %in%
        dplyr::mutate(
          bcch,
          date = as.Date(paste0(
            survey_year,
            "-",
            survey_month,
            "-",
            survey_day
          ))
        )$date
    ),
    TRUE
  )
  expect_equal(f_df$survey_year, lubridate::year(f_df$date))
  expect_equal(f_df$survey_month, lubridate::month(f_df$date))
  expect_equal(f_df$survey_day, lubridate::day(f_df$date))
  expect_equal(attr(f_df, "date_lubridate"), "date")

  expect_silent(
    f_sf_pt <- suppressMessages(data_fmt(
      sf::st_as_sf(
        dplyr::mutate(
          bcch,
          date = as.Date(paste0(
            survey_year,
            "-",
            survey_month,
            "-",
            survey_day
          ))
        ),
        coords = c("longitude", "latitude"),
        crs = 4326
      ),
      date_lubridate = "date"
    ))
  )
  expect_s3_class(f_sf_pt, "sf")
  expect_equal(
    as.character(sf::st_geometry_type(f_sf_pt, by_geometry = FALSE)),
    "POINT"
  )
  expect_named(
    f_sf_pt,
    c(
      "SurveyAreaIdentifier",
      "latitude",
      "longitude",
      "date",
      "survey_year",
      "survey_month",
      "survey_day",
      "geometry"
    )
  )
  expect_equal(
    nrow(f_sf_pt),
    nrow(dplyr::distinct(dplyr::select(
      bcch,
      latitude,
      longitude,
      survey_year,
      survey_month,
      survey_day
    )))
  )
  expect_equal(format(sf::st_crs(f_sf_pt)), "Canada_Albers_Equal_Area_Conic")
  expect_equal(
    unname(apply(
      X = apply(FUN = is.na, X = f_sf_pt, MARGIN = 1),
      FUN = unique,
      MARGIN = 1
    )),
    rep(FALSE, times = 8)
  )
  expect_equal(
    all(
      f_sf_pt$date %in%
        dplyr::mutate(
          bcch,
          date = as.Date(paste0(
            survey_year,
            "-",
            survey_month,
            "-",
            survey_day
          ))
        )$date
    ),
    TRUE
  )
  expect_equal(f_sf_pt$survey_year, lubridate::year(f_sf_pt$date))
  expect_equal(f_sf_pt$survey_month, lubridate::month(f_sf_pt$date))
  expect_equal(f_sf_pt$survey_day, lubridate::day(f_sf_pt$date))
  expect_equal(attr(f_sf_pt, "date_lubridate"), "date")

  expect_silent(
    f_terra_pt <- suppressMessages(data_fmt(
      terra::vect(
        dplyr::mutate(
          bcch,
          date = as.Date(paste0(
            survey_year,
            "-",
            survey_month,
            "-",
            survey_day
          ))
        ),
        crs = "epsg:4326",
        geom = c("longitude", "latitude")
      ),
      date_lubridate = "date"
    ))
  )
  expect_s4_class(f_terra_pt, "SpatVector")
  expect_equal(terra::geomtype(f_terra_pt), "points")
  expect_named(
    f_terra_pt,
    c(
      "SurveyAreaIdentifier",
      "latitude",
      "longitude",
      "date",
      "survey_year",
      "survey_month",
      "survey_day"
    )
  )
  expect_equal(
    nrow(f_terra_pt),
    nrow(dplyr::distinct(dplyr::select(
      bcch,
      latitude,
      longitude,
      survey_year,
      survey_month,
      survey_day
    )))
  )
  expect_equal(terra::crs(f_terra_pt) == terra::crs("ESRI:102001"), TRUE)
  expect_equal(
    unname(apply(
      X = apply(FUN = is.na, X = terra::values(f_terra_pt), MARGIN = 1),
      FUN = unique,
      MARGIN = 1
    )),
    rep(FALSE, times = 7)
  )
  expect_equal(
    all(
      f_terra_pt$date %in%
        dplyr::mutate(
          bcch,
          date = as.Date(paste0(
            survey_year,
            "-",
            survey_month,
            "-",
            survey_day
          ))
        )$date
    ),
    TRUE
  )
  expect_equal(f_terra_pt$survey_year, lubridate::year(f_terra_pt$date))
  expect_equal(f_terra_pt$survey_month, lubridate::month(f_terra_pt$date))
  expect_equal(f_terra_pt$survey_day, lubridate::day(f_terra_pt$date))
  expect_equal(attr(f_terra_pt, "date_lubridate"), "date")

  expect_silent(
    f_sf_poly <- suppressMessages(data_fmt(
      sf::st_buffer(
        sf::st_as_sf(
          dplyr::mutate(
            bcch,
            date = as.Date(paste0(
              survey_year,
              "-",
              survey_month,
              "-",
              survey_day
            ))
          ),
          coords = c("longitude", "latitude"),
          crs = 4326
        ),
        500
      ),
      date_lubridate = "date"
    ))
  )
  expect_s3_class(f_sf_poly, "sf")
  expect_equal(
    as.character(sf::st_geometry_type(f_sf_poly, by_geometry = FALSE)),
    "POLYGON"
  )
  expect_named(
    f_sf_poly,
    c(
      "SurveyAreaIdentifier",
      "latitude",
      "longitude",
      "date",
      "survey_year",
      "survey_month",
      "survey_day",
      "geometry"
    )
  )
  expect_equal(
    nrow(f_sf_poly),
    nrow(dplyr::distinct(dplyr::select(
      bcch,
      latitude,
      longitude,
      survey_year,
      survey_month,
      survey_day
    )))
  )
  expect_equal(format(sf::st_crs(f_sf_poly)), "Canada_Albers_Equal_Area_Conic")
  expect_equal(
    unname(apply(
      X = apply(FUN = is.na, X = f_sf_poly, MARGIN = 1),
      FUN = unique,
      MARGIN = 1
    )),
    rep(FALSE, times = 8)
  )
  expect_equal(
    all(
      f_sf_poly$date %in%
        dplyr::mutate(
          bcch,
          date = as.Date(paste0(
            survey_year,
            "-",
            survey_month,
            "-",
            survey_day
          ))
        )$date
    ),
    TRUE
  )
  expect_equal(f_sf_poly$survey_year, lubridate::year(f_sf_poly$date))
  expect_equal(f_sf_poly$survey_month, lubridate::month(f_sf_poly$date))
  expect_equal(f_sf_poly$survey_day, lubridate::day(f_sf_poly$date))
  expect_equal(attr(f_sf_poly, "date_lubridate"), "date")

  expect_silent(
    f_terra_poly <- suppressMessages(data_fmt(
      terra::buffer(
        terra::vect(
          dplyr::mutate(
            bcch,
            date = as.Date(paste0(
              survey_year,
              "-",
              survey_month,
              "-",
              survey_day
            ))
          ),
          crs = "epsg:4326",
          geom = c("longitude", "latitude")
        ),
        500
      ),
      date_lubridate = "date"
    ))
  )
  expect_s4_class(f_terra_poly, "SpatVector")
  expect_equal(terra::geomtype(f_terra_poly), "polygons")
  expect_named(
    f_terra_poly,
    c(
      "SurveyAreaIdentifier",
      "latitude",
      "longitude",
      "date",
      "survey_year",
      "survey_month",
      "survey_day"
    )
  )
  expect_equal(
    nrow(f_terra_poly),
    nrow(dplyr::distinct(dplyr::select(
      bcch,
      latitude,
      longitude,
      survey_year,
      survey_month,
      survey_day
    )))
  )
  expect_equal(terra::crs(f_terra_poly) == terra::crs("ESRI:102001"), TRUE)
  expect_equal(
    unname(apply(
      X = apply(FUN = is.na, X = terra::values(f_terra_poly), MARGIN = 1),
      FUN = unique,
      MARGIN = 1
    )),
    rep(FALSE, times = 7)
  )
  expect_equal(
    all(
      f_terra_poly$date %in%
        dplyr::mutate(
          bcch,
          date = as.Date(paste0(
            survey_year,
            "-",
            survey_month,
            "-",
            survey_day
          ))
        )$date
    ),
    TRUE
  )
  expect_equal(f_terra_poly$survey_year, lubridate::year(f_terra_poly$date))
  expect_equal(f_terra_poly$survey_month, lubridate::month(f_terra_poly$date))
  expect_equal(f_terra_poly$survey_day, lubridate::day(f_terra_poly$date))
  expect_equal(attr(f_terra_poly, "date_lubridate"), "date")
})

test_that("data_fmt() date conversion from ordinal works in all input formats", {
  expect_warning(
    f_df <- suppressMessages(data_fmt(
      dplyr::mutate(
        bcch,
        doy = as.numeric(
          as.Date(paste0(survey_year, "-", survey_month, "-", survey_day)) -
            as.Date(paste0(survey_year, "-01-01")) +
            1
        )
      ),
      date_ordinal = "doy"
    )),
    "\\[Data Formatting\\] as the 'crs' argument is not specified, data CRS is assumed to be EPSG:4326."
  )
  expect_s3_class(f_df, "sf")
  expect_equal(
    as.character(sf::st_geometry_type(f_df, by_geometry = FALSE)),
    "POINT"
  )
  expect_named(
    f_df,
    c(
      "SurveyAreaIdentifier",
      "latitude",
      "longitude",
      "survey_year",
      "doy",
      "survey_month",
      "survey_day",
      "geometry"
    )
  )
  expect_equal(
    nrow(f_df),
    nrow(dplyr::distinct(dplyr::select(
      bcch,
      latitude,
      longitude,
      survey_year,
      survey_month,
      survey_day
    )))
  )
  expect_equal(format(sf::st_crs(f_df)), "Canada_Albers_Equal_Area_Conic")
  expect_equal(
    unname(apply(
      X = apply(FUN = is.na, X = f_df, MARGIN = 1),
      FUN = unique,
      MARGIN = 1
    )),
    rep(FALSE, times = 8)
  )
  expect_equal(
    all(
      as.Date(paste0(
        f_df$survey_year,
        "-",
        f_df$survey_month,
        "-",
        f_df$survey_day
      )) %in%
        dplyr::mutate(
          bcch,
          date = as.Date(paste0(
            survey_year,
            "-",
            survey_month,
            "-",
            survey_day
          ))
        )$date
    ),
    TRUE
  )
  expect_equal(
    f_df$survey_year,
    lubridate::year(as.Date(paste0(
      f_df$survey_year,
      "-",
      f_df$survey_month,
      "-",
      f_df$survey_day
    )))
  )
  expect_equal(
    f_df$survey_month,
    lubridate::month(as.Date(paste0(
      f_df$survey_year,
      "-",
      f_df$survey_month,
      "-",
      f_df$survey_day
    )))
  )
  expect_equal(
    f_df$survey_day,
    lubridate::day(as.Date(paste0(
      f_df$survey_year,
      "-",
      f_df$survey_month,
      "-",
      f_df$survey_day
    )))
  )
  expect_equal(attr(f_df, "date_ordinal"), "doy")

  expect_silent(
    f_sf_pt <- suppressMessages(data_fmt(
      sf::st_as_sf(
        dplyr::mutate(
          bcch,
          doy = as.numeric(
            as.Date(paste0(survey_year, "-", survey_month, "-", survey_day)) -
              as.Date(paste0(survey_year, "-01-01")) +
              1
          )
        ),
        coords = c("longitude", "latitude"),
        crs = 4326
      ),
      date_ordinal = "doy"
    ))
  )
  expect_s3_class(f_sf_pt, "sf")
  expect_equal(
    as.character(sf::st_geometry_type(f_sf_pt, by_geometry = FALSE)),
    "POINT"
  )
  expect_named(
    f_sf_pt,
    c(
      "SurveyAreaIdentifier",
      "latitude",
      "longitude",
      "survey_year",
      "doy",
      "survey_month",
      "survey_day",
      "geometry"
    )
  )
  expect_equal(
    nrow(f_sf_pt),
    nrow(dplyr::distinct(dplyr::select(
      bcch,
      latitude,
      longitude,
      survey_year,
      survey_month,
      survey_day
    )))
  )
  expect_equal(format(sf::st_crs(f_sf_pt)), "Canada_Albers_Equal_Area_Conic")
  expect_equal(
    unname(apply(
      X = apply(FUN = is.na, X = f_sf_pt, MARGIN = 1),
      FUN = unique,
      MARGIN = 1
    )),
    rep(FALSE, times = 8)
  )
  expect_equal(
    all(
      as.Date(paste0(
        f_sf_pt$survey_year,
        "-",
        f_sf_pt$survey_month,
        "-",
        f_sf_pt$survey_day
      )) %in%
        dplyr::mutate(
          bcch,
          date = as.Date(paste0(
            survey_year,
            "-",
            survey_month,
            "-",
            survey_day
          ))
        )$date
    ),
    TRUE
  )
  expect_equal(
    f_sf_pt$survey_year,
    lubridate::year(as.Date(paste0(
      f_sf_pt$survey_year,
      "-",
      f_sf_pt$survey_month,
      "-",
      f_sf_pt$survey_day
    )))
  )
  expect_equal(
    f_sf_pt$survey_month,
    lubridate::month(as.Date(paste0(
      f_sf_pt$survey_year,
      "-",
      f_sf_pt$survey_month,
      "-",
      f_sf_pt$survey_day
    )))
  )
  expect_equal(
    f_sf_pt$survey_day,
    lubridate::day(as.Date(paste0(
      f_sf_pt$survey_year,
      "-",
      f_sf_pt$survey_month,
      "-",
      f_sf_pt$survey_day
    )))
  )
  expect_equal(attr(f_sf_pt, "date_ordinal"), "doy")

  expect_silent(
    f_terra_pt <- suppressMessages(data_fmt(
      terra::vect(
        dplyr::mutate(
          bcch,
          doy = as.numeric(
            as.Date(paste0(survey_year, "-", survey_month, "-", survey_day)) -
              as.Date(paste0(survey_year, "-01-01")) +
              1
          )
        ),
        crs = "epsg:4326",
        geom = c("longitude", "latitude")
      ),
      date_ordinal = "doy"
    ))
  )
  expect_s4_class(f_terra_pt, "SpatVector")
  expect_equal(terra::geomtype(f_terra_pt), "points")
  expect_named(
    f_terra_pt,
    c(
      "SurveyAreaIdentifier",
      "latitude",
      "longitude",
      "survey_year",
      "doy",
      "survey_month",
      "survey_day"
    )
  )
  expect_equal(
    nrow(f_terra_pt),
    nrow(dplyr::distinct(dplyr::select(
      bcch,
      latitude,
      longitude,
      survey_year,
      survey_month,
      survey_day
    )))
  )
  expect_equal(terra::crs(f_terra_pt) == terra::crs("ESRI:102001"), TRUE)
  expect_equal(
    unname(apply(
      X = apply(FUN = is.na, X = terra::values(f_terra_pt), MARGIN = 1),
      FUN = unique,
      MARGIN = 1
    )),
    rep(FALSE, times = 7)
  )
  expect_equal(
    all(
      as.Date(paste0(
        f_terra_pt$survey_year,
        "-",
        f_terra_pt$survey_month,
        "-",
        f_terra_pt$survey_day
      )) %in%
        dplyr::mutate(
          bcch,
          date = as.Date(paste0(
            survey_year,
            "-",
            survey_month,
            "-",
            survey_day
          ))
        )$date
    ),
    TRUE
  )
  expect_equal(
    f_terra_pt$survey_year,
    lubridate::year(as.Date(paste0(
      f_terra_pt$survey_year,
      "-",
      f_terra_pt$survey_month,
      "-",
      f_terra_pt$survey_day
    )))
  )
  expect_equal(
    f_terra_pt$survey_month,
    lubridate::month(as.Date(paste0(
      f_terra_pt$survey_year,
      "-",
      f_terra_pt$survey_month,
      "-",
      f_terra_pt$survey_day
    )))
  )
  expect_equal(
    f_terra_pt$survey_day,
    lubridate::day(as.Date(paste0(
      f_terra_pt$survey_year,
      "-",
      f_terra_pt$survey_month,
      "-",
      f_terra_pt$survey_day
    )))
  )
  expect_equal(attr(f_terra_pt, "date_ordinal"), "doy")

  expect_silent(
    f_sf_poly <- suppressMessages(data_fmt(
      sf::st_buffer(
        sf::st_as_sf(
          dplyr::mutate(
            bcch,
            doy = as.numeric(
              as.Date(paste0(survey_year, "-", survey_month, "-", survey_day)) -
                as.Date(paste0(survey_year, "-01-01")) +
                1
            )
          ),
          coords = c("longitude", "latitude"),
          crs = 4326
        ),
        500
      ),
      date_ordinal = "doy"
    ))
  )
  expect_s3_class(f_sf_poly, "sf")
  expect_equal(
    as.character(sf::st_geometry_type(f_sf_poly, by_geometry = FALSE)),
    "POLYGON"
  )
  expect_named(
    f_sf_poly,
    c(
      "SurveyAreaIdentifier",
      "latitude",
      "longitude",
      "survey_year",
      "doy",
      "survey_month",
      "survey_day",
      "geometry"
    )
  )
  expect_equal(
    nrow(f_sf_poly),
    nrow(dplyr::distinct(dplyr::select(
      bcch,
      latitude,
      longitude,
      survey_year,
      survey_month,
      survey_day
    )))
  )
  expect_equal(format(sf::st_crs(f_sf_poly)), "Canada_Albers_Equal_Area_Conic")
  expect_equal(
    unname(apply(
      X = apply(FUN = is.na, X = f_sf_poly, MARGIN = 1),
      FUN = unique,
      MARGIN = 1
    )),
    rep(FALSE, times = 8)
  )
  expect_equal(
    all(
      as.Date(paste0(
        f_sf_poly$survey_year,
        "-",
        f_sf_poly$survey_month,
        "-",
        f_sf_poly$survey_day
      )) %in%
        dplyr::mutate(
          bcch,
          date = as.Date(paste0(
            survey_year,
            "-",
            survey_month,
            "-",
            survey_day
          ))
        )$date
    ),
    TRUE
  )
  expect_equal(
    f_sf_poly$survey_year,
    lubridate::year(as.Date(paste0(
      f_sf_poly$survey_year,
      "-",
      f_sf_poly$survey_month,
      "-",
      f_sf_poly$survey_day
    )))
  )
  expect_equal(
    f_sf_poly$survey_month,
    lubridate::month(as.Date(paste0(
      f_sf_poly$survey_year,
      "-",
      f_sf_poly$survey_month,
      "-",
      f_sf_poly$survey_day
    )))
  )
  expect_equal(
    f_sf_poly$survey_day,
    lubridate::day(as.Date(paste0(
      f_sf_poly$survey_year,
      "-",
      f_sf_poly$survey_month,
      "-",
      f_sf_poly$survey_day
    )))
  )
  expect_equal(attr(f_sf_poly, "date_ordinal"), "doy")

  expect_silent(
    f_terra_poly <- suppressMessages(data_fmt(
      terra::buffer(
        terra::vect(
          dplyr::mutate(
            bcch,
            doy = as.numeric(
              as.Date(paste0(survey_year, "-", survey_month, "-", survey_day)) -
                as.Date(paste0(survey_year, "-01-01")) +
                1
            )
          ),
          crs = "epsg:4326",
          geom = c("longitude", "latitude")
        ),
        500
      ),
      date_ordinal = "doy"
    ))
  )
  expect_s4_class(f_terra_poly, "SpatVector")
  expect_equal(terra::geomtype(f_terra_poly), "polygons")
  expect_named(
    f_terra_poly,
    c(
      "SurveyAreaIdentifier",
      "latitude",
      "longitude",
      "survey_year",
      "doy",
      "survey_month",
      "survey_day"
    )
  )
  expect_equal(
    nrow(f_terra_poly),
    nrow(dplyr::distinct(dplyr::select(
      bcch,
      latitude,
      longitude,
      survey_year,
      survey_month,
      survey_day
    )))
  )
  expect_equal(terra::crs(f_terra_poly) == terra::crs("ESRI:102001"), TRUE)
  expect_equal(
    unname(apply(
      X = apply(FUN = is.na, X = terra::values(f_terra_poly), MARGIN = 1),
      FUN = unique,
      MARGIN = 1
    )),
    rep(FALSE, times = 7)
  )
  expect_equal(
    all(
      as.Date(paste0(
        f_terra_poly$survey_year,
        "-",
        f_terra_poly$survey_month,
        "-",
        f_terra_poly$survey_day
      )) %in%
        dplyr::mutate(
          bcch,
          date = as.Date(paste0(
            survey_year,
            "-",
            survey_month,
            "-",
            survey_day
          ))
        )$date
    ),
    TRUE
  )
  expect_equal(
    f_terra_poly$survey_year,
    lubridate::year(as.Date(paste0(
      f_terra_poly$survey_year,
      "-",
      f_terra_poly$survey_month,
      "-",
      f_terra_poly$survey_day
    )))
  )
  expect_equal(
    f_terra_poly$survey_month,
    lubridate::month(as.Date(paste0(
      f_terra_poly$survey_year,
      "-",
      f_terra_poly$survey_month,
      "-",
      f_terra_poly$survey_day
    )))
  )
  expect_equal(
    f_terra_poly$survey_day,
    lubridate::day(as.Date(paste0(
      f_terra_poly$survey_year,
      "-",
      f_terra_poly$survey_month,
      "-",
      f_terra_poly$survey_day
    )))
  )
  expect_equal(attr(f_terra_poly, "date_ordinal"), "doy")
})

test_that("data_fmt() drops sites with missing coordinate data (only data.frame input will bring this on)", {
  expect_warning(
    f_df <- suppressMessages(data_fmt(
      dplyr::mutate(bcch, latitude = c(bcch$latitude[1:nrow(bcch) - 1], NA)),
      coord_lon = "longitude",
      coord_lat = "latitude",
      crs = 4326
    )),
    "\\[Data Formatting\\] some rows missing coordinate data will be dropped."
  )
})

test_that("data_fmt() invalid input data formats return appropriate error", {
  expect_error(
    f_char <- suppressMessages(data_fmt("invalid")),
    "\\[Data Formatting\\] invalid data format. Please provide data as either a dataframe, sf object with either `POINT` or `POLYGON` geometry, or terra SpatVector object with `points` or `polygons` geometry."
  )

  expect_error(
    f_numeric <- suppressMessages(data_fmt(1)),
    "\\[Data Formatting\\] invalid data format. Please provide data as either a dataframe, sf object with either `POINT` or `POLYGON` geometry, or terra SpatVector object with `points` or `polygons` geometry."
  )

  expect_error(
    f_vector <- suppressMessages(data_fmt(c("invalid", 2, NA))),
    "\\[Data Formatting\\] invalid data format. Please provide data as either a dataframe, sf object with either `POINT` or `POLYGON` geometry, or terra SpatVector object with `points` or `polygons` geometry."
  )

  expect_error(
    f_SpatRaster <- suppressMessages(data_fmt(terra::rast(
      nrows = 108,
      ncols = 21,
      xmin = 0,
      xmax = 10
    ))),
    "\\[Data Formatting\\] invalid data format. Please provide data as either a dataframe, sf object with either `POINT` or `POLYGON` geometry, or terra SpatVector object with `points` or `polygons` geometry."
  )

  expect_error(
    f_lines <- suppressMessages(data_fmt(terra::as.lines(terra::vect(
      data.frame(longitude = c(100, 110), latitude = c(45, 46)),
      crs = "epsg:4326",
      geom = c("longitude", "latitude")
    )))),
    "\\[Data Formatting\\] terra object provided, but not a set of points or polygons."
  )

  expect_error(
    f_LINESTRING <- suppressMessages(data_fmt(sf::st_cast(
      sf::st_as_sf(
        data.frame(longitude = c(100, 110), latitude = c(45, 46)),
        coords = c("longitude", "latitude"),
        crs = 4326
      ),
      "LINESTRING"
    ))),
    "\\[Data Formatting\\] sf object provided, but not a set of POINT or POLYGON geometries."
  )

  expect_error(
    f_mixedgeoms <- suppressMessages(data_fmt(rbind(
      sf::st_as_sf(
        data.frame(x = 100, y = 45),
        coords = c("x", "y"),
        crs = 4326
      ),
      sf::st_buffer(
        sf::st_as_sf(
          data.frame(x = 100, y = 45),
          coords = c("x", "y"),
          crs = 4326
        ),
        500
      )
    ))),
    "\\[Data Formatting\\] mixed sf geometries detected. Please provide a set of only POINT geometries or only POLYGON geometries."
  )
})

test_that("data_fmt() invalid alternate column names return appropriate error", {
  expect_error(
    f <- suppressMessages(
      data_fmt(
        dplyr::rename(bcch, sites = SurveyAreaIdentifier),
        site_name = "Sites",
        coord_lon = "longitude",
        coord_lat = "latitude",
        crs = 4326
      )
    ),
    "\\[Data Formatting\\] some specified columns missing from the data: Sites. Use arguments to specify alternate column names if using data that diverges from NatureCounts default column names."
  )
})

test_that("data_fmt() invalid date data return appropriate errors", {
  expect_error(
    f <- suppressMessages(
      data_fmt(
        dplyr::mutate(bcch, date = "I'm not a date!"),
        date_lubridate = "date",
        coord_lon = "longitude",
        coord_lat = "latitude",
        crs = 4326
      )
    ),
    "\\[Data Formatting\\] column date expected to be in `Date` format, but is not."
  )
})

test_that("data_fmt() invalid CRSs return error", {
  expect_error(
    f_df <- suppressMessages(
      data_fmt(
        bcch,
        coord_lon = "longitude",
        coord_lat = "latitude",
        crs = "I'm not a CRS!"
      )
    ),
    "\\[Data Formatting\\] the provided CRS is invalid. CRS must be a valid proj4string character, a valid epsg integer value, or a list containing named elements proj4string \\(character\\) and\\/or epsg \\(integer\\)."
  )
})

test_that("data_buff() basic functionality with BMDE sf POINT", {
  expect_warning(
    buffed <- suppressMessages(data_buff(data_fmt(bcch))),
    "\\[Data Formatting\\] as the 'crs' argument is not specified, data CRS is assumed to be EPSG:4326."
  )
  expect_equal(
    suppressWarnings(sf::st_centroid(buffed)),
    suppressWarnings(sf::st_centroid(suppressWarnings(sf::st_buffer(
      suppressMessages(data_fmt(bcch)),
      500
    ))))
  )
  expect_equal(
    sf::st_area(buffed),
    sf::st_area(suppressWarnings(sf::st_buffer(
      suppressMessages(data_fmt(bcch)),
      500
    )))
  )
  expect_s3_class(buffed, "sf")
  expect_equal(
    as.character(sf::st_geometry_type(buffed, by_geometry = FALSE)),
    "POLYGON"
  )
})

test_that("data_buff() basic functionality with BMDE terra points", {
  expect_silent(
    buffed <- suppressMessages(data_buff(data_fmt(terra::vect(
      bcch,
      crs = "epsg:4326",
      geom = c("longitude", "latitude")
    ))))
  )
  expect_equal(
    suppressWarnings(terra::crds(terra::centroids(buffed))),
    terra::crds(suppressWarnings(terra::centroids(suppressWarnings(terra::buffer(
      suppressMessages(data_fmt(terra::vect(
        bcch,
        crs = "epsg:4326",
        geom = c("longitude", "latitude")
      ))),
      500
    )))))
  )
  expect_equal(
    terra::expanse(buffed),
    terra::expanse(terra::buffer(
      suppressMessages(data_fmt(terra::vect(
        bcch,
        crs = "epsg:4326",
        geom = c("longitude", "latitude")
      ))),
      500
    ))
  )
  expect_s4_class(buffed, "SpatVector")
  expect_equal(terra::geomtype(buffed), "polygons")
})

test_that("data_buff() basic functionality with BMDE sf POLYGON", {
  expect_warning(
    buffed <- suppressMessages(data_buff(data_fmt(sf::st_buffer(
      sf::st_as_sf(bcch, coords = c("longitude", "latitude"), crs = 4326),
      500
    )))),
    "\\[Data Buffering\\] sf POLYGON geometry provided. Existing polygons will be buffered by an additional 500m."
  )
  expect_equal(
    suppressWarnings(sf::st_centroid(buffed)),
    suppressWarnings(sf::st_centroid(suppressWarnings(sf::st_buffer(
      suppressMessages(data_fmt(sf::st_buffer(
        sf::st_as_sf(bcch, coords = c("longitude", "latitude"), crs = 4326),
        500
      ))),
      500
    ))))
  )
  expect_equal(
    sf::st_area(buffed),
    sf::st_area(suppressWarnings(sf::st_buffer(
      suppressMessages(data_fmt(sf::st_buffer(
        sf::st_as_sf(bcch, coords = c("longitude", "latitude"), crs = 4326),
        500
      ))),
      500
    )))
  )
  expect_s3_class(buffed, "sf")
  expect_equal(
    as.character(sf::st_geometry_type(buffed, by_geometry = FALSE)),
    "POLYGON"
  )
})

test_that("data_buff() basic functionality with BMDE terra polygons", {
  expect_warning(
    buffed <- suppressMessages(data_buff(data_fmt(terra::buffer(
      terra::vect(bcch, crs = "epsg:4326", geom = c("longitude", "latitude")),
      500
    )))),
    "\\[Data Buffering\\] terra polygons provided. Existing polygons will be buffered by an additional 500m."
  )
  expect_equal(
    suppressWarnings(terra::crds(terra::centroids(buffed))),
    terra::crds(terra::centroids(terra::buffer(
      suppressMessages(data_fmt(terra::buffer(
        terra::vect(bcch, crs = "epsg:4326", geom = c("longitude", "latitude")),
        500
      ))),
      500
    )))
  )
  expect_equal(
    terra::expanse(buffed),
    terra::expanse(terra::buffer(
      suppressMessages(data_fmt(terra::buffer(
        terra::vect(bcch, crs = "epsg:4326", geom = c("longitude", "latitude")),
        500
      ))),
      500
    ))
  )
  expect_s4_class(buffed, "SpatVector")
  expect_equal(terra::geomtype(buffed), "polygons")
})

test_that("data_buff() rejects invalid data inputs with appropriate errors NOTE: this is different from the test at line 525 as error message is adjusted with a tryCatch to remove the suggestion that data.frame inputs are valid.", {
  expect_error(
    buff_char <- data_buff("invalid"),
    "\\[Data Formatting\\] invalid data format. Please provide data as a sf object with `POINT` or `POLYGON` geometry, or terra SpatVector object with `points` or `polygons` geometry."
  )

  expect_error(
    buff_numeric <- data_buff(1),
    "\\[Data Formatting\\] invalid data format. Please provide data as a sf object with `POINT` or `POLYGON` geometry, or terra SpatVector object with `points` or `polygons` geometry."
  )

  expect_error(
    buff_vector <- data_buff(c("invalid", 2, NA)),
    "\\[Data Formatting\\] invalid data format. Please provide data as a sf object with `POINT` or `POLYGON` geometry, or terra SpatVector object with `points` or `polygons` geometry."
  )

  expect_error(
    buff_SpatRaster <- data_buff(terra::rast(
      nrows = 108,
      ncols = 21,
      xmin = 0,
      xmax = 10
    )),
    "\\[Data Formatting\\] invalid data format. Please provide data as a sf object with `POINT` or `POLYGON` geometry, or terra SpatVector object with `points` or `polygons` geometry."
  )

  expect_error(
    buff_lines <- data_buff(terra::as.lines(terra::vect(
      data.frame(longitude = c(100, 110), latitude = c(45, 46)),
      crs = "epsg:4326",
      geom = c("longitude", "latitude")
    ))),
    "\\[Data Formatting\\] terra object provided, but not a set of points or polygons."
  )

  expect_error(
    buff_LINESTRING <- data_buff(sf::st_cast(
      sf::st_as_sf(
        data.frame(longitude = c(100, 110), latitude = c(45, 46)),
        coords = c("longitude", "latitude"),
        crs = 4326
      ),
      "LINESTRING"
    )),
    "\\[Data Formatting\\] sf object provided, but not a set of POINT or POLYGON geometries."
  )

  expect_error(
    buff_mixedgeoms <- data_buff(rbind(
      sf::st_as_sf(
        data.frame(x = 100, y = 45),
        coords = c("x", "y"),
        crs = 4326
      ),
      sf::st_buffer(
        sf::st_as_sf(
          data.frame(x = 100, y = 45),
          coords = c("x", "y"),
          crs = 4326
        ),
        500
      )
    )),
    "\\[Data Formatting\\] mixed sf geometries detected. Please provide a set of only POINT geometries or only POLYGON geometries."
  )
})

test_that("data_buff() rejects invalid distance or units inputs.", {
  expect_error(
    data_buff(
      suppressMessages(data_fmt(
        bcch,
        coord_lon = "longitude",
        coord_lat = "latitude",
        crs = 4326
      )),
      buffer_distance = "I'm not a number!",
      buffer_units = "m"
    ),
    "\\[Data Buffering\\] 'buffer_distance' could not be converted to numeric. Please provide desired buffer distance as a numeric input."
  )
  expect_error(
    data_buff(
      suppressMessages(data_fmt(
        bcch,
        coord_lon = "longitude",
        coord_lat = "latitude",
        crs = 4326
      )),
      buffer = "Not Logical!"
    ),
    "\\[Data Buffering\\] argument 'buffer' should be a boolean \\(i.e. TRUE or FALSE\\)."
  )
  expect_error(
    data_buff(
      suppressMessages(data_fmt(
        bcch,
        coord_lon = "longitude",
        coord_lat = "latitude",
        crs = 4326
      )),
      buffer_distance = 500,
      buffer_units = "Lego Pieces"
    ),
    "\\[Data Buffering\\] buffer units not recognized: please set buffer_units to one of 'm' \\[metres\\], 'km' \\[kilometers\\], 'ft' \\[feet\\], 'yd' \\[yards\\], 'mi' \\[miles\\], or 'naut_mi' \\[nautical miles\\]."
  )
})
