test_that("data_fmt() basic functionality with complete BMDE data.frame", {
  expect_warning(f <- suppressMessages(data_fmt(bcch)), "[Data Formatting] as the 'crs' argument is not specified, data CRS is assumed to be EPSG:4326.")
  expect_s3_class(f, "sf")
  expect_equal(as.character(sf::st_geometry_type(f, by_geometry = FALSE)), "POINT")
  expect_named(f, c("SurveyAreaIdentifier", "latitude", "longitude", "survey_year", "survey_month", "survey_day", "geometry"))
  expect_equal(nrow(f), nrow(dplyr::distinct(dplyr::select(bcch, latitude, longitude, survey_year, survey_month, survey_day))))
  expect_equal(format(sf::st_crs(f)), "Canada_Albers_Equal_Area_Conic")
  expect_equal(unname(apply(X = apply(FUN = is.na, X = f, MARGIN = 1), FUN = unique, MARGIN = 1)), rep(FALSE, times = 7))
})

test_that("data_fmt() basic functionality with complete BMDE sf POINT", {
  expect_silent(f <- suppressMessages(data_fmt(sf::st_as_sf(bcch, coords = c("longitude", "latitude"), crs = 4326))))
  expect_s3_class(f, "sf")
  expect_equal(as.character(sf::st_geometry_type(f, by_geometry = FALSE)), "POINT")
  expect_named(f, c("SurveyAreaIdentifier", "latitude", "longitude", "survey_year", "survey_month", "survey_day", "geometry"))
  expect_equal(nrow(f), nrow(dplyr::distinct(dplyr::select(bcch, latitude, longitude, survey_year, survey_month, survey_day))))
  expect_equal(format(sf::st_crs(f)), "Canada_Albers_Equal_Area_Conic")
  expect_equal(unname(apply(X = apply(FUN = is.na, X = f, MARGIN = 1), FUN = unique, MARGIN = 1)), rep(FALSE, times = 7))
})

test_that("data_fmt() basic functionality with complete BMDE terra points", {
  expect_silent(f <- suppressMessages(data_fmt(terra::vect(bcch, crs = 4326))))
  expect_s4_class(f, "SpatVector")
  expect_equal(terra::geomtype(f), "points")
  expect_named(f, c("SurveyAreaIdentifier", "latitude", "longitude", "survey_year", "survey_month", "survey_day"))
  expect_equal(nrow(f), nrow(dplyr::distinct(dplyr::select(bcch, latitude, longitude, survey_year, survey_month, survey_day))))
  expect_equal(terra::crs(f) == terra::crs("epsg:102001"), TRUE)
  expect_equal(unname(apply(X = apply(FUN = is.na, X = terra::values(f), MARGIN = 1), FUN = unique, MARGIN = 1)), rep(FALSE, times = 6))
})

test_that("data_fmt() basic functionality with complete BMDE sf POLYGON", {
  expect_silent(f <- suppressMessages(data_fmt(sf::st_buffer(sf::st_as_sf(bcch, coords = c("longitude", "latitude"), crs = 4326), 500))))
  expect_s3_class(f, "sf")
  expect_equal(as.character(sf::st_geometry_type(f, by_geometry = FALSE)), "POLYGON")
  expect_named(f, c("SurveyAreaIdentifier", "latitude", "longitude", "survey_year", "survey_month", "survey_day", "geometry"))
  expect_equal(nrow(f), nrow(dplyr::distinct(dplyr::select(bcch, latitude, longitude, survey_year, survey_month, survey_day))))
  expect_equal(format(sf::st_crs(f)), "Canada_Albers_Equal_Area_Conic")
  expect_equal(unname(apply(X = apply(FUN = is.na, X = f, MARGIN = 1), FUN = unique, MARGIN = 1)), rep(FALSE, times = 7))
})

test_that("data_fmt() basic functionality with complete BMDE terra points", {
  expect_silent(f <- suppressMessages(data_fmt(terra::buffer(terra::vect(bcch, crs = 4326), 500))))
  expect_s4_class(f, "SpatVector")
  expect_equal(terra::geomtype(f), "polygons")
  expect_named(f, c("SurveyAreaIdentifier", "latitude", "longitude", "survey_year", "survey_month", "survey_day"))
  expect_equal(nrow(f), nrow(dplyr::distinct(dplyr::select(bcch, latitude, longitude, survey_year, survey_month, survey_day))))
  expect_equal(terra::crs(f) == terra::crs("epsg:102001"), TRUE)
  expect_equal(unname(apply(X = apply(FUN = is.na, X = terra::values(f), MARGIN = 1), FUN = unique, MARGIN = 1)), rep(FALSE, times = 6))
})

test_that("data_fmt() accepts alternate column names in all data input formats", {
  expect_silent(f_df <- suppressMessages(data_fmt(dplyr::rename(bcch, 
                                                                sites = SurveyAreaIdentifier, 
                                                                lat = latitude, 
                                                                lon = longitude, 
                                                                yr = survey_year, 
                                                                mth = survey_month, 
                                                                dy = survey_day),
                                                  site_name = "sites",
                                                  coord_lon = "lon",
                                                  coord_lat = "lat",
                                                  date_year = "yr",
                                                  date_month = "mth",
                                                  date_day = "dy",
                                                  crs = 4326)))
  expect_s3_class(f_df, "sf")
  expect_equal(as.character(sf::st_geometry_type(f_df, by_geometry = FALSE)), "POINT")
  expect_named(f_df, c("SurveyAreaIdentifier", "lat", "lon", "yr", "mth", "dy", "geometry"))
  expect_equal(nrow(f_df), nrow(dplyr::distinct(dplyr::select(bcch, latitude, longitude, survey_year, survey_month, survey_day))))
  expect_equal(format(sf::st_crs(f_df)), "Canada_Albers_Equal_Area_Conic")
  expect_equal(unname(apply(X = apply(FUN = is.na, X = f_df, MARGIN = 1), FUN = unique, MARGIN = 1)), rep(FALSE, times = 7))
  expect_equal(c(attr(f_df, "site_name"), attr(f_df, "coord_lon"), attr(f_df, "coord_lat"), attr(f_df, "date_year"), attr(f_df, "date_month"), attr(f_df, "date_day")), c("sites", "lon", "lat", "yr", "mth", "dy"))
  
  expect_warning(f_sf_pt <- suppressMessages(data_fmt(sf::st_as_sf(dplyr::rename(bcch, 
                                                                                sites = SurveyAreaIdentifier, 
                                                                                lat = latitude, 
                                                                                lon = longitude, 
                                                                                yr = survey_year, 
                                                                                mth = survey_month, 
                                                                                dy = survey_day),
                                                                  coords = c("lon", "lat"),
                                                                  crs = 4326),
                                                     site_name = "sites",
                                                     coord_lon = "lon",
                                                     coord_lat = "lat",
                                                     date_year = "yr",
                                                     date_month = "mth",
                                                     date_day = "dy")), "[Data Formatting] sf or terra object provided as well as a lat/lon column name. lat/lon will be derived from the spatial data within the sf/terra object and specified lat/lon column will be ignored.")
  expect_s3_class(f_sf_pt, "sf")
  expect_equal(as.character(sf::st_geometry_type(f_sf_pt, by_geometry = FALSE)), "POINT")
  expect_named(f_sf_pt, c("SurveyAreaIdentifier", "latitude", "longitude", "yr", "mth", "dy", "geometry"))
  expect_equal(nrow(f_sf_pt), nrow(dplyr::distinct(dplyr::select(bcch, latitude, longitude, survey_year, survey_month, survey_day))))
  expect_equal(format(sf::st_crs(f_sf_pt)), "Canada_Albers_Equal_Area_Conic")
  expect_equal(unname(apply(X = apply(FUN = is.na, X = f_sf_pt, MARGIN = 1), FUN = unique, MARGIN = 1)), rep(FALSE, times = 7))
  expect_equal(c(attr(f_sf_pt, "site_name"), attr(f_sf_pt, "coord_lon"), attr(f_sf_pt, "coord_lat"), attr(f_sf_pt, "date_year"), attr(f_sf_pt, "date_month"), attr(f_sf_pt, "date_day")), c("sites", "yr", "mth", "dy"))
  
  expect_warning(f_terra_pt <- suppressMessages(data_fmt(terra::vect(dplyr::rename(bcch, 
                                                                                   sites = SurveyAreaIdentifier, 
                                                                                   lat = latitude, 
                                                                                   lon = longitude, 
                                                                                   yr = survey_year, 
                                                                                   mth = survey_month, 
                                                                                   dy = survey_day),
                                                                     crs = "epsg:4326"),
                                                      site_name = "sites",
                                                      coord_lon = "lon",
                                                      coord_lat = "lat",
                                                      date_year = "yr",
                                                      date_month = "mth",
                                                      date_day = "dy")), "[Data Formatting] sf or terra object provided as well as a lat/lon column name. lat/lon will be derived from the spatial data within the sf/terra object and specified lat/lon column will be ignored.")
  expect_s4_class(f_terra_pt, "SpatVector")
  expect_equal(terra::geomtype(f_terra_pt), "points")
  expect_named(f_terra_pt, c("SurveyAreaIdentifier", "latitude", "longitude", "yr", "mth", "dy"))
  expect_equal(nrow(f_terra_pt), nrow(dplyr::distinct(dplyr::select(bcch, latitude, longitude, survey_year, survey_month, survey_day))))
  expect_equal(terra::crs(f_terra_pt) == terra::crs("epsg:102001"), TRUE)
  expect_equal(unname(apply(X = apply(FUN = is.na, X = terra::values(f_terra_pt), MARGIN = 1), FUN = unique, MARGIN = 1)), rep(FALSE, times = 6))
  expect_equal(c(attr(f_terra_pt, "site_name"), attr(f_terra_pt, "coord_lon"), attr(f_terra_pt, "coord_lat"), attr(f_terra_pt, "date_year"), attr(f_terra_pt, "date_month"), attr(f_terra_pt, "date_day")), c("sites", "yr", "mth", "dy"))
  
  expect_warning(f_sf_poly <- suppressMessages(data_fmt(sf::st_buffer(sf::st_as_sf(dplyr::rename(bcch, 
                                                                                                 sites = SurveyAreaIdentifier, 
                                                                                                 lat = latitude, 
                                                                                                 lon = longitude, 
                                                                                                 yr = survey_year, 
                                                                                                 mth = survey_month, 
                                                                                                 dy = survey_day),
                                                                                   coords = c("lon", "lat"),
                                                                                   crs = 4326,
                                                                                   remove = FALSE),
                                                                      500),
                                                      site_name = "sites",
                                                      coord_lon = "lon",
                                                      coord_lat = "lat",
                                                      date_year = "yr",
                                                      date_month = "mth",
                                                      date_day = "dy")), "[Data Formatting] sf or terra object provided as well as a lat/lon column name. lat/lon will be derived from the spatial data within the sf/terra object and specified lat/lon column will be ignored.")
  expect_s3_class(f_sf_poly, "sf")
  expect_equal(as.character(sf::st_geometry_type(f_sf_poly, by_geometry = FALSE)), "POLYGON")
  expect_named(f_sf_poly, c("SurveyAreaIdentifier", "latitude", "longitude", "yr", "mth", "dy", "geometry"))
  expect_equal(nrow(f_sf_poly), nrow(dplyr::distinct(dplyr::select(bcch, latitude, longitude, survey_year, survey_month, survey_day))))
  expect_equal(format(sf::st_crs(f_sf_poly)), "Canada_Albers_Equal_Area_Conic")
  expect_equal(unname(apply(X = apply(FUN = is.na, X = f_sf_poly, MARGIN = 1), FUN = unique, MARGIN = 1)), rep(FALSE, times = 7))
  expect_equal(c(attr(f_sf_poly, "site_name"), attr(f_sf_poly, "coord_lon"), attr(f_sf_poly, "coord_lat"), attr(f_sf_poly, "date_year"), attr(f_sf_poly, "date_month"), attr(f_sf_poly, "date_day")), c("sites", "yr", "mth", "dy"))
  
  expect_warning(f_terra_poly <- suppressMessages(data_fmt(terra::buffer(terra::vect(dplyr::rename(bcch, 
                                                                                                 sites = SurveyAreaIdentifier, 
                                                                                                 lat = latitude, 
                                                                                                 lon = longitude, 
                                                                                                 yr = survey_year, 
                                                                                                 mth = survey_month, 
                                                                                                 dy = survey_day),
                                                                                   crs = "epsg:4326"),
                                                                     500),
                                                         site_name = "sites",
                                                         coord_lon = "lon",
                                                         coord_lat = "lat",
                                                         date_year = "yr",
                                                         date_month = "mth",
                                                         date_day = "dy")), "[Data Formatting] sf or terra object provided as well as a lat/lon column name. lat/lon will be derived from the spatial data within the sf/terra object and specified lat/lon column will be ignored.")
  expect_s4_class(f_terra_poly, "SpatVector")
  expect_equal(terra::geomtype(f_terra_poly), "polygons")
  expect_named(f_terra_poly, c("SurveyAreaIdentifier", "latitude", "longitude", "yr", "mth", "dy"))
  expect_equal(nrow(f_terra_poly), nrow(dplyr::distinct(dplyr::select(bcch, latitude, longitude, survey_year, survey_month, survey_day))))
  expect_equal(terra::crs(f_terra_poly) == terra::crs("epsg:102001"), TRUE)
  expect_equal(unname(apply(X = apply(FUN = is.na, X = terra::values(f_terra_poly), MARGIN = 1), FUN = unique, MARGIN = 1)), rep(FALSE, times = 6))
  expect_equal(c(attr(f_terra_poly, "site_name"), attr(f_terra_poly, "coord_lon"), attr(f_terra_poly, "coord_lat"), attr(f_terra_poly, "date_year"), attr(f_terra_poly, "date_month"), attr(f_terra_poly, "date_day")), c("sites", "yr", "mth", "dy"))
})

test_that("Date conversion from lubridate works in all input formats", {
  expect_warning(f_df <- suppressMessages(data_fmt(dplyr::mutate(bcch, 
                                                                 date = as.Date(paste0(survey_year,
                                                                                       "-",
                                                                                       survey_month,
                                                                                       "-",
                                                                                       survey_day))),
                                                   date_lubridate = "date")), "[Data Formatting] as the 'crs' argument is not specified, data CRS is assumed to be EPSG:4326.")
  expect_s3_class(f_df, "sf")
  expect_equal(as.character(sf::st_geometry_type(f_df, by_geometry = FALSE)), "POINT")
  expect_named(f_df, c("SurveyAreaIdentifier", "latitude", "longitude", "date", "survey_year", "survey_month", "survey_day", "geometry"))
  expect_equal(nrow(f_df), nrow(dplyr::distinct(dplyr::select(bcch, latitude, longitude, survey_year, survey_month, survey_day))))
  expect_equal(format(sf::st_crs(f_df)), "Canada_Albers_Equal_Area_Conic")
  expect_equal(unname(apply(X = apply(FUN = is.na, X = f_df, MARGIN = 1), FUN = unique, MARGIN = 1)), rep(FALSE, times = 8))
  expect_equal(dplyr::all(f_df$date %in% dplyr::mutate(bcch, 
                                                       date = as.Date(paste0(survey_year,
                                                                             "-",
                                                                             survey_month,
                                                                             "-",
                                                                             survey_day)))$date), TRUE)
  expect_equal(f_df$survey_year, lubridate::year(f_df$date))
  expect_equal(f_df$survey_month, lubridate::month(f_df$date))
  expect_equal(f_df$survey_day, lubridate::day(f_df$date))
  expect_equal(attr(f_df, "date_lubridate"), "date")
  
})

test_that("Date conversion from ordinal works in all input formats", {})

test_that("Invalid input data formats return appropriate error", {})

test_that("Invalid alternate column names return appropriate error", {})

test_that("Invalid date data return appropriate errors", {})

test_that("Invalid CRSs return error", {})