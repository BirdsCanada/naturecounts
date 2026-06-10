if(!dir.exists("./testdir")) {
  dir.create("./testdir")
}

test_that("worldclim_download() hits API with all expected inputs.", {
  expect_silent(tavg_sf_pt <<- suppressMessages(worldclim_download(data_fmt(bcch,
                                                                            coord_lon = "longitude",
                                                                            coord_lat = "latitude",
                                                                            crs = 4326),
                                                                   covariates = "worldclim_tavg",
                                                                   dl_path = "./testdir",
                                                                   progress = FALSE)))
  expect_silent(tavg_sf_poly <<- suppressMessages(worldclim_download(data_buff(data_fmt(bcch,
                                                                                        coord_lon = "longitude",
                                                                                        coord_lat = "latitude",
                                                                                        crs = 4326)),
                                                                     covariates = "worldclim_tavg",
                                                                     dl_path = "./testdir",
                                                                     progress = FALSE)))
  expect_silent(tavg_terra_pt <<- suppressMessages(worldclim_download(data_fmt(terra::vect(bcch,
                                                                                           crs = "epsg:4326",
                                                                                           geom = c("longitude", "latitude"))),
                                                                      covariates = "worldclim_tavg",
                                                                      dl_path = "./testdir",
                                                                      progress = FALSE)))
  expect_silent(tavg_terra_poly <<- suppressMessages(worldclim_download(data_buff(data_fmt(terra::vect(bcch,
                                                                                                       crs = "epsg:4326",
                                                                                                       geom = c("longitude", "latitude")))),
                                                                        covariates = "worldclim_tavg",
                                                                        dl_path = "./testdir",
                                                                        progress = FALSE)))
  expect_silent(tavg_countryname <<- suppressMessages(worldclim_download(countries = "Canada",
                                                                         covariates = "worldclim_tavg",
                                                                         dl_path = "./testdir",
                                                                         progress = FALSE)))
  expect_silent(tavg_countrycode <<- suppressMessages(worldclim_download(countries = "CAN",
                                                                         covariates = "worldclim_tavg",
                                                                         dl_path = "./testdir",
                                                                         progress = FALSE)))
  expect_silent(tavg_countries <<- suppressMessages(worldclim_download(countries = c("Canada", "MDG"),
                                                                       covariates = "worldclim_tavg",
                                                                       dl_path = "./testdir",
                                                                       progress = FALSE)))
  expect_silent(other_vars <<- suppressMessages(worldclim_download(countries = "MDG",
                                                                   covariates = c("worldclim_tmin", 
                                                                                  "worldclim_tmax", 
                                                                                  "worldclim_prec", 
                                                                                  "worldclim_srad", 
                                                                                  "worldclim_wind"),
                                                                   dl_path = "./testdir",
                                                                   progress = FALSE)))
})

test_that("Results downloaded from elevation_download() have expected features.", {
  expect_true(dir.exists("./testdir/worldclim")) # Bonus test of custom file path specification
  expect_true(all(file.size(list.files("./testdir/TerrainTiles/climate/wc2.1_country", full.names = TRUE)) > 100000)) # Check files contain a reasonable amount of data

  expect_s4_class(tavg_sf_pt, "SpatRaster")
  expect_s4_class(tavg_sf_poly, "SpatRaster")
  expect_s4_class(tavg_terra_pt, "SpatRaster")
  expect_s4_class(tavg_countryname, "SpatRaster")
  expect_s4_class(tavg_countrycode, "SpatRaster")
  expect_s4_class(tavg_countries, "SpatRaster")
  expect_true(inherits(other_vars, "list"))
  expect_s4_class(other_vars$tmin, "SpatRaster")
  expect_s4_class(other_vars$tmax, "SpatRaster")
  expect_s4_class(other_vars$prec, "SpatRaster")
  expect_s4_class(other_vars$srad, "SpatRaster")
  expect_s4_class(other_vars$wind, "SpatRaster")

  # expect_true(terra::is.related(elev_sf_pt,
  #                               terra::vect(suppressMessages(data_fmt(bcch,
  #                                                                     coord_lon = "longitude",
  #                                                                     coord_lat = "latitude",
  #                                                                     crs = 4326))),
  #                               "contains"))
  # expect_true(terra::is.related(elev_sf_poly,
  #                               terra::vect(suppressMessages(data_buff(data_fmt(bcch,
  #                                                                               coord_lon = "longitude",
  #                                                                               coord_lat = "latitude",
  #                                                                               crs = 4326)))),
  #                               "contains"))
  # expect_true(terra::is.related(elev_terra_pt,
  #                               suppressMessages(data_fmt(terra::vect(bcch,
  #                                                                     crs = "epsg:4326",
  #                                                                     geom = c("longitude", "latitude")))),
  #                               "contains"))
  # expect_true(terra::is.related(elev_terra_poly,
  #                               suppressMessages(data_buff(data_fmt(terra::vect(bcch,
  #                                                                               crs = "epsg:4326",
  #                                                                               geom = c("longitude", "latitude"))))),
  #                               "contains"))
})

unlink("./testdir", recursive = TRUE)

# test_that("elevation_download() succeeds with alternate column names, either
#           passed through attributes or specified explicitly.", {
#             expect_silent(suppressWarnings(suppressMessages(elevation_download(data_fmt(dplyr::rename(bcch,
#                                                                                                       "sites" = "SurveyAreaIdentifier",
#                                                                                                       "yr" = "survey_year"),
#                                                                                         coord_lon = "longitude",
#                                                                                         coord_lat = "latitude",
#                                                                                         site_name = "sites",
#                                                                                         date_year = "yr",
#                                                                                         crs = 4326)))))
#             expect_silent(suppressWarnings(suppressMessages(elevation_download(dplyr::rename(data_fmt(bcch,
#                                                                                                       coord_lon = "longitude",
#                                                                                                       coord_lat = "latitude",
#                                                                                                       crs = 4326),
#                                                                                              "sites" = "SurveyAreaIdentifier",
#                                                                                              "yr" = "survey_year"),
#                                                                                site_name = "sites"))))
#           })
# 
# test_that("elevation_extract() throws appropriate error when inappropriate file provided to elevation_data or argument is missing.", {
#   expect_error(elevation_extract(suppressWarnings(suppressMessages(data_fmt(bcch)))),
#                "\\[Elevation Extraction\\] no elevation data provided to extract from. Please provide a terra SpatRaster containing the necessary elevation data. Elevation data can be downloaded using elevation_download\\(\\).")
#   
#   expect_error(elevation_extract(suppressWarnings(suppressMessages(data_fmt(bcch))),
#                                  elevation_data = suppressWarnings(suppressMessages(data_fmt(bcch)))),
#                "\\[Elevation Extraction\\] data provided to elevation_data argument is not a SpatRaster. Please provide a terra SpatRaster containing the necessary elevation data. Elevation data can be downloaded using elevation_download\\(\\).")
# })
# 
# test_that("elevation_extract() basic functionality with all expected data inputs.", {
#   sf_pt <- suppressWarnings(suppressMessages(data_fmt(bcch)))
#   sf_poly <- suppressWarnings(suppressMessages(data_buff(data_fmt(bcch))))
#   terra_pt <- terra::vect(sf_pt)
#   terra_poly <- terra::vect(sf_poly)
#   
#   expect_silent(extracted <- suppressWarnings(suppressMessages(elevation_extract(
#     sf_pt,
#     elevation_data = elev_sf_pt))))
#   expect_s3_class(extracted, "sf")
#   expect_equal(as.character(sf::st_geometry_type(extracted, by_geometry = FALSE)), "POINT")
#   expect_named(extracted, c("SurveyAreaIdentifier", "latitude", "longitude", "survey_year", "survey_month", "survey_day", "geometry", "elevation"))
#   expect_true(inherits(extracted$elevation, "numeric"))
#   expect_equal(dplyr::select(extracted, -"elevation"), sf_pt, ignore_attr = TRUE) # Ignores attributes to confirm that data has not been otherwise modified.
#   expect_equal(format(sf::st_crs(extracted)), "Canada_Albers_Equal_Area_Conic")
#   expect_equal(unname(apply(X = apply(FUN = is.na, X = extracted, MARGIN = 1), FUN = unique, MARGIN = 1)), rep(FALSE, times = 8))
#   
#   expect_silent(extracted <- suppressWarnings(suppressMessages(elevation_extract(
#     sf_poly,
#     elevation_data = elev_sf_poly))))
#   expect_s3_class(extracted, "sf")
#   expect_equal(as.character(sf::st_geometry_type(extracted, by_geometry = FALSE)), "POLYGON")
#   expect_named(extracted, c("SurveyAreaIdentifier", "latitude", "longitude", "survey_year", "survey_month", "survey_day", "geometry", "elevation"))
#   expect_true(inherits(extracted$elevation, "numeric"))
#   expect_equal(dplyr::select(extracted, -"elevation"), sf_poly, ignore_attr = TRUE) # Ignores attributes to confirm that data has not been otherwise modified.
#   expect_equal(format(sf::st_crs(extracted)), "Canada_Albers_Equal_Area_Conic")
#   expect_equal(unname(apply(X = apply(FUN = is.na, X = extracted, MARGIN = 1), FUN = unique, MARGIN = 1)), rep(FALSE, times = 8))
#   
#   expect_silent(extracted <- suppressWarnings(suppressMessages(elevation_extract(
#     terra_pt,
#     elevation_data = elev_terra_pt))))
#   expect_s3_class(extracted, "sf")
#   expect_equal(as.character(sf::st_geometry_type(extracted, by_geometry = FALSE)), "POINT")
#   expect_named(extracted, c("SurveyAreaIdentifier", "latitude", "longitude", "survey_year", "survey_month", "survey_day", "geometry", "elevation"))
#   expect_true(inherits(extracted$elevation, "numeric"))
#   expect_equal(dplyr::select(extracted, -"elevation"), sf_pt, ignore_attr = TRUE) # Ignores attributes to confirm that data has not been otherwise modified.
#   expect_equal(format(sf::st_crs(extracted)), "Canada_Albers_Equal_Area_Conic")
#   expect_equal(unname(apply(X = apply(FUN = is.na, X = extracted, MARGIN = 1), FUN = unique, MARGIN = 1)), rep(FALSE, times = 8))
#   
#   expect_silent(extracted <- suppressWarnings(suppressMessages(elevation_extract(
#     terra_poly,
#     elevation_data = elev_terra_poly))))
#   expect_s3_class(extracted, "sf")
#   expect_equal(as.character(sf::st_geometry_type(extracted, by_geometry = FALSE)), "POLYGON")
#   expect_named(extracted, c("SurveyAreaIdentifier", "latitude", "longitude", "survey_year", "survey_month", "survey_day", "geometry", "elevation"))
#   expect_true(inherits(extracted$elevation, "numeric"))
#   expect_equal(dplyr::select(extracted, -"elevation"), sf_poly, ignore_attr = TRUE) # Ignores attributes to confirm that data has not been otherwise modified.
#   expect_equal(format(sf::st_crs(extracted)), "Canada_Albers_Equal_Area_Conic")
#   expect_equal(unname(apply(X = apply(FUN = is.na, X = extracted, MARGIN = 1), FUN = unique, MARGIN = 1)), rep(FALSE, times = 8))
#   
# })
# 
# test_that("elevation_extract() succeeds with alternate column names, either passed through attributes or specified explicitly.", {
#   sf_pt <- suppressMessages(data_fmt(dplyr::rename(bcch,
#                                                    "sites" = "SurveyAreaIdentifier",
#                                                    "yr" = "survey_year"),
#                                      coord_lon = "longitude",
#                                      coord_lat = "latitude",
#                                      site_name = "sites",
#                                      date_year = "yr",
#                                      crs = 4326))
#   
#   expect_silent(extracted <- suppressWarnings(suppressMessages(elevation_extract(sf_pt,
#                                                                                  elevation_data = elev_sf_pt))))
#   expect_s3_class(extracted, "sf")
#   expect_equal(as.character(sf::st_geometry_type(extracted, by_geometry = FALSE)), "POINT")
#   expect_named(extracted, c("sites", "latitude", "longitude", "yr", "survey_month", "survey_day", "geometry", "elevation"))
#   expect_true(inherits(extracted$elevation, "numeric"))
#   expect_equal(dplyr::select(extracted, -"elevation"), sf_pt, ignore_attr = TRUE) # Ignores attributes to confirm that data has not been otherwise modified.
#   expect_equal(format(sf::st_crs(extracted)), "Canada_Albers_Equal_Area_Conic")
#   expect_equal(unname(apply(X = apply(FUN = is.na, X = extracted, MARGIN = 1), FUN = unique, MARGIN = 1)), rep(FALSE, times = 8))
#   
#   sf_pt <- dplyr::rename(suppressMessages(data_fmt(bcch,
#                                                    coord_lon = "longitude",
#                                                    coord_lat = "latitude",
#                                                    crs = 4326)),
#                          "sites" = "SurveyAreaIdentifier",
#                          "yr" = "survey_year")
#   
#   expect_silent(extracted <- suppressWarnings(suppressMessages(elevation_extract(sf_pt,
#                                                                                  elevation_data = elev_sf_pt,
#                                                                                  site_name = "sites"))))
#   expect_s3_class(extracted, "sf")
#   expect_equal(as.character(sf::st_geometry_type(extracted, by_geometry = FALSE)), "POINT")
#   expect_named(extracted, c("sites", "latitude", "longitude", "yr", "survey_month", "survey_day", "geometry", "elevation"))
#   expect_true(inherits(extracted$elevation, "numeric"))
#   expect_equal(dplyr::select(extracted, -"elevation"), sf_pt, ignore_attr = TRUE) # Ignores attributes to confirm that data has not been otherwise modified.
#   expect_equal(format(sf::st_crs(extracted)), "Canada_Albers_Equal_Area_Conic")
#   expect_equal(unname(apply(X = apply(FUN = is.na, X = extracted, MARGIN = 1), FUN = unique, MARGIN = 1)), rep(FALSE, times = 8))
# })
# 
# test_that("elevation_extract() returns appropriate warnings for out of coverage points.", {
#   bcch_mod <- bcch
#   bcch_mod <- dplyr::filter(bcch_mod, .data$survey_year %in% 2005:2015)
#   bcch_mod$latitude[1] <- 80
#   
#   sf_pt <- suppressWarnings(suppressMessages(data_fmt(bcch_mod)))
#   sf_poly <- suppressWarnings(suppressMessages(data_buff(data_fmt(bcch_mod))))
#   
#   expect_warning(extracted <- suppressMessages(elevation_extract(
#     sf_pt,
#     elevation_data = elev_sf_pt)),
#     "\\[Elevation Extraction\\] site FilledSurveyArea1 falls outside of the spatial extent of the elevation rasters provided. No value will be returned.")
#   expect_true(is.na(extracted$elevation[1]))
#   
#   expect_warning(extracted <- suppressMessages(elevation_extract(
#     sf_poly,
#     elevation_data = elev_sf_poly)),
#     "\\[Elevation Extraction\\] site FilledSurveyArea1 falls outside of the spatial extent of the elevation rasters provided. No value will be returned.")
#   expect_true(is.na(extracted$elevation[1]))
#   
#   bcch_mod <- bcch
#   bcch_mod <- dplyr::filter(bcch_mod, .data$survey_year %in% 2005:2015)
#   bcch_mod$latitude[1] <- 47.25
#   
#   sf_pt <- suppressWarnings(suppressMessages(data_fmt(bcch_mod)))
#   sf_poly <- suppressWarnings(suppressMessages(data_buff(data_fmt(bcch_mod))))
#   
#   expect_warning(extracted <- suppressMessages(elevation_extract(
#     sf_pt,
#     elevation_data = elev_sf_pt)),
#     "\\[Elevation Extraction\\] site FilledSurveyArea1 falls outside of the spatial extent of the elevation rasters provided. No value will be returned.")
#   expect_true(is.na(extracted$elevation[1]))
#   
#   expect_warning(extracted <- suppressMessages(elevation_extract(
#     sf_poly,
#     elevation_data = elev_sf_poly)),
#     "\\[Elevation Extraction\\] site FilledSurveyArea1 falls outside of the spatial extent of the elevation rasters provided. No value will be returned.")
#   expect_true(is.na(extracted$elevation[1]))
#   
#   bcch_mod <- bcch
#   bcch_mod <- dplyr::filter(bcch_mod, .data$survey_year %in% 2005:2015)
#   bcch_mod$latitude[1] <- 47.045
#   
#   sf_poly <- suppressWarnings(suppressMessages(data_buff(data_fmt(bcch_mod),
#                                                          buffer_distance = 5,
#                                                          buffer_units = "km")))
#   
#   expect_warning(extracted <- suppressMessages(elevation_extract(
#     sf_poly,
#     elevation_data = elev_sf_poly)),
#     "\\[Elevation Extraction\\] site FilledSurveyArea1\\'s buffered area is only partially contained by the spatial extent of the elevation rasters provided. Returned mean elevation value will be derived from the available values.")
#   expect_true(inherits(extracted$elevation[1], "numeric"))
# })

#unlink("./testdir", recursive = TRUE)