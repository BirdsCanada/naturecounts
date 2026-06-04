if(!dir.exists("./testdir")) {
  dir.create("./testdir")
}

test_that("landcover_download() throws error when credentials are incorrectly supplied.", {
  expect_error(suppressWarnings(suppressMessages(landcover_download(data_fmt(bcch,
                                                                             coord_lon = "longitude",
                                                                             coord_lat = "latitude",
                                                                             crs = 4326),
                                                                    ed_transfer = TRUE,
                                                                    ed_email = "incorrect@birdscanada.org",
                                                                    dl_path = "./testdir"))),
               "\\[MODIS Landcover Download\\] EarthData password incorrect. Please verify that provided password is correct."
  )
})

test_that("landcover_download() hits API with all expected inputs. May fail if filename structure is changed server-side.", {
  expected_files <- c("MCD12Q1.A2001001.h12v04.061.2022146050354.hdf",
                      "MCD12Q1.A2002001.h12v04.061.2022147215712.hdf",
                      "MCD12Q1.A2003001.h12v04.061.2022151161906.hdf",
                      "MCD12Q1.A2004001.h12v04.061.2022152043252.hdf",
                      "MCD12Q1.A2005001.h12v04.061.2022152140219.hdf",
                      "MCD12Q1.A2006001.h12v04.061.2022202151028.hdf", 
                      "MCD12Q1.A2007001.h12v04.061.2022153233438.hdf", 
                      "MCD12Q1.A2008001.h12v04.061.2022158231134.hdf", 
                      "MCD12Q1.A2009001.h12v04.061.2022159194525.hdf",
                      "MCD12Q1.A2010001.h12v04.061.2022160081327.hdf",
                      "MCD12Q1.A2011001.h12v04.061.2022161144809.hdf",
                      "MCD12Q1.A2012001.h12v04.061.2022162035938.hdf",
                      "MCD12Q1.A2013001.h12v04.061.2022164182417.hdf",
                      "MCD12Q1.A2014001.h12v04.061.2022165083049.hdf",
                      "MCD12Q1.A2015001.h12v04.061.2022165230140.hdf",
                      "MCD12Q1.A2017001.h12v04.061.2022168033428.hdf")
  expect_equal(suppressWarnings(suppressMessages(landcover_download(data_fmt(bcch,
                                                            coord_lon = "longitude",
                                                            coord_lat = "latitude",
                                                            crs = 4326),
                                                   ed_transfer = FALSE))),
               expected_files)
  expect_equal(suppressWarnings(suppressMessages(landcover_download(data_buff(data_fmt(bcch,
                                                                      coord_lon = "longitude",
                                                                      coord_lat = "latitude",
                                                                      crs = 4326)),
                                                   ed_transfer = FALSE))),
               expected_files)
  expect_equal(suppressWarnings(suppressMessages(landcover_download(data_fmt(terra::vect(bcch,
                                                                        crs = "epsg:4326",
                                                                        geom = c("longitude", "latitude"))),
                                                   ed_transfer = FALSE))),
               expected_files)
  expect_equal(suppressWarnings(suppressMessages(landcover_download(data_buff(data_fmt(terra::vect(bcch,
                                                                                  crs = "epsg:4326",
                                                                                  geom = c("longitude", "latitude")))),
                                                   ed_transfer = FALSE))),
               expected_files)
})

test_that("landcover_download() successfully downloads requested files with a test user,
          and downloaded files contain all data years and cover all data areas.", {
  expected_files <- c("./testdir/modis/MCD12Q1/MCD12Q1.A2001001.h12v04.061.2022146050354.hdf",
                      "./testdir/modis/MCD12Q1/MCD12Q1.A2002001.h12v04.061.2022147215712.hdf",
                      "./testdir/modis/MCD12Q1/MCD12Q1.A2003001.h12v04.061.2022151161906.hdf",
                      "./testdir/modis/MCD12Q1/MCD12Q1.A2004001.h12v04.061.2022152043252.hdf",
                      "./testdir/modis/MCD12Q1/MCD12Q1.A2005001.h12v04.061.2022152140219.hdf",
                      "./testdir/modis/MCD12Q1/MCD12Q1.A2006001.h12v04.061.2022202151028.hdf",
                      "./testdir/modis/MCD12Q1/MCD12Q1.A2007001.h12v04.061.2022153233438.hdf",
                      "./testdir/modis/MCD12Q1/MCD12Q1.A2008001.h12v04.061.2022158231134.hdf",
                      "./testdir/modis/MCD12Q1/MCD12Q1.A2009001.h12v04.061.2022159194525.hdf",
                      "./testdir/modis/MCD12Q1/MCD12Q1.A2010001.h12v04.061.2022160081327.hdf",
                      "./testdir/modis/MCD12Q1/MCD12Q1.A2011001.h12v04.061.2022161144809.hdf",
                      "./testdir/modis/MCD12Q1/MCD12Q1.A2012001.h12v04.061.2022162035938.hdf",
                      "./testdir/modis/MCD12Q1/MCD12Q1.A2013001.h12v04.061.2022164182417.hdf",
                      "./testdir/modis/MCD12Q1/MCD12Q1.A2014001.h12v04.061.2022165083049.hdf",
                      "./testdir/modis/MCD12Q1/MCD12Q1.A2015001.h12v04.061.2022165230140.hdf",
                      "./testdir/modis/MCD12Q1/MCD12Q1.A2017001.h12v04.061.2022168033428.hdf")
  expect_equal(suppressWarnings(suppressMessages(landcover_download(data_fmt(bcch,
                                                            coord_lon = "longitude",
                                                            coord_lat = "latitude",
                                                            crs = 4326),
                                                   ed_email = "rmacklin@birdscanada.org",
                                                   dl_path = "./testdir"))),
               expected_files)
  expect_true(dir.exists("./testdir/modis/MCD12Q1")) # Bonus test of custom file path specification
  expect_equal(list.files("./testdir/modis/MCD12Q1", full.names = TRUE), expected_files)
  files_years <- luna::modisDate(list.files("./testdir/modis/MCD12Q1"))
  expect_true(all(bcch$survey_year[bcch$survey_year %in% 2001:((lubridate::year(Sys.Date()))-2)] %in% files_years$year))
  files_extent <- luna::modisExtent(list.files("./testdir/modis/MCD12Q1"))
  bcch_spatial <- bcch %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    sf::st_transform(terra::crs(terra::rast(list.files("./testdir/modis/MCD12Q1",
                                                       full.names = TRUE)[1]))) %>%
    sf::st_coordinates()
  expect_true(all(bcch_spatial[,"X"] <= unique(files_extent[,"xmax"])))
  expect_true(all(bcch_spatial[,"X"] >= unique(files_extent[,"xmin"])))
  expect_true(all(bcch_spatial[,"Y"] <= unique(files_extent[,"ymax"])))
  expect_true(all(bcch_spatial[,"Y"] >= unique(files_extent[,"ymin"])))
  })

test_that("landcover_download() succeeds with alternate column names, either
          passed through attributes or specified explicitly.", {
  expected_files <- c("MCD12Q1.A2001001.h12v04.061.2022146050354.hdf",
                      "MCD12Q1.A2002001.h12v04.061.2022147215712.hdf",
                      "MCD12Q1.A2003001.h12v04.061.2022151161906.hdf",
                      "MCD12Q1.A2004001.h12v04.061.2022152043252.hdf",
                      "MCD12Q1.A2005001.h12v04.061.2022152140219.hdf",
                      "MCD12Q1.A2006001.h12v04.061.2022202151028.hdf", 
                      "MCD12Q1.A2007001.h12v04.061.2022153233438.hdf", 
                      "MCD12Q1.A2008001.h12v04.061.2022158231134.hdf", 
                      "MCD12Q1.A2009001.h12v04.061.2022159194525.hdf",
                      "MCD12Q1.A2010001.h12v04.061.2022160081327.hdf",
                      "MCD12Q1.A2011001.h12v04.061.2022161144809.hdf",
                      "MCD12Q1.A2012001.h12v04.061.2022162035938.hdf",
                      "MCD12Q1.A2013001.h12v04.061.2022164182417.hdf",
                      "MCD12Q1.A2014001.h12v04.061.2022165083049.hdf",
                      "MCD12Q1.A2015001.h12v04.061.2022165230140.hdf",
                      "MCD12Q1.A2017001.h12v04.061.2022168033428.hdf")
  expect_equal(suppressWarnings(suppressMessages(landcover_download(data_fmt(dplyr::rename(bcch,
                                                                          "sites" = "SurveyAreaIdentifier",
                                                                          "yr" = "survey_year"),
                                                            coord_lon = "longitude",
                                                            coord_lat = "latitude",
                                                            site_name = "sites",
                                                            date_year = "yr",
                                                            crs = 4326),
                                                   ed_transfer = FALSE))),
               expected_files)
  expect_equal(suppressWarnings(suppressMessages(landcover_download(dplyr::rename(data_fmt(bcch,
                                                                          coord_lon = "longitude",
                                                                          coord_lat = "latitude",
                                                                          crs = 4326),
                                                                 "sites" = "SurveyAreaIdentifier",
                                                                 "yr" = "survey_year"),
                                                   site_name = "sites",
                                                   date_year = "yr",
                                                   ed_transfer = FALSE))),
               expected_files)
})

test_that("landcover_download() fetches necessary nearest years for out of coverage dates.", {
  
  outside_dates <- bcch[bcch$survey_year < 2001,]
  outside_dates$survey_year[1] <- lubridate::year(Sys.Date())
  
  expect_true(length(suppressWarnings(suppressMessages(landcover_download(data_fmt(outside_dates,
                                                                                   coord_lon = "longitude",
                                                                                   coord_lat = "latitude",
                                                                                   crs = 4326),
                                                                          ed_transfer = FALSE)))) == 2)
  
  expect_true("MCD12Q1.A2001001.h12v04.061.2022146050354.hdf" %in% suppressWarnings(suppressMessages(landcover_download(data_fmt(outside_dates,
                                                                                                                                 coord_lon = "longitude",
                                                                                                                                 coord_lat = "latitude",
                                                                                                                                 crs = 4326),
                                                                                                                        ed_transfer = FALSE))))
  
  expect_true(substr(suppressWarnings(suppressMessages(landcover_download(data_fmt(outside_dates,
                                                                                   coord_lon = "longitude",
                                                                                   coord_lat = "latitude",
                                                                                   crs = 4326),
                                                                          ed_transfer = FALSE)))[2], 
                     start = 1, stop = 13) %in% paste0("MCD12Q1.A", c(lubridate::year(Sys.Date()) - 2, lubridate::year(Sys.Date()) - 1)))
  
  expect_true(length(suppressWarnings(suppressMessages(landcover_download(data_fmt(outside_dates,
                                                                                   coord_lon = "longitude",
                                                                                   coord_lat = "latitude",
                                                                                   crs = 4326),
                                                                          ed_transfer = TRUE,
                                                                          ed_email = "rmacklin@birdscanada.org",
                                                                          dl_path = "./testdir")))) == 2)
  
  expect_true("./testdir/modis/MCD12Q1/MCD12Q1.A2001001.h12v04.061.2022146050354.hdf" %in% suppressWarnings(suppressMessages(landcover_download(data_fmt(outside_dates,
                                                                                                                                 coord_lon = "longitude",
                                                                                                                                 coord_lat = "latitude",
                                                                                                                                 crs = 4326),
                                                                                                                        ed_transfer = TRUE,
                                                                                                                        ed_email = "rmacklin@birdscanada.org",
                                                                                                                        dl_path = "./testdir"))))
  
  expect_true(substr(suppressWarnings(suppressMessages(landcover_download(data_fmt(outside_dates,
                                                                                   coord_lon = "longitude",
                                                                                   coord_lat = "latitude",
                                                                                   crs = 4326),
                                                                          ed_transfer = TRUE,
                                                                          ed_email = "rmacklin@birdscanada.org",
                                                                          dl_path = "./testdir")))[2], 
                     start = 1, stop = 37) %in% paste0("./testdir/modis/MCD12Q1/MCD12Q1.A", c(lubridate::year(Sys.Date()) - 2, lubridate::year(Sys.Date()) - 1)))
})

test_that("landcover_download() returns appropriate warnings for out of coverage dates.", {
  before <- bcch[bcch$survey_year < 2001,]
  
  after <- bcch[bcch$survey_year > 2001,]
  after$survey_year[1] <- lubridate::year(Sys.Date())
  
  both <- bcch[bcch$survey_year < 2001,]
  both$survey_year[1] <- lubridate::year(Sys.Date())
  
  expect_warning(suppressMessages(landcover_download(data_fmt(before,
                                                              coord_lon = "longitude",
                                                              coord_lat = "latitude",
                                                              crs = 4326),
                                                     ed_transfer =  FALSE)),
                 "\\[MODIS Landcover Download\\] MODIS landcover data unavailable for all years before 2001. landcover_extract\\(\\) will extract landcover data from 2001 for these observations.")
  
  expect_warning(suppressMessages(landcover_download(data_fmt(before,
                                                              coord_lon = "longitude",
                                                              coord_lat = "latitude",
                                                              crs = 4326),
                                                     ed_transfer =  TRUE,
                                                     ed_email = "rmacklin@birdscanada.org")),
                 "\\[MODIS Landcover Download\\] MODIS landcover data unavailable for all years before 2001. landcover_extract\\(\\) will extract landcover data from 2001 for these observations.")
  
  expect_warning(suppressMessages(landcover_download(data_fmt(after,
                                                              coord_lon = "longitude",
                                                              coord_lat = "latitude",
                                                              crs = 4326),
                                                     ed_transfer =  FALSE)),
                 paste0("\\[MODIS Landcover Download\\] MODIS landcover data unavailable for ", lubridate::year(Sys.Date()), ". landcover_extract\\(\\) will extract landcover data from the nearest available year for these observations."))
  
  expect_warning(suppressMessages(landcover_download(data_fmt(after,
                                                              coord_lon = "longitude",
                                                              coord_lat = "latitude",
                                                              crs = 4326),
                                                     ed_transfer = TRUE,
                                                     ed_email = "rmacklin@birdscanada.org",
                                                     dl_path = "./testdir")),
                 paste0("\\[MODIS Landcover Download\\] MODIS landcover data unavailable for ", lubridate::year(Sys.Date()), ". landcover_extract\\(\\) will extract landcover data from the nearest available year for these observations."))
  
  expect_warning(suppressMessages(landcover_download(data_fmt(both,
                                                              coord_lon = "longitude",
                                                              coord_lat = "latitude",
                                                              crs = 4326),
                                                     ed_transfer =  FALSE)),
                 paste0("\\[MODIS Landcover Download\\] MODIS landcover data unavailable for all years before 2001 as well as ", lubridate::year(Sys.Date()), ". landcover_extract\\(\\) will extract landcover data from 2001 or the nearest year for these observations."))
  
  expect_warning(suppressMessages(landcover_download(data_fmt(both,
                                                              coord_lon = "longitude",
                                                              coord_lat = "latitude",
                                                              crs = 4326),
                                                     ed_transfer = TRUE,
                                                     ed_email = "rmacklin@birdscanada.org",
                                                     dl_path = "./testdir")),
                 paste0("\\[MODIS Landcover Download\\] MODIS landcover data unavailable for all years before 2001 as well as ", lubridate::year(Sys.Date()), ". landcover_extract\\(\\) will extract landcover data from 2001 or the nearest year for these observations."))
})

test_that("landcover_extract() throws appropriate error when empty vector provided to landcover_files.", {
  expect_error(landcover_extract(suppressWarnings(suppressMessages(data_fmt(bcch))),
                                 landcover_files = c()),
               "\\[MODIS Landcover Extraction\\] no landcover files provided to extract from. Please provide a vector containing filepaths of all necessary MODIS files for your data. Data can be downloaded using landcover_download\\(\\).")
  
})

test_that("landcover_extract() basic functionality with all expected data inputs.", {
  sf_pt <- suppressWarnings(suppressMessages(data_fmt(bcch)))
  sf_poly <- suppressWarnings(suppressMessages(data_buff(data_fmt(bcch))))
  terra_pt <- terra::vect(sf_pt)
  terra_poly <- terra::vect(sf_poly)
  
  expect_silent(extracted <- suppressWarnings(suppressMessages(landcover_extract(
    sf_pt,
    landcover_files = list.files("./testdir/modis/MCD12Q1", 
                                 full.names = TRUE)))))
  expect_s3_class(extracted, "sf")
  expect_equal(as.character(sf::st_geometry_type(extracted, by_geometry = FALSE)), "POINT")
  expect_named(extracted, c("SurveyAreaIdentifier", "latitude", "longitude", "survey_year", "survey_month", "survey_day", "geometry", "LC_Type1_Class"))
  expect_equal(dplyr::select(extracted, -"LC_Type1_Class"), sf_pt, ignore_attr = TRUE) # Ignores attributes to confirm that data has not been otherwise modified.
  expect_equal(format(sf::st_crs(extracted)), "Canada_Albers_Equal_Area_Conic")
  expect_equal(unname(apply(X = apply(FUN = is.na, X = extracted, MARGIN = 1), FUN = unique, MARGIN = 1)), rep(FALSE, times = 8))

  expect_silent(extracted <- suppressWarnings(suppressMessages(landcover_extract(
    sf_poly,
    landcover_files = list.files("./testdir/modis/MCD12Q1", 
                                 full.names = TRUE)))))
  expect_s3_class(extracted, "sf")
  expect_equal(as.character(sf::st_geometry_type(extracted, by_geometry = FALSE)), "POLYGON")
  expect_named(extracted, c("SurveyAreaIdentifier", "latitude", "longitude",
                            "survey_year", "survey_month", "survey_day",
                            "geometry", "LC_Type1_evergreen_needleleaf_forests",
                            "LC_Type1_evergreen_broadleaf_forests", 
                            "LC_Type1_decidious_needleleaf_forests", 
                            "LC_Type1_deciduous_broadleaf_forests", 
                            "LC_Type1_mixed_forests", 
                            "LC_Type1_closed_shrublands", 
                            "LC_Type1_open_shrublands", 
                            "LC_Type1_woody_savannas", 
                            "LC_Type1_savannas", 
                            "LC_Type1_grasslands", 
                            "LC_Type1_permanent_wetlands", 
                            "LC_Type1_croplands", 
                            "LC_Type1_urban_builtup_lands", 
                            "LC_Type1_cropland_natural_vegetation_mosaic", 
                            "LC_Type1_permanent_snow_ice", "LC_Type1_barren", 
                            "LC_Type1_water_bodies", "LC_Type1_unclassified"))
  expect_equal(dplyr::select(extracted, -tidyselect::starts_with("LC_Type1")), sf_poly, ignore_attr = TRUE) # Ignores attributes to confirm that data has not been otherwise modified.
  expect_equal(format(sf::st_crs(extracted)), "Canada_Albers_Equal_Area_Conic")
  expect_equal(unname(apply(X = apply(FUN = is.na, X = extracted, MARGIN = 1), FUN = unique, MARGIN = 1)), rep(FALSE, times = 25))
  expect_equal(unique(round(rowSums(sf::st_drop_geometry(dplyr::select(extracted, tidyselect::starts_with("LC_Type1")))),0)), 100) # Check that all rows sum to 100%
  
  expect_silent(extracted <- suppressWarnings(suppressMessages(landcover_extract(
    terra_pt,
    landcover_files = list.files("./testdir/modis/MCD12Q1", 
                                 full.names = TRUE)))))
  expect_s3_class(extracted, "sf")
  expect_equal(as.character(sf::st_geometry_type(extracted, by_geometry = FALSE)), "POINT")
  expect_named(extracted, c("SurveyAreaIdentifier", "latitude", "longitude", "survey_year", "survey_month", "survey_day", "geometry", "LC_Type1_Class"))
  expect_equal(dplyr::select(extracted, -"LC_Type1_Class"), sf_pt, ignore_attr = TRUE) # Ignores attributes to confirm that data has not been otherwise modified.
  expect_equal(format(sf::st_crs(extracted)), "Canada_Albers_Equal_Area_Conic")
  expect_equal(unname(apply(X = apply(FUN = is.na, X = extracted, MARGIN = 1), FUN = unique, MARGIN = 1)), rep(FALSE, times = 8))
  
  expect_silent(extracted <- suppressWarnings(suppressMessages(landcover_extract(
    terra_poly,
    landcover_files = list.files("./testdir/modis/MCD12Q1", 
                                 full.names = TRUE)))))
  expect_s3_class(extracted, "sf")
  expect_equal(as.character(sf::st_geometry_type(extracted, by_geometry = FALSE)), "POLYGON")
  expect_named(extracted, c("SurveyAreaIdentifier", "latitude", "longitude",
                            "survey_year", "survey_month", "survey_day",
                            "geometry", "LC_Type1_evergreen_needleleaf_forests",
                            "LC_Type1_evergreen_broadleaf_forests", 
                            "LC_Type1_decidious_needleleaf_forests", 
                            "LC_Type1_deciduous_broadleaf_forests", 
                            "LC_Type1_mixed_forests", 
                            "LC_Type1_closed_shrublands", 
                            "LC_Type1_open_shrublands", 
                            "LC_Type1_woody_savannas", 
                            "LC_Type1_savannas", 
                            "LC_Type1_grasslands", 
                            "LC_Type1_permanent_wetlands", 
                            "LC_Type1_croplands", 
                            "LC_Type1_urban_builtup_lands", 
                            "LC_Type1_cropland_natural_vegetation_mosaic", 
                            "LC_Type1_permanent_snow_ice", "LC_Type1_barren", 
                            "LC_Type1_water_bodies", "LC_Type1_unclassified"))
  expect_equal(dplyr::select(extracted, -tidyselect::starts_with("LC_Type1")), sf_poly, ignore_attr = TRUE) # Ignores attributes to confirm that data has not been otherwise modified.
  expect_equal(format(sf::st_crs(extracted)), "Canada_Albers_Equal_Area_Conic")
  expect_equal(unname(apply(X = apply(FUN = is.na, X = extracted, MARGIN = 1), FUN = unique, MARGIN = 1)), rep(FALSE, times = 25))
  expect_equal(unique(round(rowSums(sf::st_drop_geometry(dplyr::select(extracted, tidyselect::starts_with("LC_Type1")))),0)), 100) # Check that all rows sum to 100%
})


test_that("landcover_extract() succeeds with all landcover classification schema.", {
  sf_pt <- suppressWarnings(suppressMessages(data_fmt(bcch)))
  sf_poly <- suppressWarnings(suppressMessages(data_buff(data_fmt(bcch))))
  
  # Type 2
  expect_silent(extracted <- suppressWarnings(suppressMessages(landcover_extract(
    sf_pt,
    covariates = "modis_lctype2",
    landcover_files = list.files("./testdir/modis/MCD12Q1", 
                                 full.names = TRUE)))))
  expect_s3_class(extracted, "sf")
  expect_equal(as.character(sf::st_geometry_type(extracted, by_geometry = FALSE)), "POINT")
  expect_named(extracted, c("SurveyAreaIdentifier", "latitude", "longitude", "survey_year", "survey_month", "survey_day", "geometry", "LC_Type2_Class"))
  expect_equal(dplyr::select(extracted, -"LC_Type2_Class"), sf_pt, ignore_attr = TRUE) # Ignores attributes to confirm that data has not been otherwise modified.
  expect_equal(format(sf::st_crs(extracted)), "Canada_Albers_Equal_Area_Conic")
  expect_equal(unname(apply(X = apply(FUN = is.na, X = extracted, MARGIN = 1), FUN = unique, MARGIN = 1)), rep(FALSE, times = 8))
  
  expect_silent(extracted <- suppressWarnings(suppressMessages(landcover_extract(
    sf_poly,
    covariates = "modis_lctype2",
    landcover_files = list.files("./testdir/modis/MCD12Q1", 
                                 full.names = TRUE)))))
  expect_s3_class(extracted, "sf")
  expect_equal(as.character(sf::st_geometry_type(extracted, by_geometry = FALSE)), "POLYGON")
  expect_named(extracted, c("SurveyAreaIdentifier", "latitude", "longitude",
                            "survey_year", "survey_month", "survey_day", 
                            "geometry", "LC_Type2_water_bodies", 
                            "LC_Type2_evergreen_needleleaf_forests", 
                            "LC_Type2_evergreen_broadleaf_forests", 
                            "LC_Type2_deciduous_needleleaf_forests", 
                            "LC_Type2_deciduous_broadleaf_forests", 
                            "LC_Type2_mixed_forests", 
                            "LC_Type2_closed_shrublands", 
                            "LC_Type2_open_shrublands", 
                            "LC_Type2_woody_savannas", "LC_Type2_savannas", 
                            "LC_Type2_grasslands", "LC_Type2_permanent_wetlands", 
                            "LC_Type2_croplands", "LC_Type2_urban_builtup_lands",
                            "LC_Type2_cropland_natural_vegetation_mosaic", 
                            "LC_Type2_nonvegetated_lands", "LC_Type2_unclassified"))
  expect_equal(dplyr::select(extracted, -tidyselect::starts_with("LC_Type2")), sf_poly, ignore_attr = TRUE) # Ignores attributes to confirm that data has not been otherwise modified.
  expect_equal(format(sf::st_crs(extracted)), "Canada_Albers_Equal_Area_Conic")
  expect_equal(unname(apply(X = apply(FUN = is.na, X = extracted, MARGIN = 1), FUN = unique, MARGIN = 1)), rep(FALSE, times = 24))
  expect_equal(unique(round(rowSums(sf::st_drop_geometry(dplyr::select(extracted, tidyselect::starts_with("LC_Type2")))),0)), 100) # Check that all rows sum to 100%
  
  # Type 3
  expect_silent(extracted <- suppressWarnings(suppressMessages(landcover_extract(
    sf_pt,
    covariates = "modis_lctype3",
    landcover_files = list.files("./testdir/modis/MCD12Q1", 
                                 full.names = TRUE)))))
  expect_s3_class(extracted, "sf")
  expect_equal(as.character(sf::st_geometry_type(extracted, by_geometry = FALSE)), "POINT")
  expect_named(extracted, c("SurveyAreaIdentifier", "latitude", "longitude", "survey_year", "survey_month", "survey_day", "geometry", "LC_Type3_Class"))
  expect_equal(dplyr::select(extracted, -"LC_Type3_Class"), sf_pt, ignore_attr = TRUE) # Ignores attributes to confirm that data has not been otherwise modified.
  expect_equal(format(sf::st_crs(extracted)), "Canada_Albers_Equal_Area_Conic")
  expect_equal(unname(apply(X = apply(FUN = is.na, X = extracted, MARGIN = 1), FUN = unique, MARGIN = 1)), rep(FALSE, times = 8))
  
  expect_silent(extracted <- suppressWarnings(suppressMessages(landcover_extract(
    sf_poly,
    covariates = "modis_lctype3",
    landcover_files = list.files("./testdir/modis/MCD12Q1", 
                                 full.names = TRUE)))))
  expect_s3_class(extracted, "sf")
  expect_equal(as.character(sf::st_geometry_type(extracted, by_geometry = FALSE)), "POLYGON")
  expect_named(extracted, c("SurveyAreaIdentifier", "latitude", "longitude", 
                            "survey_year", "survey_month", "survey_day", 
                            "geometry", "LC_Type3_water_bodies", 
                            "LC_Type3_grasslands", "LC_Type3_shrublands", 
                            "LC_Type3_broadleaf_croplands", "LC_Type3_savannas",
                            "LC_Type3_evergreen_broadleaf_forests", 
                            "LC_Type3_deciduous_broadleaf_forests", 
                            "LC_Type3_evergreen_needleleaf_forests", 
                            "LC_Type3_deciduous_needleleaf_forests", 
                            "LC_Type3_nonvegetated_lands", 
                            "LC_Type3_urban_builtup_lands", 
                            "LC_Type3_unclassified"))
  expect_equal(dplyr::select(extracted, -tidyselect::starts_with("LC_Type3")), sf_poly, ignore_attr = TRUE) # Ignores attributes to confirm that data has not been otherwise modified.
  expect_equal(format(sf::st_crs(extracted)), "Canada_Albers_Equal_Area_Conic")
  expect_equal(unname(apply(X = apply(FUN = is.na, X = extracted, MARGIN = 1), FUN = unique, MARGIN = 1)), rep(FALSE, times = 19))
  expect_equal(unique(round(rowSums(sf::st_drop_geometry(dplyr::select(extracted, tidyselect::starts_with("LC_Type3")))),0)), 100) # Check that all rows sum to 100%
  
  # Type 4
  expect_silent(extracted <- suppressWarnings(suppressMessages(landcover_extract(
    sf_pt,
    covariates = "modis_lctype4",
    landcover_files = list.files("./testdir/modis/MCD12Q1", 
                                 full.names = TRUE)))))
  expect_s3_class(extracted, "sf")
  expect_equal(as.character(sf::st_geometry_type(extracted, by_geometry = FALSE)), "POINT")
  expect_named(extracted, c("SurveyAreaIdentifier", "latitude", "longitude", "survey_year", "survey_month", "survey_day", "geometry", "LC_Type4_Class"))
  expect_equal(dplyr::select(extracted, -"LC_Type4_Class"), sf_pt, ignore_attr = TRUE) # Ignores attributes to confirm that data has not been otherwise modified.
  expect_equal(format(sf::st_crs(extracted)), "Canada_Albers_Equal_Area_Conic")
  expect_equal(unname(apply(X = apply(FUN = is.na, X = extracted, MARGIN = 1), FUN = unique, MARGIN = 1)), rep(FALSE, times = 8))
  
  expect_silent(extracted <- suppressWarnings(suppressMessages(landcover_extract(
    sf_poly,
    covariates = "modis_lctype4",
    landcover_files = list.files("./testdir/modis/MCD12Q1", 
                                 full.names = TRUE)))))
  expect_s3_class(extracted, "sf")
  expect_equal(as.character(sf::st_geometry_type(extracted, by_geometry = FALSE)), "POLYGON")
  expect_named(extracted, c("SurveyAreaIdentifier", "latitude", "longitude", 
                            "survey_year", "survey_month", "survey_day", 
                            "geometry", "LC_Type4_water_bodies", 
                            "LC_Type4_evergreen_needleleaf_vegetation", 
                            "LC_Type4_evergreen_broadleaf_vegetation", 
                            "LC_Type4_deciduous_needleleaf_vegetation", 
                            "LC_Type4_deciduous_broadleaf_vegetation", 
                            "LC_Type4_annual_broadleaf_vegetation", 
                            "LC_Type4_annual_grass_vegetation", 
                            "LC_Type4_nonvegetated_lands", 
                            "LC_Type4_urban_builtup_lands", 
                            "LC_Type4_unclassified"))
  expect_equal(dplyr::select(extracted, -tidyselect::starts_with("LC_Type4")), sf_poly, ignore_attr = TRUE) # Ignores attributes to confirm that data has not been otherwise modified.
  expect_equal(format(sf::st_crs(extracted)), "Canada_Albers_Equal_Area_Conic")
  expect_equal(unname(apply(X = apply(FUN = is.na, X = extracted, MARGIN = 1), FUN = unique, MARGIN = 1)), rep(FALSE, times = 17))
  expect_equal(unique(round(rowSums(sf::st_drop_geometry(dplyr::select(extracted, tidyselect::starts_with("LC_Type4")))),0)), 100) # Check that all rows sum to 100%
  
  # Type 5
  expect_silent(extracted <- suppressWarnings(suppressMessages(landcover_extract(
    sf_pt,
    covariates = "modis_lctype5",
    landcover_files = list.files("./testdir/modis/MCD12Q1", 
                                 full.names = TRUE)))))
  expect_s3_class(extracted, "sf")
  expect_equal(as.character(sf::st_geometry_type(extracted, by_geometry = FALSE)), "POINT")
  expect_named(extracted, c("SurveyAreaIdentifier", "latitude", "longitude", "survey_year", "survey_month", "survey_day", "geometry", "LC_Type5_Class"))
  expect_equal(dplyr::select(extracted, -"LC_Type5_Class"), sf_pt, ignore_attr = TRUE) # Ignores attributes to confirm that data has not been otherwise modified.
  expect_equal(format(sf::st_crs(extracted)), "Canada_Albers_Equal_Area_Conic")
  expect_equal(unname(apply(X = apply(FUN = is.na, X = extracted, MARGIN = 1), FUN = unique, MARGIN = 1)), rep(FALSE, times = 8))
  
  expect_silent(extracted <- suppressWarnings(suppressMessages(landcover_extract(
    sf_poly,
    covariates = "modis_lctype5",
    landcover_files = list.files("./testdir/modis/MCD12Q1", 
                                 full.names = TRUE)))))
  expect_s3_class(extracted, "sf")
  expect_equal(as.character(sf::st_geometry_type(extracted, by_geometry = FALSE)), "POLYGON")
  expect_named(extracted, c("SurveyAreaIdentifier", "latitude", "longitude", 
                            "survey_year", "survey_month", "survey_day", 
                            "geometry", "LC_Type5_water_bodies", 
                            "LC_Type5_evergreen_needleleaf_trees", 
                            "LC_Type5_evergreen_broadleaf_trees", 
                            "LC_Type5_deciduous_needleleaf_trees", 
                            "LC_Type5_deciduous_broadleaf_trees", 
                            "LC_Type5_shrub", "LC_Type5_grass", 
                            "LC_Type5_cereal_croplands", 
                            "LC_Type5_broadleaf_croplands", 
                            "LC_Type5_urban_builtup_lands", 
                            "LC_Type5_permanent_snow_ice", "LC_Type5_barren", 
                            "LC_Type5_unclassified"))
  expect_equal(dplyr::select(extracted, -tidyselect::starts_with("LC_Type5")), sf_poly, ignore_attr = TRUE) # Ignores attributes to confirm that data has not been otherwise modified.
  expect_equal(format(sf::st_crs(extracted)), "Canada_Albers_Equal_Area_Conic")
  expect_equal(unname(apply(X = apply(FUN = is.na, X = extracted, MARGIN = 1), FUN = unique, MARGIN = 1)), rep(FALSE, times = 20))
  expect_equal(unique(round(rowSums(sf::st_drop_geometry(dplyr::select(extracted, tidyselect::starts_with("LC_Type5")))),0)), 100) # Check that all rows sum to 100%
  })


test_that("landcover_extract() returns appropriate warnings for out of coverage points and dates.", {
  bcch_mod <- bcch
  bcch_mod <- dplyr::filter(bcch_mod, .data$survey_year %in% 2005:2015)
  bcch_mod$latitude[1] <- 80
  
  sf_pt <- suppressWarnings(suppressMessages(data_fmt(bcch_mod)))
  sf_poly <- suppressWarnings(suppressMessages(data_buff(data_fmt(bcch_mod))))
  
  expect_warning(extracted <- suppressMessages(landcover_extract(
    sf_pt,
    landcover_files = list.files("./testdir/modis/MCD12Q1", 
                                 full.names = TRUE))),
    "\\[MODIS Landcover Extraction\\] site FilledSurveyArea1 falls outside of the spatial extent of the MODIS files provided. No value will be assigned.")
  expect_true(is.na(extracted$LC_Type1_Class[1]))
  
  expect_warning(extracted <- suppressMessages(landcover_extract(
    sf_poly,
    landcover_files = list.files("./testdir/modis/MCD12Q1", 
                                 full.names = TRUE))),
    "\\[MODIS Landcover Extraction\\] site FilledSurveyArea1 falls outside of the spatial extent of the MODIS files provided. No value will be assigned.")
  expect_true(all(c(unname(is.na(sf::st_drop_geometry(dplyr::select(extracted, starts_with("LC_Type1")))[1,])))))
  
  bcch_mod <- bcch
  bcch_mod <- dplyr::filter(bcch_mod, .data$survey_year %in% 2005:2015)
  bcch_mod$survey_year[1] <- 1998
  
  sf_pt <- suppressWarnings(suppressMessages(data_fmt(bcch_mod)))
  sf_poly <- suppressWarnings(suppressMessages(data_buff(data_fmt(bcch_mod))))
  
  expect_warning(extracted <- suppressMessages(landcover_extract(
    sf_pt,
    landcover_files = list.files("./testdir/modis/MCD12Q1", 
                                 full.names = TRUE))),
    "\\[MODIS Landcover Extraction\\]: MODIS data not available for 1998 - using data from nearest year\\(s\\) \\(2001\\).")
  expect_true(extracted$LC_Type1_Class[1] == "mixed_forests")
  
  expect_warning(extracted <- suppressMessages(landcover_extract(
    sf_poly,
    landcover_files = list.files("./testdir/modis/MCD12Q1", 
                                 full.names = TRUE))),
    "\\[MODIS Landcover Extraction\\]: MODIS data not available for 1998 - using data from nearest year\\(s\\) \\(2001\\).")
  expect_true(all(round(unlist(unname(sf::st_drop_geometry(dplyr::select(extracted, starts_with("LC_Type1")))[1,])), 2) == c(0,0,0,50,50,0,0,0,0,0,0,0,0,0,0,0,0,0)))
  })

test_that("landcover_extract() succeeds with alternate column names, either passed through attributes or specified explicitly.", {
  sf_pt <- suppressMessages(data_fmt(dplyr::rename(bcch,
                                  "sites" = "SurveyAreaIdentifier",
                                  "yr" = "survey_year"),
                    coord_lon = "longitude",
                    coord_lat = "latitude",
                    site_name = "sites",
                    date_year = "yr",
                    crs = 4326))
  
  expect_silent(extracted <- suppressWarnings(suppressMessages(landcover_extract(sf_pt,
                                                                                 landcover_files = list.files("./testdir/modis/MCD12Q1", 
                                                                                                              full.names = TRUE)))))
  expect_s3_class(extracted, "sf")
  expect_equal(as.character(sf::st_geometry_type(extracted, by_geometry = FALSE)), "POINT")
  expect_named(extracted, c("sites", "latitude", "longitude", "yr", "survey_month", "survey_day", "geometry", "LC_Type1_Class"))
  expect_equal(dplyr::select(extracted, -"LC_Type1_Class"), sf_pt, ignore_attr = TRUE) # Ignores attributes to confirm that data has not been otherwise modified.
  expect_equal(format(sf::st_crs(extracted)), "Canada_Albers_Equal_Area_Conic")
  expect_equal(unname(apply(X = apply(FUN = is.na, X = extracted, MARGIN = 1), FUN = unique, MARGIN = 1)), rep(FALSE, times = 8))
  
  sf_pt <- dplyr::rename(suppressMessages(data_fmt(bcch,
                                  coord_lon = "longitude",
                                  coord_lat = "latitude",
                                  crs = 4326)),
                         "sites" = "SurveyAreaIdentifier",
                         "yr" = "survey_year")
  
  expect_silent(extracted <- suppressWarnings(suppressMessages(landcover_extract(sf_pt,
                                                                                 landcover_files = list.files("./testdir/modis/MCD12Q1", 
                                                                                                              full.names = TRUE),
                                                                                 site_name = "sites",
                                                                                 date_year = "yr"))))
  expect_s3_class(extracted, "sf")
  expect_equal(as.character(sf::st_geometry_type(extracted, by_geometry = FALSE)), "POINT")
  expect_named(extracted, c("sites", "latitude", "longitude", "yr", "survey_month", "survey_day", "geometry", "LC_Type1_Class"))
  expect_equal(dplyr::select(extracted, -"LC_Type1_Class"), sf_pt, ignore_attr = TRUE) # Ignores attributes to confirm that data has not been otherwise modified.
  expect_equal(format(sf::st_crs(extracted)), "Canada_Albers_Equal_Area_Conic")
  expect_equal(unname(apply(X = apply(FUN = is.na, X = extracted, MARGIN = 1), FUN = unique, MARGIN = 1)), rep(FALSE, times = 8))
  })

unlink("./testdir", recursive = TRUE)