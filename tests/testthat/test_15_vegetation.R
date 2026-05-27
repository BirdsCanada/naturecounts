if(!dir.exists("./testdir")) {
  dir.create("./testdir")
}

test_that("vegetation_download() hits API with all expected inputs. May fail if filename structure is changed server-side.", {
  expected_files <- c("MOD13A1.A2007017.h12v04.061.2021055141240.hdf", 
                      "MOD13A1.A2007033.h12v04.061.2021053004443.hdf", 
                      "MOD13A1.A2007049.h12v04.061.2021055160007.hdf", 
                      "MOD13A1.A2007065.h12v04.061.2021057175943.hdf", 
                      "MOD13A1.A2007081.h12v04.061.2021059114255.hdf", 
                      "MOD13A1.A2007097.h12v04.061.2021060063721.hdf", 
                      "MOD13A1.A2007113.h12v04.061.2021064210119.hdf", 
                      "MOD13A1.A2007129.h12v04.061.2021064213919.hdf", 
                      "MOD13A1.A2007145.h12v04.061.2021065143148.hdf", 
                      "MOD13A1.A2007161.h12v04.061.2021066014850.hdf", 
                      "MOD13A1.A2007177.h12v04.061.2021068094740.hdf", 
                      "MOD13A1.A2007193.h12v04.061.2021068160209.hdf", 
                      "MOD13A1.A2007209.h12v04.061.2021071042709.hdf", 
                      "MOD13A1.A2007225.h12v04.061.2021072224429.hdf", 
                      "MOD13A1.A2007241.h12v04.061.2021073192515.hdf", 
                      "MOD13A1.A2007257.h12v04.061.2021074191343.hdf", 
                      "MOD13A1.A2007273.h12v04.061.2021076110230.hdf", 
                      "MOD13A1.A2007289.h12v04.061.2021077181416.hdf", 
                      "MOD13A1.A2007305.h12v04.061.2021078235951.hdf", 
                      "MOD13A1.A2007321.h12v04.061.2021079181143.hdf", 
                      "MOD13A1.A2007337.h12v04.061.2021081224633.hdf", 
                      "MOD13A1.A2007353.h12v04.061.2021081232813.hdf")
  expect_equal(suppressMessages(vegetation_download(data_fmt(bcch[bcch$survey_year == 2007,],
                                                             coord_lon = "longitude",
                                                             coord_lat = "latitude",
                                                             crs = 4326),
                                                    ed_transfer = FALSE)),
               expected_files)
  expect_equal(suppressMessages(vegetation_download(data_buff(data_fmt(bcch[bcch$survey_year == 2007,],
                                                                       coord_lon = "longitude",
                                                                       coord_lat = "latitude",
                                                                       crs = 4326)),
                                                    ed_transfer = FALSE)),
               expected_files)
  expect_equal(suppressMessages(vegetation_download(data_fmt(terra::vect(bcch[bcch$survey_year == 2007,],
                                                                         crs = "epsg:4326")),
                                                    ed_transfer = FALSE)),
               expected_files)
  expect_equal(suppressMessages(vegetation_download(data_buff(data_fmt(terra::vect(bcch[bcch$survey_year == 2007,],
                                                                                   crs = "epsg:4326"))),
                                                    ed_transfer = FALSE)),
               expected_files)
})


test_that("vegetation_download() successfully downloads requested files with a test user,
          and downloaded files contain all data years and cover all data areas.", {
            expected_files <- c("./testdir/modis/MOD13A1/MOD13A1.A2006321.h12v04.061.2020277210544.hdf", 
                                "./testdir/modis/MOD13A1/MOD13A1.A2006337.h12v04.061.2020278214814.hdf", 
                                "./testdir/modis/MOD13A1/MOD13A1.A2006353.h12v04.061.2021050204908.hdf",
                                "./testdir/modis/MOD13A1/MOD13A1.A2007017.h12v04.061.2021055141240.hdf", 
                                "./testdir/modis/MOD13A1/MOD13A1.A2007033.h12v04.061.2021053004443.hdf", 
                                "./testdir/modis/MOD13A1/MOD13A1.A2007049.h12v04.061.2021055160007.hdf", 
                                "./testdir/modis/MOD13A1/MOD13A1.A2007065.h12v04.061.2021057175943.hdf", 
                                "./testdir/modis/MOD13A1/MOD13A1.A2007081.h12v04.061.2021059114255.hdf", 
                                "./testdir/modis/MOD13A1/MOD13A1.A2007097.h12v04.061.2021060063721.hdf", 
                                "./testdir/modis/MOD13A1/MOD13A1.A2007113.h12v04.061.2021064210119.hdf", 
                                "./testdir/modis/MOD13A1/MOD13A1.A2007129.h12v04.061.2021064213919.hdf", 
                                "./testdir/modis/MOD13A1/MOD13A1.A2007145.h12v04.061.2021065143148.hdf", 
                                "./testdir/modis/MOD13A1/MOD13A1.A2007161.h12v04.061.2021066014850.hdf", 
                                "./testdir/modis/MOD13A1/MOD13A1.A2007177.h12v04.061.2021068094740.hdf", 
                                "./testdir/modis/MOD13A1/MOD13A1.A2007193.h12v04.061.2021068160209.hdf", 
                                "./testdir/modis/MOD13A1/MOD13A1.A2007209.h12v04.061.2021071042709.hdf", 
                                "./testdir/modis/MOD13A1/MOD13A1.A2007225.h12v04.061.2021072224429.hdf", 
                                "./testdir/modis/MOD13A1/MOD13A1.A2007241.h12v04.061.2021073192515.hdf", 
                                "./testdir/modis/MOD13A1/MOD13A1.A2007257.h12v04.061.2021074191343.hdf", 
                                "./testdir/modis/MOD13A1/MOD13A1.A2007273.h12v04.061.2021076110230.hdf", 
                                "./testdir/modis/MOD13A1/MOD13A1.A2007289.h12v04.061.2021077181416.hdf", 
                                "./testdir/modis/MOD13A1/MOD13A1.A2007305.h12v04.061.2021078235951.hdf", 
                                "./testdir/modis/MOD13A1/MOD13A1.A2007321.h12v04.061.2021079181143.hdf", 
                                "./testdir/modis/MOD13A1/MOD13A1.A2007337.h12v04.061.2021081224633.hdf", 
                                "./testdir/modis/MOD13A1/MOD13A1.A2007353.h12v04.061.2021081232813.hdf")
            expect_equal(suppressMessages(vegetation_download(data_fmt(bcch[bcch$survey_year %in% c(2006:2007),],
                                                                       coord_lon = "longitude",
                                                                       coord_lat = "latitude",
                                                                       crs = 4326),
                                                              ed_email = "rmacklin@birdscanada.org",
                                                              dl_path = "./testdir")),
                         expected_files)
            expect_true(dir.exists("./testdir/modis/MOD13A1")) # Bonus test of custom file path specification
            expect_equal(list.files("./testdir/modis/MOD13A1", full.names = TRUE), expected_files)
            files_years <- luna::modisDate(list.files("./testdir/modis/MOD13A1"))
            expect_true(all(bcch$survey_year[bcch$survey_year %in% c(2006:2007)] %in% files_years$year))
            files_extent <- luna::modisExtent(list.files("./testdir/modis/MOD13A1"))
            bcch_spatial <- bcch %>%
              dplyr::filter(survey_year %in% c(2006:2007)) %>%
              sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
              sf::st_transform(terra::crs(terra::rast(list.files("./testdir/modis/MOD13A1",
                                                                 full.names = TRUE)[1]))) %>%
              sf::st_coordinates()
            expect_true(all(bcch_spatial[,"X"] <= unique(files_extent[,"xmax"])))
            expect_true(all(bcch_spatial[,"X"] >= unique(files_extent[,"xmin"])))
            expect_true(all(bcch_spatial[,"Y"] <= unique(files_extent[,"ymax"])))
            expect_true(all(bcch_spatial[,"Y"] >= unique(files_extent[,"ymin"])))
          })

test_that("vegetation_download() succeeds with alternate column names, either
          passed through attributes or specified explicitly.", {
            expected_files <- c("MOD13A1.A2007017.h12v04.061.2021055141240.hdf", 
                                "MOD13A1.A2007033.h12v04.061.2021053004443.hdf", 
                                "MOD13A1.A2007049.h12v04.061.2021055160007.hdf", 
                                "MOD13A1.A2007065.h12v04.061.2021057175943.hdf", 
                                "MOD13A1.A2007081.h12v04.061.2021059114255.hdf", 
                                "MOD13A1.A2007097.h12v04.061.2021060063721.hdf", 
                                "MOD13A1.A2007113.h12v04.061.2021064210119.hdf", 
                                "MOD13A1.A2007129.h12v04.061.2021064213919.hdf", 
                                "MOD13A1.A2007145.h12v04.061.2021065143148.hdf", 
                                "MOD13A1.A2007161.h12v04.061.2021066014850.hdf", 
                                "MOD13A1.A2007177.h12v04.061.2021068094740.hdf", 
                                "MOD13A1.A2007193.h12v04.061.2021068160209.hdf", 
                                "MOD13A1.A2007209.h12v04.061.2021071042709.hdf", 
                                "MOD13A1.A2007225.h12v04.061.2021072224429.hdf", 
                                "MOD13A1.A2007241.h12v04.061.2021073192515.hdf", 
                                "MOD13A1.A2007257.h12v04.061.2021074191343.hdf", 
                                "MOD13A1.A2007273.h12v04.061.2021076110230.hdf", 
                                "MOD13A1.A2007289.h12v04.061.2021077181416.hdf", 
                                "MOD13A1.A2007305.h12v04.061.2021078235951.hdf", 
                                "MOD13A1.A2007321.h12v04.061.2021079181143.hdf", 
                                "MOD13A1.A2007337.h12v04.061.2021081224633.hdf", 
                                "MOD13A1.A2007353.h12v04.061.2021081232813.hdf")
            expect_equal(suppressMessages(vegetation_download(data_fmt(dplyr::rename(bcch[bcch$survey_year == 2007,],
                                                                                     "sites" = "SurveyAreaIdentifier",
                                                                                     "yr" = "survey_year"),
                                                                       coord_lon = "longitude",
                                                                       coord_lat = "latitude",
                                                                       site_name = "sites",
                                                                       date_year = "yr",
                                                                       crs = 4326),
                                                              ed_transfer = FALSE)),
                         expected_files)
            expect_equal(suppressMessages(vegetation_download(dplyr::rename(data_fmt(bcch[bcch$survey_year == 2007,],
                                                                                     coord_lon = "longitude",
                                                                                     coord_lat = "latitude",
                                                                                     crs = 4326),
                                                                            "sites" = "SurveyAreaIdentifier",
                                                                            "yr" = "survey_year"),
                                                              site_name = "sites",
                                                              date_year = "yr",
                                                              ed_transfer = FALSE)),
                         expected_files)
          })


test_that("landcover_extract() basic functionality with all expected data inputs.", {
  sf_pt <- suppressWarnings(suppressMessages(data_fmt(bcch[bcch$survey_year == 2007,])))
  sf_poly <- suppressWarnings(suppressMessages(data_buff(data_fmt(bcch[bcch$survey_year == 2007,]))))
  terra_pt <- terra::vect(sf_pt)
  terra_poly <- terra::vect(sf_poly)
  
  expect_silent(extracted <- suppressMessages(vegetation_extract(
    sf_pt,
    vegetation_files = list.files("./testdir/modis/MOD13A1", 
                                  full.names = TRUE),
    covariates = c("modis_ndvi", "modis_evi"))))
  expect_s3_class(extracted, "sf")
  expect_equal(as.character(sf::st_geometry_type(extracted, by_geometry = FALSE)), "POINT")
  expect_named(extracted, c("SurveyAreaIdentifier", "latitude", "longitude", "survey_year", "survey_month", "survey_day", "ndvi", "evi", "geometry"))
  expect_equal(dplyr::select(extracted, -"ndvi", -"evi"), sf_pt, ignore_attr = TRUE) # Ignores attributes to confirm that data has not been otherwise modified.
  expect_equal(format(sf::st_crs(extracted)), "Canada_Albers_Equal_Area_Conic")
  expect_equal(unname(apply(X = apply(FUN = is.na, X = extracted, MARGIN = 1), FUN = unique, MARGIN = 1)), rep(FALSE, times = 9))
  expect_true(all(extracted$ndvi < 1))
  expect_true(all(extracted$ndvi > -1))
  expect_true(all(extracted$evi < 1))
  expect_true(all(extracted$evi > -1))
  
  expect_silent(extracted <- suppressMessages(vegetation_extract(
    sf_poly,
    vegetation_files = list.files("./testdir/modis/MOD13A1",
                                 full.names = TRUE),
    covariates = c("modis_ndvi", "modis_evi"))))
  expect_s3_class(extracted, "sf")
  expect_equal(as.character(sf::st_geometry_type(extracted, by_geometry = FALSE)), "POLYGON")
  expect_named(extracted, c("SurveyAreaIdentifier", "latitude", "longitude", "survey_year", "survey_month", "survey_day", "ndvi", "evi", "geometry"))
  expect_equal(dplyr::select(extracted, -"ndvi", -"evi"), sf_poly, ignore_attr = TRUE) # Ignores attributes to confirm that data has not been otherwise modified.
  expect_equal(format(sf::st_crs(extracted)), "Canada_Albers_Equal_Area_Conic")
  expect_equal(unname(apply(X = apply(FUN = is.na, X = extracted, MARGIN = 1), FUN = unique, MARGIN = 1)), rep(FALSE, times = 9))
  expect_true(all(extracted$ndvi < 1))
  expect_true(all(extracted$ndvi > -1))
  expect_true(all(extracted$evi < 1))
  expect_true(all(extracted$evi > -1))
  

  expect_silent(extracted <- suppressMessages(vegetation_extract(
    terra_pt,
    vegetation_files = list.files("./testdir/modis/MOD13A1",
                                 full.names = TRUE),
    covariates = c("modis_ndvi", "modis_evi"))))
  expect_s3_class(extracted, "sf")
  expect_equal(as.character(sf::st_geometry_type(extracted, by_geometry = FALSE)), "POINT")
  expect_named(extracted, c("SurveyAreaIdentifier", "latitude", "longitude", "survey_year", "survey_month", "survey_day", "ndvi", "evi", "geometry"))
  expect_equal(dplyr::select(extracted, -"ndvi", -"evi"), sf_pt, ignore_attr = TRUE) # Ignores attributes to confirm that data has not been otherwise modified.
  expect_equal(format(sf::st_crs(extracted)), "Canada_Albers_Equal_Area_Conic")
  expect_equal(unname(apply(X = apply(FUN = is.na, X = extracted, MARGIN = 1), FUN = unique, MARGIN = 1)), rep(FALSE, times = 9))
  expect_true(all(extracted$ndvi < 1))
  expect_true(all(extracted$ndvi > -1))
  expect_true(all(extracted$evi < 1))
  expect_true(all(extracted$evi > -1))

  expect_silent(extracted <- suppressMessages(vegetation_extract(
    terra_poly,
    vegetation_files = list.files("./testdir/modis/MOD13A1",
                                 full.names = TRUE),
    covariates = c("modis_ndvi", "modis_evi"))))
  expect_s3_class(extracted, "sf")
  expect_equal(as.character(sf::st_geometry_type(extracted, by_geometry = FALSE)), "POLYGON")
  expect_named(extracted, c("SurveyAreaIdentifier", "latitude", "longitude", "survey_year", "survey_month", "survey_day", "ndvi", "evi", "geometry"))
  expect_equal(dplyr::select(extracted, -"ndvi", -"evi"), sf_poly, ignore_attr = TRUE) # Ignores attributes to confirm that data has not been otherwise modified.
  expect_equal(format(sf::st_crs(extracted)), "Canada_Albers_Equal_Area_Conic")
  expect_equal(unname(apply(X = apply(FUN = is.na, X = extracted, MARGIN = 1), FUN = unique, MARGIN = 1)), rep(FALSE, times = 9))
  expect_true(all(extracted$ndvi < 1))
  expect_true(all(extracted$ndvi > -1))
  expect_true(all(extracted$evi < 1))
  expect_true(all(extracted$evi > -1))
  })

test_that("vegetation_extract() successfully returns reliability information.", {
  sf_pt <- suppressWarnings(suppressMessages(data_fmt(bcch[bcch$survey_year == 2007,])))
  sf_poly <- suppressWarnings(suppressMessages(data_buff(data_fmt(bcch[bcch$survey_year == 2007,]))))
  
  expect_silent(extracted <- suppressMessages(vegetation_extract(
    sf_pt,
    vegetation_files = list.files("./testdir/modis/MOD13A1", 
                                  full.names = TRUE),
    reliability = TRUE)))
  expect_named(extracted, c("SurveyAreaIdentifier", "latitude", "longitude", "survey_year", "survey_month", "survey_day", "ndvi", "vegetation_reliability", "geometry"))
  expect_equal(dplyr::select(extracted, -"ndvi", -"vegetation_reliability"), sf_pt, ignore_attr = TRUE) # Ignores attributes to confirm that data has not been otherwise modified.
  expect_equal(format(sf::st_crs(extracted)), "Canada_Albers_Equal_Area_Conic")
  expect_equal(unname(apply(X = apply(FUN = is.na, X = extracted, MARGIN = 1), FUN = unique, MARGIN = 1)), rep(FALSE, times = 9))
  expect_true(all(extracted$vegetation_reliability %in% c("Fill/No Data", "Good Data","Marginal Data","Snow/Ice","Cloudy")))
  
  expect_silent(extracted <- suppressMessages(vegetation_extract(
    sf_poly,
    vegetation_files = list.files("./testdir/modis/MOD13A1", 
                                  full.names = TRUE),
    reliability = TRUE)))
  expect_named(extracted, c("SurveyAreaIdentifier", "latitude", "longitude", "survey_year", "survey_month", "survey_day", "ndvi", "vegetation_reliability", "geometry"))
  expect_equal(dplyr::select(extracted, -"ndvi", -"vegetation_reliability"), sf_poly, ignore_attr = TRUE) # Ignores attributes to confirm that data has not been otherwise modified.
  expect_equal(format(sf::st_crs(extracted)), "Canada_Albers_Equal_Area_Conic")
  expect_equal(unname(apply(X = apply(FUN = is.na, X = extracted, MARGIN = 1), FUN = unique, MARGIN = 1)), rep(FALSE, times = 9))
  expect_equal(stringr::str_flatten_comma(extracted$vegetation_reliability), "Snow/Ice (100%), Snow/Ice (66.67%), Cloudy (33.33%), Good Data (100%), Good Data (100%)")
})