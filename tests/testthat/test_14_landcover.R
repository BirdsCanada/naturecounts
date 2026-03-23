if(!dir.exists("./testdir")) {
  dir.create("./testdir")
}

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
                      "MCD12Q1.A2016001.h12v04.061.2022166171445.hdf",
                      "MCD12Q1.A2017001.h12v04.061.2022168033428.hdf")
  expect_equal(suppressMessages(landcover_download(data_fmt(bcch,
                                                            coord_lon = "longitude",
                                                            coord_lat = "latitude",
                                                            crs = 4326),
                                                   ed_transfer = FALSE)),
               expected_files)
  expect_equal(suppressMessages(landcover_download(data_buff(data_fmt(bcch,
                                                                      coord_lon = "longitude",
                                                                      coord_lat = "latitude",
                                                                      crs = 4326)),
                                                   ed_transfer = FALSE)),
               expected_files)
  expect_equal(suppressMessages(landcover_download(data_fmt(terra::vect(bcch,
                                                                        crs = "epsg:4326")),
                                                   ed_transfer = FALSE)),
               expected_files)
  expect_equal(suppressMessages(landcover_download(data_buff(data_fmt(terra::vect(bcch,
                                                                                  crs = "epsg:4326"))),
                                                   ed_transfer = FALSE)),
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
                      "./testdir/modis/MCD12Q1/MCD12Q1.A2016001.h12v04.061.2022166171445.hdf",
                      "./testdir/modis/MCD12Q1/MCD12Q1.A2017001.h12v04.061.2022168033428.hdf")
  expect_equal(suppressMessages(landcover_download(data_fmt(bcch,
                                                            coord_lon = "longitude",
                                                            coord_lat = "latitude",
                                                            crs = 4326),
                                                   ed_email = "rmacklin@birdscanada.org",
                                                   dl_path = "./testdir")),
               expected_files)
  expect_true(dir.exists("./testdir/modis/MCD12Q1")) # Bonus test of custom file path specification
  expect_equal(list.files("./testdir/modis/MCD12Q1", full.names = TRUE), expected_files)
  files_years <- luna::modisDate(list.files("./testdir/modis/MCD12Q1"))
  expect_true(all(bcch$survey_year[bcch$survey_year %in% 2001:((lubridate::year(Sys.Date()))-1)] %in% files_years$year))
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
                      "MCD12Q1.A2016001.h12v04.061.2022166171445.hdf",
                      "MCD12Q1.A2017001.h12v04.061.2022168033428.hdf")
  expect_equal(suppressMessages(landcover_download(data_fmt(dplyr::rename(bcch,
                                                                          "sites" = "SurveyAreaIdentifier",
                                                                          "yr" = "survey_year"),
                                                            coord_lon = "longitude",
                                                            coord_lat = "latitude",
                                                            site_name = "sites",
                                                            date_year = "yr",
                                                            crs = 4326),
                                                   ed_transfer = FALSE)),
               expected_files)
  expect_equal(suppressMessages(landcover_download(dplyr::rename(data_fmt(bcch,
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
  input <- suppressWarnings(suppressMessages(data_buff(data_fmt(bcch))))
  expect_silent(extracted <- suppressWarnings(suppressMessages(landcover_extract(
    input,
    landcover_files = list.files("./testdir/modis/MCD12Q1", 
                                 full.names = TRUE)))))
})

unlink("./testdir", recursive = T)
