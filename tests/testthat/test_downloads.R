# Download Counts -----------------------------------------------------------
context("Download counts")

test_that("Get counts for collections", {

  expect_silent(c1 <- nc_count(c("CBC", "BBS"), show = "all"))
  expect_is(c1, "data.frame")
  expect_gt(nrow(c1), 0)
  expect_true(all(c1$collection %in% c("CBC", "BBS")))

  expect_silent(c2 <- nc_count(c("CBC", "BBS"), statprov = "MB", show = "all"))
  expect_is(c2, "data.frame")
  expect_gt(nrow(c2), 0)
  expect_true(all(c2$collection %in% c("CBC", "BBS")))
  expect_true(all(c2$nrecords < c1$nrecords))

  expect_silent(c <- nc_count(country = "CA", statprov = "MB"))
  expect_is(c, "data.frame")
  expect_gt(nrow(c), 0)

})

test_that("Counts return permissions", {

  expect_silent(c_public <- nc_count(species = 7590))
  expect_silent(c_all <- nc_count(species = 7590, show = "all"))
  expect_gt(sum(c_all$nrecords), sum(c_public$nrecords))

})


# Download Data -----------------------------------------------------------


context("Download data")

test_that("Data download returns data", {
  expect_message(d <- nc_data_dl(collections = "RCBIOTABASE",
                                 start_date = 2011, end_date = 2011))
  expect_is(d, "data.frame")
  expect_gt(nrow(d), 0)
  expect_gt(ncol(d), 0)
  expect_equal(min(as.numeric(d$YearCollected), na.rm = TRUE), 2011)
  expect_equal(max(as.numeric(d$YearCollected), na.rm = TRUE), 2011)
})

test_that("Data download arguments", {
  expect_silent(nc_data_dl(collections = "RCBIOTABASE",
                           start_date = 2011, end_date = 2011, verbose = FALSE))

})

test_that("Data filters work as expected", {
  expect_silent(d <- nc_data_dl(collections = "ABBIRDRECS",
                                species = 7590,
                                start_date = 2000, end_date = 2000,
                                verbose = FALSE))
  expect_equal(unique(d$CommonName), "Barred Owl")
  expect_equal(min(as.numeric(d$YearCollected), na.rm = TRUE), 2000)
  expect_equal(max(as.numeric(d$YearCollected), na.rm = TRUE), 2000)


  expect_silent(d <- nc_data_dl(collections = "ABBIRDRECS",
                                species = c(7590, 14280),
                                start_date = 2000, end_date = 2002,
                                verbose = FALSE))
  expect_equal(sort(unique(d$CommonName)),
               c("Barred Owl", "Black-capped Chickadee"))
  expect_equal(min(as.numeric(d$YearCollected), na.rm = TRUE), 2000)
  expect_equal(max(as.numeric(d$YearCollected), na.rm = TRUE), 2002)

})

test_that("Data download returns informative errors/messages", {

  # No data for some
  expect_message(nc_data_dl(collections = c("ABBIRDRECS", "RCBIOTABASE"), start_date = 2010,
                            species = 7590, verbose = TRUE),
                 "Not all collections have data that match these filters")

  # No permission
  expect_error(nc_data_dl(collections = "BBS", species = 7590, verbose = FALSE),
               "You do not have permission")

  # No data
  expect_error(nc_data_dl(collections = "ABBIRDRECS", start_date = 2018,
                          verbose = FALSE),
               "These collections have no data that match these filters")
})
