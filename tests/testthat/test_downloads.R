# Download Counts -----------------------------------------------------------
context("Download counts")

test_that("Get counts for collections", {

  expect_silent(c1 <- nc_count(c("CBC", "BBS"), show = "all"))
  expect_is(c1, "data.frame")
  expect_gt(nrow(c1), 0)
  expect_true(all(c1$collection %in% c("CBC", "BBS")))

  expect_silent(c2 <- nc_count(c("CBC", "BBS"),
                               region = list(statprov = "MB"),
                               show = "all"))
  expect_is(c2, "data.frame")
  expect_gt(nrow(c2), 0)
  expect_true(all(c2$collection %in% c("CBC", "BBS")))
  expect_true(all(c2$nrecords < c1$nrecords))

  expect_silent(c <- nc_count(region = list(statprov = "MB")))
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
  expect_message(d <- nc_data_dl(collections = "RCBIOTABASE", years = 2011))
  expect_is(d, "data.frame")
  expect_gt(nrow(d), 0)
  expect_gt(ncol(d), 0)
  expect_equal(min(as.numeric(d$survey_year), na.rm = TRUE), 2011)
  expect_equal(max(as.numeric(d$survey_year), na.rm = TRUE), 2011)
})

test_that("Data download arguments", {
  expect_silent(nc_data_dl(collections = "RCBIOTABASE", years = 2011,
                           verbose = FALSE))

})

test_that("Data filters work as expected", {
  expect_silent(d1 <- nc_data_dl(collections = "ABBIRDRECS",
                                species = 7590, years = 2000,
                                verbose = FALSE))
  expect_equal(unique(d1$species_id), 7590)
  expect_equal(min(as.numeric(d1$survey_year), na.rm = TRUE), 2000)
  expect_equal(max(as.numeric(d1$survey_year), na.rm = TRUE), 2000)


  expect_silent(d2 <- nc_data_dl(collections = "ABBIRDRECS",
                                species = c(7590, 14280),
                                years = c(2003, 2004),
                                verbose = FALSE))
  expect_equal(sort(unique(d2$species_id)), c(7590, 14280))
  expect_equal(min(as.numeric(d2$survey_year), na.rm = TRUE), 2003)
  expect_equal(max(as.numeric(d2$survey_year), na.rm = TRUE), 2004)

  expect_silent(d3 <- nc_data_dl(collections = "ABBIRDRECS",
                                 species = 7590, years = 2000,
                                 fields_set = "core",
                                 verbose = FALSE))
  expect_equal(nrow(d1), nrow(d3))
  expect_gt(ncol(d3), ncol(d1))

  expect_silent(d4 <- nc_data_dl(collections = "ABBIRDRECS",
                                 species = 7590, years = 2000,
                                 fields_set = "custom", fields = "Locality",
                                 verbose = FALSE))
  expect_equal(nrow(d1), nrow(d4))
  expect_lt(ncol(d4), ncol(d1))
  expect_true("Locality" %in% names(d4))

})

test_that("Pagination", {
  # Get data and messages
  expect_silent(m <- capture_messages(
    d <- nc_data_dl(collections = "RCBIOTABASE")))

  # Expect pagination over three pages
  expect_gt(sum(stringr::str_count(m, "Records")), 1)

  # Expect more than one page (5000 records)
  expect_gt(nrow(d), 5000)

})


test_that("Data download returns informative errors/messages", {

  # No data for some
  expect_message(nc_data_dl(collections = c("ABBIRDRECS", "RCBIOTABASE"),
                            years = 2010,
                            species = 7590, verbose = TRUE),
                 "Not all collections have data that match these filters")

  # No permission
  expect_error(nc_data_dl(collections = "BBS", species = 7590, verbose = FALSE),
               "You do not have permission")

  # No data
  expect_error(nc_data_dl(collections = "ABBIRDRECS", years = 2018,
                          verbose = FALSE),
               "These collections have no data that match these filters")

  # Custom field_set without fields
  expect_error(nc_data_dl(collections = "ABBIRDRECS", species = 7590,
                          years = 2000,
                          fields_set = "custom", verbose = FALSE),
               "Must specify 'fields' if using a custom 'field_set'")
})
