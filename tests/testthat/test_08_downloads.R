# Counts - General ----------------------------------------------------
context("Download counts")

test_that("Get permissions for user", {
  expect_silent(p <- nc_permissions())
  expect_type(p, "character")
  expect_gt(length(p), 1)

  expect_silent(p1 <- nc_permissions(username = "sample"))
  expect_type(p1, "character")
  expect_gt(length(p1), 1)
  expect_gt(length(p1), length(p))
})


test_that("Get counts for collections", {

  # Entire collection
  expect_message(c1 <- nc_count(c("CBC", "BBS")), "Without a username")
  expect_message(c1 <- nc_count(c("CBC", "BBS")), "Using filters")
  expect_is(c1, "data.frame")
  expect_gt(nrow(c1), 0)
  expect_true(all(c1$collection %in% c("CBC", "BBS")))
  expect_true(all(c1$access %in% c("full", "no access")))

  expect_silent(c2 <- nc_count(c("CBC", "BBS"), region = list(statprov = "MB"),
                               show = "all", verbose = FALSE))
  expect_is(c2, "data.frame")
  expect_gt(nrow(c2), 0)
  expect_true(all(c2$collection %in% c("CBC", "BBS")))
  expect_true(all(c2$nrecords < c1$nrecords))

  # Expect show = "all" with no username
  expect_silent(c1 <- nc_count(region = list(statprov = "MB"), show = "all",
                               verbose = FALSE)) %>%
    expect_is("data.frame")
  expect_message(c2 <- nc_count(region = list(statprov = "MB"), show = "available",
                               verbose = FALSE)) %>%
    expect_is("data.frame")
  expect_equal(c1, c2)
})

test_that("Counts return permissions", {

  expect_silent(c_sample <- nc_count(species = 7590,
                                     verbose = FALSE, username = "sample"))
  expect_silent(c_sample_all <- nc_count(species = 7590, show = "all",
                                         verbose = FALSE, username = "sample"))
  expect_silent(c_all <- nc_count(species = 7590, show = "all",
                                  verbose = FALSE))
  expect_gt(sum(c_all$nrecords, na.rm = TRUE), sum(c_sample$nrecords, na.rm = TRUE))

  expect_named(c_sample, c("collection", "akn_level", "access", "nrecords"),
               ignore.order = TRUE)

  expect_equal(unique(c_sample[["access"]]), "full")
  expect_equal(unique(c_sample[["akn_level"]]), c(5,0))

  expect_equal(sort(unique(c_sample_all[["access"]])),
               c("by request", "full", "no access"))
  expect_equal(sort(unique(c_sample_all[["akn_level"]])), 2:5)

})

test_that("Counts error when no data returned", {
  expect_error(nc_count(collections = "steffi"), "No counts for these filters")
})


# Data - General -----------------------------------------------------

context("Download data")

test_that("Data download returns data", {
  expect_message(d <- nc_data_dl(collections = "RCBIOTABASE", years = 2011,
                                 username = "sample", info = "nc_test"))
  expect_is(d, "data.frame")
  expect_gt(nrow(d), 0)
  expect_gt(ncol(d), 0)
  expect_equal(min(as.numeric(d$survey_year), na.rm = TRUE), 2011)
  expect_equal(max(as.numeric(d$survey_year), na.rm = TRUE), 2011)
})

test_that("Data download arguments", {
  expect_silent(nc_data_dl(collections = "RCBIOTABASE", years = 2011,
                           username = "sample", verbose = FALSE,
                           info = "nc_test"))

})


# Data - Filters ----------------------------------------------------------

test_that("Data filters work as expected", {

  # single project_id
  expect_silent(d <- nc_data_dl(project_ids = 1030,
                                 species = 7590, years = 2000,
                                 username = "sample", verbose = FALSE,
                                 info = "nc_test"))
  expect_equal(unique(d$collection), "RCBIOTABASE")
  expect_equal(unique(d$project_id), 1030)

  # single collection/species/year/
  expect_silent(d1 <- nc_data_dl(collections = "ABBIRDRECS",
                                species = 7590, years = 2000,
                                username = "sample", verbose = FALSE,
                                info = "nc_test"))
  expect_equal(unique(d1$species_id), 7590)
  expect_equal(min(as.numeric(d1$survey_year), na.rm = TRUE), 2000)
  expect_equal(max(as.numeric(d1$survey_year), na.rm = TRUE), 2000)

  # mult species/year
  expect_silent(d2 <- nc_data_dl(collections = "ABBIRDRECS",
                                species = c(7590, 14280),
                                years = c(2003, 2004),
                                username = "sample", verbose = FALSE,
                                info = "nc_test"))
  expect_equal(sort(unique(d2$species_id)), c(7590, 14280))
  expect_equal(min(as.numeric(d2$survey_year), na.rm = TRUE), 2003)
  expect_equal(max(as.numeric(d2$survey_year), na.rm = TRUE), 2004)

  # fields set
  expect_silent(d3 <- nc_data_dl(collections = "ABBIRDRECS",
                                 species = 7590, years = 2000,
                                 fields_set = "core",
                                 username = "sample", verbose = FALSE,
                                 info = "nc_test"))
  expect_equal(nrow(d1), nrow(d3))
  expect_gt(ncol(d3), ncol(d1))

  # custom fields
  expect_silent(d4 <- nc_data_dl(collections = "ABBIRDRECS",
                                 species = 7590, years = 2000,
                                 fields_set = "custom", fields = "Locality",
                                 username = "sample", verbose = FALSE,
                                 info = "nc_test"))
  expect_equal(nrow(d1), nrow(d4))
  expect_lt(ncol(d4), ncol(d1))
  expect_true("Locality" %in% names(d4))
})

test_that("Filter region works as expected", {
  # IBA
  expect_silent(d <- nc_data_dl(region = list(iba = "AB001"),
                                username = "sample", verbose = FALSE,
                                info = "nc_test"))
  expect_equal(unique(d$iba_site), "AB001")

  # BCR
  expect_silent(d <- nc_data_dl(region = list(bcr = 2),
                                username = "sample", verbose = FALSE,
                                info = "nc_test"))
  expect_equal(unique(d$bcr), 2)

  # Province
  expect_silent(d <- nc_data_dl(region = list(statprov = "PE"),
                                username = "sample", verbose = FALSE,
                                info = "nc_test"))
  expect_equal(unique(d$statprov_code), "PE")

})

test_that("Filter site_type works as expected", {
  expect_silent(d <- nc_data_dl(region = list(statprov = "PE"),
                                site_type = "IBA", username = "sample",
                                verbose = FALSE, info = "nc_test"))
  expect_true(all(d$iba_site != "N/A"))
  expect_true(all(stringr::str_detect(d$iba_site, "^PE")))
})

# Data - DOY ----------------------------------------------------------

test_that("Data filters Day of year", {
  # Summer
  expect_silent(d1 <- nc_data_dl(collections = "ABBIRDRECS",
                                 species = 15770,
                                 doy = c(120, 140),
                                 username = "sample", verbose = FALSE,
                                 info = "nc_test"))
  expect_silent(d1 <- format_dates(d1))
  expect_gte(min(d1$doy), 120)
  expect_lte(max(d1$doy), 140)

  # winter

  expect_silent(d <- nc_data_dl(collections = "ABBIRDRECS",
                                 species = 15770,
                                 doy = c(300, 20),
                                username = "sample", verbose = FALSE,
                                info = "nc_test"))
  expect_silent(d <- format_dates(d))
  expect_true(all(d$doy >= 300 | d$doy <= 20))

  # Winter same as two separate requests
  expect_silent(da <- nc_data_dl(collections = "ABBIRDRECS",
                                 species = 15770,
                                 doy = c(300, NA),
                                 username = "sample", verbose = FALSE,
                                 info = "nc_test"))
  expect_silent(db <- nc_data_dl(collections = "ABBIRDRECS",
                                 species = 15770,
                                 doy = c(NA, 20),
                                 username = "sample", verbose = FALSE,
                                 info = "nc_test"))
  d2 <- dplyr::bind_rows(da, db) %>%
    format_dates()
  expect_equal(dplyr::arrange(d, record_id), dplyr::arrange(d2, record_id))
})


# Data - Pagination -------------------------------------------------------

test_that("Pagination", {
  # Get data and messages
  expect_silent(m <- capture_messages(
    d <- nc_data_dl(collections = "RCBIOTABASE", username = "sample",
                    info = "nc_test")))

  # Expect pagination over three pages
  expect_gt(sum(stringr::str_count(m, "Records")), 1)

  # Expect more than one page (5000 records)
  expect_gt(nrow(d), 5000)

})


# Data - Errors/Messages --------------------------------------------------

test_that("Data download returns informative errors/messages", {

  # No data for some
  expect_message(nc_data_dl(collections = c("ABBIRDRECS", "RCBIOTABASE"),
                            years = 2010, species = 7590,
                            username = "sample", verbose = TRUE,
                            info = "nc_test"),
                 "Not all collections have data that match these filters")

  expect_error(nc_data_dl(collections = "ABBIRDRECS",
                            years = 2010, species = 7590,
                            username = "sample", verbose = TRUE,
                            info = "nc_test"),
                 "These collections have no data that match these filters")

  # No permission
  expect_error(nc_data_dl(collections = "ATOWLS",
                          username = "sample", verbose = TRUE,
                          info = "nc_test"),
               "No access to collection\\(s\\): ATOWLS")

  # No data
  expect_error(nc_data_dl(collections = "ABBIRDRECS", years = 2018,
                          username = "sample", verbose = FALSE,
                          info = "nc_test"),
               "These collections have no data that match these filters")

  # Custom field_set without fields
  expect_error(nc_data_dl(collections = "ABBIRDRECS", species = 7590,
                          years = 2000, fields_set = "custom",
                          username = "sample", verbose = FALSE,
                          info = "nc_test"),
               "Must specify 'fields' if using a custom 'field_set'")

  # No info
  expect_error(nc_data_dl(collections = "ABBIRDRECS", species = 7590,
                          years = 2000, fields_set = "custom",
                          username = "sample", verbose = FALSE),
               "'info' is required text if not using a 'request_id'")
})
