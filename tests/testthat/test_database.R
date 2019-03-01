context("SQLite Databases")

test_that("db_check_version() works as expected", {

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  # No version
  expect_error(db_check_version(con),
               "There is no version information for this database.")

  # Old version
  DBI::dbWriteTable(con, "version", data.frame(version = "2000-01-01"))
  expect_error(db_check_version(con),
               "Your NatureCounts database is out of date \\(2000-01-01 vs. ")

  # Current version
  DBI::dbWriteTable(con, "version", overwrite = TRUE,
                    value = data.frame(version = as.character(max_version)))
  expect_silent(db_check_version(con))
})

test_that("Empty, default, database created", {

  expect_silent(con <- db_connect()) %>%
    expect_is("SQLiteConnection")

  # Check naturecounts table
  expect_silent(nc <- dplyr::tbl(con, "naturecounts")) %>%
    expect_is("tbl_sql") %>%
    expect_named()

  expect_silent(nc <- dplyr::collect(nc))
  expect_true(nrow(nc) == 0)
  expect_true(ncol(nc) == 170)

  # Check version table
  expect_silent(v <- dplyr::tbl(con, "version")) %>%
    expect_is("tbl_sql") %>%
    expect_named()

  expect_silent(v <- dplyr::collect(v))
  expect_true(nrow(v) >= 1)
  expect_true(ncol(v) == 1)

  # Check request table
  expect_silent(d <- dplyr::tbl(con, "download_request")) %>%
    expect_is("tbl_sql") %>%
    expect_named()

  expect_silent(d <- dplyr::collect(d))
  expect_true(nrow(d) == 0)
  expect_true(ncol(d) == 6)

  # Check that file present
  expect_true(file.exists(paste0("naturecounts_", Sys.Date(), ".nc")))

  # Check that can re-connect to existing database
  DBI::dbDisconnect(con)

  expect_silent(con <- db_connect()) %>%
    expect_is("SQLiteConnection")

  # Clean up
  expect_true(file.remove(paste0("naturecounts_", Sys.Date(), ".nc")))
})

test_that("Empty, named, database created", {

  expect_silent(con <- db_connect("mydatabase")) %>%
    expect_is("SQLiteConnection")

  # Check naturecounts table
  expect_silent(nc <- dplyr::tbl(con, "naturecounts")) %>%
    expect_is("tbl_sql") %>%
    expect_named()

  expect_silent(nc <- dplyr::collect(nc))
  expect_true(nrow(nc) == 0)
  expect_true(ncol(nc) == 170)

  # Check version table
  expect_silent(v <- dplyr::tbl(con, "version")) %>%
    expect_is("tbl_sql") %>%
    expect_named()

  expect_silent(v <- dplyr::collect(v))
  expect_true(nrow(v) >= 1)
  expect_true(ncol(v) == 1)

  # Check request table
  expect_silent(d <- dplyr::tbl(con, "download_request")) %>%
    expect_is("tbl_sql") %>%
    expect_named()

  expect_silent(d <- dplyr::collect(d))
  expect_true(nrow(d) == 0)
  expect_true(ncol(d) == 6)

  # Check that file present
  expect_true(file.exists(paste0("mydatabase.nc")))

  # Check that can re-connect to existing database
  DBI::dbDisconnect(con)

  expect_silent(con <- db_connect("mydatabase")) %>%
    expect_is("SQLiteConnection")

  # Clean up
  expect_true(file.remove(paste0("mydatabase.nc")))
})

test_that("db_insert overwrites and adds as required", {

})
