context("SQLite Databases")

teardown(unlink("test.nc"))

# db_check_version ------------------------------------------------------------
test_that("db_check_version() works as expected", {

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  # No version
  expect_error(db_check_version(con),
               "There is no version information for this database.")

  # Old version
  DBI::dbWriteTable(con, "versions", data.frame(Rpackage = "0.0.5"))
  expect_error(db_check_version(con),
               "Your NatureCounts database is out of date")

  # Current version
  v <- data.frame(Rpackage = as.character(packageVersion("naturecounts")))
  DBI::dbWriteTable(con, "versions", overwrite = TRUE, value = v)
  expect_silent(db_check_version(con))

  # Clean up
  DBI::dbDisconnect(con)
})

# db_create ------------------------------------------------------------
test_that("db_create creates tables in the database", {

  con <- DBI::dbConnect(RSQLite::SQLite(), dbname =  ":memory:")

  expect_silent(db_create(con))
  expect_equal(DBI::dbGetQuery(con, "PRAGMA encoding;")$encoding, "UTF-8")

  # Check naturecounts table
  expect_silent(nc <- dplyr::tbl(con, "naturecounts")) %>%
    expect_is("tbl_sql") %>%
    expect_named()

  expect_silent(nc <- dplyr::collect(nc))
  expect_equal(nrow(nc), 0)
  expect_gt(ncol(nc), 1)

  # Check version table
  expect_silent(v <- dplyr::tbl(con, "versions")) %>%
    expect_is("tbl_sql") %>%
    expect_named()

  expect_silent(v <- dplyr::collect(v))
  expect_gte(nrow(v), 1)

  # Check metadata tables
  funs <- ls("package:naturecounts") %>%
    stringr::str_subset("^meta_") %>%
    stringr::str_remove("^meta_")
  funs <- funs[!stringr::str_detect(funs, "(bmde)|(utm)")]

  expect_true(all(c(funs, "naturecounts", "versions") %in%
                    DBI::dbListTables(con)))

  for(m in funs) {
    expect_silent(d <- dplyr::tbl(con, !!m)) %>%
      expect_is("tbl_sql")
    expect_equal(dplyr::collect(d) %>% as.data.frame(),
                 do.call(paste0("meta_", !!m), args = list()))
  }

  # Clean up
  DBI::dbDisconnect(con)
})


# db_connect --------------------------------------------------------------

test_that("db_connect creates SQLite database file", {

  # Check connection and encoding
  expect_message(con <- db_connect(),
                 "Database '.\\/naturecounts_[0-9-]{10}.nc' does not exist") %>%
    expect_is("SQLiteConnection")
  expect_equal(DBI::dbGetQuery(con, "PRAGMA encoding;")$encoding, "UTF-8")

  # Check that file present
  expect_true(file.exists(paste0("naturecounts_", Sys.Date(), ".nc")))

  # Check that can re-connect to existing database
  DBI::dbDisconnect(con)

  expect_message(con <- db_connect(),
                 "Database './naturecounts_[0-9-]{10}.nc' already exists") %>%
    expect_is("SQLiteConnection")
  expect_equal(DBI::dbGetQuery(con, "PRAGMA encoding;")$encoding, "UTF-8")

  # Clean up
  DBI::dbDisconnect(con)
  expect_true(file.remove(paste0("naturecounts_", Sys.Date(), ".nc")))
})


test_that("db_connect creates named SQLite database file", {

  # Check connection and encoding
  expect_silent(con <- db_connect("mydatabase", verbose = FALSE)) %>%
    expect_is("SQLiteConnection")
  expect_equal(DBI::dbGetQuery(con, "PRAGMA encoding;")$encoding, "UTF-8")

  # Check that file present
  expect_true(file.exists(paste0("mydatabase.nc")))

  # Check that can re-connect to existing database
  DBI::dbDisconnect(con)

  expect_silent(con <- db_connect("mydatabase", verbose = FALSE)) %>%
    expect_is("SQLiteConnection")
  expect_equal(DBI::dbGetQuery(con, "PRAGMA encoding;")$encoding, "UTF-8")

  # Clean up
  DBI::dbDisconnect(con)
  expect_true(file.remove(paste0("mydatabase.nc")))
})


# db_insert ---------------------------------------------------------------
test_that("db_insert add and appends rows", {

  expect_silent(con <- db_connect(verbose = FALSE)) %>%
    expect_is("SQLiteConnection")

  # Adding data to an empty table
  expect_silent(db_insert(con, "naturecounts", bcch))
  expect_silent(nc1 <- dplyr::collect(dplyr::tbl(con, "naturecounts"))) %>%
    expect_is("tbl")

  # Appending new data to table with data
  expect_silent(db_insert(con, "naturecounts", bdow))
  expect_silent(nc2 <- dplyr::collect(dplyr::tbl(con, "naturecounts"))) %>%
    expect_is("tbl")

  expect_equal(nrow(nc2), nrow(bcch) + nrow(bdow))

  # Clean up (leave file for next tests)
  DBI::dbDisconnect(con)
  expect_true(file.remove(paste0("naturecounts_", Sys.Date(), ".nc")))
})

test_that("db_insert overwrites rows as required", {

  expect_silent(con <- db_connect(verbose = FALSE)) %>%
    expect_is("SQLiteConnection")

  # Trying to append duplicate data doesn't add anything
  expect_silent(db_insert(con, "naturecounts", bcch))
  expect_silent(db_insert(con, "naturecounts", bcch))
  expect_silent(nc1 <- dplyr::collect(dplyr::tbl(con, "naturecounts")))

  expect_equal(nrow(nc1), nrow(bcch))

  # Trying to add new data with same record_id replaces existing data
  bcch2 <- bdow
  bcch2$record_id <- bcch$record_id[1:nrow(bcch2)]

  expect_silent(db_insert(con, "naturecounts", bcch2))
  expect_silent(nc2 <- dplyr::collect(dplyr::tbl(con, "naturecounts")))

  expect_equal(nrow(nc1), nrow(nc2)) # Same rows
  expect_false(isTRUE(dplyr::all_equal(nc1, nc2))) # But data has changed

  expect_equal(sort(nc1$record_id), sort(nc2$record_id)) # Not record_ids

  # Clean up
  DBI::dbDisconnect(con)
  expect_true(file.remove(paste0("naturecounts_", Sys.Date(), ".nc")))
})

test_that("db_insert adds new cols as required", {

  expect_silent(con <- db_connect(verbose = FALSE)) %>%
    expect_is("SQLiteConnection")

  n <- DBI::dbListFields(con, "naturecounts")

  # Add data with fewer cols than db (no new)
  expect_silent(db_insert(con, "naturecounts",
                          dplyr::select(bcch, record_id, collection)))
  expect_equal(length(n), length(DBI::dbListFields(con, "naturecounts")))

  dplyr::collect(dplyr::tbl(con, "naturecounts")) %>% # All new cols are NA
    apply(., 2, function(x) all(is.na(x))) %>%
    sum() %>%
    expect_equal(length(n) - 2)

  # Add data with more cols than db
  bcch2 <- dplyr::mutate(bcch, new1 = "test", new2 = 4.56, new3 = 1L) %>%
    dplyr::as_tibble()
  expect_silent(db_insert(con, "naturecounts", bcch2))

  expect_silent(nc <- dplyr::collect(dplyr::tbl(con, "naturecounts")))
  expect_equal(names(nc), names(bcch2))
  expect_equal(nrow(nc), nrow(bcch2))
  expect_equal(dplyr::select(nc, "new1", "new2", "new3"),
               dplyr::select(bcch2, "new1", "new2", "new3"))

  # Clean up
  DBI::dbDisconnect(con)
  expect_true(file.remove(paste0("naturecounts_", Sys.Date(), ".nc")))
})


# nc_data_dl to SQL -------------------------------------------------------

test_that("Data download to sql", {
  expect_message(d <- nc_data_dl(collections = "RCBIOTABASE", years = 2011,
                                 username = "testuser", info = "nc_test",
                                 sql_db = "test"))

  expect_true(file.exists("./test.nc"))
  expect_is(d, "SQLiteConnection")

  expect_silent(d_db <- dplyr::collect(dplyr::tbl(d, "naturecounts")))

  expect_gt(nrow(d_db), 0)
  expect_gt(ncol(d_db), 0)
  expect_equal(min(as.numeric(d_db$survey_year), na.rm = TRUE), 2011)
  expect_equal(max(as.numeric(d_db$survey_year), na.rm = TRUE), 2011)
  DBI::dbDisconnect(d)
})
