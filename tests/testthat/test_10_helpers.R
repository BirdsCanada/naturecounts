context("Helper/Formating functions")


# format_dates() ----------------------------------------------------------
test_that("format_dates() with data frame", {
  for(i in 1:2) {
    if(i == 1) i <- bcch else i <- hofi
    expect_silent(f <- format_dates(i)) %>%
      expect_is("data.frame")
    expect_true(all(c("doy", "date") %in% names(f)))
    expect_is(f$date, "Date")
    expect_is(f$doy, "numeric")
    expect_equal(lubridate::as_date(paste(f$survey_year, f$survey_month, f$survey_day)), f$date)
    expect_equal(lubridate::yday(f$date), f$doy)
  }
})

test_that("format_dates() with SQLite database", {
  unlink(c("bcch.nc", "hofi.nc"))
  for(i in 1:2) {
    if(i == 1) {
      i <- nc_data_dl(collections = "RCBIOTABASE", species = 14280,
                      sql_db = "bcch", username = "testuser", info = "nc_test")
    } else if(i == 2) {
      i <- nc_data_dl(collections = "RCBIOTABASE", species = 20350,
                      sql_db = "hofi", username = "testuser", info = "nc_test")
    }
    expect_silent(f <- format_dates(i)) %>%
      expect_is("SQLiteConnection")
    expect_silent(f <- dplyr::tbl(f, "naturecounts") %>%
                    dplyr::collect())
    expect_true(all(c("doy", "date") %in% names(f)))
    expect_is(f$date, "character")
    expect_is(f$doy, "integer")
    expect_equal(lubridate::as_date(paste(f$survey_year, f$survey_month, f$survey_day)),
                 lubridate::as_date(f$date))
    expect_equal(lubridate::yday(f$date), f$doy)
    # Clean up
    DBI::dbDisconnect(i)
  }

  # Clean up
  unlink(c("bcch.nc", "hofi.nc"))
})

test_that("format_dates() overwrite", {
  # Data frame
  expect_silent(f1 <- format_dates(bcch))
  expect_error(f1 <- format_dates(f1), "'date' column already exists")
  expect_silent(f2 <- format_dates(f1, overwrite = TRUE))
  expect_equal(f1$date, f2$date)
  expect_equal(f1$doy, f1$doy)

  # SQL
  s <- nc_data_dl(collections = "RCBIOTABASE", species = 14280,
                  sql_db = "bcch", username = "testuser", info = "nc_test")
  expect_silent(format_dates(s))
  expect_error(format_dates(s), "'date' field already exists")
  f1 <- dplyr::tbl(s, "naturecounts") %>% dplyr::collect()

  expect_silent(format_dates(s, overwrite = TRUE))
  f2 <- dplyr::tbl(s, "naturecounts") %>% dplyr::collect()

  expect_equal(f1$date, f2$date)
  expect_equal(f1$doy, f1$doy)

  # Clean up
  DBI::dbDisconnect(s)
  file.remove("bcch.nc")
})



# format_zero_fill() ------------------------------------------------------

test_that("format_zero_fill() checks for columns", {
  t1 <- data.frame(species_id = 1,
                   ObservationCount = 1,
                   AllSpeciesReported = "Yes",
                   SamplingEventIdentifier = 2)
  expect_silent(format_zero_fill(t1))

  t1 <- data.frame(species_id = 1,
                   ObservationCount = 1,
                   AllSpeciesReported = "Yes")
  expect_error(format_zero_fill(t1), "'by' columns must be present in the data")

  t1 <- data.frame(species_id = 1,
                   ObservationCount = 1)
  expect_error(format_zero_fill(t1), "'AllSpeciesReported' must be present")

  t1 <- data.frame(species_id = 1)
  expect_error(format_zero_fill(t1),
               "'fill' column \\('ObservationCount'\\) is missing from the data")

  t1 <- data.frame()
  expect_error(format_zero_fill(t1), "'species_id' must be present")
})

test_that("format_zero_fill() checks AllIndividualsReported", {
  rc1 <- test_rc
  expect_silent(format_zero_fill(rc1, verbose = FALSE))
  rc1$AllSpeciesReported[3:7] <- "No"
  expect_error(format_zero_fill(rc1), "must be present and 'Yes'")
})

test_that("format_zero_fill() detects NA Sampling Events", {
  rc1 <- dplyr::filter(test_rc, AllSpeciesReported == "Yes")[1:100,]
  expect_silent(a <- format_zero_fill(rc1, verbose = FALSE))

  rc1$SamplingEventIdentifier[rc1$SamplingEventIdentifier == "RCBIOTABASE-5553-1"] <- NA
  expect_message(b <- format_zero_fill(rc1),
                 "There are missing values in 'by'")

  expect_equal(nrow(a), nrow(b))
  expect_true("RCBIOTABASE-5553-1" %in% a$SamplingEventIdentifier)
  expect_false("RCBIOTABASE-5553-1" %in% b$SamplingEventIdentifier)
})


test_that("format_zero_fill() adds zeros", {

  # No zeros to add
  t1 <- data.frame(SamplingEventIdentifier = c(1:10),
                   AllSpeciesReported = "Yes",
                   species_id = 1,
                   ObservationCount = "1")
  expect_message(format_zero_fill(t1), "Converted 'fill' column")

  # Add zeros
  t1 <- data.frame(SamplingEventIdentifier = c(1, 1, 1, 2, 2, 2, 1, 1, 1, 2, 2, 2),
                   AllSpeciesReported = "Yes",
                   species_id = c(1, 2, 2, 3, 3, 2, 1, 2, 2, 3, 3, 2),
                   ObservationCount = c(3, 1, 1, 6, 8, 4, 3, 1, 1, 6, 8, 4))
  texp <- dplyr::select(t1, -AllSpeciesReported) %>%
    rbind(data.frame(SamplingEventIdentifier = c(1, 2),
                     species_id = c(3, 1),
                     ObservationCount = 0)) %>%
    dplyr::arrange(SamplingEventIdentifier, species_id)
  expect_message(format_zero_fill(t1), "multiple observations") %>%
    expect_equal(texp)
})

test_that("format_zero_fill() adds zeros in real example", {

  # Expect missing events before fill, filled events after (for three species)
  rc2 <- dplyr::filter(test_rc,
                       CommonName %in% c("Monarch",
                                         "Black Swallowtail",
                                         "Red Admiral"),
                       AllSpeciesReported == "Yes")

  expect_false(all(dplyr::count(rc2, SamplingEventIdentifier)$n == 3))
  expect_silent(rc2_fill <- format_zero_fill(rc2, verbose = FALSE)) %>%
    expect_is("data.frame") %>%
    expect_length(3)
  expect_true(all(dplyr::count(rc2_fill, SamplingEventIdentifier)$n == 3))

  # Expect that keep SamplingEvents, but lose other species present
  rc1 <- dplyr::filter(test_rc,
                       CommonName %in% c("Monarch",
                                         "Black Swallowtail",
                                         "Red Admiral"),
                       AllSpeciesReported == "Yes")

  expect_equal(unique(rc1$species_id), c(252456, 252491, 252494))
  expect_silent(rc1_fill <- format_zero_fill(rc1, species = 19360,
                                             verbose = FALSE)) %>%
    expect_is("data.frame")
  expect_equal(unique(rc1_fill$species_id), 19360)
  expect_equal(nrow(rc1_fill),
               length(unique(rc1$SamplingEventIdentifier)))
  expect_equal(0, sum(rc1_fill$ObservationCount))
})

test_that("format_zero_fill() by", {
  rc1 <- dplyr::filter(test_rc,
                       CommonName %in% c("Monarch",
                                         "Black Swallowtail",
                                         "Red Admiral"),
                       AllSpeciesReported == "Yes")

  # Custom
  expect_silent(rc1_filled <- format_zero_fill(rc1, verbose = FALSE,
                                               by = c("collection",
                                                      "date",
                                                      "Locality",
                                                      "DurationInHours"))) %>%
    expect_named(c("collection", "date", "Locality", "DurationInHours",
                   "species_id", "ObservationCount"))
  expect_true(nrow(rc1_filled) > 0)

  # Custom warning
  expect_message(rc1_filled <- format_zero_fill(rc1,
                                                by = c("collection", "Locality")),
                 "multiple observations") %>%
    expect_named(c("collection", "Locality",
                   "species_id", "ObservationCount"))
  expect_true(nrow(rc1_filled) > 0)
})

test_that("format_zero_fill() different 'fill' columns", {
  rc1 <- dplyr::filter(test_rc,
                       CommonName %in% c("Monarch",
                                         "Black Swallowtail",
                                         "Red Admiral"),
                       AllSpeciesReported == "Yes") %>%
    dplyr::mutate(presence = as.numeric(ObservationCount > 0))

  # Different fill column
  expect_silent(rc1_filled <- format_zero_fill(rc1, fill = "presence")) %>%
    expect_named(c("SamplingEventIdentifier", "species_id", "presence"))
  expect_equal(unique(rc1_filled$presence), c(1, 0))
  expect_gt(nrow(rc1_filled), nrow(rc1))
})

test_that("format_zero_fill() extra species columns", {
  # only 6 original columns
  expect_silent(b1 <- format_zero_fill(test_rc, verbose = FALSE)) %>%
    expect_length(3)

  # Add two new
  expect_silent(b2 <- format_zero_fill(test_rc, verbose = FALSE,
                                       extra_species = c("CommonName", "Class"))) %>%
    expect_length(5)

  # But the rest is the same
  expect_true(all.equal(b1, dplyr::select(b2, names(b1))))

  # Doesn't add non-relevant columns
  expect_error(format_zero_fill(test_rc, extra_species = "species"),
               "Some 'extra_species' are not in the data")
  expect_message(format_zero_fill(test_rc, extra_species = "species_id")) %>%
    expect_equal(b1)
  expect_message(format_zero_fill(test_rc, extra_species = "record_id"),
                 "Ignoring 'extra_species' columns")
})


test_that("format_zero_fill() extra events columns", {

  expect_silent(b1 <- format_zero_fill(test_rc, verbose = FALSE))
  expect_length(b1, 3)

  # Add two new
  expect_silent(b2 <- format_zero_fill(test_rc, verbose = FALSE,
                                       extra_event = c("latitude", "longitude")))
  expect_length(b2, 5)

  # But the rest is the same
  expect_true(all.equal(b1, dplyr::select(b2, names(b1))))

  # Doesn't add non-existing columns
  expect_error(format_zero_fill(test_rc, extra_event = "test"),
               "Some 'extra_event' are not in the data")

  # Ignores non unique columns
  expect_message(format_zero_fill(test_rc, extra_event = "record_id"),
                 "Ignoring 'extra_event' columns")
})


test_that("format_zero_fill() with SQLite database", {
  bcch_sql <- nc_data_dl(collections = "RCBIOTABASE", species = 14280,
                         sql_db = "bcch", username = "testuser", info = "nc_test")
  hofi_sql <- nc_data_dl(collections = "RCBIOTABASE", species = 20350,
                         sql_db = "hofi", username = "testuser", info = "nc_test")

  # No zeros to add
  expect_message(b <- format_zero_fill(bcch_sql),
                 "Cannot work directly on SQLite database connections") %>%
    expect_is("data.frame")

  expect_true(dplyr::all_equal(
    b,
    dplyr::select(bcch, names(b)) %>%
      dplyr::mutate(ObservationCount = as.numeric(ObservationCount))))


  # All zeros to add
  db_insert(bcch_sql, table = "naturecounts",
            df = dplyr::tbl(hofi_sql, "naturecounts") %>%
              dplyr::collect())

  expect_message(b <- format_zero_fill(bcch_sql),
                 "Cannot work directly on SQLite database connections") %>%
    expect_is("data.frame")

  # All zero's to add
  expect_silent(b <- format_dates(rbind(bcch, hofi))) %>%
    expect_is("data.frame")
  expect_equal(nrow(b), nrow(bcch) + nrow(hofi))

  # Clean up
  DBI::dbDisconnect(bcch_sql)
  DBI::dbDisconnect(hofi_sql)
  file.remove(c("bcch.nc", "hofi.nc"))
})

test_that("format_zero_fill() checks for size", {
  temp <- expand.grid(species_id = 1:1100, SamplingEventIdentifier = 1:5001) %>%
    dplyr::mutate(ObservationCount = 1, AllSpeciesReported = "Yes")

    expect_error(format_zero_fill(temp), "You are trying to zero-fill over")
    expect_silent(format_zero_fill(temp, warn = FALSE))
})
