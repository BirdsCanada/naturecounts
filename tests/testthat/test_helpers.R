context("Helper/Formating functions")

test_that("format_dates() with data frame", {
  for(i in 1:2) {
    if(i == 1) i <- bcch else i <- bdow
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
  for(i in 1:2) {
    if(i == 1) {
      i <- nc_data_dl(collections = "RCBIOTABASE", species = 14280,
                      sql_db = "bcch", username = "sample")
    } else if(i == 2) {
      i <- nc_data_dl(collections = "RCBIOTABASE", species = 7590,
                      sql_db = "bdow", username = "sample")
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
  file.remove(c("bcch.nc", "bdow.nc"))
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
                  sql_db = "bcch", username = "sample")
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

test_that("format_zero_fill() with data frame", {

  # No zeros to add
  bcch_dates <- format_dates(bcch)

  expect_message(b <- format_zero_fill(bcch_dates),
                 "Converted 'fill' column") %>%
    expect_is("data.frame") %>%
    expect_length(6)
  expect_true(dplyr::all_equal(
    b, dplyr::select(bcch_dates, names(b)) %>%
      dplyr::mutate(ObservationCount = as_numeric(ObservationCount))))

  # Expect two species for each combo
  both_dates <- format_dates(rbind(bcch, bdow))
  expect_false(all(dplyr::count(both_dates, SamplingEventIdentifier, project_id,
                                collection, date)$n == 2))
  expect_silent(b <- format_zero_fill(both_dates, verbose = FALSE)) %>%
    expect_is("data.frame") %>%
    expect_length(6)
  expect_true(all(dplyr::count(b, SamplingEventIdentifier, project_id,
                               collection, date)$n == 2))
})

test_that("format_zero_fill() single missing species", {
  both_dates <- format_dates(rbind(bcch, bdow))
  expect_equal(unique(both_dates$species_id), c(14280, 7590))
  expect_silent(b <- format_zero_fill(both_dates, species = 19360,
                                      verbose = FALSE)) %>%
    expect_is("data.frame")
  expect_equal(nrow(b),
               both_dates %>%
                 dplyr::select(project_id, collection, date,
                               SamplingEventIdentifier) %>%
                 dplyr::distinct() %>%
                 nrow())
  expect_equal(unique(b$species_id), 19360)

})

test_that("format_zero_fill() by", {
  both_dates <- format_dates(rbind(bcch, bdow)) %>%
    dplyr::filter(!is.na(DurationInHours)) # Omit odd duplicates for testing

  # Event
  expect_silent(format_zero_fill(both_dates, by = "event", verbose = FALSE)) %>%
    expect_named(c("project_id", "collection", "date", "SamplingEventIdentifier",
                   "species_id", "ObservationCount"))

  # Custom
  expect_silent(b <- format_zero_fill(both_dates, verbose = FALSE,
                                      by = c("collection", "date",
                                             "Locality", "DurationInHours"))) %>%
    expect_named(c("collection", "date", "Locality", "DurationInHours",
                   "species_id", "ObservationCount"))
  expect_true(nrow(b) > 0)

  # Custom warning
  expect_warning(b <- format_zero_fill(both_dates,
                                      by = c("collection", "Locality")),
                 "There is more than one observation per species") %>%
    expect_named(c("collection", "Locality",
                   "species_id", "ObservationCount"))
  expect_true(nrow(b) > 0)
})

test_that("format_zero_fill() different 'fill' columns", {
  both_dates <- format_dates(rbind(bcch, bdow)) %>%
    dplyr::mutate(presence = as.numeric(ObservationCount > 0))

  # Different fill column
  expect_silent(b1 <- format_zero_fill(both_dates, fill = "presence")) %>%
    expect_named(c("project_id", "collection", "date", "SamplingEventIdentifier",
                   "species_id", "presence"))
  expect_equal(unique(b1$presence), c(1, 0))
  expect_gt(nrow(b1), nrow(bcch))
  expect_gt(nrow(b1), nrow(bdow))

  # diff column but same result
  expect_silent(b2 <- format_zero_fill(both_dates, verbose = FALSE)) %>%
    expect_named(c("project_id", "collection", "date", "SamplingEventIdentifier",
                   "species_id", "ObservationCount"))
  expect_equal(nrow(b1), nrow(b2))
})

test_that("format_zero_fill() extra columns", {
  both_dates <- format_dates(rbind(bcch, bdow))

  # only 6 original columns
  expect_silent(b1 <- format_zero_fill(both_dates, verbose = FALSE)) %>%
    expect_length(6)

  # Add two new
  expect_silent(b2 <- format_zero_fill(both_dates, verbose = FALSE,
                                       extra_cols = c("doy", "Locality"))) %>%
    expect_length(8)

  # But the rest is the same
  expect_true(all.equal(b1, dplyr::select(b2, names(b1))))

  # Doesn't add non-relevant columns
  expect_error(format_zero_fill(both_dates, extra_cols = "species"),
               "Some 'extra_cols' are not in the data")
  expect_error(format_zero_fill(both_dates, extra_cols = "species_id"),
               "'species_id' cannot be in 'extra_cols'")
  expect_message(format_zero_fill(both_dates, extra_cols = "record_id"),
                 "Ignoring 'extra_cols' columns not uniquely associated")
})


test_that("format_zero_fill() with SQLite database", {
  bcch_sql <- nc_data_dl(collections = "RCBIOTABASE", species = 14280,
                         sql_db = "bcch", username = "sample") %>%
    format_dates()
  bdow_sql <- nc_data_dl(collections = "RCBIOTABASE", species = 7590,
                         sql_db = "bdow", username = "sample") %>%
    format_dates()

  # No zeros to add
  expect_message(b <- format_zero_fill(bcch_sql),
                 "Cannot work directly on SQLite database connections") %>%
    expect_is("data.frame")

  expect_true(dplyr::all_equal(
    dplyr::mutate(b, date = lubridate::as_date(date)),
    dplyr::select(format_dates(bcch), names(b)) %>%
      dplyr::mutate(ObservationCount = as.numeric(ObservationCount))))


  # All zeros to add
  db_insert(bcch_sql, table = "naturecounts",
            df = dplyr::tbl(bdow_sql, "naturecounts") %>%
              dplyr::collect())

  expect_message(b <- format_zero_fill(bcch_sql),
                 "Cannot work directly on SQLite database connections") %>%
    expect_is("data.frame")

  # All zero's to add
  expect_silent(b <- format_dates(rbind(bcch, bdow))) %>%
    expect_is("data.frame")
  expect_equal(nrow(b), nrow(bcch) + nrow(bdow))

  # Clean up
  DBI::dbDisconnect(bcch_sql)
  DBI::dbDisconnect(bdow_sql)
  file.remove(c("bcch.nc", "bdow.nc"))
})

test_that("format_zero_fill() error checks", {

})
