context("Checks")

test_that("Code conversion works as expected", {
  expect_equal(codes_convert("Ontario", "statprov"), "ON")
  expect_equal(codes_convert("Canada", "country"), "CA")
})

test_that("Code checks work on species", {
  expect_silent(codes_check(NULL)) %>%
    expect_null()

  species <- 7590
  expect_silent(codes_check(species)) %>%
    expect_is("numeric") %>%
    expect_equal(species)

  species <- "7590"
  expect_silent(codes_check(species)) %>%
    expect_is("numeric") %>%
    expect_equal(as.numeric(species))

  species <- "BCCH"
  expect_error(codes_check(species), "'species' code must be a numeric code")

  species <- c("BCCH", "BDOW")
  expect_error(codes_check(species), "'species' code must be a numeric code")

  species <- "Dodo"
  expect_error(codes_check(species), "'species' code must be a numeric code")

})

test_that("Code checks work on country", {
  expect_null(codes_check(NULL))

  country <- "CA"
  expect_silent(codes_check(country)) %>%
    expect_is("character") %>%
    expect_equal(country)

  country <- "Ca"
  expect_silent(codes_check(country)) %>%
    expect_is("character") %>%
    expect_equal(toupper(country))

  country <- "Canad"
  expect_silent(codes_check(country)) %>%
    expect_is("character") %>%
    expect_equal("CA")

  country <- c("Canada", "Colom")
  expect_silent(codes_check(country)) %>%
    expect_is("character") %>%
    expect_equal(c("CA", "CO"))

  country <- "Can"
  expect_error(codes_check(country), "Matched 'Can' to [0-9]{1,2} codes: \n")

  country <- "England"
  expect_error(codes_check(country), "Unable to match 'England' to any codes")
})
