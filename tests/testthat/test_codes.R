context("Code searching and conversions")

test_that("Get codes", {

  expect_is(c1 <- location_search("Belize", type = "country"), "data.frame")
  expect_equal(nrow(c1), 1)
  expect_true(c1$country_code == "BZ")
  expect_true(c1$country_name == "Belize")

  expect_is(c1 <- location_search("Colorado", type = "statprov"), "data.frame")
  expect_equal(nrow(c1), 1)
  expect_true(c1$country_code == "US")
  expect_true(c1$statprov_code == "CO")
  expect_true(c1$statprov_name == "Colorado")
})

test_that("Accents ignored in search", {
  expect_is(c1 <- location_search("Yucatan", type = "statprov"), "data.frame")
  expect_equal(nrow(c1), 1)
  expect_true(c1$country_code == "MX")
  expect_true(c1$statprov_code == "YP")
  expect_true(c1$statprov_name == "Yucatán")

  expect_is(c1 <- location_search("Yucatán", type = "statprov"), "data.frame")
  expect_equal(nrow(c1), 1)
  expect_true(c1$country_code == "MX")
  expect_true(c1$statprov_code == "YP")
  expect_true(c1$statprov_name == "Yucatán")
})

test_that("Underscores replaced with space", {
  expect_is(c1 <- location_search("British Columbia", type = "statprov"), "data.frame")
  expect_equal(nrow(c1), 1)
  expect_true(c1$country_code == "CA")
  expect_true(c1$statprov_code == "BC")
  expect_true(c1$statprov_name == "British Columbia")

  expect_is(c1 <- location_search("British-Columbia", type = "statprov"), "data.frame")
  expect_equal(nrow(c1), 1)
  expect_true(c1$country_code == "CA")
  expect_true(c1$statprov_code == "BC")
  expect_true(c1$statprov_name == "British Columbia")
})

test_that("Search for multiple matches", {
  expect_silent(c <- location_search(c("Ontario", "British Columbia"),
                                  type = "statprov"))
  expect_equal(nrow(c), 2)

  expect_silent(c <- location_search(c("Colombia", "Canada"),
                                  type = "country"))
  expect_equal(nrow(c), 2)
})

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

