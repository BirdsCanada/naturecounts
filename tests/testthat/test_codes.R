context("Code searching and conversions")

test_that("Get codes", {

  expect_is(c1 <- codes_search(desc = "BCCH", type = "species"), "data.frame")
  expect_equal(nrow(c1), 1)
  expect_true(c1$species_code == 14280)
  expect_true(c1$species_alpha == "BCCH")

  expect_is(c1 <- codes_search(desc = "Belize", type = "country"), "data.frame")
  expect_equal(nrow(c1), 1)
  expect_true(c1$country_code == "BZ")
  expect_true(c1$country_name == "Belize")

  expect_is(c1 <- codes_search(desc = "Colorado", type = "statprov"), "data.frame")
  expect_equal(nrow(c1), 1)
  expect_true(c1$country_code == "US")
  expect_true(c1$statprov_code == "CO")
  expect_true(c1$statprov_name == "Colorado")
})

test_that("Accents ignored in search", {
  expect_is(c1 <- codes_search(desc = "Yucatan", type = "statprov"), "data.frame")
  expect_equal(nrow(c1), 1)
  expect_true(c1$country_code == "MX")
  expect_true(c1$statprov_code == "YP")
  expect_true(c1$statprov_name == "Yucatán")

  expect_is(c1 <- codes_search(desc = "Yucatán", type = "statprov"), "data.frame")
  expect_equal(nrow(c1), 1)
  expect_true(c1$country_code == "MX")
  expect_true(c1$statprov_code == "YP")
  expect_true(c1$statprov_name == "Yucatán")
})

test_that("Search for multiple matches", {
  expect_silent(c <- codes_search(c("BCCH", "BDOW"), type = "species"))
  expect_equal(nrow(c), 2)

  expect_silent(c <- codes_search(c("Ontario", "British Columbia"),
                                  type = "statprov"))
  expect_equal(nrow(c), 2)

  expect_silent(c <- codes_search(c("Colombia", "Canada"),
                                  type = "country"))
  expect_equal(nrow(c), 2)
})

test_that("Code conversion works as expected", {
  expect_equal(codes_convert("BDOW", "species"), 7590)
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
  expect_silent(codes_check(species)) %>%
    expect_is("numeric") %>%
    expect_equal(14280)

  species <- c("BCCH", "BDOW")
  expect_silent(codes_check(species)) %>%
    expect_is("numeric") %>%
    expect_equal(c(14280, 7590))

  species <- "Dodo"
  expect_error(codes_check(species), "Unable to match 'Dodo' to any codes")

  species <- "CH"
  expect_error(codes_check(species), "Matched 'CH' to [0-9]{1,3} codes: \n")

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

