context("Code searching and conversions")

test_that("Get codes", {

  expect_silent(c1 <- codes_filter(df = sp_codes,
                                   columns = "species_alpha",
                                   desc = "BCCH",
                                   code_column = "species_code"))
  expect_is(c1, "data.frame")
  expect_equal(nrow(c1), 1)
  expect_true(c1$species_code == 14280)
  expect_true(c1$species_alpha == "BCCH")

  expect_silent(c1 <- codes_filter(
    df = country_statprov_codes,
    columns = c("country_name", "country_name_fr"),
    desc = "Belize",
    code_column = "country_code"))

  expect_is(c1, "data.frame")
  expect_equal(nrow(c1), 1)
  expect_true(c1$country_code == "BZ")
  expect_true(c1$country_name == "Belize")

  expect_silent(c1 <- codes_filter(
    df = country_statprov_codes,
    columns = c("statprov_name", "statprov_name_fr", "statprov_name_es"),
    desc = "Colorado",
    code_column = "statprov_code"))

  expect_is(c1, "data.frame")
  expect_equal(nrow(c1), 1)
  expect_true(c1$country_code == "US")
  expect_true(c1$statprov_code == "CO")
  expect_true(c1$statprov_name == "Colorado")
})

test_that("Accents ignored in search", {
  expect_silent(c1 <- codes_filter(
    df = country_statprov_codes,
    columns = c("statprov_name", "statprov_name_fr", "statprov_name_es"),
    desc = "Yucatan",
    code_column = "statprov_code"))

  expect_is(c1, "data.frame")
  expect_equal(nrow(c1), 1)
  expect_true(c1$country_code == "MX")
  expect_true(c1$statprov_code == "YP")
  expect_true(c1$statprov_name == "Yucatán")

  expect_silent(c1 <- codes_filter(
    df = country_statprov_codes,
    columns = c("statprov_name", "statprov_name_fr", "statprov_name_es"),
    desc = "Yucatán",
    code_column = "statprov_code"))

  expect_is(c1, "data.frame")
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
  expect_equal(nrow(c), 14)
})

test_that("Helper functions works as expected", {
  expect_silent(c <- codes_search("Argentina"))
  expect_silent(c <- codes_search("Northwest", type = "statprov"))
  expect_silent(c <- codes_search("BDOW", type = "species"))
})

test_that("Code conversion works as expected", {
  expect_silent(c <- codes_convert("BDOW", "species"))
  expect_equal(c, 7590)

  expect_silent(c <- codes_convert("Ontario", "statprov"))
  expect_equal(c, "ON")

  expect_silent(c <- codes_convert("Canada", "country"))
  expect_equal(c, "CA")

})
