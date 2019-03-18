context("Checks")

test_that("Code conversion works as expected", {
  expect_equal(codes_convert("Ontario", "statprov"), "ON")
  expect_equal(codes_convert("Canada", "country"), "CA")
})

test_that("Code checks work on species", {

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

test_that("fields_set_check correct", {
  expect_silent(fields_set_check("minimum"))
  expect_silent(fields_set_check("extended"))
  expect_silent(fields_set_check("custom"))
  expect_silent(fields_set_check("BMDE-BAND-2.00"))
  expect_error(fields_set_check("BMDE"))
})

test_that("fields_check correct", {
  expect_silent(fields_check("AllSpeciesReported"))
})

test_that("year_check correct", {
  expect_silent(year_check(2000)) %>%
    expect_is("numeric")

  expect_silent(year_check("2000")) %>%
    expect_is("numeric")

  er <- paste0("Years must be numbers between 1900 and ", lubridate::year(Sys.Date()))
  expect_error(year_check("hello"), er)
  expect_error(year_check(1899), er)
  expect_error(year_check(2050), er)
})

test_that("doy_check correct", {
  expect_silent(doy_check(1)) %>% expect_equal(1)
  expect_silent(doy_check("1")) %>% expect_equal(1)
  expect_silent(doy_check("2010-04-01")) %>% expect_equal(91)
  expect_silent(doy_check("2010-04")) %>% expect_equal(91)

  er <- paste0("Day of year must be either a date \\(YM or YMD\\), ",
               "or a whole number \\(1-366\\)")
  expect_error(doy_check(-300), er)
  expect_error(doy_check(367), er)
  expect_error(doy_check(4.5), er)
  expect_error(doy_check(2010), er)
  expect_error(doy_check("2010"), er)
  expect_error(doy_check("hello"), er)
})

test_that("filter_checks correct", {
  expect_equal(filter_check(list(country = "Canada")), list(country = "CA"))
  expect_equal(filter_check(list(statprov = "Manitoba")), list(statprov = "MB"))
  expect_error(filter_check(list(species = "BCCH")))
  expect_equal(filter_check(list(start_year = "2011")), list(start_year = 2011))
  expect_error(filter_check(list(end_year = "1811")))

  f <- list(collections = "RCBIOTABASE", species = 14280,
            country = "CA", statprov = "ON", subnational2 = "CA.MB.07")
  expect_equal(filter_check(f), f)

  f <- list(collections = 45, species = 14280,
            country = "CA", statprov = "ON", subnational2 = "CA.MB.07")
  expect_error(filter_check(f), "'collections' must be either")

})

test_that("redundancy_checks correct", {

  # country/statprov/subnational2 redundancy
  f <- list(collections = "RCBIOTABASE", species = 14280,
            region = list(country = "CA",
                          statprov = "ON",
                          subnational2 = "CA.MB.07"))
  f2 <- f
  f2$region[c('country', 'statprov')] <- list(NULL)
  expect_message(redundancy_check(f), "keeping only 'subnational2'") %>%
    expect_equal(f2)

  # fields and fields_set != "custom" redundancy
  f <- list(collections = "RCBIOTABASE", species = 14280,
            fields_set = "BMDE2.00-min", fields = "CollectionYear")
  f2 <- f
  f2['fields'] <- list(NULL)
  expect_message(redundancy_check(f), "Ignoring 'fields' argument") %>%
    expect_equal(f2)

})
