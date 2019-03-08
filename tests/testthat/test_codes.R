context("Code searches")



# Codes search function ---------------------------------------------------

test_that("Accents ignored in location search", {
  expect_silent(c1 <- codes_search(
    "Yucatan", df = statprov_codes(),
    code_column = "statprov_code",
    columns = stringr::str_subset(names(statprov_codes()), "_name"))) %>%
    expect_is("data.frame")

  expect_equal(nrow(c1), 1)
  expect_true(c1$country_code == "MX")
  expect_true(c1$statprov_code == "YP")
  expect_true(c1$statprov_name == "Yucatán")

  expect_silent(c1 <- codes_search(
    "Yucatán", df = statprov_codes(),
    code_column = "statprov_code",
    columns = stringr::str_subset(names(statprov_codes()), "_name"))) %>%
    expect_is("data.frame")

  expect_equal(nrow(c1), 1)
  expect_true(c1$country_code == "MX")
  expect_true(c1$statprov_code == "YP")
  expect_true(c1$statprov_name == "Yucatán")
})

test_that("Underscores replaced with space", {

  expect_silent(c1 <- codes_search(
    "Black-capped chickadee", df = species_taxonomy(),
    code_column = "species_id",
    columns = "english_name")) %>%
    expect_is("data.frame")

  expect_equal(c1$species_id[1], 14280)

  expect_silent(c1 <- codes_search(
    "Black capped chickadee", df = species_taxonomy(),
    code_column = "species_id",
    columns = "english_name")) %>%
    expect_is("data.frame")

  expect_equal(c1$species_id[1], 14280)

})

test_that("Search for multiple matches", {
  expect_silent(c <- location_search(c("Ontario", "British Columbia"),
                                     type = "statprov"))
  expect_equal(nrow(c), 2)

  expect_silent(c <- location_search(c("Colombia", "Canada"),
                                     type = "country"))
  expect_equal(nrow(c), 2)
})



# Species search ----------------------------------------------------------
test_that("Search for species codes by name", {

  expect_silent(species_search()) %>%
    expect_equal(species_taxonomy() %>%
                   dplyr::select(species_id, "scientific_name", "english_name",
                                 "french_name", "taxon_group"))

  expect_silent(species_search(show = "all")) %>%
    expect_equal(species_taxonomy())

  expect_silent(s1 <- species_search(authority = "BSCDATA")) %>%
    expect_is("data.frame")
  expect_true("BSCDATA" %in% names(s1))

  expect_silent(s2 <- species_search(show = "all", authority = "BSCDATA")) %>%
    expect_is("data.frame")
  expect_true("BSCDATA" %in% names(s2))

  expect_gt(length(s2), length(s1))

  expect_silent(s1 <- species_search("moose")) %>%
    expect_is("data.frame")
  expect_gte(nrow(s1), 1)
  expect_gte(ncol(s1), 1)

  expect_silent(s2 <- species_search("chickadee", authority = "BSCDATA")) %>%
    expect_is("data.frame")
  expect_gte(nrow(s2), 1)
  expect_gte(ncol(s2), 1)
  expect_true("BSCDATA" %in% names(s2))
})


# Species code search -------------------------------------------------------
test_that("Species code returns all with no code", {

  expect_silent(species_code_search() %>%
                  dplyr::arrange(.data$species_id, .data$BSCDATA)) %>%
    expect_equal(species_codes() %>%
                   dplyr::filter(!is.na(!!rlang::sym("BSCDATA"))) %>%
                   dplyr::select(species_id = "species_id2", "BSCDATA") %>%
                   dplyr::left_join(species_taxonomy() %>%
                                      dplyr::select("species_id",
                                                    "scientific_name",
                                                    "english_name",
                                                    "french_name"),
                                    by = "species_id") %>%
                   dplyr::arrange(.data$species_id, .data$BSCDATA))

  expect_silent(species_search(show = "all")) %>%
    expect_equal(species_taxonomy())

  expect_silent(s1 <- species_search(authority = "BSCDATA")) %>%
    expect_is("data.frame")
  expect_true("BSCDATA" %in% names(s1))

  expect_silent(s2 <- species_search(show = "all", authority = "BSCDATA")) %>%
    expect_is("data.frame")
  expect_true("BSCDATA" %in% names(s2))

  expect_gt(length(s2), length(s1))

})

test_that("Species code returns id", {
  expect_silent(s1 <- species_code_search("BCCH")) %>%
    expect_is("data.frame")
  expect_gte(nrow(s1), 1)
  expect_gte(ncol(s1), 1)

  expect_silent(s2 <- species_code_search(8868, authority = "CBC")) %>%
    expect_is("data.frame")
  expect_gte(nrow(s2), 1)
  expect_gte(ncol(s2), 1)
  expect_equal(s1$species_id, s2$species_id)

  expect_silent(s3 <- species_code_search("BCCH",
                                          authority = c("CBC", "BSCDATA"))) %>%
    expect_is("data.frame")
  expect_gte(nrow(s3), 1)
  expect_gte(ncol(s3), 1)
  expect_equal(s2$species_id, s3$species_id)
})

test_that("Species code exact vs. all", {
  expect_silent(s1 <- species_code_search("DEJU")) %>%
    expect_is("data.frame")
  expect_gte(nrow(s1), 1)
  expect_gte(ncol(s1), 1)
  expect_equal(s1$BSCDATA, c("DEJU", "SCJU", "ORJU", "PSJU"))

  expect_silent(s1 <- species_code_search("DEJU", results = "exact")) %>%
    expect_is("data.frame")
  expect_equal(nrow(s1), 1)
  expect_gte(ncol(s1), 1)
  expect_equal(s1$BSCDATA, "DEJU")

  expect_silent(s1 <- species_code_search("MYWA")) %>%
    expect_is("data.frame")
  expect_gte(nrow(s1), 1)
  expect_gte(ncol(s1), 1)
  expect_equal(s1$BSCDATA, c("YRWA", "MYWA", "AUWA"))

  expect_silent(s1 <- species_code_search("MYWA", results = "exact")) %>%
    expect_is("data.frame")
  expect_equal(nrow(s1), 1)
  expect_gte(ncol(s1), 1)
  expect_equal(s1$BSCDATA, "MYWA")

})

# Locations search --------------------------------------------------------
test_that("Search for location codes", {

  expect_silent(location_search(type = "country")) %>%
    expect_equal(country_codes())

  expect_silent(location_search(type = "statprov")) %>%
    expect_equal(statprov_codes())

  expect_silent(location_search(type = "subnat")) %>%
    expect_equal(subnat_codes())

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
