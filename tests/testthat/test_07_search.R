context("Code searches")



# Codes search function ---------------------------------------------------

test_that("Accents ignored in region search", {
  expect_silent(c1 <- search_codes(
    "Yucatan", df = meta_statprov_codes(),
    code_column = "statprov_code",
    columns = stringr::str_subset(names(meta_statprov_codes()), "_name"))) %>%
    expect_is("data.frame")

  expect_equal(nrow(c1), 1)
  expect_true(c1$country_code == "MX")
  expect_true(c1$statprov_code == "YP")
  expect_true(c1$statprov_name == "Yucatán")

  expect_silent(c1 <- search_codes(
    "Yucatán", df = meta_statprov_codes(),
    code_column = "statprov_code",
    columns = stringr::str_subset(names(meta_statprov_codes()), "_name"))) %>%
    expect_is("data.frame")

  expect_equal(nrow(c1), 1)
  expect_true(c1$country_code == "MX")
  expect_true(c1$statprov_code == "YP")
  expect_true(c1$statprov_name == "Yucatán")
})

test_that("Underscores replaced with space", {

  expect_silent(c1 <- search_codes(
    "Black-capped chickadee", df = meta_species_taxonomy(),
    code_column = "species_id",
    columns = "english_name")) %>%
    expect_is("data.frame")

  expect_equal(c1$species_id[1], 14280)

  expect_silent(c1 <- search_codes(
    "Black capped chickadee", df = meta_species_taxonomy(),
    code_column = "species_id",
    columns = "english_name")) %>%
    expect_is("data.frame")

  expect_equal(c1$species_id[1], 14280)

})

test_that("Search for multiple matches", {
  expect_silent(c <- search_region(c("Ontario", "British Columbia"),
                                     type = "statprov"))
  expect_equal(nrow(c), 2)

  expect_silent(c <- search_region(c("Colombia", "Canada"),
                                     type = "country"))
  expect_equal(nrow(c), 2)
})



# Species search ----------------------------------------------------------
test_that("Search for species codes by name", {

  expect_silent(search_species()) %>%
    expect_equal(meta_species_taxonomy() %>%
                   dplyr::select(species_id, "scientific_name", "english_name",
                                 "french_name", "taxon_group"))

  expect_silent(search_species(show = "all")) %>%
    expect_equal(meta_species_taxonomy())

  expect_silent(s1 <- search_species(authority = "BSCDATA")) %>%
    expect_is("data.frame")
  expect_true("BSCDATA" %in% names(s1))

  expect_silent(s2 <- search_species(show = "all", authority = "BSCDATA")) %>%
    expect_is("data.frame")
  expect_true("BSCDATA" %in% names(s2))

  expect_gt(length(s2), length(s1))

  expect_silent(s1 <- search_species("moose")) %>%
    expect_is("data.frame")
  expect_gte(nrow(s1), 1)
  expect_gte(ncol(s1), 1)

  expect_silent(s2 <- search_species("chickadee", authority = "BSCDATA")) %>%
    expect_is("data.frame")
  expect_gte(nrow(s2), 1)
  expect_gte(ncol(s2), 1)
  expect_true("BSCDATA" %in% names(s2))
})


# Species code search -------------------------------------------------------
test_that("Species code returns all with no code", {

  expect_silent(search_species_code() %>%
                  dplyr::arrange(.data$species_id, .data$BSCDATA)) %>%
    expect_equal(meta_species_codes() %>%
                   dplyr::filter(.data$authority == "BSCDATA") %>%
                   dplyr::select(species_id = "species_id2", "BSCDATA" = "species_code") %>%
                   dplyr::left_join(meta_species_taxonomy() %>%
                                      dplyr::select("species_id",
                                                    "scientific_name",
                                                    "english_name",
                                                    "french_name"),
                                    by = "species_id") %>%
                   dplyr::arrange(.data$species_id, .data$BSCDATA))

  expect_silent(search_species(show = "all")) %>%
    expect_equal(meta_species_taxonomy())

  expect_silent(s1 <- search_species(authority = "BSCDATA")) %>%
    expect_is("data.frame")
  expect_true("BSCDATA" %in% names(s1))

  expect_silent(s2 <- search_species(show = "all", authority = "BSCDATA")) %>%
    expect_is("data.frame")
  expect_true("BSCDATA" %in% names(s2))

  expect_gt(length(s2), length(s1))

})

test_that("Species code returns id", {
  expect_silent(s1 <- search_species_code("BCCH")) %>%
    expect_is("data.frame")
  expect_gte(nrow(s1), 1)
  expect_gte(ncol(s1), 1)

  expect_silent(s2 <- search_species_code(8868, authority = "CBC")) %>%
    expect_is("data.frame")
  expect_gte(nrow(s2), 1)
  expect_gte(ncol(s2), 1)
  expect_equal(s1$species_id, s2$species_id)

  expect_silent(s3 <- search_species_code("BCCH",
                                          authority = c("CBC", "BSCDATA"))) %>%
    expect_is("data.frame")
  expect_gte(nrow(s3), 1)
  expect_gte(ncol(s3), 1)
  expect_equal(s2$species_id, s3$species_id)
})

test_that("Species code exact vs. all", {
  expect_silent(s1 <- search_species_code("DEJU")) %>%
    expect_is("data.frame")
  expect_gte(nrow(s1), 1)
  expect_gte(ncol(s1), 1)
  expect_equal(sort(s1$BSCDATA), sort(c("DEJU", "SCJU", "ORJU", "PSJU")))

  expect_silent(s1 <- search_species_code("DEJU", results = "exact")) %>%
    expect_is("data.frame")
  expect_equal(nrow(s1), 1)
  expect_gte(ncol(s1), 1)
  expect_equal(s1$BSCDATA, "DEJU")

  expect_silent(s1 <- search_species_code("MYWA")) %>%
    expect_is("data.frame")
  expect_gte(nrow(s1), 1)
  expect_gte(ncol(s1), 1)
  expect_equal(s1$BSCDATA, c("YRWA", "MYWA", "AUWA"))

  expect_silent(s1 <- search_species_code("MYWA", results = "exact")) %>%
    expect_is("data.frame")
  expect_equal(nrow(s1), 1)
  expect_gte(ncol(s1), 1)
  expect_equal(s1$BSCDATA, "MYWA")

})

# Regions search --------------------------------------------------------
test_that("Search for region codes", {

  expect_silent(search_region(type = "country")) %>%
    expect_equal(meta_country_codes())

  expect_silent(search_region(type = "statprov")) %>%
    expect_equal(meta_statprov_codes())

  expect_silent(search_region(type = "subnational2")) %>%
    expect_equal(meta_subnational2_codes())

  expect_silent(search_region(type = "iba")) %>%
    expect_equal(meta_iba_codes())

  expect_silent(search_region(type = "bcr")) %>%
    expect_equal(meta_bcr_codes())

  expect_is(r <- search_region("Belize", type = "country"), "data.frame")
  expect_equal(nrow(r), 1)
  expect_equal(r$country_code, "BZ")
  expect_equal(r$country_name, "Belize")

  expect_is(r <- search_region("Colorado", type = "statprov"), "data.frame")
  expect_equal(nrow(r), 1)
  expect_equal(r$country_cod, "US")
  expect_equal(r$statprov_code, "CO")
  expect_equal(r$statprov_name, "Colorado")

  expect_is(r <- search_region("Brandon", type = "subnational2"), "data.frame")
  expect_equal(nrow(r), 1)
  expect_equal(r$country_code, "CA")
  expect_equal(r$statprov_code, "MB")
  expect_equal(r$subnational2_code, "CA.MB.07")

  expect_is(r <- search_region("Chappice", type = "iba"), "data.frame")
  expect_equal(nrow(r), 1)
  expect_equal(r$statprov, "AB")
  expect_equal(r$iba_site, "AB112")
  expect_equal(r$iba_name, "Chappice Lake")

  expect_is(r <- search_region("Boreal Taiga", type = "bcr"), "data.frame")
  expect_equal(nrow(r), 1)
  expect_equal(r$bcr, 6)
  expect_equal(r$bcr_name, "Boreal Taiga Plains")
})
