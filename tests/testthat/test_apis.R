context("APIs")

test_that("metadata apis are accessible", {

  expect_silent(srv_query("metadata", "bmde_versions")) # Doesn't need lang
  expect_silent(srv_query("metadata", "bmde_versions",
                          query = list("lang" = "EN"))) # But can have one


  expect_silent(srv_query("metadata", "bmde_fields",
                          query = list(lang = "EN", version = "BMDE2.00")))

  # needs a language specification
  expect_silent(srv_query("metadata", "projects",
                          query = list("lang" = "EN")))

  # needs a language specification (but doesn't matter which)
  expect_silent(srv_query("metadata", "projects",
                          query = list("lang" = "FR")))

  expect_silent(srv_query("metadata", "projects_metadata",
                          query = list("lang" = "EN"))) # needs a language specification

  expect_silent(srv_query("metadata", "collections",
                          query = list("lang" = "EN"))) # needs a language specification

  expect_silent(srv_query("metadata", "species_codes_authority"))

  expect_silent(srv_query("metadata", "species_codes"))
  expect_silent(srv_query("metadata", "species_codes", query = list(authority = "BSCDATA")))
  expect_silent(srv_query("metadata", "species_codes", query = list(authority = "")))

  expect_silent(srv_query("metadata", "country"))
  expect_silent(srv_query("metadata", "statprov"))
  expect_silent(srv_query("metadata", "subnat2"))
})

test_that("species metadata", {
  expect_silent(s <- srv_query("metadata", "species"))
})

test_that("data retrieval", {

})

