test_that("metadata apis are accessible", {

  expect_silent(srv_query(api$bmde_versions)) # Doesn't need lang
  expect_silent(srv_query(api$bmde_versions,
                          query = list("lang" = "EN"))) # But can have one


  expect_silent(srv_query(api$bmde_fields,
                          query = list(lang = "EN", version = "BMDE2.00")))

  # needs a language specification
  expect_silent(srv_query(api$projects,
                          query = list("lang" = "EN")))

  # needs a language specification (but doesn't matter which)
  expect_silent(srv_query(api$projects,
                          query = list("lang" = "FR")))

  expect_silent(srv_query(api$projects_meta,
                          query = list("lang" = "EN"))) # needs a language specification

  expect_silent(srv_query(api$collections,
                          query = list("lang" = "EN"))) # needs a language specification

  expect_silent(srv_query(api$species_authority))

  expect_silent(srv_query(api$species_codes))
  expect_silent(srv_query(api$species_codes, query = list(authority = "BSCDATA")))
  expect_silent(srv_query(api$species_codes, query = list(authority = "")))

  expect_silent(srv_query(api$country_codes))
  expect_silent(srv_query(api$statprov_codes))
  expect_silent(srv_query(api$subnational2_codes))
})

test_that("species metadata", {
  expect_silent(s <- srv_query(api$species_taxonomy))
})
