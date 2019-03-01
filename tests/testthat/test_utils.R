context("Utility functions")

test_that("as_numeric() converts to numeric if possible", {
  expect_is(as_numeric(13), "numeric")
  expect_is(as_numeric("13"), "numeric")
  expect_is(as_numeric("aa"), "character")
})

test_that("capture_df() capture dataframe", {
  expect_silent(capture_df(species_codes)) %>%
    expect_is("character") %>%
    expect_length(1) %>%
    expect_match("\\.\\.\\.")

  expect_silent(capture_df(species_codes[1:3,])) %>%
    expect_is("character") %>%
    expect_length(1) %>%
    expect_match("[a-zA-Z ]+$")

  expect_silent(capture_df(country_codes)) %>%
    expect_is("character") %>%
    expect_length(1) %>%
    expect_match("\\.\\.\\.")

  expect_silent(capture_df(country_codes[1:3,])) %>%
    expect_is("character") %>%
    expect_length(1) %>%
    expect_match("[a-zA-Z ]+$")
})