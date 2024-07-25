test_that("as_numeric() converts to numeric if possible", {
  expect_type(as_numeric(13), "double")
  expect_type(as_numeric("13"), "double")
  expect_type(as_numeric("aa"), "character")
  expect_type(as_numeric(c(NA, "13")), "double")
})

test_that("capture_df() capture dataframe", {
  expect_silent(capture_df(meta_species_codes())) %>%
    expect_type("character") %>%
    expect_length(1) %>%
    expect_match("\\.\\.\\.")

  expect_silent(capture_df(meta_species_codes()[1:3,])) %>%
    expect_type("character") %>%
    expect_length(1) %>%
    expect_match("[a-zA-Z <>0-9]+$")

  expect_silent(capture_df(meta_country_codes())) %>%
    expect_type("character") %>%
    expect_length(1) %>%
    expect_match("\\.\\.\\.")

  expect_silent(capture_df(meta_country_codes()[1:3,])) %>%
    expect_type("character") %>%
    expect_length(1) %>%
    expect_match("[a-zA-Z ]+$")
})

test_that("nc_remove_cache()", {
  expect_true(memoise::is.memoised(srv_query))
  
  p <- srv_query(path = api$permissions, token = srv_auth("sample"))
  
  expect_true(memoise::has_cache(srv_query)(path = api$permissions, 
                                            token = srv_auth("sample")))

  expect_true(nc_remove_cache())
  expect_false(memoise::has_cache(srv_query)(path = api$permissions, 
                                             token = srv_auth("sample")))
  
})

test_that("parse_request()", {

  req <- srv_query(api$list_requests, token = srv_auth("testuser"))$requests
  
  expect_silent(r <- parse_request(req))
  expect_s3_class(r, "data.frame")
  expect_gt(nrow(r), 10) 
  expect_named(r, c("request_id", "request_origin", "request_label", 
                    "collection", "status", "nrecords", "filter"))
})
