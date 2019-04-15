context("Server Calls")

test_that("srv_query() returns data", {

  expect_silent(d <- srv_query(api$bmde_versions))
  expect_is(d, "list")
  expect_gt(length(d), 0)

  expect_silent(d <- srv_query(api$bmde_versions,
                               query = list(lang = "EN")))
  expect_is(d, "list")
  expect_gt(length(d), 0)

  expect_error(srv_query("metadat/hello"), "Service not found")
})

test_that("srv_query() applies settings and resets", {

  # Check that verbose functions as expected
  expect_output(d <- capture.output(srv_query(api$bmde_versions),
                                    type = "message"))
  expect_length(d, 0)
  expect_output(d <- capture.output(srv_query(api$bmde_versions, verbose = TRUE),
                                    type = "message"))
  expect_gt(length(d), 0)

  # Check that verbose resets to FALSE (i.e. that settings reset appropriately)
  expect_output(d <- capture.output(srv_query(api$bmde_versions),
                                    type = "message"))
  expect_length(d, 0)
})

test_that("srv_query() returns informative errors", {
  expect_error(srv_query(api$data, token = "hello"),
               "Authorization required")
})