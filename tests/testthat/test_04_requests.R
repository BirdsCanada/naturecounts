test_that("nc_requests() shows 'web' requests", {

  expect_silent(r <- nc_requests(username = "testuser")) %>%
    expect_s3_class("data.frame")
  expect_equal(nrow(r), 0)

  skip_if_not(have_auth())
  expect_silent(r <- nc_requests(username = "steffilazerte2")) %>%
    expect_s3_class("data.frame")
  expect_gt(nrow(r), 1)
  expect_equal(unique(r$requestOrigin), "web")
  expect_true(any(r$status == "approved"))
})

test_that("nc_requests() shows 'api' requests", {
  expect_silent(r <- nc_requests(username = "testuser", type = "api")) %>%
    expect_s3_class("data.frame")
  expect_gt(nrow(r), 5)
  expect_equal(unique(r$requestOrigin), "api")
  expect_true(any(r$status == "approved"))

  skip_if_not(have_auth())
  expect_silent(r <- nc_requests(username = "steffilazerte2", type = "api")) %>%
    expect_s3_class("data.frame")
  expect_gt(nrow(r), 5)
  expect_equal(unique(r$requestOrigin), "api")
  expect_true(any(r$status == "approved"))
})

test_that("nc_requests() shows 'all' requests", {
  expect_silent(r <- nc_requests(username = "testuser", type = "all")) %>%
    expect_s3_class("data.frame")
  expect_gt(nrow(r), 5)
  expect_equal(unique(r$requestOrigin), "api")
  expect_true(any(r$status == "approved"))

  skip_if_not(have_auth())
  expect_silent(r <- nc_requests(username = "steffilazerte2", type = "all")) %>%
    expect_s3_class("data.frame")
  expect_gt(nrow(r), 5)
  expect_equal(sort(unique(r$requestOrigin)), c("api", "mixed", "web"))
  expect_true(any(r$status == "approved"))
})
