test_that("nc_requests() shows 'web' requests", {

  expect_silent(r <- nc_requests(username = "testuser")) %>%
    expect_s3_class("data.frame")
  
  if(nrow(r) > 1) {
    expect_equal(unique(r$request_origin), "web")
    expect_true(any(r$status == "approved"))
  }
})

test_that("nc_requests() shows 'api' requests", {
  expect_silent(r <- nc_requests(username = "testuser", type = "api")) %>%
    expect_s3_class("data.frame")
  expect_gt(nrow(r), 5)
  expect_equal(unique(r$request_origin), "api")
  expect_true(any(r$status == "approved"))
})

test_that("nc_requests() shows 'all' requests", {
  expect_silent(r <- nc_requests(username = "testuser", type = "all")) %>%
    expect_s3_class("data.frame")
  expect_gt(nrow(r), 5)
  expect_equal(sort(unique(r$request_origin)), c("api", "mixed", "web"))
  expect_true(any(r$status == "approved"))
})
