
test_that("nc_query_table works as expected", {

  # List tables available
  t1 <- nc_query_table() %T>%
    expect_silent() %T>%
    expect_s3_class("data.frame") %T>%
    expect_named(c("table_name", "filters", "required"))

  t2 <- nc_query_table(username = "sample") %T>%
    expect_silent() %T>%
    expect_s3_class("data.frame") %T>%
    expect_named(c("table_name", "filters", "required"))

  expect_equal(t1, t2)

  expect_true("bmde_filter_bad_dates" %in% t1$table_name)

  # Query returns and filters
  t3 <- nc_query_table(table = "bmde_filter_bad_dates") %T>%
    expect_silent() %T>%
    expect_s3_class("data.frame") %T>%
    expect_named()

  expect_true(nrow(t3) > 100)

  t4 <- nc_query_table(table = "bmde_filter_bad_dates", SiteCode = "DMBO") %T>%
    expect_silent() %T>%
    expect_s3_class("data.frame") %T>%
    expect_named()

  expect_true(nrow(t4) < 10 & nrow(t4) > 1)

  t5 <- nc_query_table(table = "bmde_filter_bad_dates",
                       species_id = c(7680, 9750)) %T>%
    expect_silent() %T>%
    expect_s3_class("data.frame") %T>%
    expect_named()

  expect_true(nrow(t5) < 10 & nrow(t5) > 1)
})
