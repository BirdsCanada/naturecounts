context("Download counts")

test_that("Get counts for collections", {

  expect_silent(c1 <- nc_count(c("CBC", "BBS"), show = "all"))
  expect_is(c1, "data.frame")
  expect_gt(nrow(c1), 0)
  expect_true(all(c1$collection %in% c("CBC", "BBS")))

  expect_silent(c2 <- nc_count(c("CBC", "BBS"), statprov = "MB", show = "all"))
  expect_is(c2, "data.frame")
  expect_gt(nrow(c2), 0)
  expect_true(all(c2$collection %in% c("CBC", "BBS")))
  expect_true(all(c2$nrecords < c1$nrecords))

  expect_silent(c <- nc_count(country = "CA", statprov = "MB"))
  expect_is(c, "data.frame")
  expect_gt(nrow(c), 0)

})
