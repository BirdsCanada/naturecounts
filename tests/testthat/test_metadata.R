context("Metadata")

test_that("Metadata updates", {
  # Get original file dates
  loc <- list.files(system.file("extdata", package = "naturecounts"),
                    full.names = TRUE) %>%
    subset(!stringr::str_detect(., "utm")) %>%
    file.info()

  # Update metadata files
  expect_message(nc_metadata(force = TRUE), "Updating")

  loc2 <- list.files(system.file("extdata", package = "naturecounts"),
                     full.names = TRUE) %>%
    subset(!stringr::str_detect(., "utm")) %>%
    file.info()

  # Expect all file dates to be newer now
  expect_true(all(lubridate::ymd_hms(loc$mtime) <
                    lubridate::ymd_hms(loc2$mtime)))

  # Expect no update if force = FALSE
  expect_message(nc_metadata(force = FALSE), "already up-to-date with server")

  loc3 <- list.files(system.file("extdata", package = "naturecounts"),
                     full.names = TRUE) %>%
    subset(!stringr::str_detect(., "utm")) %>%
    file.info()

  # Expect all file dates to be newer now
  expect_true(all(lubridate::ymd_hms(loc2$mtime) ==
                    lubridate::ymd_hms(loc3$mtime)))

})

test_that("Metadata functions accessible", {

  # Get all the metadata functions
  funs <- ls("package:naturecounts") %>%
    stringr::str_subset("^meta_")

  for(m in funs) {
    expect_silent(get(m)()) %>%
      expect_is("data.frame")
  }
})