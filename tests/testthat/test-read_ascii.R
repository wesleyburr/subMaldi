# -----------------------------------------------------------------------
# Last Updated: March 4, 2021
# Author: Wesley Burr
# Title: testthat: read_ascii()
# -----------------------------------------------------------------------

context("Bruker read_ascii utility function")
library(subMALDI)

test_that("read_ascii parses Bruker files ok", {
  location <- system.file("extdata", "raw_test_file.ascii", package = "subMALDI")
  dat <- read_ascii(location)
  observed <- c(dat[1, 1], dat[1, 2])
  expect_equal(observed, c("53.76045", "194476.36"))
})


