# -----------------------------------------------------------------------
# Last Updated: March 6, 2021
# Author: Sophie Castel
# Title: testthat: readcsvDir()
# -----------------------------------------------------------------------

context("Import .csv Spectra from Directory")
library(subMALDI)


test_that(".csv files exist in directory", {
  expect_error(
    readcsvDir(direct = "apple")
  )
})


test_that("Specified mass column contained in file", {
  expect_error(
    readcsvDir(direct = "~/tests/ascii_test/", massCol = "apple")
  )
})


test_that("Specified intensity column contained in file", {
  expect_error(
    readcsvDir(direct = "~/tests/ascii_test/", massCol = "apple")
  )
})

test_that("Specified intensity column contained in file", {
  expect_error(
    readcsvDir(direct = "~/tests/ascii_test/", massCol = "m.z", intenseCol = "apple")
  )
})

