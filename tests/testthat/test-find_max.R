# -----------------------------------------------------------------------
# Last Updated: March 4, 2021
# Author: Wesley Burr
# Title: testthat: find_max()
# -----------------------------------------------------------------------

context("Find Max Peak: find_max()")
library(subMALDI)

test_that("Spectrum Names must be specified", {
  data(Master2)
  expect_error(
    find_max(dat = Master2[1:500, ], mass_dat = "full_mz")
  )
})

test_that("Mass Name(s) must be specified", {
  data(Master2)
  expect_error(
    find_max(dat = Master2[1:500, ], spectra_cols = "After1")
  )
})

test_that("Maximum, One Spectra", {
  data(Master2)
  mass_dat <- "full_mz"
  intensity_dat <- "After1"
  subset_df <- Master2[1:1000, c(mass_dat, intensity_dat)]
  attempt <- find_max(dat = subset_df, mass_dat = "full_mz", spectra_cols = "After1")
  expect_equal(object = attempt$Mass, expected = 62.24)
})

test_that("Maximum, Two Spectra", {
  data(Master2)
  mass_dat <- "full_mz"
  intensity_dat <- c("After1", "After2")
  subset_df <- Master2[1:1000, c(mass_dat, intensity_dat)]
  attempt <- find_max(dat = subset_df, mass_dat = "full_mz", spectra_cols = c("After1", "After2"))
  expect_equal(object = attempt$Mass[2], expected = 62.96)
})

