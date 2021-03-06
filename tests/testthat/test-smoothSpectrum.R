# -----------------------------------------------------------------------
# Last Updated: March 6, 2021
# Author: Sophie Castel
# Title: testthat: smoothSpectrum()
# -----------------------------------------------------------------------

context("Miscellaneous Smoothing")
# Covers methods: sgolay, moving average

library(subMALDI)
data("Master2")
  
  
test_that("Method = NULL yields error", {
  data(bsline)
  expect_error(
    smoothSpectrum(dat = bsline, mass_dat = "mass", intensity_dat = "raw", method = NULL, p=1, n=20, m=1, ts =1 )
    )
})

test_that("Mass column contained in data", {
  data(bsline)
  expect_error(
    smoothSpectrum(bsline, mass_dat = "apple", intensity_dat = "raw", n = 7, method = sgolay)
  )
})

test_that("Intensity column contained in data", {
  data(bsline)
  expect_error(
    smoothSpectrum(bsline, mass_dat = "full_mz", intensity_dat = "apple", n = 7, method = sgolay)
  )
})


# ---------------
# SAVITZKY-GOLAY
# ---------------

test_that("Spectrum Names are specified", {
  data(Master2)
  expect_error(
    .smooth_sg(dat = Master2, mass_dat = "full_mz", p = 1, n = 10, m = 0, ts = 1)
  )
})

test_that("Mass column is specified", {
  data(Master2)
  expect_error(
    .smooth_sg(dat = Master2, mass_dat = "apple", intensity_dat = "Before1", p = 1, n = 10, m = 0, ts = 1)
  )
})

test_that("Filter length (n) must be odd", {
  data(Master2)
  expect_error(
    .smooth_sg(dat = Master2, mass_dat = "full_mz", intensity_dat = "Before1", p = 1, n = 10, m = 0, ts = 1)
  )
})

test_that("Window does not cover entire spectrum", {
  data(Master2)
  expect_error(
    .smooth_sg(dat = Master2[1:500,], mass_dat = "full_mz", intensity_dat = "Before1", p = 1, n = 501, m = 0, ts = 1)
  )
})

test_that("Filter order (p) must be integer", {
  data(Master2)
  expect_error(
    .smooth_sg(dat = Master2, mass_dat = "full_mz", intensity_dat = "Before1", p = "apple", n = 10, m = 0, ts = 1)
  )
})

# --------------------
# MOVING AVG. FILTERS
# --------------------

test_that("Spectrum Names are specified", {
  data(Master2)
  expect_error(
    .smooth_ma(dat = Master2, mass_dat = "full_mz", n = 11)
  )
})

test_that("Mass column is specified", {
  data(Master2)
  expect_error(
    .smooth_ma(dat = Master2, mass_dat = "apple", intensity_dat = "Before1", n = 11)
  )
})

test_that("Window does not cover entire spectrum", {
  data(Master2)
  expect_error(
    .smooth_ma(dat = Master2[1:500,], mass_dat = "full_mz", intensity_dat = "Before1", n = 501)
  )
})
