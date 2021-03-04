# -----------------------------------------------------------------------
# Last Updated: March 4, 2021
# Author: Sophie Castel
# Title: testthat: smooth_sub()
# -----------------------------------------------------------------------

context("Smoothing Functions: smooth_sub()")
library(subMALDI)

  # ---------------
  # SAVITZKY-GOLAY
  # ---------------
  
  test_that("Spectrum Names must be specified", {
    data(Master2)
    expect_error(
      .smooth_sg(dat = Master2, mass_dat = "full_mz", p = 1, n = 10, m = 0, ts = 1)
    )
  })

  test_that("Mass column must be specified", {
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
  
  test_that("Window covers entire spectrum", {
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
  
  test_that("Spectrum Names must be specified", {
    data(Master2)
    expect_error(
      .smooth_ma(dat = Master2, mass_dat = "full_mz", n = 11)
    )
  })
  
  test_that("Mass column must be specified", {
    data(Master2)
    expect_error(
      .smooth_ma(dat = Master2, mass_dat = "apple", intensity_dat = "Before1", n = 11)
    )
  })
  
  test_that("Window covers entire spectrum", {
    data(Master2)
    expect_error(
      .smooth_ma(dat = Master2[1:500,], mass_dat = "full_mz", intensity_dat = "Before1", n = 501)
    )
  })
  