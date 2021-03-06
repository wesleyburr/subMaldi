# -----------------------------------------------------------------------
# Last Updated: March 4, 2021
# Author: Sophie Castel
# Title: testthat: peak_sub()
# -----------------------------------------------------------------------

context("Peak Detection: peak_sub()")
library(subMALDI)


  ######
  # SNR
  ######

  test_that("Dat input is dataframe", {
    expect_error(
      .peak_snr(dat = "apple")
    )
  })

  test_that("Spectrum Names are specified", {
    data(Master2)
    expect_error(
      .peak_snr(dat = Master2, mass_dat = "full_mz")
    )
  })
  
  test_that("Mass column contained in data frame", {
    data(Master2)
    expect_error(
      .peak_snr(dat = Master2, mass_dat = "apple", intensity_dat = "Blank2")
    )
  })
  
  test_that("Window does not cover entire spectrum", {
    data(Master2)
    expect_error(
      .peak_snr(dat = Master2, mass_dat = "full_mz", intensity_dat = "Blank2", n = length(Master2$Blank2))
    )
  })
  
  test_that("Signals detected below specified SNR", {
    data(Master2)
    expect_error(
      .peak_snr(dat = Master2, mass_dat = "full_mz", intensity_dat = "Blank2", n = 500, SNR_thresh = 0)
      )
  })
  
  #######
  # SLOPE
  #######
  
  test_that("Dat input is dataframe", {
    expect_error(
      .peak_snr(dat = "apple")
    )
  })
  
  test_that("Spectrum Names are specified", {
    data(Master2)
    expect_error(
      .peak_slope(dat = Master2, mass_dat = "full_mz")
    )
  })
  
  test_that("Mass column contained in data frame", {
    data(Master2)
    expect_error(
      .peak_slope(dat = Master2, mass_dat = "apple", intensity_dat = "Blank2")
    )
  })
  
  test_that("Window does not cover entire spectrum", {
    data(Master2)
    expect_error(
      .peak_slope(dat = Master2, mass_dat = "full_mz", intensity_dat = "Blank2", n = length(Master2$Blank2))
    )
  })
  

  
  
