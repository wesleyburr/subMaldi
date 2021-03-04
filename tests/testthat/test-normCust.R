# -----------------------------------------------------------------------
# Last Updated: March 3, 2021
# Author: Kristen Yeh, Sophie Castel
# Title: testthat: normSpectra()
# -----------------------------------------------------------------------

context("Custom Normalization")
library(subMALDI)


test_that("No norm_mz selected when normMethod_custom yields error", {
  expect_error(normSpectra(
    dat = Master2, mass_dat = "full_mz", method = "custom",
    spectra_cols = "Before1"
  ))
})

test_that("Intensity of output at norm_mz == 1 == max", {
  out <- normSpectra(Master2, mass_dat = "full_mz",
    method = "custom",
    norm_mz = 255.23, spectra_cols = "Before1", showHI = FALSE
  )
  max <- max(out$Before1)

  i <- out[which(out$full_mz == 255.23), 2]

  expect_equal(i, max, 1)
})

# ------------------
# Custom: Imprecise
# ------------------

test_that("Intensity of output at norm_mz == 1", {
  data("Master")

  out <- normSpectra(Master, mass_dat = "full_mz",
    method = "custom_imprecise",
    norm_mz = "112.959", spectra_cols = "Before1"
  )
  int <- out[which(out$full_mz == 112.9595), 2]

  expect_equal(int, 1)
})

test_that("More than one peak for given m/z", {
  data("Master")

  out <- normSpectra(Master, mass_dat = "full_mz",
  method = "custom_imprecise",
    norm_mz = "112.959", spectra_cols = "Before1"
  )
  int <- out[which(out$full_mz == 112.9595), 2]

  expect_error(normSpectra(Master, "full_mz",
    method = "custom_imprecise",
    norm_mz = "255.23", spectra_cols = "Before1"
  ))
})


