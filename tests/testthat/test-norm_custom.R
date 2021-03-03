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
    spec1 = "Before1"
  ))
})

out <- normSpectra(Master2, "full_mz",
  method = "custom",
  norm_mz = 255.23, spec1 = "Before1", showHI = FALSE
)
max <- max(out$Before1)

i <- out[which(out$full_mz == 255.23), 2]

test_that("Intensity of output at norm_mz == 1 == max", {
  expect_equal(i, max, 1)
})

# ------------------
# Custom: Imprecise
# ------------------

data("Master")

out <- normSpectra(Master, "full_mz",
  method = "custom_imprecise",
  norm_mz = "112.959", spec1 = "Before1"
)
int <- out[which(out$full_mz == 112.9595), 2]

test_that("Intensity of output at norm_mz == 1", {
  expect_equal(int, 1)
})

test_that("More than one peak for given m/z", {
  expect_error(normSpectra(Master, "full_mz",
    method = "custom_imprecise",
    norm_mz = "255.23", spec1 = "Before1"
  ))
})
