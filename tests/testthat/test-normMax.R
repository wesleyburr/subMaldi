# -----------------------------------------------------------------------
# Last Updated: March 3, 2021
# Author: Kristen Yeh, Sophie Castel
# Title: testthat: normSpectra()
# -----------------------------------------------------------------------

context("Maximum Normalization")
library(subMALDI)

test_that("Method not selected", {
  expect_error(normSpectra(Master2, "full_mz", spectra_cols = "Before1"))
})

# ---------------
# Maximum of Set
# ---------------

test_that("Spec2 = NULL when normMethod_max_set yields error", {
  expect_error(normSpectra(
    dat = Master, mass_dat = "full_mz",
    method = "max_set", spec1 = "Before1"
  ))
})
test_that("Maximum intensity of all spec = max of set", {

  max <- normSpectra(Master2, "full_mz", "max", spectra_cols = c("Blank1","Blank2", "Before1","Before2","After1", "After2"))
                     
  max_list <- as.character(which(max[, -1] == 1))

  max_set <- as.character(which(Master2[, -1] == max(Master2[, -1])))

  expect_match(max_list[1], max_set)
})
