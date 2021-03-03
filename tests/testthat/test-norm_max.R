# -----------------------------------------------------------------------
# Last Updated: March 3, 2021
# Author: Kristen Yeh, Sophie Castel
# Title: testthat: normSpectra()
# -----------------------------------------------------------------------

context("Maximum Normalization")
library(subMALDI)

test_that("Method not selected", {
  expect_error(normSpectra(Master2, "full_mz", spec1 = "Before1"))
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

max <- normSpectra(Master2, "full_mz", "max",
  spec1 = "Blank1",
  spec2 = "Blank2", spec3 = "Before1", spec4 = "Before2",
  spec5 = "After1", spec6 = "After2"
)
max_list <- as.character(which(max[, -1] == 1))

max_set <- as.character(which(Master2[, -1] == max(Master2[, -1])))

test_that("Maximum intensity of all spec = max of set", {
  expect_match(max_list[1], max_set)
})
