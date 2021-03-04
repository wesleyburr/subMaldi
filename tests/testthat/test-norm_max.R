# -----------------------------------------------------------------------
# Last Updated: March 3, 2021
# Author: Kristen Yeh, Sophie Castel
# Title: testthat: norm_max() and norm_max_set()
# -----------------------------------------------------------------------

context("Maximum Normalization")
library(subMALDI)

test_that("Method not selected", {
  expect_error(normSpectra(dat = Master2, mass_dat = "full_mz", spectra_cols = "Before1"))
})

# -------------------------------------
# Maximum of Set: Univariate (norm_max)
# -------------------------------------

test_that("Maximum intensity of spec is correct", {
  max_df <- normSpectra(dat = Master2, mass_dat = "full_mz", method = "max",
    spectra_cols = c("Blank1")
  )
  max_list <- as.character(which(max_df[, c("Blank1")] == 1))
  max_set <- as.character(which(Master2[, c("Blank1")] == max(Master2[, c("Blank1")])))

  expect_match(max_list[1], max_set)
})

# ------------------------------------
# Maximum of Set: Bivariate (norm_max)
# ------------------------------------
test_that("Maximum intensity of all spec = max of set", {
  max_df <- normSpectra(dat = Master2, mass_dat = "full_mz", method = "max",
    spectra_cols = c("Blank1", "After1")
  )
  max_list <- as.character(which(max_df[, c("Blank1", "After1")] == 1))

  max_set <- as.character(which(Master2[, c("Blank1", "After1")] == max(Master2[, c("Blank1", "After1")])
				))
  expect_match(max_list[1], max_set)
})

# -------------------------------------------
# Maximum of Set: Multivariate (norm_max_set)
# -------------------------------------------
test_that("Spec2 = NULL when normMethod_max_set yields error", {
  expect_error(normSpectra(
    dat = Master, mass_dat = "full_mz",
    method = "max_set", spectra_cols = c("Before1")
  ))
})

test_that("Maximum intensity of all spec = max of set", {
  max_df <- normSpectra(dat = Master2, mass_dat = "full_mz", method = "max",
    spectra_cols = c("Blank1", "Blank2", "After1")
  )
  max_list <- as.character(which(max_df[, c("Blank1", "Blank2", "After1")] == 1))

  max_set <- as.character(which(
	Master2[, c("Blank1", "Blank2", "After1")] == max(Master2[, c("Blank1", "Blank2", "After1")])
	))
  expect_match(max_list[1], max_set)
})


