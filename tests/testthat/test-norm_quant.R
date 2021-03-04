# -----------------------------------------------------------------------
# Last Updated: March 3, 2021
# Author: Kristen Yeh, Sophie Castel
# Title: testthat: norm_quantile()
# -----------------------------------------------------------------------

context("Quantile Normalization")
library(subMALDI)

# -------------------------------------------
# Attempt quantile normalization, one spectra
# -------------------------------------------

test_that("Maximum intensity of spec is correct", {
  max_df <- normSpectra(dat = Master2, mass_dat = "full_mz", method = "max",
    spectra_cols = c("Blank1")
  )
  max_list <- as.character(which(max_df[, c("Blank1")] == 1))
  max_set <- as.character(which(Master2[, c("Blank1")] == max(Master2[, c("Blank1")])))

  expect_match(max_list[1], max_set)
})

# ------------------------------------
# Quantile normalization, two spectra
# ------------------------------------
test_that("Quantile normalization works", {
  data(Master2)
  quant_df <- normSpectra(dat = Master2, mass_dat = "full_mz", method = "quantile",
    spectra_cols = c("Blank1", "After1")
  )
  expect_match(quant_df[10, ], c(53.82, 14814.93, 0))
})

