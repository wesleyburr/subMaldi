# -----------------------------------------------------------------------
# Last Updated: February 3, 2021
# Author: Kristen Yeh, Sophie Castel
# Title: testthat: norm_rel_TIC()
# -----------------------------------------------------------------------

raw8 <- Master2$Before1[8]

norm <- normSpectra(dat = Master2, mass_dat = "full_mz", method = "rel_TIC", 
                    spectra_cols = "Before1")
norm8 <- norm[8,2]

div <- raw8/norm8
tic <- sum(Master2$Before1)

test_that("Difference between raw and norm intensities = TIC", {
  expect_equal(div, tic)
})

