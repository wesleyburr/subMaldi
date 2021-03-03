# -----------------------------------------------------------------------
# Last Updated: February 3, 2021
# Author: Kristen Yeh, Sophie Castel
# Title: testthat: norm_TIC()
# -----------------------------------------------------------------------

test_that("One input spectrum yields error", {
  expect_error(normSpectra(Master2, mass_dat = "full_mz", method = "TIC", spectra_cols = "Before1"))
})


med_tic <- normSpectra(Master2, mass_dat = "full_mz", method = "TIC", spectra_cols = c("Before1", "Before2", "After1"))

i1 <- sum(Master2$Before1)
i2 <- sum(Master2$Before2)
i3 <- sum(Master2$After1)
i <- c(i1, i2, i3)
max <- max(i)

o1 <- med_tic$Before1
o2 <- med_tic$Before2
o3 <- med_tic$After1

test_that("Correct TIC chosen for norm", {
  expect_equal(sum(o1), max)
  expect_equal(sum(o2), max)
  expect_equal(sum(o3), max)
})
