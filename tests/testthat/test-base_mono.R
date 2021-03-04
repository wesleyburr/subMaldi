# -----------------------------------------------------------------------
# Last Updated: March 4, 2021
# Author: Wesley Burr
# Title: testthat: .base_mono()
# -----------------------------------------------------------------------
context("Baseline Monotone: .base_mono")
library(subMALDI)

#
#  Very hacky test - need to add more things to test specific variants, but that
#  is complicated!
#
test_that("Baseline Monotone Functions", {
  data(Master2)
  attempt <- baselineCorr(dat = Master2[1:500, ], mass_dat = "full_mz", intensity_dat = "After2",
			  method = "linear", n = 2)
  attempt <- round(attempt$baseline[1:3], 1)
  compare <- c(0.00, 120876.9, 213007.0)

  expect_equal(attempt, compare)
})

