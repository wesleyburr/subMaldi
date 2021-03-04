# -----------------------------------------------------------------------
# Last Updated: March 4, 2021
# Author: Wesley Burr
# Title: testthat: .base_loess()
# -----------------------------------------------------------------------

context("Baseline Loess: .base_loess")
library(subMALDI)

test_that("Method not selected", {
  expect_error(baselineCorr(dat = Master2, mass_dat = "full_mz", intensity_dat = "Before1"))
})

test_that("Baseline LOESS Functions", {
  data(Master2)
  mass_dat <- "full_mz"
  intensity_dat <- "After1"
  subset_df <- Master2[1:1000, c(mass_dat, intensity_dat)]
  attempt <- baselineCorr(dat = subset_df, mass_dat = "full_mz", intensity_dat = "After1",
			  method = "loess")
  names(subset_df) <- c("x", "y")
  bs.lo <- loess(y ~ x, data = subset_df)
  bs <- predict(bs.lo, data = subset_df)
  compare <- subset_df$y - bs
  compare[which(compare < 0)] <- 0
  expect_equal(attempt$baseline[1:10], compare[1:10])
})





