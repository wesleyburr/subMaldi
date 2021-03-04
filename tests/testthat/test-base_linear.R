# -----------------------------------------------------------------------
# Last Updated: March 4, 2021
# Author: Wesley Burr
# Title: testthat: .base_linear()
# -----------------------------------------------------------------------


context("Baseline Linear: .base_linear")
library(subMALDI)

test_that("n must be specified", {
  data(Master2)
  expect_error(
    baselineCorr(dat = Master2[1:500, ], mass_dat = "full_mz", intensity_dat = "After1",
			  method = "linear")
)})

test_that("Baseline Linear Functions", {
  data(Master2)
  mass_dat <- "full_mz"
  intensity_dat <- "After1"
  subset_df <- Master2[1:1000, c(mass_dat, intensity_dat)]
  n <- 7
  attempt <- baselineCorr(dat = subset_df, mass_dat = "full_mz", intensity_dat = "After1",
			  method = "linear", n = 7)
  names(subset_df) <- c("x", "y")

  x_seg <- split(subset_df$x, ceiling(seq_along(subset_df$x)/n))
  xi <- unlist(lapply(x_seg,mean))
  bl <- signal::interp1(subset_df$x, subset_df$y, xi, method = c("linear"))
  y_seg <- split(subset_df$y, ceiling(seq_along(subset_df$y)/n))

  baseline <- c()
  for(b in 1:length(bl)){
      bl_seg <- 
      baseline <- append(baseline, unlist(y_seg[b]) - bl[b],
			 after = length(baseline))
      baseline[which(baseline < 0)] <- 0
  }
  compare <- data.frame(baseline)
  expect_equal(attempt$baseline[1:10], compare$baseline[1:10])
})

