

context("Baseline Correction")
library(subMALDI)
data("bsline")

loess <- baselineCorr(bsline, "mass", "raw","loess")
loess <- select(loess, "baseline")

linear <- baselineCorr(bsline, "mass", "raw", "linear", 7)
linear <- select(linear, "baseline")

mono <- baselineCorr(bsline, "mass", "raw", "monotone_min")
mono <- select(mono, "baseline")

test_that("output minimum intensity is 0", {
  expect_equal(min(loess), 0)
  expect_equal(min(linear), 0)
  expect_equal(min(mono), 0)
})

test_that("method = NULL yields error", {
  expect_error(baselineCorr(bsline, "mass", "raw"))
})


# -----------------
# Monotone Minimum
# -----------------


mz_test <- rbind(bsline, bsline[1,])
mz_test <- mz_test[order("mass"),]

test_that("duplicated m/z stops fmethod = monotone_min", {
  expect_error(baselineCorr(mz_test, "mass", "raw", method = "monotone_min"))
})

