# -----------------------------------------------------------------------
# Last Updated: February 3, 2021
# Author: Kristen Yeh, Sophie Castel
# Title: testthat: baselineCorr()
# -----------------------------------------------------------------------

data("bsline")

# Generating baseline correction for all three methods 

loess <- baselineCorr(bsline, mass_dat = "mass", intensity_dat = "raw", method = "loess")
linear <- baselineCorr(bsline, mass_dat = "mass", intensity_dat = "raw", method = "linear", n = 7)
mono <- baselineCorr(bsline, mass_dat = "mass", intensity_dat = "raw", method = "monotone_min")


test_that("output minimum intensity is 0", {
  expect_equal(min(loess$baseline), 0)
  expect_equal(min(linear$baseline), 0)
  expect_equal(min(mono$baseline), 0)
})

test_that("method = NULL yields error", {
  expect_error(baselineCorr(bsline, mass_dat = "mass", intensity_dat = "raw", method = NULL))
})


# -----------------
# Monotone Minimum
# -----------------

mz_test <- rbind(bsline, bsline[1,])
mz_test <- mz_test[order("mass"),]

test_that("duplicated m/z stops fmethod = monotone_min", {
  expect_error(baselineCorr(mz_test, "mass", "raw", method = "monotone_min"))
})

# -----------------
# Linear
# -----------------




# -----------------
# LOESS
# -----------------
