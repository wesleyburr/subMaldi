

context("Peak Detection Functions")
library(subMALDI)


test_that("Method = NULL yields error", {
  expect_error(peakDet(bsline, "mass", "raw", n = 7, SNR_thresh = 3))
})

test_that("n = NULL yields error for SNR", {
  expect_error(peakDet(bsline, "mass", "raw", method = "snr", SNR_thresh = 3))
})

test_that("SNR thresh not input yields error", {
  expect_error(peakDet(bsline, "mass", "raw", method = "snr", n = 7))
})

# ----------------
# Slopes of Peaks
# ----------------


mz_test <- rbind(bsline, bsline[1,])
mz_test <- mz_test[order("mass"),]

test_that("duplicated m/z stops fmethod = monotone_min", {
  expect_error(peakDet(mz_test, "mass", "raw", method = "slopes"))
})

test_that("n = NULL yields error for Slopes of Peaks", {
  expect_error(peakDet(bsline, "mass", "raw", method = "slopes"))
})

