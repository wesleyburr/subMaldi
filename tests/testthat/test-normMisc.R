# -----------------------------------------------------------------------
# Last Updated: March 3, 2021
# Author: Kristen Yeh, Sophie Castel
# Title: testthat: normSpectra()
# -----------------------------------------------------------------------

context("Miscellaneous Normalization")
# Covers methods: St.dev, RMS, median, quantile

library(subMALDI)
data("Master2")


# --------------
# St. Deviation
# --------------

test_that("St.Dev of output in noise region = 1", {
  out <- normSpectra(Master2,
    mass_dat = "full_mz", method = "stdev",
    lower = 900, upper = 1100, spec1 = "Before1"
  )
  noise <- out$Before1[which(out$full_mz > 900 & 1100 > out$full_mz)]
  noise <- noise[noise != 0]

  expect_equal(sd(noise), 1)
})

# -------
# Median
# -------

test_that("Median of output intensities are equal in all spec", {
med <- normSpectra(Master2,
  mass_dat = "full_mz", method = "median",
  spec1 = "Before1", spec2 = "Before2"
)

med <- gather(med,
  key = "Spectra", value = "Intensity",
  Before1, Before2, factor_key = TRUE
)
med <- dplyr::filter(med, Intensity != 0)

Before1 <- med[med$Spectra == "Before1", ] %>% select("full_mz", "Intensity")
m1 <- median(Before1$Intensity)

Before2 <- med[med$Spectra == "Before2", ] %>% select("full_mz", "Intensity")
m2 <- median(Before2$Intensity)

  expect_equal(m1, m2)
})

# ---------
# Quantile
# ---------

test_that("One input spectrum yields error", {
  expect_error(normSpectra(Master2, "full_mz", "quantile", spec1 = "Before1"))
})

# ----
# RMS
# ----

test_that(".RMS calculates RMS correct", {
  test_rms <- .RMS(y = Master2$Before1, n = length(Master2$Before1))

  n <- length(Master2$Before1)
  y <- Master2$Before1
  right <- 1 / (n - 1)
  left <- sum(y^2)
  prod <- prod(left, right)
  rms <- sqrt(prod)

  expect_equal(test_rms, rms)
})

test_that("Diff b/w raw and norm = RMS", {
  raw5 <- Master2$Before1[5]

  norm <- normSpectra(
    dat = Master2, mass_dat = "full_mz", method = "RMS",
    spec1 = "Before1"
  )
  norm5 <- norm[5, 2]

  div <- raw5 / norm5
  res <- .RMS(Master2$Before1, length(Master2$Before1))

  expect_equal(div, res)
})
