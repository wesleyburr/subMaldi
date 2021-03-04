# -----------------------------------------------------------------------
# Last Updated: March 3, 2021
# Author: Kristen Yeh, Sophie Castel
# Title: testthat: avgSpectra(), subSpectra(), rmveEmpty()
# -----------------------------------------------------------------------

context("Base subMALDI functions")
library(subMALDI)

# -----------
# avgSpectra
# -----------

test_that("Averaging works (m/z not included)", {
  data("Master2")

  r5 <- as.numeric(Master2[5, c("Blank1", "Blank2", "Before1", "Before2", "After1", "After2")])
  mean_up <- mean(r5)

  avg <- avgSpectra(Master2,
    method = "mean", spectra_cols = c("Blank1", "Blank2", "Before1", "Before2", "After1", "After2")
  )
  avg <- avg[5, "Average"]

  expect_equal(avg, mean_up)
})

test_that("Sum works (m/z not included)", {
  data("Master2")
  r5 <- as.numeric(Master2[5, c("Blank1", "Blank2", "Before1", "Before2", "After1", "After2")])
  sum_up <- sum(r5)

  avg_sum <- avgSpectra(Master2,
    method = "sum", spectra_cols = c("Blank1", "Blank2", "Before1", "Before2", "After1", "After2")
  )
  avg_sum <- avg_sum[5, "Sum"]

  expect_equal(avg_sum, sum_up)
})

# -----------
# subSpectra
# -----------

test_that("Subtraction yields right value", {
  data("Master2")
  r10 <- as.numeric(Master2[10, 4:5])
  sub <- as.numeric(r10[2] - r10[1])

  bef <- select(Master2, "full_mz", "Before1", "Before2")
  bef <- transform(bef, "Subtracted" = 0)

  subSp <- subSpectra(bef,
  Blank_Var = "Before1", Sample = "Before2",
    Sub_Sample = "Subtracted"
  )
  subSp <- subSp[10, 4]
    
  expect_equal(subSp, sub)
})
  
test_that("showNeg = TRUE actually shows negative values", {
  data("Master2")
  bef <- select(Master2, "full_mz", "Before1", "Before2")
  bef <- transform(bef, "Subtracted" = 0)

  r3 <- as.numeric(Master2[3, c("Before1", "Before2")])
  neg <- as.numeric(r3[2] - r3[1])
    
  sneg <- subSpectra(bef,
                    Blank_Var = "Before1", Sample = "Before2",
                    Sub_Sample = "Subtracted", showNeg = TRUE)
  sneg <- sneg[3, "Subtracted"]

  expect_equal(sneg, neg)
})


# ----------
# rmveEmpty
# ----------

data("Blank1")
data("Blank2")

df <- createSpecDF(min_mz = 53.76, max_mz = 1100, res = 1e-04, dig = 4)
df <- select(df, "full_mz")
df <- transform(df, "Blank1" = 0, "Blank2" = 0)

df <- mapSpectrum(Blank1, "mass", "Intensity", 4, 1e-4, df, "Blank1")
df <- mapSpectrum(Blank2, "mass", "Intensity", 4, 1e-4, df, "Blank2")

df <- rmveEmpty(df)

zeroes <- length(which(rowSums(df[, -1]) == 0))

test_that("no more rows containing only 0 intensities exist", {
  expect_equal(zeroes, 0)
})
