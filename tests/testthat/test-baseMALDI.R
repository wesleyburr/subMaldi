# -----------------------------------------------------------------------
# Last Updated: March 3, 2021
# Author: Kristen Yeh, Sophie Castel
# Title: testthat: avgSpectra(), subSpectra(), rmveEmpty()
# -----------------------------------------------------------------------

context("Base subMALDI functions")
library(subMALDI)
data("Master2")

# -----------
# avgSpectra
# -----------

r5 <- as.numeric(Master2[5, -1])
sum <- sum(r5)
mean <- mean(r5)

avg <- avgSpectra(Master2,
  method = "mean", spec1 = "Blank1",
  spec2 = "Blank2", spec3 = "Before1", spec4 = "Before2",
  spec5 = "After1", spec6 = "After2"
)
avg <- avg[5, 8]

test_that("Averaging works (m/z not included)", {
  expect_equal(avg, mean)
})

avg_sum <- avgSpectra(Master2,
  method = "sum", spec1 = "Blank1",
  spec2 = "Blank2", spec3 = "Before1", spec4 = "Before2",
  spec5 = "After1", spec6 = "After2"
)
avg_sum <- avg_sum[5, 8]

test_that("Sum works (m/z not included)", {
  expect_equal(avg_sum, sum)
})

# -----------
# subSpectra
# -----------

r10 <- as.numeric(Master2[10, 4:5])
sub <- as.numeric(r10[2] - r10[1])

bef <- select(Master2, "full_mz", "Before1", "Before2")
bef <- transform(bef, "Subtracted" = 0)

subSp <- subSpectra(bef,
  Blank_Var = "Before1", Sample = "Before2",
  Sub_Sample = "Subtracted"
)
subSp <- subSp[10, 4]

test_that("Subtraction yields right value", {
  expect_equal(subSp, sub)
})

r3 <- as.numeric(Master2[3, 4:5])
neg <- as.numeric(r3[2] - r3[1])

sneg <- subSpectra(bef,
  Blank_Var = "Before1", Sample = "Before2",
  Sub_Sample = "Subtracted", showNeg = TRUE
)
sneg <- sneg[3, 4]

test_that("showNeg = TRUE actually shows negative values", {
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
