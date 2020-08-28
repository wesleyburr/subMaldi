

context("TIC Normalization")
library(subMALDI)


# ----
# TIC
# ----


test_that("One input spectrum yields error", {
  expect_error(normSpectra(Master2, "full_mz", "TIC", spec1 = "Before1"))
})

med_tic <- normSpectra(Master2, "full_mz", "TIC", spec1 = "Before1", 
                       spec2 = "Before2", spec3 = "After1")

i1 <- sum(Master2$Before1)
i2 <- sum(Master2$Before2)
i3 <- sum(Master2$After1)
i <- c(i1,i2,i3)
max <- max(i)

o1 <- med_tic$Before1
o2 <- med_tic$Before2
o3 <- med_tic$After1

test_that("Correct TIC chosen for norm", {
  expect_equal(sum(o1), max)
  expect_equal(sum(o2), max)
  expect_equal(sum(o3), max)
})


# -------------
# Relative TIC
# -------------


raw8 <- Master2$Before1[8]

norm <- normSpectra(dat = Master2, mass_dat = "full_mz", method = "rel_TIC", 
                    spec1 = "Before1")
norm8 <- norm[8,2]

div <- raw8/norm8
tic <- sum(Master2$Before1)

test_that("Difference between raw and norm intensities = TIC", {
  expect_equal(div, tic)
})



