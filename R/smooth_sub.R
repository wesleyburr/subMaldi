# ----------------------------------------------------------------------------
# Last Updated: July 8, 2020
# Author: Kristen Yeh
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
# Title: subMALDI - Smoothing: Savitzky-Golay, Moving Average
=======
# Title: subMALDI - Smoothing: Savitzky-Golay, Kaiser Window, Moving Average
>>>>>>> e43371a... Added smoothing
=======
# Title: subMALDI - Smoothing: Savitzky-Golay, Moving Average
>>>>>>> 4bbf2ad... Baseline correction added
=======
# Title: subMALDI - Smoothing: Savitzky-Golay, Kaiser Window, Moving Average
>>>>>>> c29c117... Added smoothing
# ----------------------------------------------------------------------------


# ---------------
# SAVITZKY-GOLAY
# ---------------


.smooth_sg <- function(dat, mass_dat, intensity_dat, p, n, m = 0, ts = 1){
  
  intense <- dat[[intensity_dat]]
  sg <- sgolay(p = p, n = n, m = m, ts = ts)
  dat$sg <- signal::filter(sg, intense)
  out <- select(dat, all_of(mass_dat), sg)
  return(out)
  
}


# p = filter order
# n = filter length, must be odd
# m = return the m-th derivative of the filter coefficients
# ts = time scaling factor


# ---------------
# ----------------------------------------------------------------------------
# ---------------


# --------------------
# MOVING AVG. FILTERS
# --------------------


# n = window span
.smooth_ma <- function(dat, mass_dat, intensity_dat, n){
  out_i <- stats::filter(intensity_dat, rep(1 / n, n), sides = 2)
  out<- data.frame(mass_dat, out_i)
  names(out) <- c("mass", "mov_avg")
  out <- na.omit(out)
  return(out)
}


# ----------------------------------------------------------------------------