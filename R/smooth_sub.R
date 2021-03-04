# ----------------------------------------------------------------------------
# Last Updated: July 8, 2020
# Author: Kristen Yeh
# Title: subMALDI - Smoothing: Savitzky-Golay, Moving Average
# ----------------------------------------------------------------------------


# ---------------
# SAVITZKY-GOLAY
# ---------------


.smooth_sg <- function(dat, mass_dat, intensity_dat, p, n, m = 0, ts = 1){
  
  stopifnot(mass_dat %in% colnames(dat),
            intensity_dat %in% colnames(dat),
            !(is.null(n)),
            !(is.null(p)),
            is.numeric(p),
            p%%1 == 0)
  
  if(n%%2 == 0){
    stop(paste0("Erroneous input: n = ", n, ". Filter order must be an odd number."))
  }
  
  intense <- dat[[intensity_dat]]
  
  if(n > length(intense)){
    stop(paste0("Chosen window covers entire spectrum. To avoid this, please select a value of n much less than ", length(intense),"."))
  }
  
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
  
  stopifnot(mass_dat %in% colnames(dat),
            intensity_dat %in% colnames(dat),
            !(is.null(n))
            )
  
  
  
  x <- dat[[mass_dat]]
  i <- dat[[intensity_dat]]
  
  if(n > length(i)){
    stop(paste0("Chosen window covers entire spectrum. To avoid this, please select a value of n much less than ", length(i),"."))
  }
  
  out_i <- stats::filter(i, rep(1 / n, n), sides = 2)
  out<- data.frame(mass_dat, out_i)
  names(out) <- c("mass", "mov_avg")
  out <- na.omit(out)
  return(out)
}


# ----------------------------------------------------------------------------