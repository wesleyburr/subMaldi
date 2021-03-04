# ----------------------------------------------------------------------------
# Last Updated: July 29, 2020
# Author: Kristen Yeh
# Title: subMALDI - Linear Interpolation Baseline Correction 
# ----------------------------------------------------------------------------

# Linear interpolation takes two steps to estimate baseline:

  # Divide the raw spectrum into small segments and use the mean, 
  # the minimum or the median of the points in each segment as the 
  # baseline point.

  # Generate a baseline for the raw spectrum by linearly interpolating 
  # baseline points across all small segments.

.base_linear <- function(dat, mass_dat, intensity_dat, n){
 
  stopifnot(!is.null(n))

  x <- dat[[mass_dat]]
  y <- dat[[intensity_dat]]
  
  # Divide spectrum into segments and calculate the mean/min/median
  # n = segment size
  x_seg <- split(x, ceiling(seq_along(x)/n))
  
  # method = mean
  xi <- lapply(x_seg,mean)
  xi <- unlist(xi)
  
  # Generate baseline for spectrum w/ linear interpolation on points
  bl <- interp1(x, y, xi, method = "linear")
  
  # for each window of size n
  # subtract all intensity values by interpolated baseline in window
  y_seg <- split(y, ceiling(seq_along(y)/n))
  baseline <- c()
  
  for(b in 1:length(bl)){
      bl_seg <- unlist(y_seg[b]) - bl[b]
      baseline <- append(baseline, bl_seg, after = length(baseline))
      baseline[which(baseline < 0)] <- 0
  }
  out <- data.frame(x, baseline)
  names(out) <- c("mz", "baseline")
  return(out)
}


# ----------------------------------------------------------------------------
