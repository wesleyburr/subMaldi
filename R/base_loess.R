# ----------------------------------------------------------------------------
# Last Updated: August 4, 2020
# Author: Kristen Yeh
# Title: subMALDI - LOESS Baseline Correction
# ----------------------------------------------------------------------------

# First, Divide the raw spectrum into small segments. Then, in each 
# small segment, it computes the quantile. After that, it estimates
# a predictor in every small segment for baseline estimation. 

# The predictor in each small segment is obtained using the following rules:

  # If the intensity of a point A is smaller than the quantile in the 
  # segment, then the intensity of corresponding point on predictor 
  # equals the intensity of A.

  # If the intensity of a point is larger than or equal to the quantile
  # in the segment, then the intensity of corresponding point on predictor 
  # equals the quantile.

# Baseline is obtained by applying local polynomial regression 
# fitting to the predictor.

.base_loess <- function(dat, mass_dat, intensity_dat){
   
  x <- dat[[mass_dat]]
  y <- dat[[intensity_dat]]
  loess_df <- data.frame(x, y)

  bs.lo <- loess(y ~ x, loess_df)
  bs <- predict(bs.lo, loess_df)
 
  out <- y - bs 
  out[which(out < 0)] <- 0

  output <- data.frame(x, out)
  names(output) <- c(mass_dat, "baseline")
  return(output)
}

# ----------------------------------------------------------------------------
