# -----------------------------------------------------------------------
# Last Updated: March 3, 2021
# Author: Kristen Yeh, Sophie Castel
# Title: subMALDI: Normalization Method - Standard Deviation of Noise
# -----------------------------------------------------------------------

# ------------------------------------
# METHOD: STANDARD DEVIATION OF NOISE
# ------------------------------------

# Find a region of the spectrum with only noise and minimal peaks
# (Should be same region for all spectra)
# Evaluate standard deviation of intensity in that region for each spec
# Divide intensity of EACH PEAK IN SPEC by its st. dev.


norm_stdev <- function(dat, mass_dat, lower = 900, upper = 1100 , spectra_cols){
  
  # ---------------------
  # LOGICAL CHECKS
  # ---------------------
  
  stopifnot(
    is.character(mass_dat),
    is.character(spectra_cols),
    mass_dat %in% colnames(dat),
    all(spectra_cols %in% colnames(dat)),
    is.numeric(lower),
    is.numeric(upper)
  )
  
  mz <- dat[[mass_dat]]
  
  spectra <- lapply(spectra_cols, function(x){dat[x]})
  i <- do.call(what = data.frame, args = c(spectra))
  
  noise <- i[which(mz > lower & upper > mz),]
  noise[noise == 0] <- NA
  
  if(length(spectra_cols) == 1){
    std_dev <- sd(noise, na.rm = TRUE)
  } else{
    std_dev <- apply(noise, 2, FUN = sd, na.rm = TRUE)
  }
  
  i_sd <- t(t(i)*std_dev^-1) # i / std_dev matrix multiplication
  
  dat <- data.frame(mz, i_sd)
  
  colnames(dat) <- c("full_mz", spectra_cols)
  
  return(dat)
}












