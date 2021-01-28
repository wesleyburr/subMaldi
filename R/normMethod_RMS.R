# -----------------------------------------------------------------------
# Last Updated: January 28, 2021
# Author: Kristen Yeh, Sophie Castel
# Title: subMALDI: Normalization Method - RMS
# -----------------------------------------------------------------------

# -------------------------
# METHOD: ROOT MEAN SQUARE
# -------------------------

.RMS <- function(y, n){
  right <- 1 / (n - 1)
  left <- sum(y^2)
  prod <- prod(left, right)
  sqrt(prod)
}

.norm_RMS <- function(y, RMS){
  return(y/RMS) # single value
}

.nMethod_RMS <- function(dat, mass_dat, spectra_cols){
  
  # ---------------------
  # LOGICAL CHECKS
  # ---------------------
  
  stopifnot(
    is.character(mass_dat),
    is.character(spectra_cols),
    mass_dat %in% colnames(dat),
    all(spectra_cols %in% colnames(dat))
  )
  
  mz <- dat[[mass_dat]]
  
  spectra <- lapply(spectra_cols, function(x){dat[x]})
  i <- do.call(what = data.frame, args = c(spectra))
  
  n <- nrow(dat)
  
  rms <- apply(i, 2, FUN = function(x) { .RMS(y = x, n = n)})
  
  i_rms <- t(t(i)*rms^-1)  # i / rms matrix multiplication
  
  dat <- data.frame(mz, i_rms)
  
  colnames(dat) <- c("full_mz", spectra_cols)
  
  return(dat)
  
}









