# -----------------------------------------------------------------------
# Last Updated: June 9, 2020
# Author: Kristen Yeh
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
  return(y/RMS)
}

.normMethod_RMS <- function(dat, mass_dat, spec1, spec2 = NULL, 
                            spec3 = NULL, spec4 = NULL, spec5 = NULL, spec6 = NULL){
  
  if(is.null(spec6)){
  
  if(is.null(spec5)){
  
  if(is.null(spec4)){
  
  if(is.null(spec3)){
  
  if(is.null(spec2)){
  
  # One spectra
  intense <- dat[[spec1]]
  dat <- select(dat, all_of(mass_dat))
  
  n <- length(intense)
  rms <- .RMS(y = intense, n = n)
  dat[spec1] <- .norm_RMS(y = intense, RMS = rms)
  return(dat)}
  
  # Two spectra
  else{
    intense1 <- dat[[spec1]]
    intense2 <- dat[[spec2]]
    dat <- select(dat, all_of(mass_dat))
    
    n1 <- length(intense1)
    n2 <- length(intense2)
    
    rms1 <- .RMS(y = intense1, n = n1)
    rms2 <- .RMS(y = intense2, n = n2)
    
    dat[spec1] <- .norm_RMS(y = intense1, RMS = rms1)
    dat[spec2] <- .norm_RMS(y = intense2, RMS = rms2)
    return(dat)}}
  
  # Three spectra
  else{
    intense1 <- dat[[spec1]]
    intense2 <- dat[[spec2]]
    intense3 <- dat[[spec3]]
    dat <- select(dat, all_of(mass_dat))
    
    n1 <- length(intense1)
    n2 <- length(intense2)
    n3 <- length(intense3)
    
    rms1 <- .RMS(y = intense1, n = n1)
    rms2 <- .RMS(y = intense2, n = n2)
    rms3 <- .RMS(y = intense3, n = n3)
    
    dat[spec1] <- .norm_RMS(y = intense1, RMS = rms1)
    dat[spec2] <- .norm_RMS(y = intense2, RMS = rms2)
    dat[spec3] <- .norm_RMS(y = intense3, RMS = rms3)
    return(dat)}}
  
  # Four spectra
  else {
    intense1 <- dat[[spec1]]
    intense2 <- dat[[spec2]]
    intense3 <- dat[[spec3]]
    intense4 <- dat[[spec4]]
    dat <- select(dat, all_of(mass_dat))
    
    n1 <- length(intense1)
    n2 <- length(intense2)
    n3 <- length(intense3)
    n4 <- length(intense4)
    
    rms1 <- .RMS(y = intense1, n = n1)
    rms2 <- .RMS(y = intense2, n = n2)
    rms3 <- .RMS(y = intense3, n = n3)
    rms4 <- .RMS(y = intense4, n = n4)
    
    dat[spec1] <- .norm_RMS(y = intense1, RMS = rms1)
    dat[spec2] <- .norm_RMS(y = intense2, RMS = rms2)
    dat[spec3] <- .norm_RMS(y = intense3, RMS = rms3)
    dat[spec4] <- .norm_RMS(y = intense4, RMS = rms4)
    return(dat)}}
  
  # Five spectra
  else{
    intense1 <- dat[[spec1]]
    intense2 <- dat[[spec2]]
    intense3 <- dat[[spec3]]
    intense4 <- dat[[spec4]]
    intense5 <- dat[[spec5]]
    dat <- select(dat, all_of(mass_dat))
    
    n1 <- length(intense1)
    n2 <- length(intense2)
    n3 <- length(intense3)
    n4 <- length(intense4)
    n5 <- length(intense5)
    
    rms1 <- .RMS(y = intense1, n = n1)
    rms2 <- .RMS(y = intense2, n = n2)
    rms3 <- .RMS(y = intense3, n = n3)
    rms4 <- .RMS(y = intense4, n = n4)
    rms5 <- .RMS(y = intense5, n = n5)
    
    dat[spec1] <- .norm_RMS(y = intense1, RMS = rms1)
    dat[spec2] <- .norm_RMS(y = intense2, RMS = rms2)
    dat[spec3] <- .norm_RMS(y = intense3, RMS = rms3)
    dat[spec4] <- .norm_RMS(y = intense4, RMS = rms4)
    dat[spec5] <- .norm_RMS(y = intense5, RMS = rms5)
    return(dat)}}
  
  # Six spectra
  else{
    intense1 <- dat[[spec1]]
    intense2 <- dat[[spec2]]
    intense3 <- dat[[spec3]]
    intense4 <- dat[[spec4]]
    intense5 <- dat[[spec5]]
    intense6 <- dat[[spec6]]
    dat <- select(dat, all_of(mass_dat))
    
    n1 <- length(intense1)
    n2 <- length(intense2)
    n3 <- length(intense3)
    n4 <- length(intense4)
    n5 <- length(intense5)
    n6 <- length(intense6)
    
    rms1 <- .RMS(y = intense1, n = n1)
    rms2 <- .RMS(y = intense2, n = n2)
    rms3 <- .RMS(y = intense3, n = n3)
    rms4 <- .RMS(y = intense4, n = n4)
    rms5 <- .RMS(y = intense5, n = n5)
    rms6 <- .RMS(y = intense6, n = n6)
    
    dat[spec1] <- .norm_RMS(y = intense1, RMS = rms1)
    dat[spec2] <- .norm_RMS(y = intense2, RMS = rms2)
    dat[spec3] <- .norm_RMS(y = intense3, RMS = rms3)
    dat[spec4] <- .norm_RMS(y = intense4, RMS = rms4)
    dat[spec5] <- .norm_RMS(y = intense5, RMS = rms5)
    dat[spec6] <- .norm_RMS(y = intense6, RMS = rms6)
    return(dat)}
}



























