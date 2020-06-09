# -----------------------------------------------------------------------
# Last Updated: June 8, 2020
# Author: Kristen Yeh
# Title: subMALDI: Normalization Method - TIC
# -----------------------------------------------------------------------


# -------------------------
# METHOD: TOTAL ION COUNT
# -------------------------


# Divide intensity of each peak in spectrum by sum of all intensities
.norm_TIC <- function(y, all_y){
  return(y/all_y)
}

.normMethod_TIC <- function(dat, mass_dat, spec1, spec2 = NULL, 
                           spec3 = NULL, spec4 = NULL, spec5 = NULL, spec6 = NULL){
  
  if(is.null(spec6)){
  
  if(is.null(spec5)){
  
  if(is.null(spec4)){
  
  if(is.null(spec3)){
  
  if(is.null(spec2)){
    
  # One spectrum
  intense <- dat[[spec1]]
  i <- sum(intense)
  
  dat <- select(dat, all_of(mass_dat))
  dat <- transform(dat, "Normalized" = 0)
  dat$Normalized <- .norm_TIC(y = intense, all_y = i)
  return(dat)}
  
  # Two spectra
  else{
    intense1 <- dat[[spec1]]
    intense2 <- dat[[spec2]]
    
    i1 <- sum(intense1)
    i2 <- sum(intense2)
    
    dat <- select(dat, all_of(mass_dat))
    dat[spec1] <- .norm_TIC(y = intense1, all_y = i1)
    dat[spec2] <- .norm_TIC(y = intense2, all_y = i2)
    return(dat)}}
  
  # Three spectra
  else{
    intense1 <- dat[[spec1]]
    intense2 <- dat[[spec2]]
    intense3 <- dat[[spec3]]
    
    i1 <- sum(intense1)
    i2 <- sum(intense2)
    i3 <- sum(intense3)
    
    dat <- select(dat, all_of(mass_dat))
    dat[spec1] <- .norm_TIC(y = intense1, all_y = i1)
    dat[spec2] <- .norm_TIC(y = intense2, all_y = i2)
    dat[spec3] <- .norm_TIC(y = intense3, all_y = i3)
    return(dat)}}
  
  # Four spectra
  else{
    intense1 <- dat[[spec1]]
    intense2 <- dat[[spec2]]
    intense3 <- dat[[spec3]]
    intense4 <- dat[[spec4]]
    
    i1 <- sum(intense1)
    i2 <- sum(intense2)
    i3 <- sum(intense3)
    i4 <- sum(intense4)
    
    dat <- select(dat, all_of(mass_dat))
    dat[spec1] <- .norm_TIC(y = intense1, all_y = i1)
    dat[spec2] <- .norm_TIC(y = intense2, all_y = i2)
    dat[spec3] <- .norm_TIC(y = intense3, all_y = i3)
    dat[spec4] <- .norm_TIC(y = intense4, all_y = i4)
    return(dat)}}
  
  # Five spectra
  else{
    intense1 <- dat[[spec1]]
    intense2 <- dat[[spec2]]
    intense3 <- dat[[spec3]]
    intense4 <- dat[[spec4]]
    intense5 <- dat[[spec5]]
    
    i1 <- sum(intense1)
    i2 <- sum(intense2)
    i3 <- sum(intense3)
    i4 <- sum(intense4)
    i5 <- sum(intense5)
    
    dat <- select(dat, all_of(mass_dat))
    dat[spec1] <- .norm_TIC(y = intense1, all_y = i1)
    dat[spec2] <- .norm_TIC(y = intense2, all_y = i2)
    dat[spec3] <- .norm_TIC(y = intense3, all_y = i3)
    dat[spec4] <- .norm_TIC(y = intense4, all_y = i4)
    dat[spec5] <- .norm_TIC(y = intense5, all_y = i5)
    return(dat)}}
  
  # Six spectra
  else{
    intense1 <- dat[[spec1]]
    intense2 <- dat[[spec2]]
    intense3 <- dat[[spec3]]
    intense4 <- dat[[spec4]]
    intense5 <- dat[[spec5]]
    intense6 <- dat[[spec6]]
    
    i1 <- sum(intense1)
    i2 <- sum(intense2)
    i3 <- sum(intense3)
    i4 <- sum(intense4)
    i5 <- sum(intense5)
    i6 <- sum(intense6)
    
    dat <- select(dat, all_of(mass_dat))
    dat[spec1] <- .norm_TIC(y = intense1, all_y = i1)
    dat[spec2] <- .norm_TIC(y = intense2, all_y = i2)
    dat[spec3] <- .norm_TIC(y = intense3, all_y = i3)
    dat[spec4] <- .norm_TIC(y = intense4, all_y = i4)
    dat[spec5] <- .norm_TIC(y = intense5, all_y = i5)
    dat[spec6] <- .norm_TIC(y = intense6, all_y = i6)
    return(dat)}
}










