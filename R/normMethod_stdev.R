# -----------------------------------------------------------------------
# Last Updated: June 12, 2020
# Author: Kristen Yeh
# Title: subMALDI: Normalization Method - Standard Deviation of Noise
# -----------------------------------------------------------------------


# ------------------------------------
# METHOD: STANDARD DEVIATION OF NOISE
# ------------------------------------

# Find a region of the spectrum with only noise and minimal peaks
    # (Should be same region for all spectra)
# Evaluate standard deviation of intensity in that region for each spec
# Divide intensity of EACH PEAK IN SPEC by its st. dev.


.normMethod_stdev <- function(dat, mass_dat, lower = 900, upper= 1100 , spec1, spec2 = NULL, spec3 = NULL,
                              spec4 = NULL, spec5 = NULL, spec6 = NULL){
  
  if(is.null(spec6)){
  
  if(is.null(spec5)){
  
  if(is.null(spec4)){
  
  if(is.null(spec3)){
  
  if(is.null(spec2)){
    
    # One spectrum
    mass <- dat[[mass_dat]]
    intense <- dat[[spec1]]
    
    noise <- intense[which(mass > lower & upper > mass)]
    noise <- noise[ noise != 0 ]
    st_dev <- sd(noise)
    
    dat <- select(dat, all_of(mass_dat))
    dat[spec1] <- .norm_TIC(y = intense, all_y = st_dev)
    return(dat) }
  
  # ---------------------------------------------------------------------
  # Two spectra
  else{
    mass <- dat[[mass_dat]]
    intense1 <- dat[[spec1]]
    intense2 <- dat[[spec2]]
    
    noise1 <- intense1[which(mass > lower & upper > mass)]
    noise2 <- intense2[which(mass > lower & upper > mass)]
    
    noise1 <- noise1[ noise1 != 0 ]
    noise2 <- noise2[ noise2 != 0 ]
    
    st_dev1 <- sd(noise1)
    st_dev2 <- sd(noise2)
    
    dat <- select(dat, all_of(mass_dat))
    dat[spec1] <- .norm_TIC(y = intense1, all_y = st_dev1)
    dat[spec2] <- .norm_TIC(y = intense2, all_y = st_dev2)
    return(dat) } }
  
  # ---------------------------------------------------------------------
  # Three spectra
  else{
    mass <- dat[[mass_dat]]
    intense1 <- dat[[spec1]]
    intense2 <- dat[[spec2]]
    intense3 <- dat[[spec3]]
    
    noise1 <- intense1[which(mass > lower & upper > mass)]
    noise2 <- intense2[which(mass > lower & upper > mass)]
    noise3 <- intense3[which(mass > lower & upper > mass)]
    
    noise1 <- noise1[ noise1 != 0 ]
    noise2 <- noise2[ noise2 != 0 ]
    noise3 <- noise3[ noise3 != 0 ]
    
    st_dev1 <- sd(noise1)
    st_dev2 <- sd(noise2)
    st_dev3 <- sd(noise3)
    
    dat <- select(dat, all_of(mass_dat))
    dat[spec1] <- .norm_TIC(y = intense1, all_y = st_dev1)
    dat[spec2] <- .norm_TIC(y = intense2, all_y = st_dev2)
    dat[spec3] <- .norm_TIC(y = intense3, all_y = st_dev3)
    return(dat) } }
  
  # ---------------------------------------------------------------------
  # Four spectra
  else{
    mass <- dat[[mass_dat]]
    intense1 <- dat[[spec1]]
    intense2 <- dat[[spec2]]
    intense3 <- dat[[spec3]]
    intense4 <- dat[[spec4]]
    
    noise1 <- intense1[which(mass > lower & upper > mass)]
    noise2 <- intense2[which(mass > lower & upper > mass)]
    noise3 <- intense3[which(mass > lower & upper > mass)]
    noise4 <- intense4[which(mass > lower & upper > mass)]
    
    noise1 <- noise1[ noise1 != 0 ]
    noise2 <- noise2[ noise2 != 0 ]
    noise3 <- noise3[ noise3 != 0 ]
    noise4 <- noise4[ noise4 != 0 ]
    
    st_dev1 <- sd(noise1)
    st_dev2 <- sd(noise2)
    st_dev3 <- sd(noise3)
    st_dev4 <- sd(noise4)
    
    dat <- select(dat, all_of(mass_dat))
    dat[spec1] <- .norm_TIC(y = intense1, all_y = st_dev1)
    dat[spec2] <- .norm_TIC(y = intense2, all_y = st_dev2)
    dat[spec3] <- .norm_TIC(y = intense3, all_y = st_dev3)
    dat[spec4] <- .norm_TIC(y = intense4, all_y = st_dev4)
    return(dat) } }
    
    # ---------------------------------------------------------------------
    # Five spectra
    else{
      mass <- dat[[mass_dat]]
      intense1 <- dat[[spec1]]
      intense2 <- dat[[spec2]]
      intense3 <- dat[[spec3]]
      intense4 <- dat[[spec4]]
      intense5 <- dat[[spec5]]
      
      noise1 <- intense1[which(mass > lower & upper > mass)]
      noise2 <- intense2[which(mass > lower & upper > mass)]
      noise3 <- intense3[which(mass > lower & upper > mass)]
      noise4 <- intense4[which(mass > lower & upper > mass)]
      noise5 <- intense5[which(mass > lower & upper > mass)]
      
      noise1 <- noise1[ noise1 != 0 ]
      noise2 <- noise2[ noise2 != 0 ]
      noise3 <- noise3[ noise3 != 0 ]
      noise4 <- noise4[ noise4 != 0 ]
      noise5 <- noise5[ noise5 != 0 ]
      
      st_dev1 <- sd(noise1)
      st_dev2 <- sd(noise2)
      st_dev3 <- sd(noise3)
      st_dev4 <- sd(noise4)
      st_dev5 <- sd(noise5)
      
      dat <- select(dat, all_of(mass_dat))
      dat[spec1] <- .norm_TIC(y = intense1, all_y = st_dev1)
      dat[spec2] <- .norm_TIC(y = intense2, all_y = st_dev2)
      dat[spec3] <- .norm_TIC(y = intense3, all_y = st_dev3)
      dat[spec4] <- .norm_TIC(y = intense4, all_y = st_dev4)
      dat[spec5] <- .norm_TIC(y = intense5, all_y = st_dev5)
      return(dat) } }
  
  # ---------------------------------------------------------------------
  # Six spectra
  else{
    mass <- dat[[mass_dat]]
    intense1 <- dat[[spec1]]
    intense2 <- dat[[spec2]]
    intense3 <- dat[[spec3]]
    intense4 <- dat[[spec4]]
    intense5 <- dat[[spec5]]
    intense6 <- dat[[spec6]]
    
    noise1 <- intense1[which(mass > lower & upper > mass)]
    noise2 <- intense2[which(mass > lower & upper > mass)]
    noise3 <- intense3[which(mass > lower & upper > mass)]
    noise4 <- intense4[which(mass > lower & upper > mass)]
    noise5 <- intense5[which(mass > lower & upper > mass)]
    noise6 <- intense6[which(mass > lower & upper > mass)]
    
    noise1 <- noise1[ noise1 != 0 ]
    noise2 <- noise2[ noise2 != 0 ]
    noise3 <- noise3[ noise3 != 0 ]
    noise4 <- noise4[ noise4 != 0 ]
    noise5 <- noise5[ noise5 != 0 ]
    noise6 <- noise6[ noise6 != 0 ]
    
    st_dev1 <- sd(noise1)
    st_dev2 <- sd(noise2)
    st_dev3 <- sd(noise3)
    st_dev4 <- sd(noise4)
    st_dev5 <- sd(noise5)
    st_dev6 <- sd(noise6)
    
    dat <- select(dat, all_of(mass_dat))
    dat[spec1] <- .norm_TIC(y = intense1, all_y = st_dev1)
    dat[spec2] <- .norm_TIC(y = intense2, all_y = st_dev2)
    dat[spec3] <- .norm_TIC(y = intense3, all_y = st_dev3)
    dat[spec4] <- .norm_TIC(y = intense4, all_y = st_dev4)
    dat[spec5] <- .norm_TIC(y = intense5, all_y = st_dev5)
    dat[spec6] <- .norm_TIC(y = intense6, all_y = st_dev6)
    return(dat)
  }
}
























