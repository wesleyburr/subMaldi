# -----------------------------------------------------------------------
# Last Updated: June 1, 2020
# Author: Kristen Yeh
# Title: subMALDI: Normalization Method - Maximum Intensity
# -----------------------------------------------------------------------


# -----------------------
# NORMALIZATION FUNCTION
# -----------------------


# Function rescales intensity to 0,1
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}


# ------------------------------
# METHOD: MAX. OF EACH SPECTRUM
# ------------------------------


normMethod_max <- function(dat, mass_dat, spec1, spec2 = NULL, 
                           spec3 = NULL, spec4 = NULL, spec5 = NULL, 
                           spec6 = NULL){
  
  if(is.null(spec6)){
  
  if(is.null(spec5)){
  
  if(is.null(spec4)){
  
  if(is.null(spec3)){
  
  if(is.null(spec2)){
    
  # Single spectrum
  mass <- dat[[mass_dat]]
  intense <- dat[[spec1]]
  
  max_Intensity <- max(intense)
  
  
  dat <- select(dat, all_of(mass_dat))
  dat <- transform(dat, "Normalized" = 0)
  dat["Normalized"] <- normalize(intense)
  return(dat)
  }
  
  # Two spectra
  else{
    mass <- dat[[mass_dat]]
    intense1 <- dat[[spec1]]
    intense2 <- dat[[spec2]]
    
    max_Intensity1 <- max(intense1)
    max_Intensity2 <- max(intense2)
    
    dat <- select(dat, all_of(mass_dat))
    dat[spec1] <- normalize(intense1)
    dat[spec2] <- normalize(intense2)
    return(dat)
  }}
  
  # Three spectra
  else{
    mass <- dat[[mass_dat]]
    intense1 <- dat[[spec1]]
    intense2 <- dat[[spec2]]
    intense3 <- dat[[spec3]]
    
    max_Intensity1 <- max(intense1)
    max_Intensity2 <- max(intense2)
    max_Intensity3 <- max(intense3)
    
    dat <- select(dat, all_of(mass_dat))
    dat[spec1] <- normalize(intense1)
    dat[spec2] <- normalize(intense2)
    dat[spec3] <- normalize(intense3)
    return(dat)
  }}
  
  # Four spectra
  else{
    mass <- dat[[mass_dat]]
    intense1 <- dat[[spec1]]
    intense2 <- dat[[spec2]]
    intense3 <- dat[[spec3]]
    intense4 <- dat[[spec4]]
    
    max_Intensity1 <- max(intense1)
    max_Intensity2 <- max(intense2)
    max_Intensity3 <- max(intense3)
    max_Intensity4 <- max(intense4)
    
    dat <- select(dat, all_of(mass_dat))
    dat[spec1] <- normalize(intense1)
    dat[spec2] <- normalize(intense2)
    dat[spec3] <- normalize(intense3)
    dat[spec4] <- normalize(intense4)
    return(dat)
  }}
  
  # Five spectra
  else{
    mass <- dat[[mass_dat]]
    intense1 <- dat[[spec1]]
    intense2 <- dat[[spec2]]
    intense3 <- dat[[spec3]]
    intense4 <- dat[[spec4]]
    intense5 <- dat[[spec5]]
    
    max_Intensity1 <- max(intense1)
    max_Intensity2 <- max(intense2)
    max_Intensity3 <- max(intense3)
    max_Intensity4 <- max(intense4)
    max_Intensity5 <- max(intense5)
    
    dat <- select(dat, all_of(mass_dat))
    dat[spec1] <- normalize(intense1)
    dat[spec2] <- normalize(intense2)
    dat[spec3] <- normalize(intense3)
    dat[spec4] <- normalize(intense4)
    dat[spec5] <- normalize(intense5)
    return(dat)
  }}
  
  # Six spectra
  else{
    mass <- dat[[mass_dat]]
    intense1 <- dat[[spec1]]
    intense2 <- dat[[spec2]]
    intense3 <- dat[[spec3]]
    intense4 <- dat[[spec4]]
    intense5 <- dat[[spec5]]
    intense6 <- dat[[spec6]]
    
    max_Intensity1 <- max(intense1)
    max_Intensity2 <- max(intense2)
    max_Intensity3 <- max(intense3)
    max_Intensity4 <- max(intense4)
    max_Intensity5 <- max(intense5)
    max_Intensity6 <- max(intense6)
    
    dat <- select(dat, all_of(mass_dat))
    dat[spec1] <- normalize(intense1)
    dat[spec2] <- normalize(intense2)
    dat[spec3] <- normalize(intense3)
    dat[spec4] <- normalize(intense4)
    dat[spec5] <- normalize(intense5)
    dat[spec6] <- normalize(intense6)
    return(dat)
  }
}


# ---------------------
# METHOD: MAX. OF SET
# ---------------------

# Normalization function specific to custom max_y
normalize_set <- function(y, max_y){
  return((y - min(y)) / (max_y - min(y)))
}

normMethod_max_set <- function(dat, mass_dat, spec1, spec2, 
                           spec3 = NULL, spec4 = NULL, spec5 = NULL, 
                           spec6 = NULL){
  
  if(is.null(spec6)){
  
  if(is.null(spec5)){
  
  if(is.null(spec4)){
  
  if(is.null(spec3)){
  # Two spectra
  sorted <- gather(dat, key = "Spectra", value = "Intensity", 
                   all_of(spec1), all_of(spec2), factor_key= TRUE)
  
  intense <- sorted$Intensity
  max_int <- max(intense)
  
  mass <- dat[[mass_dat]]
  intense1 <- dat[[spec1]]
  intense2 <- dat[[spec2]]
  
  max_Intensity1 <- max(intense1)
  max_Intensity2 <- max(intense2)
  
  dat <- select(dat, all_of(mass_dat))
  dat[spec1] <- normalize_set(y = intense1, max_y = max_int)
  dat[spec2] <- normalize_set(y = intense2, max_y = max_int)
  return(dat)
  }
  
  # Three spectra
  else{
    sorted <- gather(dat, key = "Spectra", value = "Intensity", 
                     all_of(spec1), all_of(spec2), all_of(spec3),
                     factor_key= TRUE)
    
    intense <- sorted$Intensity
    max_int <- max(intense)
    
    mass <- dat[[mass_dat]]
    intense1 <- dat[[spec1]]
    intense2 <- dat[[spec2]]
    intense3 <- dat[[spec3]]
    
    max_Intensity1 <- max(intense1)
    max_Intensity2 <- max(intense2)
    max_Intensity3 <- max(intense3)
    
    dat <- select(dat, all_of(mass_dat))
    dat[spec1] <- normalize_set(y = intense1, max_y = max_int)
    dat[spec2] <- normalize_set(y = intense2, max_y = max_int)
    dat[spec3] <- normalize_set(y = intense3, max_y = max_int)
    return(dat)
  }}
  
  # Four spectra
  else{
    sorted <- gather(dat, key = "Spectra", value = "Intensity", 
                     all_of(spec1), all_of(spec2), all_of(spec3),
                     all_of(spec4), factor_key= TRUE)
    
    intense <- sorted$Intensity
    max_int <- max(intense)
    
    mass <- dat[[mass_dat]]
    intense1 <- dat[[spec1]]
    intense2 <- dat[[spec2]]
    intense3 <- dat[[spec3]]
    intense4 <- dat[[spec4]]
    
    max_Intensity1 <- max(intense1)
    max_Intensity2 <- max(intense2)
    max_Intensity3 <- max(intense3)
    max_Intensity4 <- max(intense4)
    
    dat <- select(dat, all_of(mass_dat))
    dat[spec1] <- normalize_set(y = intense1, max_y = max_int)
    dat[spec2] <- normalize_set(y = intense2, max_y = max_int)
    dat[spec3] <- normalize_set(y = intense3, max_y = max_int)
    dat[spec4] <- normalize_set(y = intense4, max_y = max_int)
    return(dat)
  }}
  
  # Five spectra
  else{
    sorted <- gather(dat, key = "Spectra", value = "Intensity", 
                     all_of(spec1), all_of(spec2), all_of(spec3),
                     all_of(spec4),all_of(spec5), factor_key= TRUE)
    
    intense <- sorted$Intensity
    max_int <- max(intense)
    
    mass <- dat[[mass_dat]]
    intense1 <- dat[[spec1]]
    intense2 <- dat[[spec2]]
    intense3 <- dat[[spec3]]
    intense4 <- dat[[spec4]]
    intense5 <- dat[[spec5]]
    
    max_Intensity1 <- max(intense1)
    max_Intensity2 <- max(intense2)
    max_Intensity3 <- max(intense3)
    max_Intensity4 <- max(intense4)
    max_Intensity5 <- max(intense5)
    
    dat <- select(dat, all_of(mass_dat))
    dat[spec1] <- normalize_set(y = intense1, max_y = max_int)
    dat[spec2] <- normalize_set(y = intense2, max_y = max_int)
    dat[spec3] <- normalize_set(y = intense3, max_y = max_int)
    dat[spec4] <- normalize_set(y = intense4, max_y = max_int)
    dat[spec5] <- normalize_set(y = intense5, max_y = max_int)
    return(dat)
  }}
  
  # Six spectra
  else{
    sorted <- gather(dat, key = "Spectra", value = "Intensity", 
                     all_of(spec1), all_of(spec2), all_of(spec3),
                     all_of(spec4),all_of(spec5), all_of(spec6),
                     factor_key= TRUE)
    
    intense <- sorted$Intensity
    max_int <- max(intense)
    
    mass <- dat[[mass_dat]]
    intense1 <- dat[[spec1]]
    intense2 <- dat[[spec2]]
    intense3 <- dat[[spec3]]
    intense4 <- dat[[spec4]]
    intense5 <- dat[[spec5]]
    intense6 <- dat[[spec6]]
    
    max_Intensity1 <- max(intense1)
    max_Intensity2 <- max(intense2)
    max_Intensity3 <- max(intense3)
    max_Intensity4 <- max(intense4)
    max_Intensity5 <- max(intense5)
    max_Intensity6 <- max(intense6)
    
    dat <- select(dat, all_of(mass_dat))
    dat[spec1] <- normalize_set(y = intense1, max_y = max_int)
    dat[spec2] <- normalize_set(y = intense2, max_y = max_int)
    dat[spec3] <- normalize_set(y = intense3, max_y = max_int)
    dat[spec4] <- normalize_set(y = intense4, max_y = max_int)
    dat[spec5] <- normalize_set(y = intense5, max_y = max_int)
    dat[spec6] <- normalize_set(y = intense6, max_y = max_int)
    return(dat)
  }
}











































































