# -----------------------------------------------------------------------
# Last Updated: June 2, 2020
# Author: Kristen Yeh
# Title: subMALDI Peak Maximum Functions
# -----------------------------------------------------------------------


# -----------------
# FIND PEAK MAXIMA
# -----------------


# Find peak maximum and associated m/z of a single spectrum
find_max <- function (dat, mass_dat, spec1, spec2 = NULL, 
                      spec3 = NULL, spec4 = NULL, spec5 = NULL, spec6 = NULL){
  
  if(is.null(spec6)) {
    
    if(is.null(spec5)) {
      
      if(is.null(spec4)){
        
        if(is.null(spec3)){
          
          if(is.null(spec2)){
            # Single spectra
            mass <- dat[[mass_dat]]
            intense <- dat[[spec1]]
            
            max_Intensity <- max(intense)
            mz <- mass[which(intense == max_Intensity)]
            max_spec <- c(spec1, mz, max_Intensity)
            
            result <- data.frame("Spectrum" = 0, "Mass" = 0, "Max_Intensity" = 0)
            result [ 1,] <- max_spec
            return(result)} 
          
          # Two spectra
          else{
            mass <- dat[[mass_dat]]
            intense1 <- dat[[spec1]]
            intense2 <- dat[[spec2]]
            
            max_Intensity1 <- max(intense1)
            mz1 <- mass[which(intense1 == max_Intensity1)]
            max_spec1 <- c(spec1, mz1, max_Intensity1)
            
            max_Intensity2 <- max(intense2)
            mz2 <- mass[which(intense2 == max_Intensity2)]
            max_spec2 <- c(spec2, mz2, max_Intensity2)
            
            result <- data.frame("Spectrum" = 0, "Mass" = 0, "Max_Intensity" = 0)
            result[1, ] <- max_spec1
            result[2, ] <- max_spec2
            return(result)
          }} 
        
        # Three spectra
        else{
          mass <- dat[[mass_dat]]
          intense1 <- dat[[spec1]]
          intense2 <- dat[[spec2]]
          intense3 <- dat[[spec3]]
          
          max_Intensity1 <- max(intense1)
          mz1 <- mass[which(intense1 == max_Intensity1)]
          max_spec1 <- c(spec1, mz1, max_Intensity1)
          
          max_Intensity2 <- max(intense2)
          mz2 <- mass[which(intense2 == max_Intensity2)]
          max_spec2 <- c(spec2, mz2, max_Intensity2)
          
          max_Intensity3 <- max(intense3)
          mz3 <- mass[which(intense3 == max_Intensity3)]
          max_spec3 <- c(spec3, mz3, max_Intensity3)
          
          result <- data.frame("Spectrum" = 0, "Mass" = 0, "Max_Intensity" = 0)
          result[1, ] <- max_spec1
          result[2, ] <- max_spec2
          result[3, ] <- max_spec3
          return(result)
        }}
      
      # Four spectra
      else{
        mass <- dat[[mass_dat]]
        intense1 <- dat[[spec1]]
        intense2 <- dat[[spec2]]
        intense3 <- dat[[spec3]]
        intense4 <- dat[[spec4]]
        
        max_Intensity1 <- max(intense1)
        mz1 <- mass[which(intense1 == max_Intensity1)]
        max_spec1 <- c(spec1, mz1, max_Intensity1)
        
        max_Intensity2 <- max(intense2)
        mz2 <- mass[which(intense2 == max_Intensity2)]
        max_spec2 <- c(spec2, mz2, max_Intensity2)
        
        max_Intensity3 <- max(intense3)
        mz3 <- mass[which(intense3 == max_Intensity3)]
        max_spec3 <- c(spec3, mz3, max_Intensity3)
        
        max_Intensity4 <- max(intense4)
        mz4 <- mass[which(intense4 == max_Intensity4)]
        max_spec4 <- c(spec4, mz4, max_Intensity4)
        
        result <- data.frame("Spectrum" = 0, "Mass" = 0, "Max_Intensity" = 0)
        result[1, ] <- max_spec1
        result[2, ] <- max_spec2
        result[3, ] <- max_spec3
        result[4, ] <- max_spec4
        return(result)
      }} 
    
    # Five spectra
    else {
      mass <- dat[[mass_dat]]
      intense1 <- dat[[spec1]]
      intense2 <- dat[[spec2]]
      intense3 <- dat[[spec3]]
      intense4 <- dat[[spec4]]
      intense5 <- dat[[spec5]]
      
      max_Intensity1 <- max(intense1)
      mz1 <- mass[which(intense1 == max_Intensity1)]
      max_spec1 <- c(spec1, mz1, max_Intensity1)
      
      max_Intensity2 <- max(intense2)
      mz2 <- mass[which(intense2 == max_Intensity2)]
      max_spec2 <- c(spec2, mz2, max_Intensity2)
      
      max_Intensity3 <- max(intense3)
      mz3 <- mass[which(intense3 == max_Intensity3)]
      max_spec3 <- c(spec3, mz3, max_Intensity3)
      
      max_Intensity4 <- max(intense4)
      mz4 <- mass[which(intense4 == max_Intensity4)]
      max_spec4 <- c(spec4, mz4, max_Intensity4)
      
      max_Intensity5 <- max(intense5)
      mz5 <- mass[which(intense5 == max_Intensity5)]
      max_spec5 <- c(spec5, mz5, max_Intensity5)
      
      result <- data.frame("Spectrum" = 0, "Mass" = 0, "Max_Intensity" = 0)
      result[1, ] <- max_spec1
      result[2, ] <- max_spec2
      result[3, ] <- max_spec3
      result[4, ] <- max_spec4
      result[5, ] <- max_spec5
      return(result)
    }}
  
  # Six spectra!
  else{
    mass <- dat[[mass_dat]]
    intense1 <- dat[[spec1]]
    intense2 <- dat[[spec2]]
    intense3 <- dat[[spec3]]
    intense4 <- dat[[spec4]]
    intense5 <- dat[[spec5]]
    intense6 <- dat[[spec6]]
    
    max_Intensity1 <- max(intense1)
    mz1 <- mass[which(intense1 == max_Intensity1)]
    max_spec1 <- c(spec1, mz1, max_Intensity1)
    
    max_Intensity2 <- max(intense2)
    mz2 <- mass[which(intense2 == max_Intensity2)]
    max_spec2 <- c(spec2, mz2, max_Intensity2)
    
    max_Intensity3 <- max(intense3)
    mz3 <- mass[which(intense3 == max_Intensity3)]
    max_spec3 <- c(spec3, mz3, max_Intensity3)
    
    max_Intensity4 <- max(intense4)
    mz4 <- mass[which(intense4 == max_Intensity4)]
    max_spec4 <- c(spec4, mz4, max_Intensity4)
    
    max_Intensity5 <- max(intense5)
    mz5 <- mass[which(intense5 == max_Intensity5)]
    max_spec5 <- c(spec5, mz5, max_Intensity5)
    
    max_Intensity6 <- max(intense6)
    mz6 <- mass[which(intense6 == max_Intensity6)]
    max_spec6 <- c(spec6, mz6, max_Intensity6)
    
    result <- data.frame("Spectrum" = 0, "Mass" = 0, "Max_Intensity" = 0)
    result[1, ] <- max_spec1
    result[2, ] <- max_spec2
    result[3, ] <- max_spec3
    result[4, ] <- max_spec4
    result[5, ] <- max_spec5
    result[6, ] <- max_spec6
    return(result)
  }
}


# -----------------------------------------------------------------------


# ------------------------
# FIND PEAK MAXIMA OF SET
# ------------------------


find_max_set <- function(dat, mass_dat, spec1, spec2, 
                         spec3 = NULL, spec4 = NULL, spec5 = NULL, 
                         spec6 = NULL){
  
  if(is.null(spec6)){
  
  if(is.null(spec5)){
  
  if(is.null(spec4)){
  
  if(is.null(spec3)){
    
  # Two spectra
    
    # Sanity checks
    stopifnot(is.character(mass_dat), is.character(spec1),
              is.character(spec2), mass_dat %in% names(dat),
              spec1 %in% names(dat), 
              spec2 %in% names(dat))
  
  sorted <- gather(dat, key = "Spectra", value = "Intensity", 
                   all_of(spec1), all_of(spec2), factor_key= TRUE)
  
  intense <- sorted$Intensity
  mass <- sorted[[mass_dat]]
  spectra <- sorted$Spectra
  
  max_Intensity <- max(intense)
  mz <- mass[which(intense == max_Intensity)]
  spec <- spectra[which(intense == max_Intensity)]
  max_spec <- c(spec, mz, max_Intensity)
  
  result <- data.frame("Spectrum" = 0, "Mass" = 0, "Max_Intensity" = 0)
  result [ 1,] <- max_spec
  return(result)
  } 

  # Three spectra
  else {
    
    # Sanity checks
    stopifnot(is.character(mass_dat), is.character(spec1),
              is.character(spec2), is.character(spec3), 
              mass_dat %in% names(dat),
              spec1 %in% names(dat), 
              spec2 %in% names(dat),
              spec3 %in% names(dat))
    
    sorted <- gather(dat, key = "Spectra", value = "Intensity", 
                     all_of(spec1), all_of(spec2),
                     all_of(spec3), factor_key= TRUE)
    
    intense <- sorted$Intensity
    mass <- sorted[[mass_dat]]
    spectra <- sorted$Spectra
    
    max_Intensity <- max(intense)
    mz <- mass[which(intense == max_Intensity)]
    spec <- spectra[which(intense == max_Intensity)]
    max_spec <- c(spec, mz, max_Intensity)
    
    result <- data.frame("Spectrum" = 0, "Mass" = 0, "Max_Intensity" = 0)
    result [ 1,] <- max_spec
    return(result)
  }}
  
  # Four spectra
  else{
    
    # Sanity checks
    stopifnot(is.character(mass_dat), is.character(spec1),
              is.character(spec2), is.character(spec3), 
              is.character(spec4),
              mass_dat %in% names(dat),
              spec1 %in% names(dat), 
              spec2 %in% names(dat),
              spec3 %in% names(dat),
              spec4 %in% names(dat))
    
    sorted <- gather(dat, key = "Spectra", value = "Intensity", 
                     all_of(spec1), all_of(spec2),
                     all_of(spec3), all_of(spec4), factor_key= TRUE)
    
    intense <- sorted$Intensity
    mass <- sorted[[mass_dat]]
    spectra <- sorted$Spectra
    
    max_Intensity <- max(intense)
    mz <- mass[which(intense == max_Intensity)]
    spec <- spectra[which(intense == max_Intensity)]
    max_spec <- c(spec, mz, max_Intensity)
    
    result <- data.frame("Spectrum" = 0, "Mass" = 0, "Max_Intensity" = 0)
    result [ 1,] <- max_spec
    return(result)
  }}
  
  # Five spectra
  else{
    
    # Sanity checks
    stopifnot(is.character(mass_dat), is.character(spec1),
              is.character(spec2), is.character(spec3), 
              is.character(spec4), is.character(spec5),
              mass_dat %in% names(dat),
              spec1 %in% names(dat), 
              spec2 %in% names(dat),
              spec3 %in% names(dat),
              spec4 %in% names(dat),
              spec5 %in% names(dat))
    
    sorted <- gather(dat, key = "Spectra", value = "Intensity", 
                     all_of(spec1), all_of(spec2),
                     all_of(spec3), all_of(spec4),
                     all_of(spec5), factor_key= TRUE)
    
    intense <- sorted$Intensity
    mass <- sorted[[mass_dat]]
    spectra <- sorted$Spectra
    
    max_Intensity <- max(intense)
    mz <- mass[which(intense == max_Intensity)]
    spec <- spectra[which(intense == max_Intensity)]
    max_spec <- c(spec, mz, max_Intensity)
    
    result <- data.frame("Spectrum" = 0, "Mass" = 0, "Max_Intensity" = 0)
    result [ 1,] <- max_spec
    return(result)
  }}
  
  # Six spectra
  else{
    
    # Sanity checks
    stopifnot(is.character(mass_dat), is.character(spec1),
              is.character(spec2), is.character(spec3), 
              is.character(spec4), is.character(spec5),
              is.character(spec6), mass_dat %in% names(dat),
              spec1 %in% names(dat), 
              spec2 %in% names(dat),
              spec3 %in% names(dat),
              spec4 %in% names(dat),
              spec5 %in% names(dat),
              spec6 %in% names(dat))
    
    sorted <- gather(dat, key = "Spectra", value = "Intensity", 
                     all_of(spec1), all_of(spec2),
                     all_of(spec3), all_of(spec4),
                     all_of(spec5), all_of(spec6), factor_key= TRUE)
    
    intense <- sorted$Intensity
    mass <- sorted[[mass_dat]]
    spectra <- sorted$Spectra
    
    max_Intensity <- max(intense)
    mz <- mass[which(intense == max_Intensity)]
    spec <- spectra[which(intense == max_Intensity)]
    max_spec <- c(spec, mz, max_Intensity)
    
    result <- data.frame("Spectrum" = 0, "Mass" = 0, "Max_Intensity" = 0)
    result [ 1,] <- max_spec
    return(result)
  }
}


# -----------------------------------------------------------------------

