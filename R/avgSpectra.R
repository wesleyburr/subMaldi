# --------------------------------------------------------------------------------------------
# Date: June 23, 2020
# Author: Kristen Yeh
# Title: subMALDI Average Spectra
# --------------------------------------------------------------------------------------------

# Average intensities across rows per sample
# First user should organize standardized data frame so all samples are together
# First column should always be full_mz, the rest should be spectra, each scan per column

# UPDATE JUNE 2020:
# Editing the function so that you select which spectra are averaged
# New column is made in original df beside the selected spectra

avgSpectra <- function(dat, method = "mean", spec1, spec2, 
                       spec3 = NULL, spec4 = NULL, spec5 = NULL, spec6 = NULL){
  
  if(method == "sum"){
    .avg_sum(dat = dat, spec1 = spec1, spec2 = spec2, spec3 = spec3, spec4 = spec4,
             spec5 = spec5, spec6 = spec6) } 
  else{
    .avg_mean(dat = dat, spec1 = spec1, spec2 = spec2, spec3 = spec3, spec4 = spec4,
             spec5 = spec5, spec6 = spec6) }
}

# --------------
# METHOD = SUM
# --------------


.avg_sum <- function(dat, spec1, spec2, spec3 = NULL, spec4 = NULL,
                     spec5 = NULL, spec6 = NULL){
  
  if(is.null(spec6)){
  
  if(is.null(spec5)){
  
  if(is.null(spec4)){
  
  if(is.null(spec3)){
  
  if(is.null(spec2)){
  
  # One spectrum
  stop("Only one spectrum input. Please enter two spectra for averaging.")}

  # ---------------------------------------------------------------------
  # Two spectra
  else{
    i1 <- dat[[spec1]]
    i2 <- dat[[spec2]]
    i <- data.frame(i1,i2)
    i <- transform(i, avg = apply(i,1, sum, na.rm = TRUE))
    
    dat <- transform(dat, "Sum" = 0)
    dat["Sum"] <- i$avg
    return(dat) }}
  
  # ---------------------------------------------------------------------
  # Three spectra
  else{
    i1 <- dat[[spec1]]
    i2 <- dat[[spec2]]
    i3 <- dat[[spec3]]
    i <- data.frame(i1,i2,i3)
    i <- transform(i, avg = apply(i,1, sum, na.rm = TRUE))
    
    dat <- transform(dat, "Sum" = 0)
    dat["Sum"] <- i$avg
    return(dat) }}
  
  # ---------------------------------------------------------------------
  #Four spectra
  else{
    i1 <- dat[[spec1]]
    i2 <- dat[[spec2]]
    i3 <- dat[[spec3]]
    i4 <- dat[[spec4]]
    i <- data.frame(i1,i2,i3,i4)
    i <- transform(i, avg = apply(i,1, sum, na.rm = TRUE))
    
    dat <- transform(dat, "Sum" = 0)
    dat["Sum"] <- i$avg
    return(dat) }}

  # ---------------------------------------------------------------------
  #Five spectra
  else{
    i1 <- dat[[spec1]]
    i2 <- dat[[spec2]]
    i3 <- dat[[spec3]]
    i4 <- dat[[spec4]]
    i5 <- dat[[spec5]]
    i <- data.frame(i1,i2,i3,i4,i5)
    i <- transform(i, avg = apply(i,1, sum, na.rm = TRUE))
    
    dat <- transform(dat, "Sum" = 0)
    dat["Sum"] <- i$avg
    return(dat) }}

  # ---------------------------------------------------------------------
  # Six spectra
  else{
    i1 <- dat[[spec1]]
    i2 <- dat[[spec2]]
    i3 <- dat[[spec3]]
    i4 <- dat[[spec4]]
    i5 <- dat[[spec5]]
    i6 <- dat[[spec6]]
    i <- data.frame(i1,i2,i3,i4,i5,i6)
    i <- transform(i, avg = apply(i,1, sum, na.rm = TRUE))
    
    dat <- transform(dat, "Sum" = 0)
    dat["Sum"] <- i$avg
    return(dat)
  }
}


# --------------
# METHOD = MEAN
# --------------

.avg_mean <- function(dat, spec1, spec2, spec3 = NULL, spec4 = NULL,
                      spec5 = NULL, spec6 = NULL){
  
  if(is.null(spec6)){
    
    if(is.null(spec5)){
      
      if(is.null(spec4)){
        
        if(is.null(spec3)){
          
          if(is.null(spec2)){
            
            # One spectrum
            stop("Only one spectrum input. Please enter two spectra for averaging.")}
          
          # ---------------------------------------------------------------------
          # Two spectra
          else{
            i1 <- dat[[spec1]]
            i2 <- dat[[spec2]]
            i <- data.frame(i1,i2)
            i <- transform(i, avg = apply(i,1, mean, na.rm = TRUE))
            
            dat <- transform(dat, "Average" = 0)
            dat["Average"] <- i$avg
            return(dat) }}
        
        # ---------------------------------------------------------------------
        # Three spectra
        else{
          i1 <- dat[[spec1]]
          i2 <- dat[[spec2]]
          i3 <- dat[[spec3]]
          i <- data.frame(i1,i2,i3)
          i <- transform(i, avg = apply(i,1, mean, na.rm = TRUE))
          
          dat <- transform(dat, "Average" = 0)
          dat["Average"] <- i$avg
          return(dat) }}
      
      # ---------------------------------------------------------------------
      #Four spectra
      else{
        i1 <- dat[[spec1]]
        i2 <- dat[[spec2]]
        i3 <- dat[[spec3]]
        i4 <- dat[[spec4]]
        i <- data.frame(i1,i2,i3,i4)
        i <- transform(i, avg = apply(i,1, mean, na.rm = TRUE))
        
        dat <- transform(dat, "Average" = 0)
        dat["Average"] <- i$avg
        return(dat) }}
    
    # ---------------------------------------------------------------------
    #Five spectra
    else{
      i1 <- dat[[spec1]]
      i2 <- dat[[spec2]]
      i3 <- dat[[spec3]]
      i4 <- dat[[spec4]]
      i5 <- dat[[spec5]]
      i <- data.frame(i1,i2,i3,i4,i5)
      i <- transform(i, avg = apply(i,1, mean, na.rm = TRUE))
      
      dat <- transform(dat, "Average" = 0)
      dat["Average"] <- i$avg
      return(dat) }}
  
  # ---------------------------------------------------------------------
  # Six spectra
  else{
    i1 <- dat[[spec1]]
    i2 <- dat[[spec2]]
    i3 <- dat[[spec3]]
    i4 <- dat[[spec4]]
    i5 <- dat[[spec5]]
    i6 <- dat[[spec6]]
    i <- data.frame(i1,i2,i3,i4,i5,i6)
    i <- transform(i, avg = apply(i,1, mean, na.rm = TRUE))
    
    dat <- transform(dat, "Average" = 0)
    dat["Average"] <- i$avg
    return(dat)
  }
}


















