# -----------------------------------------------------------------------
# Last Updated: June 12, 2020
# Author: Kristen Yeh
# Title: subMALDI: Normalization Method - TIC
# -----------------------------------------------------------------------


# ----------------------
# METHOD: NORMALIZE TIC
# ----------------------

# This method evaluates the sum of all intensities of each spectrum in a set.
# If the sum is not equivalent between spectra, multiplies the spectra by
# normalization factor.


.normMethod_TIC <- function(dat, mass_dat, spec1, spec2 = NULL, 
                            spec3 = NULL, spec4 = NULL, spec5 = NULL, spec6 = NULL){
  
  if(is.null(spec6)){
  
  if(is.null(spec5)){
  
  if(is.null(spec4)){
  
  if(is.null(spec3)){
    
    if(is.null(spec2)){
      
      # One spectrum
      stop("Only one spectrum input. Please enter two spectra for comparison.")}
    
    # ---------------------------------------------------------------------
    # Two spectra
    else{
      intense1 <- dat[[spec1]]
      intense2 <- dat[[spec2]]
      
      i1 <- sum(intense1)
      i2 <- sum(intense2)
      
      if(i1 > i2){
        i <- i1/i2
        dat <- select(dat, all_of(mass_dat))
        dat[spec1] <- intense1
        dat[spec2] <- intense2*i
        return(dat)} 
      else{
        i <- i2/i1
        dat <- select(dat, all_of(mass_dat))
        dat[spec1] <- intense1*i
        dat[spec2] <- intense2
        return(dat)}}}
  
  # ---------------------------------------------------------------------
  # Three spectra
  else{
    intense1 <- dat[[spec1]]
    intense2 <- dat[[spec2]]
    intense3 <- dat[[spec3]]
    
    i1 <- sum(intense1)
    i2 <- sum(intense2)
    i3 <- sum(intense3)
    t <- c(i1,i2,i3)
    t_val <- max(t)
    
    if(i1 == t_val){
      a <- i1/i2
      b <- i1/i3
      
      dat <- select(dat, all_of(mass_dat))
      dat[spec1] <- intense1
      dat[spec2] <- intense2*a
      dat[spec3] <- intense3*b
      return(dat)}
    else if(i2 == t_val){
      a <- i2/i1
      b <- i2/i3
      
      dat <- select(dat, all_of(mass_dat))
      dat[spec1] <- intense1*a
      dat[spec2] <- intense2
      dat[spec3] <- intense3*b
      return(dat)}
    else{
      a <- i3/i1
      b <- i3/i2
      
      dat <- select(dat, all_of(mass_dat))
      dat[spec1] <- intense1*a
      dat[spec2] <- intense2*b
      dat[spec3] <- intense3
      return(dat)}}}
  
  # ---------------------------------------------------------------------
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
    t <- c(i1,i2,i3,4)
    t_val <- max(t)
    
    if(i1 == t_val){
      a <- i1/i2
      b <- i1/i3
      c <- i1/i4
      
      dat <- select(dat, all_of(mass_dat))
      dat[spec1] <- intense1
      dat[spec2] <- intense2*a
      dat[spec3] <- intense3*b
      dat[spec4] <- intense4*c
      return(dat)}
    else if(i2 == t_val){
      a <- i2/i1
      b <- i2/i3
      c <- i2/i4
      
      dat <- select(dat, all_of(mass_dat))
      dat[spec1] <- intense1*a
      dat[spec2] <- intense2
      dat[spec3] <- intense3*b
      dat[spec4] <- intense4*c
      return(dat)}
    else if(i3 == t_val){
      a <- i3/i1
      b <- i3/i2
      c <- i3/i4
      
      dat <- select(dat, all_of(mass_dat))
      dat[spec1] <- intense1*a
      dat[spec2] <- intense2*b
      dat[spec3] <- intense3
      dat[spec4] <- intense4*c
      return(dat)}
    else{
      a <- i4/i1
      b <- i4/i2
      c <- i4 <- i3
      
      dat <- select(dat, all_of(mass_dat))
      dat[spec1] <- intense1*a
      dat[spec2] <- intense2*b
      dat[spec3] <- intense3*c
      dat[spec4] <- intense4
      return(dat)}}}
  
  # ---------------------------------------------------------------------
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
    t <- c(i1,i2,i3,i4,i5)
    t_val <- max(t)
    
    if(i1 == t_val){
      a <- i1/i2
      b <- i1/i3
      c <- i1/i4
      d <- i1/i5
      
      dat <- select(dat, all_of(mass_dat))
      dat[spec1] <- intense1
      dat[spec2] <- intense2*a
      dat[spec3] <- intense3*b
      dat[spec4] <- intense4*c
      dat[spec5] <- intense5*d
      return(dat)}
    else if(i2 == t_val){
      a <- i2/i1
      b <- i2/i3
      c <- i2/i4
      d <- i2/i5
      
      dat <- select(dat, all_of(mass_dat))
      dat[spec1] <- intense1*a
      dat[spec2] <- intense2
      dat[spec3] <- intense3*b
      dat[spec4] <- intense4*c
      dat[spec5] <- intense5*d
      return(dat)}
    else if(i3 == t_val){
      a <- i3/i1
      b <- i3/i2
      c <- i3/i4
      d <- i3/i5
      
      dat <- select(dat, all_of(mass_dat))
      dat[spec1] <- intense1*a
      dat[spec2] <- intense2*b
      dat[spec3] <- intense3
      dat[spec4] <- intense4*c
      dat[spec5] <- intense5*d
      return(dat)}
    else if(i4 == t_val){
      a <- i4/i1
      b <- i4/i2
      c <- i4/i3
      d <- i4/i5
      
      dat <- select(dat, all_of(mass_dat))
      dat[spec1] <- intense1*a
      dat[spec2] <- intense2*b
      dat[spec3] <- intense3*c
      dat[spec4] <- intense4
      dat[spec5] <- intense5*d
      return(dat)}
    else{
      a <- i5/i1
      b <- i5/i2
      c <- i5/i3
      d <- i5/i4
      
      dat <- select(dat, all_of(mass_dat))
      dat[spec1] <- intense1*a
      dat[spec2] <- intense2*b
      dat[spec3] <- intense3*c
      dat[spec4] <- intense4*d
      dat[spec5] <- intense5
      return(dat)}}}
  
  # ---------------------------------------------------------------------
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
    t <- c(i1,i2,i3,i4,i5,i6)
    t_val <- max(t)
    
    if(i1 == t_val){
      a <- i1/i2
      b <- i1/i3
      c <- i1/i4
      d <- i1/i5
      e <- i1/i6
      
      dat <- select(dat, all_of(mass_dat))
      dat[spec1] <- intense1
      dat[spec2] <- intense2*a
      dat[spec3] <- intense3*b
      dat[spec4] <- intense4*c
      dat[spec5] <- intense5*d
      dat[spec6] <- intense6*e
      return(dat)}
    else if(i2 == t_val){
      a <- i2/i1
      b <- i2/i3
      c <- i2/i4
      d <- i2/i5
      e <- i2/i6
      
      dat <- select(dat, all_of(mass_dat))
      dat[spec1] <- intense1*a
      dat[spec2] <- intense2
      dat[spec3] <- intense3*b
      dat[spec4] <- intense4*c
      dat[spec5] <- intense5*d
      dat[spec6] <- intense6*e
      return(dat)}
    else if(i3 == t_val){
      a <- i3/i1
      b <- i3/i2
      c <- i3/i4
      d <- i3/i5
      e <- i3/i6
      
      dat <- select(dat, all_of(mass_dat))
      dat[spec1] <- intense1*a
      dat[spec2] <- intense2*b
      dat[spec3] <- intense3
      dat[spec4] <- intense4*c
      dat[spec5] <- intense5*d
      dat[spec6] <- intense6*e
      return(dat)}
    else if(i4 == t_val){
      a <- i4/i1
      b <- i4/i2
      c <- i4/i3
      d <- i4/i5
      e <- i4/i6
      
      dat <- select(dat, all_of(mass_dat))
      dat[spec1] <- intense1*a
      dat[spec2] <- intense2*b
      dat[spec3] <- intense3*c
      dat[spec4] <- intense4
      dat[spec5] <- intense5*d
      dat[spec6] <- intense6*e
      return(dat)}
    else if(i5 == t_val){
      a <- i5/i1
      b <- i5/i2
      c <- i5/i3
      d <- i5/i4
      e <- i5/i6
      
      dat <- select(dat, all_of(mass_dat))
      dat[spec1] <- intense1*a
      dat[spec2] <- intense2*b
      dat[spec3] <- intense3*c
      dat[spec4] <- intense4*d
      dat[spec5] <- intense5
      dat[spec6] <- intense6*e
      return(dat)}
    else{
      a <- i6/i1
      b <- i6/i2
      c <- i6/i3
      d <- i6/i4
      e <- i6/i5
      
      dat <- select(dat, all_of(mass_dat))
      dat[spec1] <- intense1*a
      dat[spec2] <- intense2*b
      dat[spec3] <- intense3*c
      dat[spec4] <- intense4*d
      dat[spec5] <- intense5*e
      dat[spec6] <- intense6
      return(dat)}
    }
}


# -----------------------------------------------------------------------


# ---------------------
# METHOD: RELATIVE TIC
# ---------------------

# This method evaluates the sum of all intensities of each spectrum in a set.
# If the sum is not equivalent between spectra, multiplies the spectra by
# normalization factor.
# Each peak in each spectrum is then divided by the normalized TIC.

# Divide intensity of each peak in spectrum by sum of all intensities
.norm_TIC <- function(y, all_y){
  return(y/all_y)
}

.normMethod_rel_TIC <- function(dat, mass_dat, spec1, spec2 = NULL, 
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
          
          # ---------------------------------------------------------------------
          # Two spectra
          else{
            intense1 <- dat[[spec1]]
            intense2 <- dat[[spec2]]
            
            i1 <- sum(intense1)
            i2 <- sum(intense2)
            
            if(i1 > i2){
              i <- i1/i2
              dat <- select(dat, all_of(mass_dat))
              intense2 <- intense2*i
              tic <- sum(intense1)
              
              dat[spec1] <- .norm_TIC(y = intense1, all_y = tic)
              dat[spec2] <- .norm_TIC(y = intense2, all_y = tic)
              return(dat)} 
            
            else {
              i <- i2/i1
              dat <- select(dat, all_of(mass_dat))
              intense1 <- intense1*i
              tic <- sum(intense2)
              
              dat[spec1] <- .norm_TIC(y = intense1, all_y = tic)
              dat[spec2] <- .norm_TIC(y = intense2, all_y = tic)
              return(dat)}}}
  
  # ---------------------------------------------------------------------
  # Three spectra
  else{
    intense1 <- dat[[spec1]]
    intense2 <- dat[[spec2]]
    intense3 <- dat[[spec3]]
    
    i1 <- sum(intense1)
    i2 <- sum(intense2)
    i3 <- sum(intense3)
    t <- c(i1,i2,i3)
    t_val <- max(t)
    
    if(i1 == t_val){
      a <- i1/i2
      b <- i1/i3
      
      dat <- select(dat, all_of(mass_dat))
      intense2 <- intense2*a
      intense3 <- intense3*b
      
      dat[spec1] <- .norm_TIC(y = intense1, all_y = t_val)
      dat[spec2] <- .norm_TIC(y = intense2, all_y = t_val)
      dat[spec3] <- .norm_TIC(y = intense3, all_y = t_val)
      return(dat)}
    else if(i2 == t_val){
      a <- i2/i1
      b <- i2/i3
      
      dat <- select(dat, all_of(mass_dat))
      intense1 <- intense1*a
      intense3 <- intense3*b
      
      dat[spec1] <- .norm_TIC(y = intense1, all_y = t_val)
      dat[spec2] <- .norm_TIC(y = intense2, all_y = t_val)
      dat[spec3] <- .norm_TIC(y = intense3, all_y = t_val)
      return(dat)}
    else{
      a <- i3/i1
      b <- i3/i2
      
      dat <- select(dat, all_of(mass_dat))
      intense1 <- intense1*a
      intense2  <- intense2*b
      
      dat[spec1] <- .norm_TIC(y = intense1, all_y = t_val)
      dat[spec2] <- .norm_TIC(y = intense2, all_y = t_val)
      dat[spec3] <- .norm_TIC(y = intense3, all_y = t_val)
      return(dat)}}}
    
    # ---------------------------------------------------------------------
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
      t <- c(i1,i2,i3,4)
      t_val <- max(t)
      
      if(i1 == t_val){
        a <- i1/i2
        b <- i1/i3
        c <- i1/i4
        
        dat <- select(dat, all_of(mass_dat))
        intense2 <- intense2*a
        intense3 <- intense3*b
        intense4 <- intense4*c
        
        dat[spec1] <- .norm_TIC(y = intense1, all_y = t_val)
        dat[spec2] <- .norm_TIC(y = intense2, all_y = t_val)
        dat[spec3] <- .norm_TIC(y = intense3, all_y = t_val)
        dat[spec4] <- .norm_TIC(y = intense4, all_y = t_val)
        return(dat)}
      else if(i2 == t_val){
        a <- i2/i1
        b <- i2/i3
        c <- i2/i4
        
        dat <- select(dat, all_of(mass_dat))
        intense1 <- intense1*a
        intense3 <- intense3*b
        intense4 <- intense4*c
        
        dat[spec1] <- .norm_TIC(y = intense1, all_y = t_val)
        dat[spec2] <- .norm_TIC(y = intense2, all_y = t_val)
        dat[spec3] <- .norm_TIC(y = intense3, all_y = t_val)
        dat[spec4] <- .norm_TIC(y = intense4, all_y = t_val)
        return(dat)}
      else if(i3 == t_val){
        a <- i3/i1
        b <- i3/i2
        c <- i3/i4
        
        dat <- select(dat, all_of(mass_dat))
        intense1 <- intense1*a
        intense2 <- intense2*b
        intense4 <- intense4*c
        return(dat)}
      else{
        a <- i4/i1
        b <- i4/i2
        c <- i4 <- i3
        
        dat <- select(dat, all_of(mass_dat))
        intense1 <- intense1*a
        intense2 <- intense2*b
        intense3 <- intense3*c
        
        dat[spec1] <- .norm_TIC(y = intense1, all_y = t_val)
        dat[spec2] <- .norm_TIC(y = intense2, all_y = t_val)
        dat[spec3] <- .norm_TIC(y = intense3, all_y = t_val)
        dat[spec4] <- .norm_TIC(y = intense4, all_y = t_val)
        return(dat)}}}
    
    # ---------------------------------------------------------------------
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
      t <- c(i1,i2,i3,i4,i5)
      t_val <- max(t)
      
      if(i1 == t_val){
        a <- i1/i2
        b <- i1/i3
        c <- i1/i4
        d <- i1/i5
        
        dat <- select(dat, all_of(mass_dat))
        intense2 <- intense2*a
        intense3 <- intense3*b
        intense4 <- intense4*c
        intense5 <- intense5*d
        
        dat[spec1] <- .norm_TIC(y = intense1, all_y = t_val)
        dat[spec2] <- .norm_TIC(y = intense2, all_y = t_val)
        dat[spec3] <- .norm_TIC(y = intense3, all_y = t_val)
        dat[spec4] <- .norm_TIC(y = intense4, all_y = t_val)
        dat[spec5] <- .norm_TIC(y = intense5, all_y = t_val)
        return(dat)}
      else if(i2 == t_val){
        a <- i2/i1
        b <- i2/i3
        c <- i2/i4
        d <- i2/i5
        
        dat <- select(dat, all_of(mass_dat))
        intense1 <- intense1*a
        intense3 <- intense3*b
        intense4 <- intense4*c
        intense5 <- intense5*d
        
        dat[spec1] <- .norm_TIC(y = intense1, all_y = t_val)
        dat[spec2] <- .norm_TIC(y = intense2, all_y = t_val)
        dat[spec3] <- .norm_TIC(y = intense3, all_y = t_val)
        dat[spec4] <- .norm_TIC(y = intense4, all_y = t_val)
        dat[spec5] <- .norm_TIC(y = intense5, all_y = t_val)
        return(dat)}
      else if(i3 == t_val){
        a <- i3/i1
        b <- i3/i2
        c <- i3/i4
        d <- i3/i5
        
        dat <- select(dat, all_of(mass_dat))
        intense1 <- intense1*a
        intense2 <- intense2*b
        intense4 <- intense4*c
        intense5 <- intense5*d
        
        dat[spec1] <- .norm_TIC(y = intense1, all_y = t_val)
        dat[spec2] <- .norm_TIC(y = intense2, all_y = t_val)
        dat[spec3] <- .norm_TIC(y = intense3, all_y = t_val)
        dat[spec4] <- .norm_TIC(y = intense4, all_y = t_val)
        dat[spec5] <- .norm_TIC(y = intense5, all_y = t_val)
        return(dat)}
      else if(i4 == t_val){
        a <- i4/i1
        b <- i4/i2
        c <- i4/i3
        d <- i4/i5
        
        dat <- select(dat, all_of(mass_dat))
        intense1 <- intense1*a
        intense2 <- intense2*b
        intense3 <- intense3*c
        intense5 <- intense5*d
        
        dat[spec1] <- .norm_TIC(y = intense1, all_y = t_val)
        dat[spec2] <- .norm_TIC(y = intense2, all_y = t_val)
        dat[spec3] <- .norm_TIC(y = intense3, all_y = t_val)
        dat[spec4] <- .norm_TIC(y = intense4, all_y = t_val)
        dat[spec5] <- .norm_TIC(y = intense5, all_y = t_val)
        return(dat)}
      else{
        a <- i5/i1
        b <- i5/i2
        c <- i5/i3
        d <- i5/i4
        
        dat <- select(dat, all_of(mass_dat))
        intense1 <- intense1*a
        intense2 <- intense2*b
        intense3 <- intense3*c
        intense4 <- intense4*d
        
        dat[spec1] <- .norm_TIC(y = intense1, all_y = t_val)
        dat[spec2] <- .norm_TIC(y = intense2, all_y = t_val)
        dat[spec3] <- .norm_TIC(y = intense3, all_y = t_val)
        dat[spec4] <- .norm_TIC(y = intense4, all_y = t_val)
        dat[spec5] <- .norm_TIC(y = intense5, all_y = t_val)
        return(dat)}}}
  
  # ---------------------------------------------------------------------
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
    t <- c(i1,i2,i3,i4,i5,i6)
    t_val <- max(t)
    
    if(i1 == t_val){
      a <- i1/i2
      b <- i1/i3
      c <- i1/i4
      d <- i1/i5
      e <- i1/i6
      
      dat <- select(dat, all_of(mass_dat))
      intense2 <- intense2*a
      intense3 <- intense3*b
      intense4 <- intense4*c
      intense5 <- intense5*d
      intense6 <- intense6*e
      
      dat[spec1] <- .norm_TIC(y = intense1, all_y = t_val)
      dat[spec2] <- .norm_TIC(y = intense2, all_y = t_val)
      dat[spec3] <- .norm_TIC(y = intense3, all_y = t_val)
      dat[spec4] <- .norm_TIC(y = intense4, all_y = t_val)
      dat[spec5] <- .norm_TIC(y = intense5, all_y = t_val)
      dat[spec6] <- .norm_TIC(y = intense6, all_y = t_val)
      return(dat)}
    else if(i2 == t_val){
      a <- i2/i1
      b <- i2/i3
      c <- i2/i4
      d <- i2/i5
      e <- i2/i6
      
      dat <- select(dat, all_of(mass_dat))
      intense1 <- intense1*a
      intense3 <- intense3*b
      intense4 <- intense4*c
      intense5 <- intense5*d
      intense6 <- intense6*e
      
      dat[spec1] <- .norm_TIC(y = intense1, all_y = t_val)
      dat[spec2] <- .norm_TIC(y = intense2, all_y = t_val)
      dat[spec3] <- .norm_TIC(y = intense3, all_y = t_val)
      dat[spec4] <- .norm_TIC(y = intense4, all_y = t_val)
      dat[spec5] <- .norm_TIC(y = intense5, all_y = t_val)
      dat[spec6] <- .norm_TIC(y = intense6, all_y = t_val)
      return(dat)}
    else if(i3 == t_val){
      a <- i3/i1
      b <- i3/i2
      c <- i3/i4
      d <- i3/i5
      e <- i3/i6
      
      dat <- select(dat, all_of(mass_dat))
      intense1 <- intense1*a
      intense2 <- intense2*b
      intense4 <- intense4*c
      intense5 <- intense5*d
      intense6 <- intense6*e
      
      dat[spec1] <- .norm_TIC(y = intense1, all_y = t_val)
      dat[spec2] <- .norm_TIC(y = intense2, all_y = t_val)
      dat[spec3] <- .norm_TIC(y = intense3, all_y = t_val)
      dat[spec4] <- .norm_TIC(y = intense4, all_y = t_val)
      dat[spec5] <- .norm_TIC(y = intense5, all_y = t_val)
      dat[spec6] <- .norm_TIC(y = intense6, all_y = t_val)
      return(dat)}
    else if(i4 == t_val){
      a <- i4/i1
      b <- i4/i2
      c <- i4/i3
      d <- i4/i5
      e <- i4/i6
      
      dat <- select(dat, all_of(mass_dat))
      intense1 <- intense1*a
      intense2 <- intense2*b
      intense3 <- intense3*c
      intense5 <- intense5*d
      intense6 <- intense6*e
      
      dat[spec1] <- .norm_TIC(y = intense1, all_y = t_val)
      dat[spec2] <- .norm_TIC(y = intense2, all_y = t_val)
      dat[spec3] <- .norm_TIC(y = intense3, all_y = t_val)
      dat[spec4] <- .norm_TIC(y = intense4, all_y = t_val)
      dat[spec5] <- .norm_TIC(y = intense5, all_y = t_val)
      dat[spec6] <- .norm_TIC(y = intense6, all_y = t_val)
      return(dat)}
    else if(i5 == t_val){
      a <- i5/i1
      b <- i5/i2
      c <- i5/i3
      d <- i5/i4
      e <- i5/i6
      
      dat <- select(dat, all_of(mass_dat))
      intense1 <- intense1*a
      intense2 <- intense2*b
      intense3 <- intense3*c
      intense4 <- intense4*d
      intense6 <- intense6*e
      
      dat[spec1] <- .norm_TIC(y = intense1, all_y = t_val)
      dat[spec2] <- .norm_TIC(y = intense2, all_y = t_val)
      dat[spec3] <- .norm_TIC(y = intense3, all_y = t_val)
      dat[spec4] <- .norm_TIC(y = intense4, all_y = t_val)
      dat[spec5] <- .norm_TIC(y = intense5, all_y = t_val)
      dat[spec6] <- .norm_TIC(y = intense6, all_y = t_val)
      return(dat)}
    else{
      a <- i6/i1
      b <- i6/i2
      c <- i6/i3
      d <- i6/i4
      e <- i6/i5
      
      dat <- select(dat, all_of(mass_dat))
      intense1 <- intense1*a
      intense2 <- intense2*b
      intense3 <- intense3*c
      intense4 <- intense4*d
      intense5 <- intense5*e
      
      dat[spec1] <- .norm_TIC(y = intense1, all_y = t_val)
      dat[spec2] <- .norm_TIC(y = intense2, all_y = t_val)
      dat[spec3] <- .norm_TIC(y = intense3, all_y = t_val)
      dat[spec4] <- .norm_TIC(y = intense4, all_y = t_val)
      dat[spec5] <- .norm_TIC(y = intense5, all_y = t_val)
      dat[spec6] <- .norm_TIC(y = intense6, all_y = t_val)
      return(dat)}
  }
}
  

# -----------------------------------------------------------------------

