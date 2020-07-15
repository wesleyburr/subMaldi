# -----------------------------------------------------------------------
# Last Updated: June 12, 2020
# Author: Kristen Yeh
# Title: subMALDI: Normalization Method - Median
# -----------------------------------------------------------------------


# ---------------
# METHOD: MEDIAN
# ---------------

.normMethod_median <- function(dat, mass_dat, spec1, spec2 = NULL, spec3 = NULL, 
                              spec4 = NULL, spec5 = NULL, spec6 = NULL){
  
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
    
    int <- gather(dat, key = "Spectra", value = "Intensity",
                  all_of(spec1), all_of(spec2), factor_key = TRUE)
    int <- dplyr::filter(int, Intensity != 0)
    
    s1 <- int[int$Spectra == spec1,] %>% select("full_mz", "Intensity")
    m1 <- median(s1$Intensity)
    
    s2 <- int[int$Spectra == spec2,] %>% select("full_mz", "Intensity")
    m2 <- median(s2$Intensity)
    
    if(m1 > m2){
      m <- m1/m2
      dat <- select(dat, all_of(mass_dat))
      dat[spec1] <- intense1
      dat[spec2] <- intense2*m
      return(dat)} 
    else{
      m <- m2/m1
      dat <- select(dat, all_of(mass_dat))
      dat[spec1] <- intense1*m
      dat[spec2] <- intense2
      return(dat)}}}
  
  # ---------------------------------------------------------------------
  # Three spectra
  else{
    intense1 <- dat[[spec1]]
    intense2 <- dat[[spec2]]
    intense3 <- dat[[spec3]]
    
    int <- gather(dat, key = "Spectra", value = "Intensity",
                  all_of(spec1), all_of(spec2), all_of(spec3), factor_key = TRUE)
    int <- dplyr::filter(int, Intensity != 0)
    
    s1 <- int[int$Spectra == spec1,] %>% select("full_mz", "Intensity")
    s2 <- int[int$Spectra == spec2,] %>% select("full_mz", "Intensity")
    s3 <- int[int$Spectra == spec3,] %>% select("full_mz", "Intensity")
    
    m1 <- median(s1$Intensity)
    m2 <- median(s2$Intensity)
    m3 <- median(s3$Intensity)
    m <- c(m1,m2,m3)
    m_val <- max(m)
    
    if(m1 == m_val){
      a <- m1/m2
      b <- m1/m3
      
      dat <- select(dat, all_of(mass_dat))
      dat[spec1] <- intense1
      dat[spec2] <- intense2*a
      dat[spec3] <- intense3*b
      return(dat)} 
    else if(m2 == m_val){
      a <- m2/m1
      b <- m2/m3
      
      dat <- select(dat, all_of(mass_dat))
      dat[spec1] <- intense1*a
      dat[spec2] <- intense2
      dat[spec3] <- intense3*b
      return(dat)}
    else{
      a <- m3/m1
      b <- m3/m2
      
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
    
    int <- gather(dat, key = "Spectra", value = "Intensity",
                  all_of(spec1), all_of(spec2), all_of(spec3),
                  all_of(spec4), factor_key = TRUE)
    int <- dplyr::filter(int, Intensity != 0) 
    
    s1 <- int[int$Spectra == spec1,] %>% select("full_mz", "Intensity")
    s2 <- int[int$Spectra == spec2,] %>% select("full_mz", "Intensity")
    s3 <- int[int$Spectra == spec3,] %>% select("full_mz", "Intensity")
    s4 <- int[int$Spectra == spec4,] %>% select("full_mz", "Intensity")
    
    m1 <- median(s1$Intensity)
    m2 <- median(s2$Intensity)
    m3 <- median(s3$Intensity)
    m4 <- median(s4$Intensity)
    m <- c(m1,m2,m3,m4)
    m_val <- max(m)
    
    if(m1 == m_val){
      a <- m1/m2
      b <- m1/m3
      c <- m1/m4
      
      dat <- select(dat, all_of(mass_dat))
      dat[spec1] <- intense1
      dat[spec2] <- intense2*a
      dat[spec3] <- intense3*b
      dat[spec4] <- intense4*c
      return(dat)} 
    else if(m2 == m_val){
      a <- m2/m1
      b <- m2/m3
      c <- m2/m4
      
      dat <- select(dat, all_of(mass_dat))
      dat[spec1] <- intense1*a
      dat[spec2] <- intense2
      dat[spec3] <- intense3*b
      dat[spec4] <- intense4*c
      return(dat)}
    else if(m3 == m_val){
      a <- m3/m1
      b <- m3/m2
      c <- m3/m4
      
      dat <- select(dat, all_of(mass_dat))
      dat[spec1] <- intense1*a
      dat[spec2] <- intense2*b
      dat[spec3] <- intense3
      dat[spec4] <- intense4*c
      return(dat)}
    else{
      a <- m4/m1
      b <- m4/m2
      c <- m4/m3
      
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
    
    int <- gather(dat, key = "Spectra", value = "Intensity",
                  all_of(spec1), all_of(spec2), all_of(spec3),
                  all_of(spec4), all_of(spec5), factor_key = TRUE)
    int <- dplyr::filter(int, Intensity != 0) 
    
    s1 <- int[int$Spectra == spec1,] %>% select("full_mz", "Intensity")
    s2 <- int[int$Spectra == spec2,] %>% select("full_mz", "Intensity")
    s3 <- int[int$Spectra == spec3,] %>% select("full_mz", "Intensity")
    s4 <- int[int$Spectra == spec4,] %>% select("full_mz", "Intensity")
    s5 <- int[int$Spectra == spec5,] %>% select("full_mz", "Intensity")
    
    m1 <- median(s1$Intensity)
    m2 <- median(s2$Intensity)
    m3 <- median(s3$Intensity)
    m4 <- median(s4$Intensity)
    m5 <- median(s5$Intensity)
    m <- c(m1,m2,m3,m4,m5)
    m_val <- max(m)
    
    if(m1 == m_val){
      a <- m1/m2
      b <- m1/m3
      c <- m1/m4
      d <- m1/m5
      
      dat <- select(dat, all_of(mass_dat))
      dat[spec1] <- intense1
      dat[spec2] <- intense2*a
      dat[spec3] <- intense3*b
      dat[spec4] <- intense4*c
      dat[spec5] <- intense5*d
      return(dat)} 
    else if(m2 == m_val){
      a <- m2/m1
      b <- m2/m3
      c <- m2/m4
      d <- m2/m5
      
      dat <- select(dat, all_of(mass_dat))
      dat[spec1] <- intense1*a
      dat[spec2] <- intense2
      dat[spec3] <- intense3*b
      dat[spec4] <- intense4*c
      dat[spec5] <- intense5*d
      return(dat)}
    else if(m3 == m_val){
      a <- m3/m1
      b <- m3/m2
      c <- m3/m4
      d <- m3/m5
      
      dat <- select(dat, all_of(mass_dat))
      dat[spec1] <- intense1*a
      dat[spec2] <- intense2*b
      dat[spec3] <- intense3
      dat[spec4] <- intense4*c
      dat[spec5] <- intense5*d
      return(dat)}
    else if(m4 == m_val){
      a <- m4/m1
      b <- m4/m2
      c <- m4/m3
      d <- m4/m5
      
      dat <- select(dat, all_of(mass_dat))
      dat[spec1] <- intense1*a
      dat[spec2] <- intense2*b
      dat[spec3] <- intense3*c
      dat[spec4] <- intense4
      dat[spec5] <- intense5*d
      return(dat)}
    else{
      a <- m5/m1
      b <- m5/m2
      c <- m5/m3
      d <- m5/m4
      
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
    
    int <- gather(dat, key = "Spectra", value = "Intensity",
                  all_of(spec1), all_of(spec2), all_of(spec3),
                  all_of(spec4), all_of(spec5), all_of(spec6),
                  factor_key = TRUE)
    int <- dplyr::filter(int, Intensity != 0) 
    
    s1 <- int[int$Spectra == spec1,] %>% select("full_mz", "Intensity")
    s2 <- int[int$Spectra == spec2,] %>% select("full_mz", "Intensity")
    s3 <- int[int$Spectra == spec3,] %>% select("full_mz", "Intensity")
    s4 <- int[int$Spectra == spec4,] %>% select("full_mz", "Intensity")
    s5 <- int[int$Spectra == spec5,] %>% select("full_mz", "Intensity")
    s6 <- int[int$Spectra == spec6,] %>% select("full_mz", "Intensity")
    
    m1 <- median(s1$Intensity)
    m2 <- median(s2$Intensity)
    m3 <- median(s3$Intensity)
    m4 <- median(s4$Intensity)
    m5 <- median(s5$Intensity)
    m6 <- median(s6$Intensity)
    m <- c(m1,m2,m3,m4,m5,m6)
    m_val <- max(m)
    
    if(m1 == m_val){
      a <- m1/m2
      b <- m1/m3
      c <- m1/m4
      d <- m1/m5
      e <- m1/m6
      
      dat <- select(dat, all_of(mass_dat))
      dat[spec1] <- intense1
      dat[spec2] <- intense2*a
      dat[spec3] <- intense3*b
      dat[spec4] <- intense4*c
      dat[spec5] <- intense5*d
      dat[spec6] <- intense6*e
      return(dat)} 
    else if(m2 == m_val){
      a <- m2/m1
      b <- m2/m3
      c <- m2/m4
      d <- m2/m5
      e <- m2/m6
      
      dat <- select(dat, all_of(mass_dat))
      dat[spec1] <- intense1*a
      dat[spec2] <- intense2
      dat[spec3] <- intense3*b
      dat[spec4] <- intense4*c
      dat[spec5] <- intense5*d
      dat[spec6] <- intense6*e
      return(dat)}
    else if(m3 == m_val){
      a <- m3/m1
      b <- m3/m2
      c <- m3/m4
      d <- m3/m5
      e <- m3/m6
      
      dat <- select(dat, all_of(mass_dat))
      dat[spec1] <- intense1*a
      dat[spec2] <- intense2*b
      dat[spec3] <- intense3
      dat[spec4] <- intense4*c
      dat[spec5] <- intense5*d
      dat[spec6] <- intense6*e
      return(dat)}
    else if(m4 == m_val){
      a <- m4/m1
      b <- m4/m2
      c <- m4/m3
      d <- m4/m5
      e <- m4/m6
      
      dat <- select(dat, all_of(mass_dat))
      dat[spec1] <- intense1*a
      dat[spec2] <- intense2*b
      dat[spec3] <- intense3*c
      dat[spec4] <- intense4
      dat[spec5] <- intense5*d
      dat[spec6] <- intense6*e
      return(dat)}
    else if(m5 == m_val){
      a <- m5/m1
      b <- m5/m2
      c <- m5/m3
      d <- m5/m4
      e <- m5/m6
      
      dat <- select(dat, all_of(mass_dat))
      dat[spec1] <- intense1*a
      dat[spec2] <- intense2*b
      dat[spec3] <- intense3*c
      dat[spec4] <- intense4*d
      dat[spec5] <- intense5
      dat[spec6] <- intense6*e
      return(dat)}
    else{
      a <- m6/m1
      b <- m6/m2
      c <- m6/m3
      d <- m6/m4
      e <- m6/m5
      
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








