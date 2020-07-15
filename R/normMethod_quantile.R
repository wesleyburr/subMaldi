# --------------------------------------------------------------------------------------------
# Date: June 16, 2020
# Author: Kristen Yeh
# Title: Normalization Method - Quantile
# --------------------------------------------------------------------------------------------


# Quantile normalization consists of two steps:
    # 1. Create a mapping between ranks and values. For rank 1, find the n values,
       # one per spectrum, that are the smallest value on the spectrum, and save their
       # averages. Similarly for rank 2 and the second smallest values, and on up to
       # the n largest values, one per spectrum.
    # 2. For each spectrum, replace the actual values with these averages.

# Basically, index each intensity value in each spectrum then sort
# by intensity (value). Probably need 2 truncate. Average spectrum for the sorted spectra
# is calculated. The averages are then inserted into the sorted spectra.
# The spectra are then reverted to their unsorted order using the index.


.normMethod_quantile <- function(dat, mass_dat, spec1, spec2 = NULL, spec3 = NULL, 
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
    int <- select(dat, all_of(mass_dat), all_of(spec1), all_of(spec2))
    int <- gather(int, key = "Spectra", value = "Intensity",
                  all_of(spec1), all_of(spec2), factor_key = TRUE)
    int <- dplyr::filter(int, Intensity != 0)
    int <- int %>% group_by(Spectra)  %>% mutate(id = row_number())
    ordr <- int[order(int$Intensity, decreasing = TRUE),]
    
    r1 <- int$id[which(int$Spectra == spec1)] %>% max() %>% as.numeric()
    r2 <- int$id[which(int$Spectra == spec2)] %>% max() %>% as.numeric()
    
    if(r1 > r2){
      trunc <- ordr[(which(ordr$id <= r2)),]
      
      i1 <- trunc$Intensity[which(trunc$Spectra == spec1)]
      i2 <- trunc$Intensity[which(trunc$Spectra == spec2)]
      i <- data.frame(i1,i2)
      i <- transform(i, avg = apply(i,1, mean, na.rm = TRUE))
      
      trunc$Intensity[which(trunc$Spectra == spec1)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec2)] <- i$avg
      out <- trunc[order(trunc[mass_dat], decreasing = FALSE),] %>% spread(Spectra, Intensity)
      
      out[is.na(out)] <- 0
      out <- out[,-2]
      return(out) }
    else{
      trunc <- ordr[(which(ordr$id <= r1)),]
      
      i1 <- trunc$Intensity[which(trunc$Spectra == spec1)]
      i2 <- trunc$Intensity[which(trunc$Spectra == spec2)]
      i <- data.frame(i1,i2)
      i <- transform(i, avg = apply(i,1, mean, na.rm = TRUE))
      
      trunc$Intensity[which(trunc$Spectra == spec1)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec2)] <- i$avg
      out <- trunc[order(trunc[mass_dat], decreasing = FALSE),] %>% spread(Spectra, Intensity)
      
      out[is.na(out)] <- 0
      out <- out[,-2]
      return(out) }}}
  
  # ---------------------------------------------------------------------
  # Three spectra
  else{
    int <- select(dat, all_of(mass_dat), all_of(spec1), all_of(spec2), all_of(spec3))
    int <- gather(int, key = "Spectra", value = "Intensity",
                  all_of(spec1), all_of(spec2), all_of(spec3), factor_key = TRUE)
    int <- dplyr::filter(int, Intensity != 0)
    int <- int %>% group_by(Spectra)  %>% mutate(id = row_number())
    ordr <- int[order(int$Intensity, decreasing = TRUE),]
    
    r1 <- int$id[which(int$Spectra == spec1)] %>% max() %>% as.numeric()
    r2 <- int$id[which(int$Spectra == spec2)] %>% max() %>% as.numeric()
    r3 <- int$id[which(int$Spectra == spec3)] %>% max() %>% as.numeric()
    r <- c(r1,r2,r3)
    r_val <- min(r)
    
    if(r1 == r_val){
      trunc <- ordr[(which(ordr$id <= r1)),]
      
      i1 <- trunc$Intensity[which(trunc$Spectra == spec1)]
      i2 <- trunc$Intensity[which(trunc$Spectra == spec2)]
      i3 <- trunc$Intensity[which(trunc$Spectra == spec3)]
      i <- data.frame(i1,i2,i3)
      i <- transform(i, avg = apply(i,1, mean, na.rm = TRUE))
      
      trunc$Intensity[which(trunc$Spectra == spec1)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec2)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec3)] <- i$avg
      out <- trunc[order(trunc[mass_dat], decreasing = FALSE),] %>% spread(Spectra, Intensity)
      
      out[is.na(out)] <- 0
      out <- out[,-2]
      return(out) }
    else if(r2 == r_val){
      trunc <- ordr[(which(ordr$id <= r2)),]
      
      i1 <- trunc$Intensity[which(trunc$Spectra == spec1)]
      i2 <- trunc$Intensity[which(trunc$Spectra == spec2)]
      i3 <- trunc$Intensity[which(trunc$Spectra == spec3)]
      i <- data.frame(i1,i2,i3)
      i <- transform(i, avg = apply(i,1, mean, na.rm = TRUE))
      
      trunc$Intensity[which(trunc$Spectra == spec1)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec2)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec3)] <- i$avg
      out <- trunc[order(trunc[mass_dat], decreasing = FALSE),] %>% spread(Spectra, Intensity)
      
      out[is.na(out)] <- 0
      out <- out[,-2]
      return(out) }
    else{
      trunc <- ordr[(which(ordr$id <= r3)),]
      
      i1 <- trunc$Intensity[which(trunc$Spectra == spec1)]
      i2 <- trunc$Intensity[which(trunc$Spectra == spec2)]
      i3 <- trunc$Intensity[which(trunc$Spectra == spec3)]
      i <- data.frame(i1,i2,i3)
      i <- transform(i, avg = apply(i,1, mean, na.rm = TRUE))
      
      trunc$Intensity[which(trunc$Spectra == spec1)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec2)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec3)] <- i$avg
      out <- trunc[order(trunc[mass_dat], decreasing = FALSE),] %>% spread(Spectra, Intensity)
      
      out[is.na(out)] <- 0
      out <- out[,-2]
      return(out) }}}
  
  # ---------------------------------------------------------------------
  # Four spectra
  else{
    int <- select(dat, all_of(mass_dat), all_of(spec1), all_of(spec2), all_of(spec3),
                  all_of(spec4))
    int <- gather(int, key = "Spectra", value = "Intensity",
                  all_of(spec1), all_of(spec2), all_of(spec3),
                  all_of(spec4),factor_key = TRUE)
    int <- dplyr::filter(int, Intensity != 0)
    int <- int %>% group_by(Spectra)  %>% mutate(id = row_number())
    ordr <- int[order(int$Intensity, decreasing = TRUE),]
    
    r1 <- int$id[which(int$Spectra == spec1)] %>% max() %>% as.numeric()
    r2 <- int$id[which(int$Spectra == spec2)] %>% max() %>% as.numeric()
    r3 <- int$id[which(int$Spectra == spec3)] %>% max() %>% as.numeric()
    r4 <- int$id[which(int$Spectra == spec4)] %>% max() %>% as.numeric()
    r <- c(r1,r2,r3,r4)
    r_val <- min(r)
    
    if(r1 == r_val){
      trunc <- ordr[(which(ordr$id <= r1)),]
      
      i1 <- trunc$Intensity[which(trunc$Spectra == spec1)]
      i2 <- trunc$Intensity[which(trunc$Spectra == spec2)]
      i3 <- trunc$Intensity[which(trunc$Spectra == spec3)]
      i4 <- trunc$Intensity[which(trunc$Spectra == spec4)]
      i <- data.frame(i1,i2,i3,i4)
      i <- transform(i, avg = apply(i,1, mean, na.rm = TRUE))
      
      trunc$Intensity[which(trunc$Spectra == spec1)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec2)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec3)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec4)] <- i$avg
      out <- trunc[order(trunc[mass_dat], decreasing = FALSE),] %>% spread(Spectra, Intensity)
      
      out[is.na(out)] <- 0
      out <- out[,-2]
      return(out) }
    else if(r2 == r_val){
      trunc <- ordr[(which(ordr$id <= r2)),]
      
      i1 <- trunc$Intensity[which(trunc$Spectra == spec1)]
      i2 <- trunc$Intensity[which(trunc$Spectra == spec2)]
      i3 <- trunc$Intensity[which(trunc$Spectra == spec3)]
      i4 <- trunc$Intensity[which(trunc$Spectra == spec4)]
      i <- data.frame(i1,i2,i3,i4)
      i <- transform(i, avg = apply(i,1, mean, na.rm = TRUE))
      
      trunc$Intensity[which(trunc$Spectra == spec1)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec2)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec3)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec4)] <- i$avg
      out <- trunc[order(trunc[mass_dat], decreasing = FALSE),] %>% spread(Spectra, Intensity)
      
      out[is.na(out)] <- 0
      out <- out[,-2]
      return(out) }
    else if(r3 ==r_val){
      trunc <- ordr[(which(ordr$id <= r3)),]
      
      i1 <- trunc$Intensity[which(trunc$Spectra == spec1)]
      i2 <- trunc$Intensity[which(trunc$Spectra == spec2)]
      i3 <- trunc$Intensity[which(trunc$Spectra == spec3)]
      i4 <- trunc$Intensity[which(trunc$Spectra == spec4)]
      i <- data.frame(i1,i2,i3,i4)
      i <- transform(i, avg = apply(i,1, mean, na.rm = TRUE))
      
      trunc$Intensity[which(trunc$Spectra == spec1)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec2)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec3)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec4)] <- i$avg
      out <- trunc[order(trunc[mass_dat], decreasing = FALSE),] %>% spread(Spectra, Intensity)
      
      out[is.na(out)] <- 0
      out <- out[,-2]
      return(out) }
    else{
      trunc <- ordr[(which(ordr$id <= r4)),]
      
      i1 <- trunc$Intensity[which(trunc$Spectra == spec1)]
      i2 <- trunc$Intensity[which(trunc$Spectra == spec2)]
      i3 <- trunc$Intensity[which(trunc$Spectra == spec3)]
      i4 <- trunc$Intensity[which(trunc$Spectra == spec4)]
      i <- data.frame(i1,i2,i3,i4)
      i <- transform(i, avg = apply(i,1, mean, na.rm = TRUE))
      
      trunc$Intensity[which(trunc$Spectra == spec1)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec2)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec3)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec4)] <- i$avg
      out <- trunc[order(trunc[mass_dat], decreasing = FALSE),] %>% spread(Spectra, Intensity)
      
      out[is.na(out)] <- 0
      out <- out[,-2]
      return(out) }}}
  
  # ---------------------------------------------------------------------
  # Five spectra
  else{
    int <- select(dat, all_of(mass_dat), all_of(spec1), all_of(spec2), all_of(spec3),
                  all_of(spec4), all_of(spec5))
    int <- gather(int, key = "Spectra", value = "Intensity",
                  all_of(spec1), all_of(spec2), all_of(spec3),
                  all_of(spec4), all_of(spec5), factor_key = TRUE)
    int <- dplyr::filter(int, Intensity != 0)
    int <- int %>% group_by(Spectra)  %>% mutate(id = row_number())
    ordr <- int[order(int$Intensity, decreasing = TRUE),]
    
    r1 <- int$id[which(int$Spectra == spec1)] %>% max() %>% as.numeric()
    r2 <- int$id[which(int$Spectra == spec2)] %>% max() %>% as.numeric()
    r3 <- int$id[which(int$Spectra == spec3)] %>% max() %>% as.numeric()
    r4 <- int$id[which(int$Spectra == spec4)] %>% max() %>% as.numeric()
    r5 <- int$id[which(int$Spectra == spec5)] %>% max() %>% as.numeric()
    r <- c(r1,r2,r3,r4,r5)
    r_val <- min(r)
    
    if(r1 == r_val){
      trunc <- ordr[(which(ordr$id <= r1)),]
      
      i1 <- trunc$Intensity[which(trunc$Spectra == spec1)]
      i2 <- trunc$Intensity[which(trunc$Spectra == spec2)]
      i3 <- trunc$Intensity[which(trunc$Spectra == spec3)]
      i4 <- trunc$Intensity[which(trunc$Spectra == spec4)]
      i5 <- trunc$Intensity[which(trunc$Spectra == spec5)]
      i <- data.frame(i1,i2,i3,i4,i5)
      i <- transform(i, avg = apply(i,1, mean, na.rm = TRUE))
      
      trunc$Intensity[which(trunc$Spectra == spec1)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec2)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec3)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec4)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec5)] <- i$avg
      out <- trunc[order(trunc[mass_dat], decreasing = FALSE),] %>% spread(Spectra, Intensity)
      
      out[is.na(out)] <- 0
      out <- out[,-2]
      return(out) }
    else if(r2 == r_val){
      trunc <- ordr[(which(ordr$id <= r2)),]
      
      i1 <- trunc$Intensity[which(trunc$Spectra == spec1)]
      i2 <- trunc$Intensity[which(trunc$Spectra == spec2)]
      i3 <- trunc$Intensity[which(trunc$Spectra == spec3)]
      i4 <- trunc$Intensity[which(trunc$Spectra == spec4)]
      i5 <- trunc$Intensity[which(trunc$Spectra == spec5)]
      i <- data.frame(i1,i2,i3,i4,i5)
      i <- transform(i, avg = apply(i,1, mean, na.rm = TRUE))
      
      trunc$Intensity[which(trunc$Spectra == spec1)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec2)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec3)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec4)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec5)] <- i$avg
      out <- trunc[order(trunc[mass_dat], decreasing = FALSE),] %>% spread(Spectra, Intensity)
      
      out[is.na(out)] <- 0
      out <- out[,-2]
      return(out) }
    else if(r3 ==r_val){
      trunc <- ordr[(which(ordr$id <= r3)),]
      
      i1 <- trunc$Intensity[which(trunc$Spectra == spec1)]
      i2 <- trunc$Intensity[which(trunc$Spectra == spec2)]
      i3 <- trunc$Intensity[which(trunc$Spectra == spec3)]
      i4 <- trunc$Intensity[which(trunc$Spectra == spec4)]
      i5 <- trunc$Intensity[which(trunc$Spectra == spec5)]
      i <- data.frame(i1,i2,i3,i4,i5)
      i <- transform(i, avg = apply(i,1, mean, na.rm = TRUE))
      
      trunc$Intensity[which(trunc$Spectra == spec1)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec2)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec3)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec4)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec5)] <- i$avg
      out <- trunc[order(trunc[mass_dat], decreasing = FALSE),] %>% spread(Spectra, Intensity)
      
      out[is.na(out)] <- 0
      out <- out[,-2]
      return(out) }
    else if(r4 == r_val){
      trunc <- ordr[(which(ordr$id <= r4)),]
      
      i1 <- trunc$Intensity[which(trunc$Spectra == spec1)]
      i2 <- trunc$Intensity[which(trunc$Spectra == spec2)]
      i3 <- trunc$Intensity[which(trunc$Spectra == spec3)]
      i4 <- trunc$Intensity[which(trunc$Spectra == spec4)]
      i5 <- trunc$Intensity[which(trunc$Spectra == spec5)]
      i <- data.frame(i1,i2,i3,i4,i5)
      i <- transform(i, avg = apply(i,1, mean, na.rm = TRUE))
      
      trunc$Intensity[which(trunc$Spectra == spec1)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec2)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec3)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec4)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec5)] <- i$avg
      out <- trunc[order(trunc[mass_dat], decreasing = FALSE),] %>% spread(Spectra, Intensity)
      
      out[is.na(out)] <- 0
      out <- out[,-2]
      return(out) }
    else{
      trunc <- ordr[(which(ordr$id <= r5)),]
      
      i1 <- trunc$Intensity[which(trunc$Spectra == spec1)]
      i2 <- trunc$Intensity[which(trunc$Spectra == spec2)]
      i3 <- trunc$Intensity[which(trunc$Spectra == spec3)]
      i4 <- trunc$Intensity[which(trunc$Spectra == spec4)]
      i5 <- trunc$Intensity[which(trunc$Spectra == spec5)]
      i <- data.frame(i1,i2,i3,i4,i5)
      i <- transform(i, avg = apply(i,1, mean, na.rm = TRUE))
      
      trunc$Intensity[which(trunc$Spectra == spec1)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec2)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec3)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec4)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec5)] <- i$avg
      out <- trunc[order(trunc[mass_dat], decreasing = FALSE),] %>% spread(Spectra, Intensity)
      
      out[is.na(out)] <- 0
      out <- out[,-2]
      return(out) }}}
  
  # ---------------------------------------------------------------------
  # Six spectra
  else{
    int <- select(dat, all_of(mass_dat), all_of(spec1), all_of(spec2), all_of(spec3),
                  all_of(spec4), all_of(spec5), all_of(spec6))
    int <- gather(int, key = "Spectra", value = "Intensity",
                  all_of(spec1), all_of(spec2), all_of(spec3),
                  all_of(spec4), all_of(spec5), all_of(spec6), factor_key = TRUE)
    int <- dplyr::filter(int, Intensity != 0)
    int <- int %>% group_by(Spectra)  %>% mutate(id = row_number())
    ordr <- int[order(int$Intensity, decreasing = TRUE),]
    
    r1 <- int$id[which(int$Spectra == spec1)] %>% max() %>% as.numeric()
    r2 <- int$id[which(int$Spectra == spec2)] %>% max() %>% as.numeric()
    r3 <- int$id[which(int$Spectra == spec3)] %>% max() %>% as.numeric()
    r4 <- int$id[which(int$Spectra == spec4)] %>% max() %>% as.numeric()
    r5 <- int$id[which(int$Spectra == spec5)] %>% max() %>% as.numeric()
    r6 <- int$id[which(int$Spectra == spec6)] %>% max() %>% as.numeric()
    r <- c(r1,r2,r3,r4,r5,r6)
    r_val <- min(r)
    
    if(r1 == r_val){
      trunc <- ordr[(which(ordr$id <= r1)),]
      
      i1 <- trunc$Intensity[which(trunc$Spectra == spec1)]
      i2 <- trunc$Intensity[which(trunc$Spectra == spec2)]
      i3 <- trunc$Intensity[which(trunc$Spectra == spec3)]
      i4 <- trunc$Intensity[which(trunc$Spectra == spec4)]
      i5 <- trunc$Intensity[which(trunc$Spectra == spec5)]
      i6 <- trunc$Intensity[which(trunc$Spectra == spec6)]
      i <- data.frame(i1,i2,i3,i4,i5,i6)
      i <- transform(i, avg = apply(i,1, mean, na.rm = TRUE))
      
      trunc$Intensity[which(trunc$Spectra == spec1)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec2)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec3)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec4)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec5)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec6)] <- i$avg
      out <- trunc[order(trunc[mass_dat], decreasing = FALSE),] %>% spread(Spectra, Intensity)
      
      out[is.na(out)] <- 0
      out <- out[,-2]
      return(out) }
    else if(r2 == r_val){
      trunc <- ordr[(which(ordr$id <= r2)),]
      
      i1 <- trunc$Intensity[which(trunc$Spectra == spec1)]
      i2 <- trunc$Intensity[which(trunc$Spectra == spec2)]
      i3 <- trunc$Intensity[which(trunc$Spectra == spec3)]
      i4 <- trunc$Intensity[which(trunc$Spectra == spec4)]
      i5 <- trunc$Intensity[which(trunc$Spectra == spec5)]
      i6 <- trunc$Intensity[which(trunc$Spectra == spec6)]
      i <- data.frame(i1,i2,i3,i4,i5,i6)
      i <- transform(i, avg = apply(i,1, mean, na.rm = TRUE))
      
      trunc$Intensity[which(trunc$Spectra == spec1)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec2)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec3)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec4)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec5)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec6)] <- i$avg
      out <- trunc[order(trunc[mass_dat], decreasing = FALSE),] %>% spread(Spectra, Intensity)
      
      out[is.na(out)] <- 0
      out <- out[,-2]
      return(out) }
    else if(r3 ==r_val){
      trunc <- ordr[(which(ordr$id <= r3)),]
      
      i1 <- trunc$Intensity[which(trunc$Spectra == spec1)]
      i2 <- trunc$Intensity[which(trunc$Spectra == spec2)]
      i3 <- trunc$Intensity[which(trunc$Spectra == spec3)]
      i4 <- trunc$Intensity[which(trunc$Spectra == spec4)]
      i5 <- trunc$Intensity[which(trunc$Spectra == spec5)]
      i6 <- trunc$Intensity[which(trunc$Spectra == spec6)]
      i <- data.frame(i1,i2,i3,i4,i5,i6)
      i <- transform(i, avg = apply(i,1, mean, na.rm = TRUE))
      
      trunc$Intensity[which(trunc$Spectra == spec1)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec2)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec3)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec4)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec5)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec6)] <- i$avg
      out <- trunc[order(trunc[mass_dat], decreasing = FALSE),] %>% spread(Spectra, Intensity)
      
      out[is.na(out)] <- 0
      out <- out[,-2]
      return(out) }
    else if(r4 == r_val){
      trunc <- ordr[(which(ordr$id <= r4)),]
      
      i1 <- trunc$Intensity[which(trunc$Spectra == spec1)]
      i2 <- trunc$Intensity[which(trunc$Spectra == spec2)]
      i3 <- trunc$Intensity[which(trunc$Spectra == spec3)]
      i4 <- trunc$Intensity[which(trunc$Spectra == spec4)]
      i5 <- trunc$Intensity[which(trunc$Spectra == spec5)]
      i6 <- trunc$Intensity[which(trunc$Spectra == spec6)]
      i <- data.frame(i1,i2,i3,i4,i5,i6)
      i <- transform(i, avg = apply(i,1, mean, na.rm = TRUE))
      
      trunc$Intensity[which(trunc$Spectra == spec1)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec2)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec3)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec4)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec5)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec6)] <- i$avg
      out <- trunc[order(trunc[mass_dat], decreasing = FALSE),] %>% spread(Spectra, Intensity)
      
      out[is.na(out)] <- 0
      out <- out[,-2]
      return(out) }
    else if(r5 == r_val){
      trunc <- ordr[(which(ordr$id <= r5)),]
      
      i1 <- trunc$Intensity[which(trunc$Spectra == spec1)]
      i2 <- trunc$Intensity[which(trunc$Spectra == spec2)]
      i3 <- trunc$Intensity[which(trunc$Spectra == spec3)]
      i4 <- trunc$Intensity[which(trunc$Spectra == spec4)]
      i5 <- trunc$Intensity[which(trunc$Spectra == spec5)]
      i6 <- trunc$Intensity[which(trunc$Spectra == spec6)]
      i <- data.frame(i1,i2,i3,i4,i5,i6)
      i <- transform(i, avg = apply(i,1, mean, na.rm = TRUE))
      
      trunc$Intensity[which(trunc$Spectra == spec1)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec2)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec3)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec4)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec5)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec6)] <- i$avg
      out <- trunc[order(trunc[mass_dat], decreasing = FALSE),] %>% spread(Spectra, Intensity)
      
      out[is.na(out)] <- 0
      out <- out[,-2]
      return(out) }
    else{
      trunc <- ordr[(which(ordr$id <= r6)),]
      
      i1 <- trunc$Intensity[which(trunc$Spectra == spec1)]
      i2 <- trunc$Intensity[which(trunc$Spectra == spec2)]
      i3 <- trunc$Intensity[which(trunc$Spectra == spec3)]
      i4 <- trunc$Intensity[which(trunc$Spectra == spec4)]
      i5 <- trunc$Intensity[which(trunc$Spectra == spec5)]
      i6 <- trunc$Intensity[which(trunc$Spectra == spec6)]
      i <- data.frame(i1,i2,i3,i4,i5,i6)
      i <- transform(i, avg = apply(i,1, mean, na.rm = TRUE))
      
      trunc$Intensity[which(trunc$Spectra == spec1)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec2)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec3)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec4)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec5)] <- i$avg
      trunc$Intensity[which(trunc$Spectra == spec6)] <- i$avg
      out <- trunc[order(trunc[mass_dat], decreasing = FALSE),] %>% spread(Spectra, Intensity)
      
      out[is.na(out)] <- 0
      out <- out[,-2]
      return(out) }
  }
}






