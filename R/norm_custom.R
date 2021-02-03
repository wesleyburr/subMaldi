# -----------------------------------------------------------------------
# Last Updated: January 28, 2020
# Author: Kristen Yeh, Sophie Castel
# Title: subMALDI: Normalization Method - Custom m/z
# -----------------------------------------------------------------------

# -------------------------
# METHOD: CUSTOM M/Z VALUE
# -------------------------

.norm_custom <- function(y, custom_y){
  return((y - min(y)) / (custom_y - min(y))) # single value
}

norm_custom <- function(dat, mass_dat, norm_mz, spectra_cols, showHI = FALSE){
  
  # ---------------------
  # LOGICAL CHECKS
  # ---------------------
  
  stopifnot(
    is.character(mass_dat),
    is.character(spectra_cols),
    mass_dat %in% colnames(dat),
    all(spectra_cols %in% colnames(dat))
  )
  
  
  
  if(is.null(norm_mz)){
    stop('Please select a m/z value. norm_mz is NULL.')
  } 
  
  
  mz <- dat[[mass_dat]]
  
  spectra <- lapply(spectra_cols, function(x){dat[x]})
  i <- do.call(what = data.frame, args = c(spectra))
  
  i_cust <- as.numeric(i[which(mz == norm_mz),])
  
  if(all(is.na(i_cust))){
    stop(c("The selected value norm_mz = ", norm_mz, " not found in '", mass_dat,"'. Please try another value."))
  }
  
  logic <- i_cust == 0 
  
  if(any(logic)){
    stop(c("The selected maximum intensity is 0 in spectra labeled: ", paste0(colnames(i)[logic], sep = " ")))
  }
  
  i_norm <- t(t( i - min(i) )* ( i_cust - min(i) )^-1) # (i-min(i))/(i_cust-min(i)) Matrix multiplication
  
  if(!showHI){
    i_norm[i_norm > 1] <- 1
  }
  
  dat <- data.frame(mz, i_norm)
  colnames(dat) <- c("full_mz", spectra_cols)
  
  return(dat)
  
}

# -----------------------------------------------------------------------