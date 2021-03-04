# -----------------------------------------------------------------------
# Last Updated: January 28, 2021
# Author: Kristen Yeh, Sophie Castel
# Title: subMALDI: Normalization Method - Custom m/z, imprecise
# -----------------------------------------------------------------------

norm_custimp <- function(dat, mass_dat, norm_mz, spectra_cols, showHI = FALSE){
  
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
  
  logic_i <- which(startsWith(as.character(mz), as.character(norm_mz)))
  
  if(length(logic_i) == 0){
    stop(c("The selected value norm_mz = ", norm_mz, " not found in '", mass_dat,"'. Please try another value."))
  }
  
  spectra <- lapply(spectra_cols, function(x){dat[x]})
  i <- do.call(what = data.frame, args = c(spectra))
  
  i_cust <- i[logic_i,]
  
  if(length(logic_i) > 1){
    stop("More than one peak per spectra for the given norm_mz value. Please be more precise.")    
  }
  
  i_cust <- as.numeric(i_cust)
  
  logic <- (i_cust == 0)
  
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
