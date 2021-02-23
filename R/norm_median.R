# -----------------------------------------------------------------------
# Last Updated: January 27, 2021
# Author: Kristen Yeh, Sophie Castel
# Title: subMALDI: Normalization Method - Median
# -----------------------------------------------------------------------

# ---------------
# METHOD: MEDIAN
# ---------------
  
norm_median <- function(dat, mass_dat, spectra_cols){
    
    # ---------------------
    # LOGICAL CHECKS
    # ---------------------
    
    stopifnot(
      is.character(mass_dat),
      is.character(spectra_cols),
      mass_dat %in% colnames(dat),
      all(spectra_cols %in% colnames(dat))
    )
    
    if(length(spectra_cols) < 2){
      stop("Only one spectrum input. Please enter two spectra.")
    }
    
    
    mz <- dat[[mass_dat]]
    
    spectra <- lapply(spectra_cols, function(x){dat[x]})
    i <- do.call(what = data.frame, args = c(spectra))
    i[i == 0 ] <- NA
    
    i_med <- apply(i,2, FUN = median, na.rm = TRUE)
    max_med <- max(i_med)
    
    for(j in 1:length(spectra_cols)){
      
      if(i_med[j] == max_med){
        
        other <- i_med[-j]
        other_index <- which(i_med %in% other)
        
        factors <- rep(1,length(spectra_cols))
        
        factors[other_index] = i_med[j]/i_med[other_index]
        
      }
    }
    
    i_mult <- t(t(i)*factors)
    
    dat <- data.frame(mz, i_mult)
    colnames(dat) <- c("full_mz", spectra_cols)
    
    return(dat)
    
  }








