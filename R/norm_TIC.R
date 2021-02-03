# -----------------------------------------------------------------------
# Last Updated: January 28, 2021
# Author: Kristen Yeh, Sophie Castel
# Title: subMALDI: Normalization Method - TIC
# -----------------------------------------------------------------------

# This method evaluates the sum of all intensities of each spectrum in a set.
# If the sum is not equivalent between spectra, multiplies the spectra by
# normalization factor.


norm_TIC <- function(dat, mass_dat, spectra_cols){
  
  # ---------------------
  # LOGICAL CHECKS
  # ---------------------
  
  stopifnot(
    is.character(mass_dat),
    is.character(spectra_cols),
    mass_dat %in% colnames(dat),
<<<<<<< HEAD
<<<<<<< HEAD
    all(spectra_cols %in% colnames(dat))
=======
    all(spectra_cols %in% colnames(dat)),
    is.numeric(lower),
    is.numeric(upper)
>>>>>>> b2e89cf... .normMethod_ function format deprecated ----> norm_ . Scripts and functions renamed accordingly.
=======
    all(spectra_cols %in% colnames(dat))
>>>>>>> 7b46ea1... Removed lower/upper from stopifnot()
  )    
  
  if(length(spectra_cols) < 2){
    stop("Only one spectrum input. Please enter at least two spectra.")
  }
  
  
  mz <- dat[[mass_dat]]
  
  spectra <- lapply(spectra_cols, function(x){dat[x]})
  i <- do.call(what = data.frame, args = c(spectra))
  
  i_sum <- apply(i, 2, FUN = sum)
  
  max_sum <- max(i_sum)
  
  for(j in 1:length(spectra_cols)){
    
    if(i_sum[j] == max_sum){
      
      other <- i_sum[-j]
      other_index <- which(i_sum %in% other)
      
      factors <- rep(1,length(spectra_cols))
      
      factors[other_index] = i_sum[j]/i_sum[other_index]
      
    }
  }
  
  i_mult <- t(t(i)*factors)
  
  dat <- data.frame(mz, i_mult)
  colnames(dat) <- c("full_mz", spectra_cols)
  
  return(dat)
  
}

# -----------------------------------------------------------------------

