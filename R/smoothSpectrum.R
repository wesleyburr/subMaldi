# ----------------------------------------------------------------------------
# Last Updated: July 10, 2020
# Author: Kristen Yeh
# Title: subMALDI - Smoothing Parent Function
# ----------------------------------------------------------------------------


smoothSpectrum <- function(dat, mass_dat, intensity_dat, method = NULL, 
                           p = NULL, n = NULL, m = 0, ts = 1){
  
  if(is.null(method)){ stop('Please select a valid smoothing method. 
                            See ?smoothSpectrum for list of methods.') } 
  else { 
    if(method == "sgolay"){ .smooth_sg(dat = dat, mass_dat = mass_dat, 
                                       intensity_dat = intensity_dat, p = p, 
                                       n = n, m = m, ts = ts) }
    
    else if(method == "mov_avg"){ .smooth_ma(dat = dat, mass_dat = mass_dat, 
                                             intensity_dat = intensity_dat,
                                             n = n) } 
  }
}


# ----------------------------------------------------------------------------