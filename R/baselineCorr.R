# ----------------------------------------------------------------------------
# Last Updated: July 29, 2020
# Author: Kristen Yeh
# Title: subMALDI - Baseline Correction Parent Function
# ----------------------------------------------------------------------------



baselineCorr <- function(dat, mass_dat, intensity_dat, method = NULL, 
                           n = NULL){
  if(is.null(method)){ stop('Please select a valid baseline correction method. 
                            See ?baselineCorr for list of methods.') } 
  else { 
    if(method == "monotone_min"){ .base_mono(dat = dat, mass_dat = mass_dat, 
                                       intensity_dat = intensity_dat) }
    
    else if(method == "linear"){ .base_linear(dat = dat, mass_dat = mass_dat, 
                                             intensity_dat = intensity_dat,
                                             n = n) }
    else if(method == "loess"){.base_loess(dat = dat, mass_dat = mass_dat,
                                           intensity_dat = intensity_dat) }
  }
}


# ----------------------------------------------------------------------------