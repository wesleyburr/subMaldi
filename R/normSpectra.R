# -----------------------------------------------------------------------
# Last Updated: January 28, 2021
# Author: Kristen Yeh, Sophie Castel
# Title: subMALDI: Umbrella Normalization Function
# -----------------------------------------------------------------------


# ------------
# normSpectra
# ------------


normSpectra <- function(dat, mass_dat, method = NULL, norm_mz = NULL, upper = NULL, lower = NULL,
                        spectra_cols = NULL, showHI = FALSE){
  
  methods <- c("max", "max_set", "custom", "custom_imprecise", "TIC", "rel_TIC", "RMS", "median", "stdev", "quantile")
    
  # ---------------------
  # LOGICAL CHECKS
  # ---------------------
  
  if(is.null(method)){ stop('Please select a valid normalization method. See ?normSpectra for list of methods.') } 
  
  if(!(method %in% methods)){
    stop(c("Invalid selection: method = '", method, "'.  Please make your selection from the following options: ", paste0(methods, sep = ", "),"."))
  }
  
  
  
  if(method == "max"){ 
    .normMethod_max(dat = dat, mass_dat = mass_dat, spectra_cols = spectra_cols) 
  }
  
  if(method == "custom"){ 
    .normMethod_custom(dat = dat, mass_dat = mass_dat, norm_mz = norm_mz, spectra_cols = spectra_cols, showHI = showHI) 
  } 
    
  if(method == "custom_imprecise"){ 
    .normMethod_custimp(dat = dat, mass_dat = mass_dat, norm_mz = norm_mz, spectra_cols = spectra_cols, showHI = showHI) 
  } 
    
  if(method == "max_set"){ 
    .normMethod_max_set(dat = dat, mass_dat = mass_dat, spectra_cols = spectra_cols) 
  }
    
  if( method == "TIC"){
    .normMethod_TIC(dat = dat, mass_dat = mass_dat, spectra_cols = spectra_cols) 
  } 
    
  if(method == "RMS"){
    .normMethod_RMS(dat = dat, mass_dat = mass_dat, spectra_cols = spectra_cols) 
  }
    
  if(method == "rel_TIC"){
    .normMethod_rel_TIC(dat = dat, mass_dat = mass_dat, spectra_cols = spectra_cols) 
  }
    
  if(method == "median"){
    .normMethod_median(dat = dat, mass_dat = mass_dat, spectra_cols = spectra_cols)
  }
      
  if(method == "stdev"){
    .normMethod_stdev(dat = dat, mass_dat = mass_dat, lower = lower, upper = upper, spectra_cols = spectra_cols) 
  }
    
  if(method == "quantile"){
    .normMethod_quantile(dat = dat, mass_dat = mass_dat, spectra_cols = spectra_cols) 
  }
}  




