# -----------------------------------------------------------------------
# Last Updated: January 28, 2021
# Author: Kristen Yeh, Sophie Castel
# Title: subMALDI: Umbrella Normalization Function
# -----------------------------------------------------------------------


# ------------
# normSpectra
# ------------


normSpectra <- function(dat, mass_dat, method = NULL, norm_mz = NULL, upper = NULL, lower = NULL,
                        spectra_cols = spectra_cols, showHI = FALSE){
  
  if(is.null(method)){ stop('Please select a valid normalization method. See ?normSpectra for list of methods.') } 
  
  else { 
    if(method == "max"){ .normMethod_max(dat = dat, mass_dat = mass_dat, spectra_cols = spectra_cols) }
  
    else if(method == "custom"){ .normMethod_custom(dat = dat, mass_dat = mass_dat, norm_mz = norm_mz, spectra_cols = spectra_cols, showHI = showHI) } 
    
    else if(method == "custom_imprecise"){ .normMethod_custimp(dat = dat, mass_dat = mass_dat, norm_mz = norm_mz, spectra_cols = spectra_cols, showHI = showHI) } 
    
    else if(method == "max_set"){ 
       if(is.null(spec2)){ stop('Only one spectrum input. Please enter two spectra for comparison.') } 
        else{ .normMethod_max_set(dat = dat, mass_dat = mass_dat, spectra_cols = spectra_cols) } }
    
    else if( method == "TIC"){
      if(is.null(spec2)){ stop('Only one spectrum input. Please enter two spectra for comparison.') } 
      else{ .normMethod_TIC(dat = dat, mass_dat = mass_dat, spectra_cols = spectra_cols) } }
    
    else if(method == "RMS"){
      .normMethod_RMS(dat = dat, mass_dat = mass_dat, spectra_cols = spectra_cols) }
    
    else if(method == "rel_TIC"){
      .normMethod_rel_TIC(dat = dat, mass_dat = mass_dat, spectra_cols = spectra_cols) }
    
    else if(method == "median"){
      if(is.null(spec2)){ stop('Only one spectrum input. Please enter two spectra for comparison.') } 
      else{ .normMethod_median(dat = dat, mass_dat = mass_dat, spectra_cols = spectra_cols) } }
      
    else if(method == "stdev"){
      .normMethod_stdev(dat = dat, mass_dat = mass_dat, lower = lower, upper = upper, spectra_cols = spectra_cols) }
    
    else if(method == "quantile"){
      if(is.null(spec2)){ stop('Only one spectrum input. Please enter two spectra for comparison.') } 
      else{ .normMethod_quantile(dat = dat, mass_dat = mass_dat, spectra_cols = spectra_cols) } }
  }
}  




