# -----------------------------------------------------------------------
# Last Updated: June 8, 2020
# Author: Kristen Yeh
# Title: subMALDI: Umbrella Normalization Function
# -----------------------------------------------------------------------


# ------------
# normSpectra
# ------------


normSpectra <- function(dat, mass_dat, method = NULL, norm_mz = NULL, 
                        spec1, spec2 = NULL, spec3 = NULL, spec4 = NULL, spec5 = NULL, spec6 = NULL){
  
  if(is.null(method)){ stop('Please select a valid normalization method. See ?normSpectra for list of methods.') } 
  
  else { 
    if(method == "max"){ .normMethod_max(dat = dat, mass_dat = mass_dat, spec1 = spec1, spec2 = spec2, 
                                        spec3 = spec3, spec4 = spec4, spec5 = spec5, spec6 = spec6) }
  
    else if(method == "custom"){ .normMethod_custom(dat = dat, mass_dat = mass_dat, norm_mz = norm_mz, spec1 = spec1, spec2 = spec2, 
                                                   spec3 = spec3, spec4 = spec4, spec5 = spec5, spec6 = spec6) } 
  
    else if(method == "max_set"){ 
       if(is.null(spec2)){ stop('Only one input spectrum. Please input a set of spectra (2 -6).') } 
        else{ .normMethod_max_set(dat = dat, mass_dat = mass_dat, spec1 = spec1, spec2 = spec2, 
                                 spec3 = spec3, spec4 = spec4, spec5 = spec5, spec6 = spec6) } }
    
    else if( method == "TIC"){
      .normMethod_TIC(dat = dat, mass_dat = mass_dat, spec1 = spec1, spec2 = spec2, 
                     spec3 = spec3, spec4 = spec4, spec5 = spec5, spec6 = spec6) }
    
    else if(method == "RMS"){
      .normMethod_RMS(dat = dat, mass_dat = mass_dat, spec1 = spec1, spec2 = spec2, 
                      spec3 = spec3, spec4 = spec4, spec5 = spec5, spec6 = spec6) }
  }
}  




