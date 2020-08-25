# ----------------------------------------------------------------------------
# Last Updated: August 25, 2020
# Author: Kristen Yeh
# Title: subMALDI - Peak Detection Parent Function
# ----------------------------------------------------------------------------


peakDet <- function(dat, mass_dat, intensity_dat, method = NULL, n = NULL, 
                    SNR_thresh = NULL){
  
  if(is.null(method)){
    stop("Please select a valid method. See ?peakDet for list of methods.")
  } else if(method == "snr"){ 
    if(is.null(n) | is.null(SNR_thresh)){
      stop("Missing argument n or SNR_thresh.")
    }else{ .peak_snr(dat, mass_dat, intensity_dat, n = n, 
                     SNR_thresh = SNR_thresh) }
  } else if(method == "slopes"){ 
    if(is.null(n)){
      stop("Missing argument n. Please select a window size.")
    }else{.peak_slope(dat = dat, mass_dat = mass_dat, 
                intensity_dat = intensity_dat, n = n) }
  }
}


# ----------------------------------------------------------------------------