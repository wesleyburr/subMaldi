<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> 191e6b7... Rendered new documentation using package 'roxygen2'.
##' Smooth Noise in Spectrum
##' 
##' Offers two different methods for smoothing noise in raw spectral data: a
##' moving average filter and the Savitzky-Golay filter (1).
##' 
##' 
##' @param dat The name of the spectral data frame, containing \code{m/z} data
##' in the first column and spectral intensity data in subsequent columns.
##' @param mass_dat A character string; the name of the column in \code{dat}
##' containing the \emph{m/z} data for the spectrum.
##' @param intensity_dat A character string; the name of the column in
##' \code{dat} containing the intensity data for the spectrum to be smoothed.
##' @param method A character string; the method to be used for smoothing.
##' Available methods include a Savitzky-Golay filter (\code{"sgolay"}) and a
##' moving average filter (\code{"mov_avg."})
##' @param p Single numeric value. If \code{method = "sgolay"}, the filter
##' order of smoothing. Default = NULL.
##' @param n Single odd numeric value. If \code{method = "sgolay"}, the length
##' of the smoothing filter. If \code{method = "mov_avg"}, the window span
##' size. Default = NULL.
##' @param m Single numeric value. If \code{method = "sgolay"}, returns the
##' m-th derivative of the filter coefficients. Default = 0.
##' @param ts Single numeric value. If \code{method = "sgolay"}, the time
##' scaling factor. Default = 1.
##' @return Returns a new data frame containing the smoothed spectral data.
##' @author Kristen Yeh <kristenyeh@@trentu.ca> Wesley Burr <wburr@@trentu.ca>
##' @references https://github.com/wesleyburr/subMaldi (1) A. Savitzky, M.J.E.
##' Golay, Smoothing and differentiation of data by simplified least-squares
##' procedures, Anal. Chem. 36 (8) (1964) 1627-1639.
##' @keywords methods manip
##' @examples
##' 
##' ## Load sample dataset "Before1.rda"
##' data("Before1")
##' 
##' ## Testing method "sgolay"
##' test <- smoothSpectrum(dat = Before1, mass_dat = "mass",
##'                         intensity_dat = "Intensity", 
##'                         method = "sgolay", p = 4, 
##'                         n = 25, m = 0)
##' 
##' ## Testing method "mov_avg"                        
##' test <- smoothSpectrum(dat = Before1, mass_dat = Before1$mass,
##'                         intensity_dat = Before1$Intensity,
##'                         method = "mov_avg", n = 25)
##'                    
##' 


<<<<<<< HEAD
# ----------------------------------------------------------------------------
# Last Updated: July 10, 2020
# Author: Kristen Yeh
# Title: subMALDI: Smooth Noise in Spectrum
=======
=======
>>>>>>> c29c117... Added smoothing
# ----------------------------------------------------------------------------
# Last Updated: July 10, 2020
# Author: Kristen Yeh
# Title: subMALDI - Smoothing Parent Function
<<<<<<< HEAD
>>>>>>> e43371a... Added smoothing
=======
# ----------------------------------------------------------------------------
# Last Updated: July 10, 2020
# Author: Kristen Yeh
# Title: subMALDI: Smooth Noise in Spectrum
>>>>>>> 191e6b7... Rendered new documentation using package 'roxygen2'.
=======
>>>>>>> c29c117... Added smoothing
# ----------------------------------------------------------------------------


smoothSpectrum <- function(dat, mass_dat, intensity_dat, method = NULL, 
                           p = NULL, n = NULL, m = 0, ts = 1){
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
  
  if(is.null(method)){ stop('Please select a valid smoothing method. 
=======
  if(is.null(method)){ stop('Please select a valid normalization method. 
>>>>>>> e43371a... Added smoothing
=======
=======
  
>>>>>>> 5831098... Added pre-processing vignette
  if(is.null(method)){ stop('Please select a valid smoothing method. 
>>>>>>> 4bbf2ad... Baseline correction added
=======
  if(is.null(method)){ stop('Please select a valid normalization method. 
>>>>>>> c29c117... Added smoothing
=======
  if(is.null(method)){ stop('Please select a valid smoothing method. 
>>>>>>> 8cb7c44... Baseline correction added
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