<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> 191e6b7... Rendered new documentation using package 'roxygen2'.
##' Normalize Spectral Data
##' 
##' Normalize spectral data to a common scale using several different methods.
##' 
##' 
##' @param dat The name of the spectral data frame, containing \code{m/z} data
##' in the first column and spectral intensity data in subsequent columns.
##' @param mass_dat A character string; the name of the column in \code{dat}
##' containing the \emph{m/z} data for the spectrum.
##' @param method A character string; the normalization method that should be
##' used to process the data. See 'Methods' below for list of methods. Default
##' = NULL.
##' @param norm_mz Numeric. If \code{method = "custom"}, the \emph{m/z} peak to
##' which the spectral intensity should be normalized to. Value should have the
##' same number of decimal places as the \emph{m/z} data in \code{dat}. If
##' \code{method = "custom_imprecise", this value must be given as a character
##' string of numbers.}
##' @param lower Numeric. If \code{method = "stdev"}, the lower \emph{m/z}
##' bound of the noise region of the spectrum.
##' @param upper Numeric. If \code{method = "stdev"}, the upper \emph{m/z}
##' bound of the noise region of the spectrum.
##' @param spectra_cols A character vector; the names of the column in
##' \code{dat} containing the intensity data for the spectra-of-interest.
##' @param showHI Logical. To be used with \code{method = "custom"}. If
##' \code{TRUE}, intensity values greater than \code{norm_mz} will be kept. If
##' \code{FALSE}, any peaks with intensity greater than \code{norm_mz} will be
##' truncated to the intensity of \code{norm_mz}. Default = FALSE.
##' @return Returns a new data frame including the original \emph{m/z} data and
##' normalized intensity data.
##' @section Methods: \describe{ \item{max}{ Normalizes the intensity data of
##' spectra to a scale of 0,1. } \item{max_set}{ Normalizes the intensity data
##' of spectra to a scale of 0,1, where 1 is the single most intense peak of
##' the spectral set.} \item{custom}{ Normalizes the intensity data of each
##' input spectrum to the intensity of the selected \emph{m/z} peak. }
##' \item{custom_imprecise}{Normalizes the intensity data of each input
##' spectrum to the intensity of the selected \emph{m/z} peak. Allows for less
##' precise normalization; if data contains four decimal places, input
##' \emph{m/z} values can be input to 2 or 3 decimal places.} \item{TIC}{
##' Evaluates the sum of all intensities (TIC) of each spectrum in a dataset.
##' If the TIC of all spectra are not equal, their intensities are multiplied
##' by a normalization factor. } \item{rel_TIC}{ Evaluates the sum of all
##' intensities (TIC) of each spectrum in a dataset. If the TIC of all spectra
##' are not equal, their intensities are multiplied by a normalization factor.
##' Each peak intensity is then divided by the normalized peak intensity so
##' that each spectrum in the dataset has a TIC of 1.} \item{RMS}{ Normalizes
##' each spectrum by dividing each intensity by the spectrum's RMS. }
##' \item{median}{ Evaluates the median of each spectrum in a dataset after
##' removing 0 values introduced by mapping. If the medians are not equal
##' between spectra, a normalization factor is applied to the spectral
##' intensities until all median intensities in the dataset are equal.}
##' \item{stdev}{ Evaluates the standard deviation of intensity values within
##' the same noisy region (a region lacking peaks) of each spectrum. All
##' intensities in each spectrum are then divided by the spectrum's standard
##' deviation in the noise region. All output spectra should have a standard
##' deviation of 1 in the selected region. } \item{quantile}{ Normalizes the
##' distributions of the values in each spectrum in a set.  Sorts the intensity
##' data of each spectrum and evaluates the average intensity for each rank.
##' The intensity values are then replaces with the averaged intensities,
##' rearranged in their original order. } }
##' @author Kristen Yeh <kristenyeh@@trentu.ca> Wesley Burr <wburr@@trentu.ca>
##' Sophie Castel <sophie.castel@@ontariotechu.net>
##' @references https://github.com/wesleyburr/subMaldi
##' @keywords methods manip
##' @examples
##' 
##' ## Load sample dataset "Master2.rda"
##' data("Master2")
##' 
##' ## Normalize spectrum "Before1" to its maximum intensity
##' ex <- normSpectra(dat = Master2, mass_dat = "full_mz", 
##'             method = "max", spectra_cols = "Before1")
##' 
##' ## Normalize the spectra "Before1" and "Before2" to the TIC
##' ex <- normSpectra(dat = Master2, mass_dat = "full_mz",
##'             method = "TIC", spectra_cols = c("Before1", "Before2")
##' 
##' ## Normalize spectrum "After1" to the intensity of the peak at m/z 253.22
##' ex <- normSpectra(dat = Master2, mass_dat = "full_mz", 
##'             method = "custom", norm_mz = 253.22, spectra_cols = "After1")
##' 
##' 

<<<<<<< HEAD
# -----------------------------------------------------------------------
# Last Updated: January 28, 2021
# Author: Kristen Yeh, Sophie Castel
# Title: subMALDI: Normalize Spectral Data
# -----------------------------------------------------------------------

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
    r <- norm_max(dat = dat, mass_dat = mass_dat, spectra_cols = spectra_cols) 
  }
  
  if(method == "custom"){ 
    r <- norm_custom(dat = dat, mass_dat = mass_dat, norm_mz = norm_mz, spectra_cols = spectra_cols, showHI = showHI) 
  } 
    
  if(method == "custom_imprecise"){ 
    r <- norm_custimp(dat = dat, mass_dat = mass_dat, norm_mz = norm_mz, spectra_cols = spectra_cols, showHI = showHI) 
  } 
    
  if(method == "max_set"){ 
    r <- norm_max_set(dat = dat, mass_dat = mass_dat, spectra_cols = spectra_cols) 
  }
    
  if( method == "TIC"){
    r <- norm_TIC(dat = dat, mass_dat = mass_dat, spectra_cols = spectra_cols) 
  } 
    
  if(method == "RMS"){
    r <- norm_RMS(dat = dat, mass_dat = mass_dat, spectra_cols = spectra_cols) 
  }
    
  if(method == "rel_TIC"){
    r <- norm_rel_TIC(dat = dat, mass_dat = mass_dat, spectra_cols = spectra_cols) 
  }
    
  if(method == "median"){
    r <- norm_median(dat = dat, mass_dat = mass_dat, spectra_cols = spectra_cols)
  }
      
  if(method == "stdev"){
    r <- norm_stdev(dat = dat, mass_dat = mass_dat, lower = lower, upper = upper, spectra_cols = spectra_cols) 
  }
    
  if(method == "quantile"){
    r <- norm_quantile(dat = dat, mass_dat = mass_dat, spectra_cols = spectra_cols) 
  }
  
  return(r)
=======
=======
>>>>>>> 191e6b7... Rendered new documentation using package 'roxygen2'.
# -----------------------------------------------------------------------
# Last Updated: January 28, 2021
# Author: Kristen Yeh, Sophie Castel
# Title: subMALDI: Normalize Spectral Data
# -----------------------------------------------------------------------

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
    r <- norm_max(dat = dat, mass_dat = mass_dat, spectra_cols = spectra_cols) 
  }
  
  if(method == "custom"){ 
    r <- norm_custom(dat = dat, mass_dat = mass_dat, norm_mz = norm_mz, spectra_cols = spectra_cols, showHI = showHI) 
  } 
    
  if(method == "custom_imprecise"){ 
    r <- norm_custimp(dat = dat, mass_dat = mass_dat, norm_mz = norm_mz, spectra_cols = spectra_cols, showHI = showHI) 
  } 
    
  if(method == "max_set"){ 
    r <- norm_max_set(dat = dat, mass_dat = mass_dat, spectra_cols = spectra_cols) 
  }
    
  if( method == "TIC"){
    r <- norm_TIC(dat = dat, mass_dat = mass_dat, spectra_cols = spectra_cols) 
  } 
    
  if(method == "RMS"){
    r <- norm_RMS(dat = dat, mass_dat = mass_dat, spectra_cols = spectra_cols) 
  }
    
  if(method == "rel_TIC"){
    r <- norm_rel_TIC(dat = dat, mass_dat = mass_dat, spectra_cols = spectra_cols) 
  }
    
  if(method == "median"){
    r <- norm_median(dat = dat, mass_dat = mass_dat, spectra_cols = spectra_cols)
  }
      
  if(method == "stdev"){
    r <- norm_stdev(dat = dat, mass_dat = mass_dat, lower = lower, upper = upper, spectra_cols = spectra_cols) 
  }
    
  if(method == "quantile"){
    r <- norm_quantile(dat = dat, mass_dat = mass_dat, spectra_cols = spectra_cols) 
  }
<<<<<<< HEAD
>>>>>>> f70720d... TIC norm added
=======
  
  return(r)
>>>>>>> 7fb0bb9... Fixed object return bug
=======
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
>>>>>>> af5047e... TIC norm added- removed .tar.gz
}  



<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
# -----------------------------------------------------------------------
=======
>>>>>>> f70720d... TIC norm added
=======
# -----------------------------------------------------------------------
>>>>>>> 191e6b7... Rendered new documentation using package 'roxygen2'.
=======
>>>>>>> af5047e... TIC norm added- removed .tar.gz

