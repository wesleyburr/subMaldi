##' Find Most Intense Peak of a Set
##' 
##' Analyzes spectral data and returns information about the most intense peak
##' in all of the spectral set. Indicates the most intense spectrum, the
##' \emph{m/z} value, and intesnity of the maximum. User must specify at least
##' two spectra.
##' 
##' 
##' @param dat The name of the spectral data frame, containing \code{m/z} data
##' in the first column and spectral intensity data in subsequent columns.
##' @param mass_dat A character string; the name of the column in \code{dat}
##' containing the \emph{m/z} data for the spectrum.
##' @param spectra_cols A character vector; the names of the column in
##' \code{dat} containing the intensity data for the spectra-of-interest.
##' @return Returns a data frame indidcating the peak maximum of the spectral
##' set. The sample number, \emph{m/z} value, and intensity data are returned.
##' @author Kristen Yeh <kristenyeh@@trentu.ca> Wesley Burr <wesleyburr@@trentu.ca>
##' Sophie Castel <sophie.castel@@ontariotechu.net>
##' @references https://github.com/wesleyburr/subMaldi
##' @keywords methods
##' @examples
##' 
##' ## Load sample dataset "Master.rda"
##' data("Master")
##' 
##' ## Find the most intense peak of six spectra
##' find_max_set(dat = Master, mass_dat = "full_mz", spectra_cols = c("After1", "Blank2", 
##'             "Before1", "Blank1", "After2", "Before2"))
##' 


# -----------------------------------------------------------------------
# Last Updated: February 2, 2021
# Author: Kristen Yeh, Sophie Castel
# Title: subMALDI: Find Most Intense Peak of Set
# -----------------------------------------------------------------------

find_max_set <- function(dat, mass_dat, spectra_cols){
  
  # --------------
  # LOGICAL CHECKS
  # --------------
  
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
  rownames(i) <- mz
  
  which_max <- which(i == max(i), arr.ind = TRUE)
  
  max_spec <- data.frame(Intensity = as.numeric(i[which_max[1], which_max[2]]), Mass = as.numeric(rownames(which_max)))
  
  colnames(max_spec) <- c("Intensity (Max)", "Mass")
  rownames(max_spec) <- colnames(i)[which_max[2]]
  
  return(max_spec)
  
}


# -----------------------------------------------------------------------

