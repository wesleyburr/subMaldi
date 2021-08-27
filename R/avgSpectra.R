##' Average Spectral Replicates
##' 
##' Combines spectral replicates either by averaging (method = "mean") or
##' summing (method = "sum") the intensity values across each row representing a
##' mass-to-charge value in \code{full_mz}.
##' 
##' 
##' @param dat The mapped spectral data frame, containing \code{full_mz} in the
##' first column.
##' @param method A character string; the method used to combine the spectra.
##' Methods include "sum" and "mean". Default = "mean."
##' @param spectra_cols A character vector; the names of the column in
##' \code{dat} containing the intensity data for the spectra-of-interest.
##' @return Returns a new column in the input data frame containing the
##' averaged intensity data.
##' @author Kristen Yeh <kristenyeh@@trentu.ca> Wesley Burr <wesleyburr@@trentu.ca>
##' Sophie Castel <sophie.castel@@ontariotechu.net>
##' @references https://github.com/wesleyburr/subMaldi
##' @keywords manip methods
##' @examples
##' 
##' ## Load sample dataset "Master.rda"
##' data("Master")
##' 
##' ## Average blank spectrum 1 and 2 using the method "mean"
##' ex <- avgSpectra(Master, method = "mean", spectra_cols = c("Blank1", "Blank2"))
##' 
##' ## Average blank spectrum 1 and 2 using the method "sum"
##' ex <- avgSpectra(Master, method = "sum", spectra_cols = c("Blank1", "Blank2"))
##' 


# --------------------------------------------------------------------------------------------
# Date: January 27, 2021
# Author: Kristen Yeh, Sophie Castel
# Title: subMALDI: Average Spectral Replicates
# --------------------------------------------------------------------------------------------

# Average intensities across rows per sample
# First user should organize standardized data frame so all samples are together
# First column should always be full_mz, the rest should be spectra, each scan per column


avgSpectra <- function(dat, method = "mean", spectra_cols){
  
  # --------------
  # LOGICAL CHECKS
  # --------------
  
  if(length(spectra_cols) < 2){
    stop("Only one spectrum input. Please enter two spectra for averaging.")
  }
  
  if(!all(spectra_cols %in% colnames(dat))){
    logic <- which(!(spectra_cols %in% colnames(dat)))
    stop(c("Columns '",paste0(as.character(spectra_cols[logic]), sep = "', "), " not found in specified dataframe."))
  }
  
  if(method == "sum"){
    .avg_sum(dat = dat, spectra_cols) } 
  else if(method == "mean"){
    .avg_mean(dat = dat, spectra_cols) }
}

# --------------
# METHOD = SUM
# --------------

.avg_sum <- function(dat, spectra_cols){
  
  mz <- dat$full_mz
  
  spectra <- lapply(spectra_cols, function(x){dat[x]})
  i <- do.call(what = data.frame, args = c(spectra))
  
  dat <- data.frame(cbind(mz, dat, Sum = apply(i, 1, sum, na.rm = TRUE)))
  
  return(dat)
}

# --------------
# METHOD = MEAN
# --------------

.avg_mean <- function(dat, spectra_cols){
  
  mz <- dat$full_mz
  
  spectra <- lapply(spectra_cols, function(x){dat[x]})
  i <- do.call(what = data.frame, args = c(spectra))
  
  dat <- data.frame(mz, dat, Average = apply(i, 1, mean, na.rm = TRUE))
  
  return(dat)
}