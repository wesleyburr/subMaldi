# --------------------------------------------------------------------------------------------
# Date: January 27, 2021
# Author: Kristen Yeh, Sophie Castel
# Title: subMALDI Average Spectra
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