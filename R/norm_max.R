# -----------------------------------------------------------------------
# Last Updated: January 27, 2021
# Author: Kristen Yeh, Sophie Castel
# Title: subMALDI: Normalization Method - Maximum Intensity
# -----------------------------------------------------------------------

# -----------------------
# NORMALIZATION FUNCTION
# -----------------------

# Function rescales intensity to 0,1
.normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# ------------------------------
# METHOD: MAX. OF EACH SPECTRUM
# ------------------------------

norm_max <- function(dat, mass_dat, spectra_cols){
  
  # ---------------------
  # LOGICAL CHECKS
  # ---------------------
  
  stopifnot(
    is.character(mass_dat),
    is.character(spectra_cols),
    mass_dat %in% colnames(dat),
    all(spectra_cols %in% colnames(dat))
  )
  
  
  
  mz <- dat[[mass_dat]]
  
  spectra <- lapply(spectra_cols, function(x){dat[x]})
  i <- do.call(what = data.frame, args = c(spectra))
  
  i_n <- apply(i, 2, FUN = .normalize)
  dat <- data.frame(cbind(mz, i_n))
  colnames(dat) <- c("full_mz", spectra_cols)
  
  return(dat)
}

# ---------------------
# METHOD: MAX. OF SET
# ---------------------

# Normalization function specific to custom max_y
.normalize_set <- function(y, max_y){
  return((y - min(y)) / (max_y - min(y)))
}

.norm_max_set <- function(dat, mass_dat, spectra_cols){
  
  # ---------------------
  # LOGICAL CHECKS
  # ---------------------
  
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
  
  which_max <- which(i == max(i), arr.ind = TRUE)
  max_i <- i[which_max[1], which_max[2]]
  
  i_n <- apply(i, 2, FUN = function(x) {.normalize_set(y = x, max_y = max_i)} )
  dat <- data.frame(mz, i_n)
  colnames(dat) <- c("full_mz", spectra_cols)
  
  return(dat)
}
