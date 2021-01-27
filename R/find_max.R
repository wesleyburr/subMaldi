# -----------------------------------------------------------------------
# Last Updated: January 27, 2021
# Author: Kristen Yeh, Sophie Castel
# Title: subMALDI Peak Maximum Functions
# -----------------------------------------------------------------------

# -----------------
# FIND PEAK MAXIMA
# -----------------

# Find peak maximum and associated m/z of spectra
find_max <- function (dat, mass_dat, spectra_cols){
  
  # --------------
  # LOGICAL CHECKS
  # --------------
  
  stopifnot(
    is.character(mass_dat),
    is.character(spectra_cols),
    mass_dat %in% colnames(dat),
    all(spectra_cols %in% colnames(dat))
  )
  
  
  
  mz <- dat[[mass_dat]]
  
  spectra <- lapply(spectra_cols, function(x){dat[x]})
  i <- do.call(what = data.frame, args = c(spectra))
  rownames(i) <- mz
  
  max_i <- apply(i, 2, max)
  which_max <- apply(i, 2, which.max)
  max_mz <- mz[which_max]
  
  max_spec <- cbind(max_i, max_mz)
  colnames(max_spec) <- c("Intensity (Max)", "Mass")
  
  return(max_spec)
}

# -----------------------------------------------------------------------


# ------------------------
# FIND PEAK MAXIMA OF SET
# ------------------------

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

