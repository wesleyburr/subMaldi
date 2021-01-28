<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> 191e6b7... Rendered new documentation using package 'roxygen2'.
##' Find Peak Maxima
##' 
##' Analyzes spectral data and returns a list of the most intense peak in each
##' spectrum, including the \emph{m/z} value associated with the peak.
##' 
##' 
##' @param dat The name of the spectral data frame, containing \code{m/z} data
##' in the first column and spectral intensity data in subsequent columns.
##' @param mass_dat A character string; the name of the column in \code{dat}
##' containing the \emph{m/z} data for the spectrum.
##' @param spectra_cols A character vector; the names of the column in
##' \code{dat} containing the intensity data for the spectra-of-interest.
##' @return Returns a data frame indidcating the most intense peaks of each
##' input spectrum. Indicates the spectrum the data is from, the \emph{m/z}
##' value associated with the peak, and the intensity of the maxima.
##' @author Kristen Yeh <kristenyeh@@trentu.ca> Wesley Burr <wburr@@trentu.ca>
##' Sophie Castel <sophie.castel@@ontariotechu.net>
##' @references https://github.com/wesleyburr/subMaldi
##' @keywords methods
##' @examples
##' 
##' ## Load sample dataset "Master.rda"
##' data("Master")
##' 
##' 
##' ## Find maxima of four spectra
##' find_max(dat = Master, mass_dat = "full_mz", spectra_cols = c("Blank1", "Before1", "After1", "After2")
##'
<<<<<<< HEAD
=======
=======

>>>>>>> 191e6b7... Rendered new documentation using package 'roxygen2'.
# -----------------------------------------------------------------------
# Last Updated: January 27, 2021
# Author: Kristen Yeh, Sophie Castel
<<<<<<< HEAD
# Title: subMALDI: Find Peak Maxima
# -----------------------------------------------------------------------


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
  
=======
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
  
>>>>>>> 29ad768... JOSS 5 Issue: Generalized functions for any number of spectra; removed redundancies
  max_i <- apply(i, 2, max)
  which_max <- apply(i, 2, which.max)
  max_mz <- mz[which_max]
  
<<<<<<< HEAD
<<<<<<< HEAD
  max_spec <- data.frame(cbind(max_i, max_mz))
=======
  max_spec <- cbind(max_i, max_mz)
>>>>>>> 29ad768... JOSS 5 Issue: Generalized functions for any number of spectra; removed redundancies
=======
  max_spec <- data.frame(cbind(max_i, max_mz))
>>>>>>> f78c2c2... JOSS 12 (Issue):  Generalized functions for any number of spectra; removed redundancies
  colnames(max_spec) <- c("Intensity (Max)", "Mass")
  
  return(max_spec)
}
<<<<<<< HEAD
>>>>>>> 5cce370... JOSS 5 Issue: Generalized functions for any number of spectra; removed redundancies
=======
>>>>>>> 29ad768... JOSS 5 Issue: Generalized functions for any number of spectra; removed redundancies

# -----------------------------------------------------------------------
<<<<<<< HEAD
# Last Updated: January 27, 2021
# Author: Kristen Yeh, Sophie Castel
# Title: subMALDI: Find Peak Maxima
# -----------------------------------------------------------------------


<<<<<<< HEAD
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
  
=======
# ------------------------
# FIND PEAK MAXIMA OF SET
# ------------------------

find_max_set <- function(dat, mass_dat, spectra_cols){
<<<<<<< HEAD
  
  # --------------
  # LOGICAL CHECKS
  # --------------
  
  stopifnot(
    is.character(mass_dat),
    is.character(spectra_cols),
    mass_dat %in% colnames(dat),
    all(spectra_cols %in% colnames(dat))
  )
>>>>>>> 5cce370... JOSS 5 Issue: Generalized functions for any number of spectra; removed redundancies
  
  if(length(spectra_cols) < 2){
    stop("Only one spectrum input. Please enter two spectra.")
  }
  
  
  mz <- dat[[mass_dat]]
  
  spectra <- lapply(spectra_cols, function(x){dat[x]})
  i <- do.call(what = data.frame, args = c(spectra))
  rownames(i) <- mz
  
<<<<<<< HEAD
  max_i <- apply(i, 2, max)
  which_max <- apply(i, 2, which.max)
  max_mz <- mz[which_max]
  
  max_spec <- data.frame(cbind(max_i, max_mz))
  colnames(max_spec) <- c("Intensity (Max)", "Mass")
  
  return(max_spec)
=======
  which_max <- which(i == max(i), arr.ind = TRUE)
  
  max_spec <- data.frame(Intensity = as.numeric(i[which_max[1], which_max[2]]), Mass = as.numeric(rownames(which_max)))
  
  colnames(max_spec) <- c("Intensity (Max)", "Mass")
  rownames(max_spec) <- colnames(i)[which_max[2]]
  
  return(max_spec)
  
>>>>>>> 5cce370... JOSS 5 Issue: Generalized functions for any number of spectra; removed redundancies
=======
  
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
  
>>>>>>> 29ad768... JOSS 5 Issue: Generalized functions for any number of spectra; removed redundancies
}

# -----------------------------------------------------------------------
=======
>>>>>>> 191e6b7... Rendered new documentation using package 'roxygen2'.
