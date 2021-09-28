##' Remove Empty Rows
##' 
##' Data frames created using createSpecDF can have thousands to millions of
##' rows, especially when dealing with high resolution mass spectrometry data.
##' This can be quite taxing to the speed of executing certain functions,
##' especially when it comes to visualizing data. This function removes empty
##' rows to reduce the computational load and increase the ease of use of
##' functions on the data frame. Any row where all elements (but the first,
##' which is \code{full_mz}) equal 0 are removed from the data frame.
##' 
##' 
##' @param dat The spectral data frame, containing \code{full_mz} in the first
##' column, to be trimmed of all-zero rows.
##' @param mass The name of the mass column. Defaults to \code{full_mz}.
##' @return Returns the input data frame without its all-zero rows.
##' @note Avoid truncating a dataframe until all samples to be compared have
##' been mapped.
##' @author Kristen Yeh <kristenyeh@@trentu.ca>
##' @seealso \code{\link{createSpecDF}},\code{\link{mapSpectrum}}
##' @references https://github.com/wesleyburr/subMaldi
##' @keywords manip methods
##' @examples
##' 
##' ## Load sample dataset "Mastre.rda"
##' data("Master")
##' 
##' ## Select only the spectra "Before1" and "Before2"
##' ex <- select(Master, "full_mz", "Before1", "Before2")
##' 
##' ## Use rmveEmpty(x) on those data frames to reduce computational load for use with
##' # other functions and packages
##' ex <- rmveEmpty(dat = ex)
##' 


# -----------------------------------------------------------------------
# Last Updated: September 28, 2021
# Author: Kristen Yeh, Wesley Burr
# Title: subMALDI: Remove Empty Rows
# -----------------------------------------------------------------------


# Throw out every all-zero row to lessen the computational load and get rid of empty values
# Wouldn't recommend using this unless you've already made a master frame with all of the samples you
# want to compare as your data will, once again, become irregularly spaced!
rmveEmpty <- function(dat, mass = "full_mz"){
  stopifnot(is.character(mass), mass %in% names(dat))
  mass_col <- which(names(dat) == mass)
  dat <- dat[rowSums(dat[, -mass_col]) > 0, ]
  return(dat)
}


# ----------------------------------------------------------------------------
