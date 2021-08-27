##' Import a Single .csv Spectrum
##' 
##' Imports a single spectrum in .csv format and turns it into a binary data
##' frame containing a "mass" and "Intensity" column.
##' 
##' 
##' @param spec_file A character string; the file path to the \code{.csv}
##' spectrum that is to be imported.
##' @param massCol A character string; the name of the mass column in the
##' spectrum's \code{.csv} file.
##' @param intenseCol A character string; the name of the intensity column in
##' the spectrum's \code{.csv} file.
##' @return Returns a binary data frame containing the imported data in a
##' \emph{m/z} column denoted "mass", and an intensity column denoted
##' "Intensity".
##' @author Kristen Yeh <kristenyeh@@trentu.ca> Wesley Burr <wesleyburr@@trentu.ca>
##' @references https://github.com/wesleyburr/subMaldi
##' @keywords methods
##' 

# -----------------------------------------------------------------------
# Last Updated: May 25, 2020
# Author: Kristen Yeh
# Title: subMALDI: Import a Single .csv Spectrum
# -----------------------------------------------------------------------


# Import .csv mass spectra files from a directory and export as .rda's for use in data frame
# Basically just a quick and easy way to turn .csv files into individual data frames per spectrum
readcsvSpec <- function(spec_file, massCol, intenseCol){
  temp <- read.csv(file = spec_file)
  mass <- select(temp, massCol)
  Intensity <- select(temp, intenseCol)
  mass <- unlist(cbind(mass))
  Intensity <- unlist(cbind(Intensity))
  spec <- data.frame(mass, Intensity)
  return(spec)
}


# -----------------------------------------------------------------------