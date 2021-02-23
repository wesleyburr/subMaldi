##' Import .csv Spectra from a Directory
##' 
##' Imports all \code{.csv} files in a directory, turns them into a binary data
##' frame containing a "mass" and "Intensity" column, and outputs them as
##' \code{.rda} files into an output directory. Allows for rapid import of many
##' spectral datasets at once.
##' 
##' 
##' @param direct The path to the directory where the \code{.csv} files are
##' held.
##' @param massCol A character string; the name of the mass column in the
##' \code{.csv} files.
##' @param intenseCol A character string; the name of the intensity column in
##' the \code{.csv} files.
##' @param output The path to the directory where the exported \code{.rda}
##' files containing the binary spectral data frames should go.
##' @return Returns a directory of \code{.rda} files containing binary spectral
##' data frames into the \code{output} file path.
##' @author Kristen Yeh <kristenyeh@@trentu.ca> Wesley Burr <wburr@@trentu.ca>
##' @references https://github.com/wesleyburr/subMaldi
##' @keywords methods
##' 
##' 


# -----------------------------------------------------------------------
# Last Updated: May 25, 2020
# Author: Kristen Yeh
# Title: subMALDI: Import .csv Spectra from a Directory
# -----------------------------------------------------------------------


readcsvDir <- function(direct, massCol, intenseCol, output){
  all_files <- list.files(path = direct, pattern = ".csv")
  for(j in 1:length(all_files)) {
    temp <- read.csv(  paste0(direct, all_files[ j ]) )
    mass <- select(temp, massCol)
    Intensity <- select(temp, intenseCol)
    mass <- unlist(cbind(mass))
    Intensity <- unlist(cbind(Intensity))
    spec <- data.frame(mass, Intensity)
    
    new_name <- strsplit(all_files[ j ], "\\.")[[1]][1]
    assign(x = new_name, spec)
    save(file = paste0(output, new_name, ".rda"), list = new_name)
  }
}


# -----------------------------------------------------------------------