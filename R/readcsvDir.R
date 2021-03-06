##' Import .csv Spectra from a Directory and Export
##' 
##' Imports all \code{.csv} files in a directory, turns them into a binary data
##' frame containing a "mass" and "Intensity" column, and outputs them as
##' \code{.rda} files into an output directory. Allows for rapid import of many
##' spectral datasets at once.
##' 
##' Assumes that the \code{.csv} files are organized in tidy format, i.e.,
##' columns are variables and rows are individual m/z observations.
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
  
  if(!length(all_files) > 0){
    stop(paste0("No .csv files found in directory: ", direct, ".  Please double check filepath and directory contents."))
  }

  for(j in 1:length(all_files)) {
    cat(paste0("Reading: ", paste0(direct, all_files[ j ]), "\n"))
    temp <- read.csv( paste0(direct, all_files[ j ]) )

    if(!massCol %in% temp){
      stop(c("Mass column specified in massCol not found in file #", j, ". Column names: ", paste0(colnames(temp), sep = " ")))
    }
    
    if(!intenseCol %in% temp){
      stop(c("Intensity column specified in intenseCol not found in file #", j, ". Column names: ", paste0(colnames(temp), sep = " ")))
    }
    
    mass <- select(temp, massCol)
    
    Intensity <- select(temp, intenseCol)
    

    mass <- unlist(cbind(mass))
    Intensity <- unlist(cbind(Intensity))
    spec <- data.frame(mass, Intensity)
     
    new_name <- strsplit(all_files[ j ], "\\.")[[1]][1]  # grab the file name without CSV
    assign(x = new_name, spec)

    cat(paste0("Writing: ", paste0(output, new_name, ".rda"), "\n"))
    save(file = paste0(output, new_name, ".rda"), list = new_name)
  }
}


# -----------------------------------------------------------------------
