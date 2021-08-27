##' Read Raw ASCII Files
##' 
##' A function to convert raw ASCII files obtained from the Bruker SolariX XR (eXtreme Resolution) FT-ICR Mass Spectrometer.
##' 
##' @param filename character; The filepath to the local \code{.ascii} data that is to be converted.
##' 
##' @return Returns an object of class \code{data.frame} with two numeric columns: \code{"mass"} and \code{"intensity"}
##'
##' @author Sophie Castel <sophie.castel@@ontariotechu.net>
##' @references https://github.com/wesleyburr/subMaldi
##' @keywords data ascii convert read
##' @examples
##' 
##' ## Converting sample ASCII file 'raw_ascii.ascii' to 'data.frame'
##' file_loc <- system.file("extdata", "raw_ascii.ascii", package = "subMALDI")
##' converted <- read_ascii(filename = file_loc)
##' 


# -----------------------------------------------------------------------
# Last Updated: August 27, 2021
# Authors: Sophie Castel, Wesley Burr
# Title: subMALDI: Read Raw ASCII Files
# -----------------------------------------------------------------------

read_ascii <- function(filename){
  
  ################
  # LOGICAL CHECKS
  ################
  
  stopifnot(is.character(filename))
  
  # Read in ASCII file as a character string
  txt <- unlist(read.table(file = filename))
  
  # Add newline character every two commas
  regex <- gsub(pattern = "[^,]*,[^,]*\\K", replacement = ",\n", x = txt, perl = TRUE)
  
  # Replace commas with space delimiter
  regex <- gsub(pattern = ",", replacement = " ", x = regex, perl = TRUE)
  
  # Convert to data.frame object
  tbl <- utils::read.table(text = regex, header = FALSE, col.names = c("m/z", "intensity"), 
                           fill = TRUE, row.names = NULL)
  
  # Remove first four rows of metadata
  tbl <- tbl[-(1:4),]
  
  return(tbl)
}
