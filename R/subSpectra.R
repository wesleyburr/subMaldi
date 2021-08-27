##' Subtract Blank Peaks from Sample Spectra
##' 
##' This function takes the intensity values in the blank column of the mapped
##' spectral data frame and subtracts them from the intensity values in the
##' sample column. If the intensity value in the blank spectrum is greater than
##' that in the sample spectrum, the intensity of the sample peak is given an
##' intensity of 0.
##' 
##' 
##' @param dat The spectral data frame, containing \code{full_mz} in the first
##' column and intensity data in the subsequent columns.
##' @param Blank_Var A character string; the name of the blank column that will
##' be subtracted from the sample column.
##' @param Sample A character string; the name of the sample column that the
##' blank data will be subtracted from.
##' @param Sub_Sample A character string; the name of the column to be filled
##' with the subtracted spectrum.
##' @param showNeg Logical; if \code{showNeg = TRUE}, then negative values
##' produced by the subtraction will be kept in the dataset. If \code{showNeg =
##' FALSE}, then negative values created by the subtraction are set to 0 in the
##' output dataset.
##' @return Returns a vector of intensity values that are filled into the
##' \code{Sub_Sample} column of the mapped data frame at the corresponding rows
##' of \code{full_mz}.
##' @author Kristen Yeh <kristenyeh@@trentu.ca> Wesley Burr <wesleyburr@@trentu.ca>
##' @seealso \code{\link{createSpecDF}}, \code{\link{mapSpectrum}}
##' @references https://github.com/wesleyburr/subMaldi
##' @keywords methods manip
##' @examples
##' 
##' ## Load sample datasets "Master.rda"
##' data("Master")
##' 
##' ## Make subtraction column for new data
##' Master <- transform(Master, "Subtracted" = 0)
##' 
##' ## Subtract Blank1 from Before1
##' Master <- subSpectra(dat = Master, Blank_Var = "Blank1", 
##'                       Sample = "Before1", Sub_Sample = "Subtracted")
##' 
##' 


# -----------------------------------------------------------------------
# Last Updated: May 25, 2020
# Author: Kristen Yeh
# Title: subMALDI: Subtract Blank Peaks from Sample Spectra
# -----------------------------------------------------------------------

# Make sure you transform dat to have a Sub_Sample column for the subtracted spec.
# spec.df <- transform(spec.df, Sub.C15 = 0)

# Fill in the subtraction columns/Subtract your blank
subSpectra <- function(dat, Blank_Var, Sample, Sub_Sample, showNeg = FALSE){
  # Sanity checks
  stopifnot( is.character(Blank_Var), is.character(Sample), is.character(Sub_Sample),
             Blank_Var %in% names(dat), Sample %in% names(dat), Sub_Sample %in% names(dat) )

  samp <- dat[[Sample]]
  blank <- dat[[Blank_Var]]
  sub_samp <- dat[[Sub_Sample]]

  # Fill in subtraction vector
  sub_samp[blank == 0] <- samp[blank == 0]
  # Otherwise, subtract
  sub_samp[blank != 0 ] <- samp[blank != 0] - blank[blank != 0]

  if(showNeg == TRUE){
  } else {
    # Find negatives that make no sense, and wipe them out
    sub_samp[sub_samp < 0] <- 0
  }

  # Replace the data into the data frame and return
  dat[[Sub_Sample]] <- sub_samp
  return(dat)
}

# -----------------------------------------------------------------------
