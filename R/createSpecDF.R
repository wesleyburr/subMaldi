##' Create Empty Spectral Data Frame
##' 
##' Creates an empty data frame to be used for mapping spectral data to a
##' vector of \emph{m/z} values. The vector of \emph{m/z} values is stored in
##' the first column and can be called by "\code{full_mz}".
##' 
##' 
##' @param min_mz Single numeric value; minimum \emph{m/z} value of the
##' observed range. Default = 53.76.
##' @param max_mz Single numeric value; upper end of \emph{m/z} range observed
##' in spectra. Default = 1100.
##' @param res The resolution of peaks; a single numeric value indicating the
##' step size from the minimum to maximum \emph{m/z} bin. Also known as bin
##' width. Default = 0.0001.
##' @param dig Number of decimal places to round the \emph{m/z} vector to;
##' required for \emph{m/z} values to match full \emph{m/z} vector. Default =
##' 4.
##' @return Returns a data frame which can be used to map irregularly spaced
##' spectral data to a set range of \emph{m/z} values, contained in the first
##' column of the frame.
##' @author Kristen Yeh <kristenyeh@@trentu.ca>
##' @references https://github.com/wesleyburr/subMaldi
##' @keywords methods array
##' @examples
##' 
##' ## Creating an empty spectrum with an m/z range of 500 to 2000 m/z, with a step size of 0.001
##' spec_df <- createSpecDF(min_mz = 500, max_mz = 2000, res = 0.001, dig = 3)
##' 
##' ## Creating an empty spectrum with an m/z range of 100 to 1000, 
##' # with a step size of 0.0001 and sample names
##' spec_df <- createSpecDF(min_mz = 100, max_mz = 1000, res = 0.0001, dig = 4)
##' 

# -----------------------------------------------------------------------
# Last Updated: May 25, 2020
# Author: Kristen Yeh
# Title: subMALDI : Create Empty Spectral Data Frame
# -----------------------------------------------------------------------

createSpecDF <- function(min_mz = 53.76, max_mz = 1100, res = 0.0001, dig = 4) {
  full_mz <- seq(min_mz, max_mz, by = res)
  spec_df <- data.frame(full_mz,
                        Sample = 0)
  spec_df$full_mz <- round(spec_df$full_mz, digits = dig)
  return(spec_df)
}

# -----------------------------------------------------------------------
