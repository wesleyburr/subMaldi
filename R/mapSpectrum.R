##' Map Spectrum to \emph{m/z} Vector
##' 
##' Fills in the columns of the empty data frame created using
##' \code{createSpecDF}. Mass to charge values from pairwise spectral data are
##' compared to the \code{full_mz} vector. All peaks within the \code{thresh}
##' of one another are binned, and only the maximum intensity of that bin is
##' filled into the mapped spectrum.
##' 
##' 
##' @param dat A pairwise data frame containing your spectral data. Should
##' contain two columns: one for \emph{m/z} and one for intensity.
##' @param massCol A character string; the name of the \emph{m/z} column in
##' \code{dat}.
##' @param intenseCol A character string; the name of the intensity column in
##' \code{dat}.
##' @param dig Number of decimal places to round the \emph{m/z} data to; must
##' match the same value used in \code{createSpecDF} in order for the columns
##' to fill. Default = 4.
##' @param thresh Single numeric value; all \emph{m/z} values within
##' \code{thresh} of each other are binned under that with the maximum
##' intensity. Default = 5e-5.
##' @param spec_df An empty data frame created using \code{createSpecDF}.
##' @param colName A character string; the name of the column that should be
##' filled with the spectral data.
##' @return Returns a vector that is used to fill in the \code{colName} column
##' of the mapped spectral data frame.
##' @section Warning: It is important that the values for \code{thresh} and
##' \code{dig} are equal to that of \code{res} and \code{dig} used in
##' \code{createSpecDF()}. Otherwise the data will fail to map.
##' @author Kristen Yeh <kristenyeh@@trentu.ca> Wesley Burr <wburr@@trentu.ca>
##' @seealso \code{\link{createSpecDF}}
##' @references https://github.com/wesleyburr/subMaldi
##' @keywords methods array
##' @examples
##' 
##' ## Load sample dataset "Blank1.rda"
##' data("lank1")
##' 
##' ## Create empty spectral data frame to map to
##' spec_df <- createSpecDF(min_mz = 53.76, max_mz = 1100, res = 0.0001, dig = 4)
##' 
##' ## Map binary spectral data to empty spectral data frame
##' spec_df <- mapSpectrum(dat = Blank1, massCol = "mass", intenseCol = "Intensity", 
##'             spec_df = spec_df, colName = "Sample", thresh = 1e-04, dig = 4)
##' 
##' 


# -----------------------------------------------------------------------
# Last Updated: February 2, 2021
# Author: Kristen Yeh, Sophie Castel
# Title: subMALDI: Map Spectrum to m/z Vector
# -----------------------------------------------------------------------


mapSpectrum <- function(dat, massCol, intenseCol, dig = 4, thresh = 1e-4, spec_df, colName) {
  # Sanity checks
  stopifnot( is.character(massCol), is.character(intenseCol),
             massCol %in% names(dat), intenseCol %in% names(dat) )
  stopifnot(colName %in% names(spec_df)) 
  
  # Round m/z data from dat to match full_mz
  dat <- round(dat, digits = dig)
  
  # Clean code by subsetting to vectors
  mass <- dat[[massCol]]
  intense <- dat[[intenseCol]]
  
  # Quantify the spaces between m/z values, make vector of all differences greater than threshold
  diffs <- mass[-1] - mass[-length(mass)]
  too_close <- which(diffs < thresh)
  throw_away <- c()
  
  if(length(too_close) > 0) { # not only isolated peaks
    # fix the 'too close together' problem: identify local maximums
    dupes <- which(duplicated(mass))  # identifies the duplicates, but not
    # the points they're duplicates _of_
    if(length(dupes) > 0) { # found some ...
      for(j in 1:length(dupes)) { # cycle through
        mz.val <- mass[dupes[j]]  # grab the mz that's duplicated
        all_dupes_tmp <- which(mass == mz.val)  # find all mz's that match
        which_max <- which(intense[all_dupes_tmp] ==
                             max(intense[all_dupes_tmp]))  # find the peak with the biggest intensity
        throw_away <- c(throw_away, all_dupes_tmp[-which_max]) # put the rest in throw-away
      }
    }
  }
  
  throw_away <- unique(throw_away) # could get duplicates in throw away
  
  # I think this is the part that needs to be fixed - original data frame isn't getting trimmed
  if(length(throw_away) > 0) {
    dat <- dat[-throw_away, ]
  } 
  
  # last minute sanity check
  if(length(which(duplicated(dat[[massCol]])) > 0)) {
    dat <- dat[-which(duplicated(dat[[massCol]])), ]
  }
  
  # Fill in the data frame with the rounded, unique data
  spec_df[spec_df$full_mz %in% dat[[massCol]], colName] <- dat[[intenseCol]]
  return(spec_df)
  
}


# -----------------------------------------------------------------------
