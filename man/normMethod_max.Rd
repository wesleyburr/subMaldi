\name{norm_max}
\alias{norm_max}
\title{
Normalization Method: Maximum Intensity
}
\description{
Called internally by \code{normSpectra}. Normalizes the intensity data of spectra to a scale of 0,1. Capable of normalizing 1-6 spectra, to their individual maxima, at once.
}
\usage{
norm_max(dat, mass_dat, spectra_cols)
}
\arguments{
  \item{dat}{The name of the spectral data frame, containing \code{m/z} data in the first column and spectral intensity data in subsequent columns.}
  \item{mass_dat}{A character string; the name of the column in \code{dat} containing the \emph{m/z} data for the spectrum.}
  \item{spectra_cols}{A character string; the names of the column in \code{dat} containing the intensity data for the spectrs to be analyzed.}
}
\value{Returns a new data frame including the original \emph{m/z} data and normalized intensity data.
}
\references{
https://github.com/wesleyburr/subMaldi
}
\author{
Kristen Yeh <kristenyeh@trentu.ca>
Wesley Burr <wesleyburr@trentu.ca>
}
\seealso{
\code{\link{normSpectra}}
}
\examples{
## Load sample dataset "Master2.rda"
data("Master2")

## Normalize intensity data for three spectra
normSpectra(dat = Master2, mass_dat = "full_mz", method = "max", 
            spectra_cols = c("Blank1", "Before1", "After1"))
}
\keyword{ methods }
\keyword{ manip }
