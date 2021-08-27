\name{norm_RMS}
\alias{norm_RMS}
\title{
Normalize Spectral Intensity by Root Mean Square
}
\description{
Called internally by \code{normSpectra}. Normalizes the intensity data of each input spectrum by dividing each peak's intensity by RMS error. Capable of normalizing 6 spectra to the same \emph{m/z} value at once.
}
\usage{
norm_RMS(dat, mass_dat, spectra_cols)
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

## Normalize spectrum "Before1" using the RMS method
ex <- normSpectra(dat = Master2, mass_dat = "full_mz", method = "RMS", spectra_cols = "Before1")


}

\keyword{ methods }
\keyword{ manip }
