\name{norm_custimp}
\alias{norm_custimp}
\title{
Normalization Method: Maximum Intensity of Set
}
\description{
Called internally by \code{normSpectra}. Normalizes the intensity data of spectra to a scale of 0,1, where 1 is the single most intense peak of the spectral set; does so against a specified m/z. Capable of normalizing 1-6 spectra at once.
}
\usage{
norm_custimp(dat, mass_dat, norm_mz, spectra_cols, showHI = FALSE)
}
\arguments{
  \item{dat}{The name of the spectral data frame, containing \code{m/z} data in the first column and spectral intensity data in subsequent columns.}
  \item{mass_dat}{A character string; the name of the column in \code{dat} containing the \emph{m/z} data for the spectrum.}
  \item{norm_mz}{Numeric m/z reference to be normalized against.}
  \item{spectra_cols}{A character string; the names of the column in \code{dat} containing the intensity data for the spectrs to be analyzed.}
  \item{showHI}{A Boolean variable; clipping argument for results greater than 1 after normalization.}
}
\value{
Returns a new data frame including the original \emph{m/z} data and normalized intensity data of each input spectrum.
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
## Load sample datasat "Master2.rda"
data("Master2")

## Normalize the intensity data of two samples to the maximum of the blank
ex <- normSpectra(dat = Master2, mass_dat = "full_mz", norm_mz = 255.23, 
                  method = "custom_imprecise", spectra_cols = c("Blank1", "Before1", "Before2"))
}

\keyword{ methods }
\keyword{ manip }
