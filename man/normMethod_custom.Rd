\name{norm_custom}
\alias{norm_custom}
\title{
Normalize Peak Intensity to Custom \emph{m/z}
}
\description{
Called internally by \code{normSpectra}. Normalizes the intensity data of each input spectrum to the intensity of the selected \emph{m/z} peak. Capable of normalizing 6 spectra to the same \emph{m/z} value at once.
}
\usage{
norm_custom(dat, mass_dat, norm_mz, spectra_cols, showHI = FALSE)
}
\arguments{
  \item{dat}{The name of the spectral data frame, containing \code{m/z} data in the first column and spectral intensity data in subsequent columns.
}
  \item{mass_dat}{A character string; the name of the column in \code{dat} containing the \emph{m/z} data for the spectrum.
}
  \item{norm_mz}{Numeric; the \emph{m/z} peak to which the spectral intensity should be normalized to. Value should have the same number of decimal places as the \emph{m/z} data in \code{dat}.
}
  \item{spectra_cols}{A character string; the names of the column in \code{dat} containing the intensity data for the spectrs to be analyzed.}
  \item{showHI}{A Boolean variable; clipping argument for results greater than 1 after normalization.}
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

## Normalize the intensity of peaks in "Before1" to the intensity of the peak at 255.23
ex <- normSpectra(dat = Master2, mass_dat = "full_mz", method = "custom", norm_mz = 255.23, 
            spectra_cols = "Before1")

## Normalize the intensity of peaks in 6 spectra to the intensity of the peak at 255.23
ex <- normSpectra(dat = Master2, mass_dat = "full_mz", method = "custom", 
            norm_mz = 255.23, spectra_cols = c("Blank1", "Blank2", "Before1", 
            "Before2", "After1", "After2"))

}
\keyword{ methods }
\keyword{ manip }
