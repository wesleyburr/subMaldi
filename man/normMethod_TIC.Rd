\name{norm_TIC}
\alias{norm_TIC}
\title{
Normalize Peak Intensity by Total Ion Current
}
\description{
Called internally by \code{normSpectra}. Evaluates the sum of all intensities (TIC) of each spectrum in a dataset. If the TIC of all spectra are not equal, their intensities are multiplied by a normalization factor. Capable of normalizing 2-6 spectra at once.
}
\usage{
norm_TIC(dat, mass_dat, spectra_cols)
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

# Normalize the TIC of "Before1" and "Before2"
ex <- normSpectra(dat = Master2, mass_dat = "full_mz", method = "TIC", 
            spectra_cols = c("Before1", "Before2"))

}

\keyword{ methods }
\keyword{ manip }
