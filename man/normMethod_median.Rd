\name{norm_median}
\alias{norm_median}
\title{
Normalize a Spectral Dataset to its Median Intensity
}
\description{
Called internally by \code{normSpectra()}. Evaluates the median of each spectrum in a dataset after removing 0 values introduced by mapping. If the medians are not equal between spectra, a normalization factor is applied to the spectral intensities until all median intensities in the dataset are equal. Capable of normalizing 2-6 spectra at once.
}
\usage{
norm_median(dat, mass_dat, spectra_cols)
}
\arguments{
  \item{dat}{The name of the spectral data frame, containing \code{m/z} data in the first column and spectral intensity data in subsequent columns.}
  \item{mass_dat}{A character string; the name of the column in \code{dat} containing the \emph{m/z} data for the spectrum.}
  \item{spectra_cols}{A character string; the names of the column in \code{dat} containing the intensity data for the spectrs to be analyzed.}
}
\value{
Returns a new data frame including the original \emph{m/z} data and normalized intensity data.
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

## Normalize spectra "Before1" and "Before2" to their median intensity
ex <- normSpectra(dat = Master, mass_dat = "full_mz", method = "median", 
            spectra_cols = c("Before1", "Before2"))

}

\keyword{ methods }
\keyword{ manip }
