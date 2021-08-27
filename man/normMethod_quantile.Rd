\name{norm_quantile}
\alias{norm_quantile}
\title{
Normalize Distribution of Intensities
}
\description{
Called internally by \code{normSpectra()}. Normalizes the distributions of the values in each spectrum in a set.  Sorts the intensity data of each spectrum and evaluates the average intensity for each rank. The intensity values are then replaces with the averaged intensities, rearranged in their original order. Capable of analyzing 2-6 spectra at once.
}
\usage{
norm_quantile(dat, mass_dat, spectra_cols)
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
\examples{
## Load sample dataset "Master.rda"
data("Master")

## Normalize the spectra "Before1" and "Before2"
# using the quantile method
ex <- normSpectra(dat = Master, mass_dat = "full_mz", method = "quantile",
                  spectra_cols = c("Before1", "Before2"))

}

\keyword{ methods }
\keyword{ manip }
