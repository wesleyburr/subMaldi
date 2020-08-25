\name{find_max}
\alias{find_max}
\title{
Find Peak Maxima
}
\description{
Analyzes spectral data and returns a list of the most intense peak in each spectrum, including the \emph{m/z} value associated with the peak. Able to determine the maxima of 1-6 spectra at once.
}
\usage{
find_max(dat, mass_dat, spec1, spec2 = NULL, spec3 = NULL, 
          spec4 = NULL, spec5 = NULL, spec6 = NULL)
}
\arguments{
  \item{dat}{The name of the spectral data frame, containing \code{m/z} data in the first column and spectral intensity data in subsequent columns.}
  \item{mass_dat}{A character string; the name of the column in \code{dat} containing the \emph{m/z} data for the spectrum.}
  \item{spec1}{A character string; the name of the column in \code{dat} containing the intensity data for the spectrum to be analyzed.}
  \item{spec2}{A character string; the name of the column in \code{dat} containing the intensity data for the second spectrum to be analyzed. Default = NULL.}
  \item{spec3}{A character string; the name of the column in \code{dat} containing the intensity data for the third spectrum to be analyzed. Default = NULL.}
  \item{spec4}{A character string; the name of the column in \code{dat} containing the intensity data for the fourth spectrum to be analyzed. Default = NULL.}
  \item{spec5}{A character string; the name of the column in \code{dat} containing the intensity data for the fifth spectrum to be analyzed. Default = NULL.}
  \item{spec6}{A character string; the name of the column in \code{dat} containing the intensity data for the sixth spectrum to be analyzed. Default = NULL.}
}

\value{Returns a data frame indidcating the most intense peaks of each input spectrum. Indicates the spectrum the data is from, the \emph{m/z} value associated with the peak, and the intensity of the maxima.
}
\references{
https://github.com/wesleyburr/subMaldi
}
\author{
Kristen Yeh <kristenyeh@trentu.ca>
Wesley Burr <wburr@trentu.ca>
}

\examples{
## Load sample dataset "Master.rda"
data("Master")

## Find maxima of a single spectrum
find_max(dat = Master, mass_dat = "full_mz", spec1 = "Blank1")

## Find maxima of four spectra
find_max(dat = Master, mass_dat = "full_mz", spec1 = "Blank1", 
          spec2 = "Before1", spec3 = "After1", spec4 = "After2")
}

\keyword{ methods }
