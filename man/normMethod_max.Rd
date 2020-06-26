\name{.normMethod_max}
\alias{.normMethod_max}
\title{
Normalization Method: Maximum Intensity
}
\description{
Called internally by \code{normSpectra}. Normalizes the intensity data of spectra to a scale of 0,1. Capable of normalizing 1-6 spectra, to their individual maxima, at once.
}
\usage{
.normMethod_max(dat, mass_dat, spec1, spec2, spec3 = NULL, 
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
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{Returns a new data frame including the original \emph{m/z} data and normalized intensity data.
}
\references{
https://github.com/wesleyburr/subMaldi
}
\author{
Kristen Yeh <kristenyeh@trentu.ca>
Wesley Burr <wburr@trentu.ca>
}
\note{
%%  ~~further notes~~
}

\seealso{
\code{\link{normSpectra}}
}
\examples{
## Load sample dataset "Master2.rda"
data("Master2")

## Normalize intensity data for three spectra
normSpectra(dat = Master2, mass_dat = "full_mz", method = "max", 
            spec1 = "Blank1", spec2 = "Before1", spec3 = "After1")
}
\keyword{ methods }
\keyword{ manip }
