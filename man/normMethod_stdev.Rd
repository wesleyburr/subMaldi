\name{.normMethod_stdev}
\alias{.normMethod_stdev}
\title{
Normalize Spectrum to Standard Deviation of Noise
}
\description{
Called internally by \code{normSpectra()}.Evaluates the standard deviation of intensity values within the same noisy region (a region lacking peaks) of each spectrum. All intensities in each spectrum are then divided by the spectrum's standard deviation in the noise region. All output spectra should have a standard deviation of 1 in the selected region. Capable of normalizing 1-6 spectra at once.
}
\usage{
.normMethod_stdev(dat, mass_dat, lower = 900, upper= 1100 , 
                  spec1, spec2 = NULL, spec3 = NULL, spec4 = NULL, 
                  spec5 = NULL, spec6 = NULL)
}
\arguments{
  \item{dat}{The name of the spectral data frame, containing \code{m/z} data in the first column and spectral intensity data in subsequent columns.}
  \item{mass_dat}{A character string; the name of the column in \code{dat} containing the \emph{m/z} data for the spectrum.}
  \item{lower}{Single numeric value; the lower \emph{m/z} bound of the noisy region in all spectra.}
  \item{upper}{Single numeric value; the lower \emph{m/z} bound of the noisy region in all spectra.}
  \item{spec1}{A character string; the name of the column in \code{dat} containing the intensity data for the spectrum to be analyzed.}
  \item{spec2}{A character string; the name of the column in \code{dat} containing the intensity data for the second spectrum to be normalized. Default = NULL.}
  \item{spec3}{A character string; the name of the column in \code{dat} containing the intensity data for the third spectrum to be normalized. Default = NULL.}
  \item{spec4}{A character string; the name of the column in \code{dat} containing the intensity data for the fourth spectrum to be normalized. Default = NULL.}
  \item{spec5}{A character string; the name of the column in \code{dat} containing the intensity data for the fifth spectrum to be normalized. Default = NULL.}
  \item{spec6}{A character string; the name of the column in \code{dat} containing the intensity data for the sixth spectrum to be normalized. Default = NULL.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{Returns a new data frame including the original \emph{m/z} data and normalized intensity data.
}
\references{
https://github.com/wesleybur/subMaldi
}
\author{
Kristen Yeh <kristenyeh@trentu.ca>
Wesley Burr <wburr@trentu.ca>
}
\note{
%%  ~~further notes~~
}

\seealso{
code{\link{normSpectra}}
}
\examples{
## Load sample dataset "Master.rda"
data("Master")

## Normalize spectra "After1" and "After2" 
# using the noisy region from m/z 900 to 1100
ex <- normSpectra(Master, mass_dat = "full_mz", method = "stdev",
            lower = 900, upper = 1100, spec1 = "After1", spec2 = "After2")
}

\keyword{ methods }
\keyword{ manip }
