\name{.normMethod_median}
\alias{.normMethod_median}
\title{
Normalize a Spectral Dataset to its Median Intensity
}
\description{
Called internally by \code{normSpectra()}. Evaluates the median of each spectrum in a dataset after removing 0 values introduced by mapping. If the medians are not equal between spectra, a normalization factor is applied to the spectral intensities until all median intensities in the dataset are equal. Capable of normalizing 2-6 spectra at once.
}
\usage{
.normMethod_median(dat, mass_dat, spec1, spec2 = NULL, 
                  spec3 = NULL, spec4 = NULL, spec5 = NULL, spec6 = NULL)
}
\arguments{
  \item{dat}{The name of the spectral data frame, containing \code{m/z} data in the first column and spectral intensity data in subsequent columns.}
  \item{mass_dat}{A character string; the name of the column in \code{dat} containing the \emph{m/z} data for the spectrum.}
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
\value{
Returns a new data frame including the original \emph{m/z} data and normalized intensity data.
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
## Load sample dataset "Master.rda"
data("Master")

## Normalize spectra "Before1" and "Before2" to their median intensity
ex <- normSpectra(dat = Master, mass_dat = "full_mz", method = "median", 
            spec1 = "Before1", spec2 = "Before2")

}

\keyword{ methods }
\keyword{ manip }
