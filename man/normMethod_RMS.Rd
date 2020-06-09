\name{normMethod_RMS}
\alias{normMethod_RMS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Normalize Spectral Intensity by Root Mean Square
}
\description{
Called internally by \code{normSpectra}. Normalizes the intensity data of each input spectrum by dividing each peak's intensity by RMS error. Capable of normalizing 6 spectra to the same \emph{m/z} value at once.
}
\usage{
normMethod_RMS(dat, mass_dat, spec1, spec2 = NULL, spec3 = NULL, spec4 = NULL, spec5 = NULL, spec6 = NULL)
}
%- maybe also 'usage' for other objects documented here.
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

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{normSpectra}}
}
\examples{
## Load sample dataset "Master2.rda"
data("Master2")

## Normalize spectrum "Before1" using the RMS method
normSpectra(dat = Master2, mass_dat = "full_mz", method = "RMS", spec1 = "Before1")


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
