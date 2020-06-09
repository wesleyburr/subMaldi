\name{normMethod_custom}
\alias{normMethod_custom}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Normalize Peak Intensity to Custom \emph{m/z}
}
\description{
Called internally by \code{normSpectra}. Normalizes the intensity data of each input spectrum to the intensity of the selected \emph{m/z} peak. Capable of normalizing 6 spectra to the same \emph{m/z} value at once.
}
\usage{
normMethod_custom(dat, mass_dat, norm_mz, spec1, spec2 = NULL, spec3 = NULL, spec4 = NULL, spec5 = NULL, spec6 = NULL)
}
\arguments{
  \item{dat}{The name of the spectral data frame, containing \code{m/z} data in the first column and spectral intensity data in subsequent columns.
}
  \item{mass_dat}{A character string; the name of the column in \code{dat} containing the \emph{m/z} data for the spectrum.
}
  \item{norm_mz}{Numeric; the \emph{m/z} peak to which the spectral intensity should be normalized to. Value should have the same number of decimal places as the \emph{m/z} data in \code{dat}.
}
  \item{spec1}{A character string; the name of the column in \code{dat} containing the intensity data for the first spectrum to be normalized.}
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

## Normalize the intensity of peaks in "Before1" to the intensity of the peak at 255.23
normMethod_custom(dat = Master2, mass_dat = "full_mz", norm_mz = 255.23, spec1 = "Before1")

## Normalize the intensity of peaks in 6 spectra to the intensity of the peak at 255.23
normMethod_custom(dat = Master2, mass_dat = "full_mz", norm_mz = 255.23, spec1 = "Blank1", spec2 = "Blank2", spec3 = "Before1", spec4 = "Before2", spec5 = "After1", spec6 = "After2")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
