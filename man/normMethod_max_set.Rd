\name{normMethod_max_set}
\alias{normMethod_max_set}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Normalization Method: Maximum Intensity of Set
}
\description{
Normalizes the intensity data of spectra to a scale of 0,1, where 1 is the single most intense peak of the spectral set. Capable of normalizing 1-6 spectra at once.
}
\usage{
normMethod_max_set(dat, mass_dat, spec1, spec2, spec3 = NULL, spec4 = NULL, spec5 = NULL, spec6 = NULL)
}
%- maybe also 'usage' for other objects documented here.
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
\value{
Returns a new data frame including the original \emph{m/z} data and normalized intensity data of each input spectrum.
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## Load sample datasat "Master.rda"
data("Master")

## Normalize the intensity data of two samples to the maximum of the blank
normMethod_max_set(dat = Master, mass_dat = "full_mz", spec1 = "Blank1", spec2 = "Before1", spec3 = "Before2") 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
