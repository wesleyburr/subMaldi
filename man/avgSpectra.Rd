\name{avgSpectra}
\alias{avgSpectra}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Average Spectral Replicates
}
\description{
Combines spectral replicates either by averaging (method = "mean") or summing (method = "sum) the intensity values across each row representing a mass-to-charge value in \code{full_mz}. Capable of averaging 1-6 spectra at once.
}
\usage{
avgSpectra(dat, method = "mean", spec1, spec2, spec3 = NULL, spec4 = NULL, spec5 = NULL, spec6 = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{The mapped spectral data frame, containing \code{full_mz} in the first column.}
  \item{method}{A character string; the method used to combine the spectra. Methods include "sum" and "mean". Default = "mean."}
  \item{spec1}{A character string; the name of the column in \code{dat} containing the intensity data for the first spectrum to be averaged.}
  \item{spec2}{A character string; the name of the column in \code{dat} containing the intensity data for the second spectrum to be averaged. Default = NULL.}
  \item{spec3}{A character string; the name of the column in \code{dat} containing the intensity data for the third spectrum to be averaged. Default = NULL.}
  \item{spec4}{A character string; the name of the column in \code{dat} containing the intensity data for the fourth spectrum to be averaged. Default = NULL.}
  \item{spec5}{A character string; the name of the column in \code{dat} containing the intensity data for the fifth spectrum to be averaged. Default = NULL.}
  \item{spec6}{A character string; the name of the column in \code{dat} containing the intensity data for the sixth spectrum to be averaged. Default = NULL.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a new column in the input data frame containing the averaged intensity data.
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
## Load sample dataset "Master.rda"
data("Master")

## Average blank spectrum 1 and 2 using the method "mean"
ex <- avgSpectra(Master, method = "mean", spec1 = "Blank1", spec2 = "Blank2")

## Average blank spectrum 1 and 2 using the method "sum"
ex <- avgSpectra(Master, method = "sum", spec1 = "Blank1", spec2 = "Blank2")

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
