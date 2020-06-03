\name{avgSpectra}
\alias{avgSpectra}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Average Spectral Replicates
}
\description{
Combines spectral replicates either by averaging (method = "mean") or summing (method = "sum) the intensity values across each row representing a mass-to-charge value in \code{full_mz}.
}
\usage{
avgSpectra(dat, method)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{The mapped spectral data frame, containing \code{full_mz} in the first column.}
  \item{method}{A character string; the method used to combine the spectra. Methods include "sum" and "mean". Default = "mean."}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a vector of intensity values that are printed to the "Average" column in the corresponding rows of the \code{full_mz} vector in the mapped spectral data frame.
}
\references{
https://github.com/wesleyburr/subMaldi
}
\author{
Kristen Yeh <kristenyeh@trentu.ca>
}
\note{
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{createSpecDF}}, \code{\link{mapSpectrum}}
}
\examples{
## Load sample datasets
data("Before1")
data("After1")

## Create empty spectral data frame to map to
spec_df <- createSpecDF(min_mz = 100, max = 1000, res = 0.0001, dig = 4)

## Map binary spectral data to empty spectral data frame
spec_df <- mapSpectrum(dat = Blank_01, massCol = "mass", intenseCol = "Intensity", spec_df = spec_df, colName = "Blank1")
spec_df <- mapSpectrum(dat = Blank_02, massCol = "mass", intenseCol = "Intensity", spec_df = spec_df, colName = "Blank2")

## Average the two spectral replicates
spec_df <- avgSpectra(dat = spec_df, method = "mean")
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
