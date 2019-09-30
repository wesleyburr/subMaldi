\name{subSpectra}
\alias{subSpectra}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Subtract Blank Peaks from Sample Spectra
}
\description{
This function takes the intensity values in the blank column of the mapped spectral data frame and subtracts them from the intensity values in the sample column. If the intensity value in the blank spectrum is greater than that in the sample spectrum, the intensity of the sample peak is given an intensity of 0.
}
\usage{
subSpectra(dat, Blank_Var, Sample, Sub_Sample)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{the spectral data frame, containing \code{full_mz} in the first column and intensity data in the subsequent columns.}
  \item{Blank_Var}{a character string; the name of the blank column that will be subtracted from the sample column.}
  \item{Sample}{a character string; the name of the sample column that the blank data will be subtracted from.}
  \item{Sub_Sample}{a character string; the name of the column to be filled with the subtracted spectrum.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a vector of intensity values that are filled into the \code{Sub_Sample} column of the mapped data frame at the corresponding rows of \code{full_mz}.
}
\references{
%% ~put references to the literature/web site here ~
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
\code{\link{createSpecDF}}, \code{\link{mapSpectrum}}
}
\examples{
## Example data frames of mass and intensity data
mass <- c(200, 400, 600, 800)
Intensity <- c(30958223, 209348208391, 203948589, 90234032)
samp1_spec <- data.frame(mass, Intensity)

mass <- c(200, 250, 400, 450, 600, 650, 800, 850)
Intensity <- c(349540386, 9685986, 9222222, 92034985, 309483458, 348534985, 29358425, 209358295)
samp2_spec <- data.frame(mass, Intensity)

## Create empty spectral data frame to map to
spec_df <- createSpecDF(min_mz = 100, max_mz = 1000, res = 0.0001, dig = 4)

## Map binary spectral data to empty spectral data frame
spec_df <- mapSpectrum(dat = samp1_spec, massCol = "mass", intenseCol = "Intensity", spec_df = spec_df, colName = "Samp1")
spec_df <- mapSpectrum(dat = samp2_spec, massCol = "mass", intenseCol = "Intensity", spec_df = spec_df, colName = "Samp2")

## Subtract samp1 from samp2
spec_df <- subSpectra(dat = spec_df, Blank_Var = "Samp1", Sample = "Samp2", Sub_Sample = "Sub_Samp2")
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
