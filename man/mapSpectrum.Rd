\name{mapSpectrum}
\alias{mapSpectrum}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Map Spectrum to \emph{m/z} Vector
}
\description{
Fills in the columns of the empty data frame created using \code{createSpecDF}. Mass to charge values from pairwise spectral data are compared to the \code{full_mz} vector. All peaks within the \code{thresh} of one another are binned, and only the maximum intensity of that bin is filled into the mapped spectrum.
}
\usage{
tmp <- mapSpectrum(dat, massCol, intenseCol, dig = 4, thresh = 5e-5 spec_df, colName)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{a pairwise data frame containing your spectral data. Should contain two columns: one for \emph{m/z} and one for intensity.}
  \item{massCol}{a character string; the name of the \emph{m/z} column in \code{dat}.}
  \item{intenseCol}{a character string; the name of the intensity column in \code{dat}.}
  \item{dig}{number of decimal places to round the \emph{m/z} data to; must match the same value used in \code{createSpecDF} in order for the columns to fill. Default = 4.}
  \item{thresh}{single numeric value; all \emph{m/z} values within \code{thresh} of each other are binned under that with the maximum intensity. Default = 5e-5.}
  \item{spec_df}{an empty data frame created using \code{createSpecDF}.}
  \item{colName}{a character string; the name of the column that should be filled with the spectral data.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a vector that is used to fill in the \code{colName} column of the mapped spectral data frame.
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
\code{\link{createSpecDF}}
}
\examples{
## Create example data frame of mass and intensity data
mass <- c(200, 400, 600, 800)
Intensity <- c(30958223, 209348208391, 203948589, 90234032)
blank_spec <- data.frame(mass, Intensity)

## Create empty spectral data frame to map to
spec_df <- createSpecDF(min_mz = 100, nax_mz = 1000, res = 0.0001, dig = 4, Blank = Blank1)

## Map binary spectral data to empty spectral data frame
spec_df <- mapSpectrum(dat = spec, massCol = "mass", intenseCol = "Intensity", spec_df = spec_df, colName = "Blank1")
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
