\name{rmveEmpty}
\alias{rmveEmpty}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Remove Empty Rows
}
\description{
Data frames created using createSpecDF can have thousands to millions of rows, especially when dealing with high resolution mass spectrometry data. This can be quite taxing to the speed of executing certain functions, especially when it comes to visualizing data. This function removes empty rows to reduce the computational load and increase the ease of use of functions on the data frame. Any row where all elements (but the first, which is \code{full_mz}) equal 0 are removed from the data frame.
}
\usage{
rmveEmpty(dat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{the spectral data frame, containing \code{full_mz} in the first column, to be trimmed of all-zero rows.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns the input data frame without its all-zero rows.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Kristen Yeh <kristenyeh@trentu.ca>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{createSpecDF}},\code{\link{mapSpectrum}}
}
\examples{
## Example data frame of mass and intensity data
mass <- c(200, 400, 600, 800)
Intensity <- c(30958223, 209348208391, 203948589, 90234032)
blank_spec <- data.frame(mass, Intensity)

## Create empty spectral data frame to map to
spec_df <- createSpecDF(min_mz = 100, max_mz = 1000, res = 0.0001, dig = 4, Blank = Blank1)

## Map binary spectral data to empty spectral data frame
spec_df <- mapSpectrum(dat = spec, massCol = "mass", intenseCol = "Intensity", spec_df = spec_df, colName = "Blank1")

## Use rmveEmpty(x) on those data frames to reduce computational load for use with
# other functions and packages
spec_df <- rmveEmpty(dat = spec_df)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
